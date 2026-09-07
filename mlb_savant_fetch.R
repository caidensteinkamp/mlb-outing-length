# =============================================================================
# mlb_savant_fetch.R -- pull league-wide Baseball Savant pitch data by date chunk
#
#   Rscript mlb_savant_fetch.R 2025 2026
#
# Two things force the shape of this script.
#
# 1. R's own networking (httr / curl bindings) is firewall-blocked on this
#    machine, so every request shells out to the system curl binary.
#
# 2. The statcast_search CSV endpoint caps a response at 25,000 rows and it
#    truncates SILENTLY -- no error, no warning, no flag in the payload. It
#    drops from the EARLIEST date in the window (results come back newest
#    first), so a too-wide window doesn't fail, it just quietly returns a
#    season with holes in it. A 7-day window in May 2025 came back at exactly
#    24,999 rows having thrown away all of May 1st.
#
#    At ~4,000 pitches a day, 3-day windows land near 12,000 -- comfortably
#    clear -- and any chunk that still comes back at the cap is bisected and
#    re-pulled rather than trusted. That guard is the whole reason to prefer
#    this over one big request.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

RAW_DIR <- Sys.getenv("OL_RAW_DIR",
                      "/Users/caidensteinkamp/Downloads/savant_data/mlb_raw")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

ROW_CAP    <- 25000   # the endpoint's hard ceiling
CHUNK_DAYS <- 3
SLEEP_SEC  <- 1.5     # hammering it three-at-once got connections dropped

# How many trailing days are treated as never-final. Any chunk whose END date
# falls inside this window is re-fetched even if it is already cached.
#
# This exists because caching a trailing chunk once is actively wrong, and it
# fails SILENTLY. The first run of this script happened mid-morning on
# 2026-09-03, before that day's games had posted, so the chunk
# `2026-09-03_2026-09-03.rds` was cached holding ZERO pitches. On a nightly
# schedule that cache entry would be honoured forever: September 3rd would
# never backfill, every later day would land in its own already-"complete"
# chunk, and the leaderboard would quietly freeze while the job kept reporting
# success. Savant also revises recent days -- suspended games get completed,
# late games post after midnight UTC -- so a few days of overlap is correct
# regardless of what time the job runs.
FRESH_DAYS <- as.integer(Sys.getenv("OL_FRESH_DAYS", "4"))

# Regular-season date spans. Ends are clamped to today for a season in progress.
# A season with no entry falls back to a deliberately generous window, so this
# does not need editing every March to keep the scheduled job alive.
SEASON_SPAN <- list(
  `2024` = c("2024-03-20", "2024-09-30"),
  `2025` = c("2025-03-18", "2025-09-29"),
  `2026` = c("2026-03-25", "2026-09-30")
)
season_span <- function(season) {
  s <- SEASON_SPAN[[as.character(season)]]
  if (!is.null(s)) return(s)
  message("  no span configured for ", season, " -- using Mar 1 .. Nov 1")
  c(sprintf("%s-03-01", season), sprintf("%s-11-01", season))
}

# Only the columns the outing-length model actually uses. Savant hands back ~118
# columns; keeping all of them across two seasons is ~1.5 GB of CSV for no gain,
# so each chunk is reduced on arrival and stored as compact RDS.
KEEP <- c(
  # game / sequence identity -- the DP needs pitches in exact order
  "game_pk", "game_date", "game_year", "game_type",
  "inning", "inning_topbot", "at_bat_number", "pitch_number",
  # who
  "pitcher", "player_name", "p_throws", "batter", "stand",
  "home_team", "away_team",
  # count / base-out state
  "balls", "strikes", "outs_when_up", "on_1b", "on_2b", "on_3b",
  # what happened
  "events", "description", "type", "bb_type",
  # pitch characteristics
  "pitch_type", "pitch_name", "release_speed", "release_spin_rate",
  "release_extension", "pfx_x", "pfx_z", "plate_x", "plate_z",
  "sz_top", "sz_bot",
  # contact quality / run value
  "launch_speed", "launch_angle", "estimated_woba_using_speedangle",
  "woba_value", "woba_denom", "delta_run_exp",
  # scoring, for runs-allowed per PA
  "bat_score", "post_bat_score", "fld_score", "post_fld_score",
  # batted-ball location and defensive alignment, for the spray run-value model.
  # hc_x/hc_y are STRINGER-placed pixel coordinates on a Gameday field image --
  # good for DIRECTION, useless as a distance (they disagree with
  # hit_distance_sc by a median of 30 ft because one is where the ball was
  # fielded and the other is projected flight).
  "hc_x", "hc_y", "hit_distance_sc",
  "if_fielding_alignment", "of_fielding_alignment"
)

# Cache schema version, derived from KEEP itself.
#
# Chunks are cached as reduced RDS holding only the KEEP columns, so ADDING a
# column silently invalidates every cached chunk -- rbindlist(fill = TRUE) would
# happily glue old chunks (no hc_x) to new ones and hand back a season whose
# coordinates are NA for everything downloaded before the change. Nothing errors;
# the model just quietly fits on a fraction of the data.
#
# Hashing the column set into the cache path means a KEEP change starts a clean
# cache automatically instead of depending on someone remembering to purge it.
SCHEMA_TAG <- local({
  f <- tempfile(); on.exit(unlink(f), add = TRUE)
  writeLines(paste(sort(KEEP), collapse = ","), f)
  substr(unname(tools::md5sum(f)), 1, 8)
})

savant_chunk_url <- function(season, from, to) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv",
    "?all=true&type=details",
    "&hfSea=", season, "%7C",
    "&hfGT=R%7C",                      # regular season only
    "&game_date_gt=", from,
    "&game_date_lt=", to
  )
}

# One window -> data.table of kept columns. Returns NULL if the window is empty
# (off-days, All-Star break) and errors only on a genuine transport failure.
pull_window <- function(season, from, to, tries = 4) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  ok <- FALSE
  for (attempt in seq_len(tries)) {
    st <- system2("curl", c("-sS", "--fail", "--compressed",
                            "--max-time", "300",
                            "--retry", "2", "--retry-delay", "3",
                            "-o", shQuote(tmp),
                            shQuote(savant_chunk_url(season, from, to))),
                  stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(st, "status")) && attr(st, "status") != 0) {
      message("    attempt ", attempt, " failed; backing off")
      Sys.sleep(SLEEP_SEC * 2 * attempt)
      next
    }
    if (file.exists(tmp) && file.size(tmp) > 200) { ok <- TRUE; break }
    Sys.sleep(SLEEP_SEC * 2 * attempt)
  }
  if (!ok) stop("curl failed for ", from, " .. ", to)

  x <- tryCatch(fread(tmp, showProgress = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)

  # THE GUARD. At the cap we cannot tell a complete window from a truncated
  # one, so treat it as truncated and bisect. Recursion bottoms out at a single
  # day; a single day over 25,000 pitches has never happened (league record is
  # ~5,600) but if it ever does we want to hear about it, not paper over it.
  if (nrow(x) >= ROW_CAP - 1) {
    if (from == to)
      stop("single day ", from, " hit the row cap -- cannot subdivide further")
    message("    ", from, "..", to, " hit the cap (", nrow(x), ") -- bisecting")
    d1 <- as.Date(from); d2 <- as.Date(to)
    mid <- d1 + floor(as.numeric(d2 - d1) / 2)
    a <- pull_window(season, as.character(d1),      as.character(mid))
    Sys.sleep(SLEEP_SEC)
    b <- pull_window(season, as.character(mid + 1), as.character(d2))
    return(rbindlist(list(a, b), use.names = TRUE, fill = TRUE))
  }

  have <- intersect(KEEP, names(x))
  miss <- setdiff(KEEP, names(x))
  if (length(miss)) message("    missing columns: ", paste(miss, collapse = ", "))
  x[, ..have]
}

fetch_season <- function(season, force = FALSE) {
  out   <- file.path(RAW_DIR, sprintf("statcast_%s.rds", season))
  span  <- season_span(season)
  start <- as.Date(span[1])
  end   <- min(as.Date(span[2]), Sys.Date())

  # The season file is only reusable once the SEASON is over. Skipping on mere
  # existence was the outer half of the trailing-cache bug: a nightly run found
  # statcast_2026.rds already there and returned in 0.19s without ever reaching
  # the chunk logic, so the fix below it never got a chance to run. A finished
  # season (2025) still short-circuits here, which is the point.
  if (file.exists(out) && !force && end < (Sys.Date() - FRESH_DAYS)) {
    message("cached (season complete): ", out); return(out)
  }
  edges <- seq(start, end, by = paste(CHUNK_DAYS, "days"))

  message("== season ", season, ": ", start, " .. ", end,
          " (", length(edges), " chunks)")

  # Each chunk is cached to its own file. The first version of this script held
  # all 66 chunks in memory and assembled at the end; a crash on the LAST chunk
  # threw away the entire 40-minute download. Caching per chunk makes a re-run
  # cost only what actually failed.
  cdir <- file.path(RAW_DIR, "chunks", as.character(season), SCHEMA_TAG)
  dir.create(cdir, showWarnings = FALSE, recursive = TRUE)

  parts <- vector("list", length(edges))
  for (i in seq_along(edges)) {
    from <- edges[i]
    to   <- min(from + (CHUNK_DAYS - 1), end)
    cf   <- file.path(cdir, sprintf("%s_%s.rds", from, to))

    # A chunk is only final once its last day is well behind us.
    settled <- to < (Sys.Date() - FRESH_DAYS)

    if (file.exists(cf) && settled) {
      res <- readRDS(cf)
      message(sprintf("  [%2d/%2d] %s .. %s  cached (%d)",
                      i, length(edges), from, to, nrow(res)))
    } else {
      message(sprintf("  [%2d/%2d] %s .. %s%s", i, length(edges), from, to,
                      if (file.exists(cf)) "  refetch (not settled)" else ""))
      res <- pull_window(season, as.character(from), as.character(to))
      # An empty SETTLED window is a real answer (off-days, the All-Star break)
      # and is worth caching. An empty unsettled one usually just means the
      # games haven't posted yet, so it is written but will be re-fetched on the
      # next run rather than being taken as final.
      saveRDS(if (is.null(res)) data.table() else res, cf)
      message("          ", if (is.null(res)) 0 else nrow(res), " pitches")
      Sys.sleep(SLEEP_SEC)
    }
    # `parts[[i]] <- NULL` DELETES element i and shifts everything after it
    # down, which is how the original run walked off the end of its own list on
    # the final chunk. `parts[i] <- list(res)` preserves the slot.
    parts[i] <- list(res)
  }

  parts <- parts[vapply(parts, function(x) !is.null(x) && nrow(x) > 0, TRUE)]
  all <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  # A pitch is uniquely keyed by game x at-bat x pitch number. Overlapping
  # window edges would double-count, so dedupe on the key rather than trusting
  # the arithmetic.
  before <- nrow(all)
  all <- unique(all, by = c("game_pk", "at_bat_number", "pitch_number"))
  message("  ", before, " rows -> ", nrow(all), " unique pitches",
          " across ", uniqueN(all$game_pk), " games")

  saveRDS(all, out, compress = "xz")
  message("wrote ", out, " (", format(file.size(out), big.mark = ","), " bytes)")
  out
}

# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
seasons <- if (length(args)) args else c("2025", "2026")
for (s in seasons) fetch_season(s)
