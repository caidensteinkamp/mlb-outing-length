# =============================================================================
# mlb_outing_length.R -- Outing Length Value, rebuilt on Baseball Savant
#
#   Rscript mlb_outing_length.R
#
# This is the Butler outing-length framework (pitch economy -> removal hazard ->
# backward-induction outing length -> per-pitch OLV) pointed at league-wide MLB
# Statcast data, and asked a DIFFERENT question.
#
# The Butler build was prescriptive: it took ~160 pitchers and told each one
# where to aim. That only works because we control those pitchers. Here the
# question is descriptive -- WHO IS ALREADY GOOD AT THIS -- so the aim/shape/
# command machinery is dropped entirely and replaced by a ranking layer.
#
# The headline metric is new, and it exists because the obvious metric is bad.
# "Innings per start" is mostly a measure of the MANAGER: leash length, bullpen
# quality, roster construction, September shutdowns. It tells you who WAS
# ALLOWED to go deep, not who EARNED it. So instead:
#
#   xOuts/start = solve the outing-length DP using each pitcher's OWN per-pitch
#                 event rates, under the LEAGUE-AVERAGE removal hazard.
#
# Holding the manager constant is the whole trick. Two pitchers with identical
# stuff and different managers get the same xOuts; two pitchers with the same
# manager and different pitch economy do not. The gap between actual outs/start
# and xOuts/start then becomes its own readable quantity -- the leash.
#
# Everything is reported on two axes, never one. Outing length ALONE always
# prefers pitching to contact: a ground ball is the cheapest out in baseball,
# so a pitcher who lets everything get put in play looks maximally efficient
# right up until you notice the runs. Every table therefore carries runs saved
# per 100 pitches beside OLV, and the leaderboard that matters is the one where
# both are good.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
})

RAW_DIR <- "/Users/caidensteinkamp/Downloads/savant_data/mlb_raw"
OUT_RDS <- "/Users/caidensteinkamp/Downloads/Code/mlb_outing_bundle.rds"
SEASONS <- c("2025", "2026")

# The scheduled GitHub Actions job runs this on a fresh Linux runner with no
# /Users/caidensteinkamp anywhere, so every path below is overridable. Defaults
# stay pointed at the local machine so interactive use is unchanged.

MIN_STARTS  <- 12    # below this the per-pitcher event rates are noise

# Overrides so the whole pipeline can be dry-run against a few days of pitches
# while the real pull is still downloading. Nothing here changes a full run.
if (nzchar(Sys.getenv("OL_RAW_DIR")))    RAW_DIR    <- Sys.getenv("OL_RAW_DIR")
if (nzchar(Sys.getenv("OL_OUT_RDS")))    OUT_RDS    <- Sys.getenv("OL_OUT_RDS")
if (nzchar(Sys.getenv("OL_SEASONS")))    SEASONS    <- strsplit(Sys.getenv("OL_SEASONS"), ",")[[1]]
if (nzchar(Sys.getenv("OL_MIN_STARTS"))) MIN_STARTS <- as.integer(Sys.getenv("OL_MIN_STARTS"))
PMAX        <- 140   # pitch counts past here are terminal
K_SHRINK    <- 400   # pseudo-counts shrinking a pitcher's count-transition
                     # rates toward the league rates for the SAME count. A
                     # qualified starter throws ~2,700 pitches spread over 12
                     # count cells, and 3-0 gets ~40 of them, so the thin cells
                     # need a prior. 400 puts roughly a 1:6 prior-to-data weight
                     # on a typical starter's overall sample.
K_BIP       <- 150   # same idea for his outs-per-ball-in-play distribution

# -----------------------------------------------------------------------------
# 1. Load and put the pitches in order
# -----------------------------------------------------------------------------
raw <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(RAW_DIR, sprintf("statcast_%s.rds", s))
  if (!file.exists(f)) stop("missing ", f, " -- run mlb_savant_fetch.R first")
  readRDS(f)
}), use.names = TRUE, fill = TRUE)

setorder(raw, game_pk, at_bat_number, pitch_number)
cat(sprintf("loaded %s pitches, %s games, %s seasons\n",
            format(nrow(raw), big.mark = ","),
            format(uniqueN(raw$game_pk), big.mark = ","),
            paste(sort(unique(raw$game_year)), collapse = "/")))

# Savant's `type` is B/S/X. That is too coarse for the DP, which has to know
# whether a strike ends the PA (a foul with two strikes does not) and whether a
# ball hit a batter. `description` carries the detail.
raw[, e := fcase(
  description %chin% c("ball", "blocked_ball", "pitchout",
                       "automatic_ball"),                            "ball",
  description %chin% c("hit_by_pitch"),                              "hbp",
  description %chin% c("called_strike", "swinging_strike",
                       "swinging_strike_blocked", "missed_bunt",
                       "swinging_pitchout", "automatic_strike"),     "strike",
  description %chin% c("foul", "foul_tip", "foul_bunt",
                       "bunt_foul_tip", "foul_pitchout"),            "foul",
  description %chin% c("hit_into_play", "hit_into_play_score",
                       "hit_into_play_no_out"),                      "inplay",
  default = NA_character_)]

unmapped <- raw[is.na(e), .N, by = description][order(-N)]
if (nrow(unmapped)) {
  cat("\nunmapped descriptions (dropped):\n"); print(head(unmapped, 10))
}
raw <- raw[!is.na(e)]

# CAREFUL. Savant's `player_name` column is NOT the pitcher in a league-wide
# `player_type=pitcher&all=true` export -- it is the BATTER. Grouping starts by
# it silently shatters each outing into one "start" per hitter faced (121 games
# came out as 2,182 starts averaging 1.7 outs, which is how this was caught).
# Pitcher identity lives only in the numeric `pitcher` id, so names come from
# the Stats API instead: one request per season, cached to disk.
# Cached PER SEASON. A single shared cache file is wrong here: whichever run
# created it fixed its coverage forever, so a 2025-only run left a cache with no
# 2026 debutants in it and the 2026 board rendered three pitchers as raw ids.
parse_people <- function(txt) {
  ids <- regmatches(txt, gregexpr('"id":[0-9]+,"fullName":"[^"]*"', txt))[[1]]
  data.table(pitcher = as.integer(sub('"id":([0-9]+).*', "\\1", ids)),
             name    = sub('.*"fullName":"([^"]*)"', "\\1", ids))
}
curl_txt <- function(url)
  paste(system2("curl", c("-sS", "--fail", "--max-time", "120", shQuote(url)),
                stdout = TRUE), collapse = "")

pnames <- rbindlist(lapply(sort(unique(raw$game_year)), function(y) {
  cf <- file.path(RAW_DIR, sprintf("player_names_%s.rds", y))
  if (file.exists(cf)) return(readRDS(cf))
  nm <- parse_people(curl_txt(sprintf(
    "https://statsapi.mlb.com/api/v1/sports/1/players?season=%s", y)))
  saveRDS(nm, cf); nm
}), use.names = TRUE)
pnames <- unique(pnames, by = "pitcher")

# Top up anyone the season rosters still miss (mid-season signings from other
# organizations show up in the pitch data before they appear on a roster pull).
missing <- setdiff(unique(raw$pitcher), pnames$pitcher)
if (length(missing)) {
  extra <- rbindlist(lapply(split(missing, ceiling(seq_along(missing) / 50)),
    function(ch) parse_people(curl_txt(sprintf(
      "https://statsapi.mlb.com/api/v1/people?personIds=%s",
      paste(ch, collapse = ","))))), use.names = TRUE)
  pnames <- unique(rbindlist(list(pnames, extra), use.names = TRUE), by = "pitcher")
  cat("topped up ", nrow(extra), " names not on a season roster pull\n", sep = "")
}
cat("resolved ", nrow(pnames), " player names",
    sprintf(" (%d pitcher ids unresolved)\n",
            length(setdiff(unique(raw$pitcher), pnames$pitcher))), sep = "")

# -----------------------------------------------------------------------------
# 2. Plate appearances, and the outs each one actually recorded
# -----------------------------------------------------------------------------
# Savant reports `outs_when_up` -- the out state ENTERING the PA -- and never
# the outs the PA produced. Differencing it against the next PA in the same
# half-inning recovers that, and it is better than reading `events`, because it
# also catches outs the batter didn't make: a caught stealing, a runner erased
# on a fielder's choice, a pickoff. Those spend the pitcher's out budget too.
#
# The last PA of a half-inning has no successor to difference against. If any
# later PA exists in the game, the half-inning was retired and it made
# (3 - outs_when_up) outs. If none does, the game ended there -- a walk-off, or
# the final out -- and only then do we fall back to reading `events`.
EVENT_OUTS <- c(
  strikeout = 1L, field_out = 1L, force_out = 1L, sac_fly = 1L, sac_bunt = 1L,
  fielders_choice_out = 1L, other_out = 1L, caught_stealing_2b = 1L,
  caught_stealing_3b = 1L, caught_stealing_home = 1L, pickoff_1b = 1L,
  pickoff_2b = 1L, pickoff_3b = 1L, strikeout_double_play = 2L,
  grounded_into_double_play = 2L, double_play = 2L, sac_fly_double_play = 2L,
  sac_bunt_double_play = 2L, triple_play = 3L)

pa <- raw[, .(
    pitcher      = first(pitcher),
    p_throws     = first(p_throws),
    game_date    = first(game_date),
    game_year    = first(game_year),
    inning       = first(inning),
    inning_topbot= first(inning_topbot),
    home_team    = first(home_team),
    away_team    = first(away_team),
    outs_before  = first(outs_when_up),
    pa_pitches   = .N,
    events       = last(events),
    bb_type      = last(bb_type),
    runs         = last(post_bat_score) - first(bat_score),
    rv           = sum(-delta_run_exp, na.rm = TRUE)   # runs saved, pitcher POV
  ), by = .(game_pk, at_bat_number)]

setorder(pa, game_pk, at_bat_number)
pa[, max_ab := max(at_bat_number), by = game_pk]
pa[, next_outs := shift(outs_before, type = "lead"),
   by = .(game_pk, inning, inning_topbot)]
pa[, outs_made := fifelse(
      !is.na(next_outs), as.integer(next_outs - outs_before),
      fifelse(at_bat_number < max_ab, as.integer(3L - outs_before),
              coalesce(EVENT_OUTS[events], 0L)))]
pa[outs_made < 0L | is.na(outs_made), outs_made := 0L]

# Sanity floor: a 9-inning game banks 54 outs over ~76 PA, so mean outs/PA has
# to land near 0.71. Anything far off means the differencing above is wrong.
cat(sprintf("\n%s plate appearances | mean outs/PA %.3f (expect ~0.71)\n",
            format(nrow(pa), big.mark = ","), mean(pa$outs_made)))

# -----------------------------------------------------------------------------
# 3. Isolate starters and their outings
# -----------------------------------------------------------------------------
# The starter is whoever threw the first pitch to the opposing lineup. A
# half-inning labelled "Top" is the away team batting, so its pitcher is the
# home club's -- but we never need the team, only the identity, so the label
# itself is enough of a key.
starters <- pa[, .SD[which.min(at_bat_number)], by = .(game_pk, inning_topbot)
              ][, .(game_pk, inning_topbot, starter = pitcher)]

pa <- merge(pa, starters, by = c("game_pk", "inning_topbot"), all.x = TRUE)
pa[, is_starter_pa := pitcher == starter]
setorder(pa, game_pk, at_bat_number)

# An outing is the starter's own contiguous opening block of PAs. Modern
# starters never re-enter, but deriving it contiguously costs nothing and means
# an opener followed later by the same arm can't silently fuse into one outing.
sp <- pa[is_starter_pa == TRUE]
setorder(sp, game_pk, inning_topbot, at_bat_number)
sp[, pa_idx       := seq_len(.N),        by = .(game_pk, pitcher)]
sp[, cum_pitches  := cumsum(pa_pitches), by = .(game_pk, pitcher)]
sp[, cum_runs     := cumsum(runs),       by = .(game_pk, pitcher)]
sp[, inning_over  := as.integer(outs_before + outs_made >= 3L)]
sp[, removed      := as.integer(pa_idx == max(pa_idx)), by = .(game_pk, pitcher)]

outings <- sp[, .(outs = sum(outs_made), pitches = sum(pa_pitches),
                  runs = sum(runs), rv = sum(rv), pa = .N),
              by = .(game_pk, pitcher, p_throws, game_year)]

# Second sanity floor: two starts per game, and a modern MLB start is ~5.2 IP on
# ~87 pitches. This is the check that caught the player_name bug.
cat(sprintf("%s starts over %s games (%.2f/game) | mean %.2f outs (%.2f IP), %.1f pitches\n",
            format(nrow(outings), big.mark = ","),
            format(uniqueN(outings$game_pk), big.mark = ","),
            nrow(outings) / uniqueN(outings$game_pk),
            mean(outings$outs), mean(outings$outs) / 3, mean(outings$pitches)))

# -----------------------------------------------------------------------------
# 4. The pitch economy of each outcome  (the premise, unchanged)
# -----------------------------------------------------------------------------
# Dividing a PA's own pitches by the outs it made is meaningless for anything
# that doesn't make an out. The cost of a walk is not infinity -- it is the
# pitches spent getting the out it failed to produce. So measure the forward
# waiting time to the next banked out, within the starter's own sequence.
setorder(sp, game_pk, pitcher, at_bat_number)
sp[, `:=`(pitches_to_out = NA_real_, outs_when_it_came = NA_real_)]
sp[, c("pitches_to_out", "outs_when_it_came") := {
    n <- .N; pw <- pa_pitches; om <- outs_made
    to_out <- rep(NA_real_, n); outs_at <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      j <- i
      while (j <= n && om[j] == 0L) j <- j + 1L
      if (j <= n) { to_out[i] <- sum(pw[i:j]); outs_at[i] <- om[j] }
      # j > n: the outing ended before another out -> censored, left NA
    }
    list(to_out, outs_at)
  }, by = .(game_pk, pitcher)]
sp[, pitches_per_out := pitches_to_out / outs_when_it_came]

OUTCOME_LABEL <- function(ev, bb) fcase(
  ev == "strikeout", "K",
  ev == "walk", "BB",
  ev == "hit_by_pitch", "HBP",
  ev == "home_run", "HR",
  ev == "triple", "3B",
  ev == "double", "2B",
  ev == "single", "1B",
  ev %chin% c("grounded_into_double_play", "double_play",
              "strikeout_double_play"), "Double play",
  ev %chin% c("field_out", "force_out", "fielders_choice_out", "other_out") &
    bb == "ground_ball", "GB out",
  ev %chin% c("field_out", "force_out", "fielders_choice_out", "other_out") &
    bb == "fly_ball", "FB out",
  ev %chin% c("field_out", "force_out", "fielders_choice_out", "other_out") &
    bb == "line_drive", "LD out",
  ev %chin% c("field_out", "force_out", "fielders_choice_out", "other_out") &
    bb == "popup", "PU out",
  ev %chin% c("sac_fly", "sac_bunt", "sac_fly_double_play",
              "sac_bunt_double_play"), "Sacrifice",
  ev %chin% c("field_error", "catcher_interf"), "Reached on error",
  ev == "fielders_choice", "Fielders choice",
  default = NA_character_)

sp[, outcome := OUTCOME_LABEL(events, bb_type)]

economy <- sp[!is.na(outcome), .(
    n               = .N,
    own_pitches     = mean(pa_pitches),
    outs            = mean(outs_made),
    runs            = mean(runs),
    pitches_to_out  = mean(pitches_to_out, na.rm = TRUE),
    pitches_per_out = mean(pitches_per_out, na.rm = TRUE),
    censored        = sum(is.na(pitches_to_out))
  ), by = outcome][order(pitches_per_out)]

cat("\n---- pitch economy of each outcome (starters) ----\n")
print(economy[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

# -----------------------------------------------------------------------------
# 5. The removal model
# -----------------------------------------------------------------------------
# Managers hook starters on a pitch budget and strongly prefer to do it between
# innings. Runs allowed adds a little in MLB (it added nothing at Butler), so it
# is offered to the model rather than assumed away.
cand <- list(
  removed ~ cum_pitches,
  removed ~ cum_pitches + inning_over,
  removed ~ cum_pitches + inning_over + cum_runs,
  removed ~ poly(cum_pitches, 2) + inning_over,
  removed ~ splines::ns(cum_pitches, 5) + inning_over,
  removed ~ splines::ns(cum_pitches, 5) + inning_over + cum_runs
)
aic <- data.frame(
  model = vapply(cand, function(f) paste(deparse(f), collapse = ""), ""),
  AIC   = vapply(cand, function(f) AIC(glm(f, data = sp, family = binomial)), 0))
cat("\n---- removal hazard model selection ----\n")
print(aic[order(aic$AIC), ], row.names = FALSE)

# The DP needs a hazard that is a function of (pitch count, inning boundary)
# only -- cum_runs is a within-outing path variable the backward induction has
# no state for, so it stays a diagnostic. It is worth knowing it matters; it is
# not worth expanding the state space to carry it.
#
# A LINEAR logit in pitch count will not do here, and the calibration table is
# what shows it: removal risk is ~0.001 under 40 pitches and ~0.47 between 90
# and 105, a threshold effect that a straight line fits by predicting the base
# rate of 0.046 everywhere. Managers do not ease starters out, they have a
# number in mind. A spline in pitch count keeps the hazard a function of the
# same two state variables while actually tracking that cliff.
haz_mod <- glm(removed ~ splines::ns(cum_pitches, 5) + inning_over,
               data = sp, family = binomial)

# Precompute the hazard on the (pitch count x inning boundary) grid the DP
# walks. The DP asks for hz() ~10^7 times per solve; a table lookup makes that
# free, and it keeps predict() away from the inner loop.
HZ_TAB <- local({
  g <- expand.grid(cum_pitches = 0:PMAX, inning_over = 0:1)
  matrix(predict(haz_mod, newdata = g, type = "response"),
         nrow = PMAX + 1, ncol = 2)
})
hz <- function(p, io) HZ_TAB[cbind(pmin(pmax(p, 0), PMAX) + 1, io + 1)]

cat("\n---- hazard calibration ----\n")
sp[, haz_pred := predict(haz_mod, type = "response")]
print(sp[, .(n = .N, observed = round(mean(removed), 3),
             predicted = round(mean(haz_pred), 3)),
         by = .(bucket = cut(cum_pitches, c(0, 40, 60, 75, 90, 105, 150)))
        ][order(bucket)])

# -----------------------------------------------------------------------------
# 6. Outing Length Expectancy by backward induction
# -----------------------------------------------------------------------------
# V(p, balls, strikes, outs_in_inning, outs_recorded) = expected outs the
# starter still records, given p pitches thrown and not yet removed. Every pitch
# increments p by exactly one, so the state space is acyclic in p and V solves
# exactly -- no simulation.
#
# This is written vectorised over (outs_in_inning, outs_recorded) because it now
# has to run ~200 times, once per pitcher, rather than once for a staff. The
# scalar quintuple loop the Butler version used is ~15 s a solve; folding the
# (o, co) plane into matrix arithmetic makes it ~0.05 s, which is what makes a
# per-pitcher xOuts affordable at all.
E_NAMES <- c("ball", "strike", "foul", "hbp", "inplay")

solve_dp <- function(tp, bip_dist) {
  # tp        : 4 x 3 x 5 array of P(event | balls, strikes)
  # bip_dist  : P(0,1,2,3 outs | ball in play)
  # The outs-recorded axis is 28 wide (0..27) but only 0..26 are live states: at
  # 27 the outing is mathematically over, so that slab stays 0 and is written to
  # by nothing. Every assignment below therefore targets columns 1:27, and
  # forgetting that is a silent shape error -- V[p, b, s, , ] is 3 x 28 while
  # the (o, co) plane being computed is 3 x 27.
  V  <- array(0, c(PMAX + 1, 4, 3, 3, 28))
  NCO <- 27
  O  <- matrix(0:2,  nrow = 3, ncol = NCO)                 # outs in inning
  CO <- matrix(0:26, nrow = 3, ncol = NCO, byrow = TRUE)   # outs recorded

  for (p in (PMAX - 1):0) {
    p2 <- p + 1L
    # C[[k+1]] = value of ending a PA that produced k outs, then surviving to
    # the next hitter. Vectorised over the whole (o, co) plane.
    C <- lapply(0:3, function(k) {
      co2 <- pmin(CO + k, 27); o2 <- O + k
      io  <- (o2 >= 3); o2[io] <- 0
      cont <- k + (1 - hz(p2, as.integer(io))) *
        V[cbind(p2 + 1L, 1L, 1L, as.vector(o2) + 1L, as.vector(co2) + 1L)]
      out <- matrix(cont, 3, NCO)
      out[p2 > PMAX | co2 >= 27] <- k        # terminal: bank k and stop
      out
    })
    Cbip <- Reduce(`+`, Map(function(m, w) m * w, C, bip_dist))

    for (b in 0:3) for (s in 0:2) {
      pr <- tp[b + 1, s + 1, ]
      j  <- seq_len(NCO)
      nb <- if (b == 3) C[[1]] else V[p2 + 1, b + 2, s + 1, , j]
      ns <- if (s == 2) C[[2]] else V[p2 + 1, b + 1, s + 2, , j]
      nf <- if (s == 2) V[p2 + 1, b + 1, s + 1, , j] else V[p2 + 1, b + 1, s + 2, , j]
      V[p + 1, b + 1, s + 1, , j] <-
        pr["ball"] * nb + pr["strike"] * ns + pr["foul"] * nf +
        pr["hbp"] * C[[1]] + pr["inplay"] * Cbip
    }
  }
  V
}

# ---- league transition rates -------------------------------------------------
spk <- merge(raw, sp[, .(game_pk, at_bat_number)],
             by = c("game_pk", "at_bat_number"))   # starters' pitches only
setorder(spk, game_pk, at_bat_number, pitch_number)

lg_cell <- spk[, .N, by = .(balls, strikes, e)]
lg_cell <- as.data.table(complete(lg_cell, balls = 0:3, strikes = 0:2,
                                  e = E_NAMES, fill = list(N = 0)))
lg_cell[, p_lg := N / sum(N), by = .(balls, strikes)]

to_tp <- function(d, col) {
  a <- array(0, c(4, 3, length(E_NAMES)), dimnames = list(NULL, NULL, E_NAMES))
  for (i in seq_len(nrow(d)))
    a[d$balls[i] + 1, d$strikes[i] + 1, d$e[i]] <- d[[col]][i]
  a
}
tp_lg <- to_tp(lg_cell, "p_lg")

bip <- sp[!is.na(outcome) & !outcome %chin% c("K", "BB", "HBP")]
bip_lg <- prop.table(table(factor(pmin(bip$outs_made, 3), levels = 0:3)))
cat(sprintf("\nE[outs | ball in play] = %.3f  (0/1/2/3: %s)\n",
            sum(as.numeric(names(bip_lg)) * bip_lg),
            paste(sprintf("%.3f", bip_lg), collapse = " ")))

V_lg <- solve_dp(tp_lg, as.numeric(bip_lg))
cat(sprintf("league V(start) = %.2f expected outs  |  actual mean = %.2f\n",
            V_lg[1, 1, 1, 1, 1], mean(outings$outs)))

# -----------------------------------------------------------------------------
# 7. Per-pitch Outing Length Value
# -----------------------------------------------------------------------------
Vg <- function(V, p, b, s, o, co)
  V[cbind(pmin(p, PMAX) + 1, pmin(b, 3) + 1, pmin(s, 2) + 1,
          pmin(o, 2) + 1, pmin(co, 27) + 1)]

contin_v <- function(V, p, o, co, k) {
  co2 <- pmin(co + k, 27); o2 <- o + k
  io  <- as.integer(o2 >= 3); o2 <- ifelse(io == 1, 0, o2)
  ifelse(p > PMAX | co2 >= 27, k,
         k + (1 - hz(p, io)) * Vg(V, pmin(p, PMAX), 0, 0, o2, co2))
}

# outs banked on this very pitch, and the count/out state entering it
spk[, outs_on_pitch := 0L]
spk[e == "strike" & strikes == 2L, outs_on_pitch := 1L]
spk <- merge(spk, sp[, .(game_pk, at_bat_number, pa_outs = outs_made)],
             by = c("game_pk", "at_bat_number"))
spk[e == "inplay", outs_on_pitch := pmin(pa_outs, 3L)]

setorder(spk, game_pk, pitcher, at_bat_number, pitch_number)
spk[, p_before  := seq_len(.N) - 1L,                       by = .(game_pk, pitcher)]
spk[, co_before := shift(cumsum(outs_on_pitch), fill = 0L), by = .(game_pk, pitcher)]
spk[, o_before  := outs_when_up]
spk <- spk[o_before <= 2L & co_before <= 26L]

spk[, p2 := p_before + 1L]
spk[, v_before := Vg(V_lg, p_before, balls, strikes, o_before, co_before)]
spk[, v_after := fcase(
  e == "ball"   & balls   == 3L, contin_v(V_lg, p2, o_before, co_before, 0L),
  e == "ball",                   Vg(V_lg, p2, balls + 1L, strikes, o_before, co_before),
  e == "strike" & strikes == 2L, contin_v(V_lg, p2, o_before, co_before, 1L),
  e == "strike",                 Vg(V_lg, p2, balls, strikes + 1L, o_before, co_before),
  e == "foul"   & strikes == 2L, Vg(V_lg, p2, balls, strikes, o_before, co_before),
  e == "foul",                   Vg(V_lg, p2, balls, strikes + 1L, o_before, co_before),
  e == "hbp",                    contin_v(V_lg, p2, o_before, co_before, 0L),
  e == "inplay",                 contin_v(V_lg, p2, o_before, co_before, outs_on_pitch))]
spk[, olv := v_after - v_before]

pa_olv <- spk[, .(olv_pa = sum(olv)), by = .(game_pk, at_bat_number)]
sp <- merge(sp, pa_olv, by = c("game_pk", "at_bat_number"), all.x = TRUE)

outcome_value <- sp[!is.na(outcome), .(
    n = .N, pitches = round(mean(pa_pitches), 2),
    outs = round(mean(outs_made), 2),
    olv  = round(mean(olv_pa, na.rm = TRUE), 3),
    rv   = round(mean(rv, na.rm = TRUE), 3)
  ), by = outcome][order(-olv)]

cat("\n---- what extends an MLB start (OLV per PA, outs of outing) ----\n")
print(outcome_value)

# -----------------------------------------------------------------------------
# 8. xOuts: each pitcher's own event rates through the league DP
# -----------------------------------------------------------------------------
# This is the answer to the user's question and the one metric here that is not
# a rescaling of something already public.
#
# Take a pitcher's own P(ball / strike / foul / hbp / inplay | count) and his own
# outs-per-ball-in-play distribution, shrink both toward the league rates for the
# SAME count, and solve the DP. The removal hazard stays LEAGUE-AVERAGE, so the
# result is: how deep would this pitcher go for a league-average manager?
#
# That deliberately strips out leash, bullpen quality, and September shutdowns.
# It also deliberately does NOT strip out defense or park -- balls in play are
# credited as the outs they actually produced, so a starter in front of a good
# infield does bank cheaper outs. That is real to his outing length even though
# it isn't his skill, and the xOuts/OLV split below is where it shows up.
qual <- outings[, .(starts = .N), by = pitcher][starts >= MIN_STARTS]
cat(sprintf("\n%d starters with >= %d starts\n", nrow(qual), MIN_STARTS))

pitcher_cell <- spk[pitcher %in% qual$pitcher, .N, by = .(pitcher, balls, strikes, e)]
pitcher_cell <- as.data.table(complete(pitcher_cell,
  pitcher, balls = 0:3, strikes = 0:2, e = E_NAMES, fill = list(N = 0)))
pitcher_cell <- merge(pitcher_cell, lg_cell[, .(balls, strikes, e, p_lg)],
                      by = c("balls", "strikes", "e"))
pitcher_cell[, n_cell := sum(N), by = .(pitcher, balls, strikes)]
pitcher_cell[, n_tot  := sum(N), by = pitcher]
# shrink toward the league rate for this same count; weight by how much of the
# pitcher's overall sample we have, not by the thin cell alone
pitcher_cell[, p_sh := (N + K_SHRINK * n_cell / pmax(n_tot, 1) * p_lg) /
                       (n_cell + K_SHRINK * n_cell / pmax(n_tot, 1))]
pitcher_cell[is.na(p_sh) | n_cell == 0, p_sh := p_lg]
pitcher_cell[, p_sh := p_sh / sum(p_sh), by = .(pitcher, balls, strikes)]

bip_p <- bip[, .N, by = .(pitcher, k = pmin(outs_made, 3L))]
bip_p <- as.data.table(complete(bip_p, pitcher, k = 0:3, fill = list(N = 0)))
bip_p[, n_tot := sum(N), by = pitcher]
bip_p[, p_sh := (N + K_BIP * as.numeric(bip_lg)[k + 1]) / (n_tot + K_BIP)]

cat("solving per-pitcher DPs ...\n")

# Each pitcher gets four solves, not one, because "he goes deep" has two
# genuinely different causes and they call for different conclusions:
#
#   count rates -- how often he throws a ball / strike / foul / puts it in play
#                  from each count. Command and whiffs. His.
#   BIP outs    -- how many outs a ball in play converts into. Contact quality,
#                  but also his defense and his park, which are NOT his.
#
# Swapping one to league while holding the other at his own rates prices each
# channel separately, and the decomposition is what keeps the leaderboard
# honest: a pitcher whose whole edge is the BIP channel is partly being
# credited for the fielders behind him.
xo <- rbindlist(lapply(qual$pitcher, function(pid) {
  d  <- pitcher_cell[pitcher == pid]
  bd <- bip_p[pitcher == pid][order(k), p_sh]
  if (!nrow(d) || length(bd) != 4) return(NULL)
  tp_own <- to_tp(d, "p_sh")
  bd_lg  <- as.numeric(bip_lg)
  data.table(
    pitcher    = pid,
    xouts      = solve_dp(tp_own, bd)[1, 1, 1, 1, 1],   # everything his
    xouts_cnt  = solve_dp(tp_own, bd_lg)[1, 1, 1, 1, 1],# his counts, lg contact
    xouts_bip  = solve_dp(tp_lg,  bd)[1, 1, 1, 1, 1])   # lg counts, his contact
}))
xo[, `:=`(gain_count = xouts_cnt - V_lg[1, 1, 1, 1, 1],
          gain_bip   = xouts_bip - V_lg[1, 1, 1, 1, 1])]

# -----------------------------------------------------------------------------
# 9. The leaderboard
# -----------------------------------------------------------------------------
per_pitch <- spk[, .(olv100 = 100 * mean(olv, na.rm = TRUE)), by = pitcher]
rv_pitch  <- sp[, .(rv100 = 100 * sum(rv, na.rm = TRUE) / sum(pa_pitches)),
                by = pitcher]

board <- outings[, .(
    starts   = .N,
    outs     = mean(outs),
    ip       = mean(outs) / 3,
    pitches  = mean(pitches),
    p_per_out= sum(pitches) / sum(outs),
    era      = 9 * sum(runs) / (sum(outs) / 3),
    six_plus = mean(outs >= 18),
    seven_plus = mean(outs >= 21)
  ), by = .(pitcher, p_throws)][starts >= MIN_STARTS]

board <- board |>
  merge(xo, by = "pitcher", all.x = TRUE) |>
  merge(per_pitch, by = "pitcher", all.x = TRUE) |>
  merge(rv_pitch,  by = "pitcher", all.x = TRUE) |>
  merge(pnames, by = "pitcher", all.x = TRUE)
board[is.na(name), name := paste0("id ", pitcher)]

board[, leash := outs - xouts]        # allowed minus earned
setorder(board, -xouts)
board[, rank_x := seq_len(.N)]

fmt <- function(d) d[, .(
  Rank = rank_x, Pitcher = name, T = p_throws, GS = starts,
  `xOuts/st` = round(xouts, 2), `xIP/st` = round(xouts / 3, 2),
  `Act outs` = round(outs, 2), Leash = round(leash, 2),
  `P/st` = round(pitches, 1), `P/out` = round(p_per_out, 2),
  `OLV/100` = round(olv100, 3), `RS/100` = round(rv100, 2),
  ERA = round(era, 2), `6+IP%` = round(100 * six_plus))]

cat("\n================ TOP 30: expected outing length (xOuts/start) ================\n")
print(fmt(head(board, 30)), row.names = FALSE)

cat("\n---- bottom 10 (shortest expected outings) ----\n")
print(fmt(tail(board, 10)), row.names = FALSE)

cat("\n---- longest leash: allowed far more than earned ----\n")
print(fmt(head(board[order(-leash)], 10)), row.names = FALSE)

cat("\n---- shortest leash: earned far more than allowed ----\n")
print(fmt(head(board[order(leash)], 10)), row.names = FALSE)

# The two-axis view. Outing length alone rewards pitching to contact, so the
# pitchers who matter are the ones above the median on BOTH efficiency and run
# prevention.
board[, `:=`(x_pct = 100 * rank(xouts) / .N, rs_pct = 100 * rank(rv100) / .N)]
board[, quadrant := fcase(
  x_pct >= 50 & rs_pct >= 50, "workhorse ace",
  x_pct >= 50 & rs_pct <  50, "innings, not quality",
  x_pct <  50 & rs_pct >= 50, "quality, not innings",
  default = "neither")]

cat("\n---- best on BOTH axes (efficiency x run prevention) ----\n")
print(fmt(head(board[quadrant == "workhorse ace"][order(-(x_pct + rs_pct))], 20)),
      row.names = FALSE)

cat("\n---- quadrant counts ----\n")
print(board[, .N, by = quadrant][order(-N)])

# Does expected outing length even persist year to year? If it doesn't, the
# leaderboard is a description of 2025-26 and nothing more.
yoy <- outings[, .(starts = .N), by = .(pitcher, game_year)][starts >= 8]
if (uniqueN(yoy$game_year) > 1) {
  yr <- sort(unique(yoy$game_year))
  ys <- rbindlist(lapply(yr, function(y) {
    ids <- yoy[game_year == y, pitcher]
    pc <- spk[game_year == y & pitcher %in% ids, .N, by = .(pitcher, balls, strikes, e)]
    pc <- as.data.table(complete(pc, pitcher, balls = 0:3, strikes = 0:2,
                                 e = E_NAMES, fill = list(N = 0)))
    pc <- merge(pc, lg_cell[, .(balls, strikes, e, p_lg)],
                by = c("balls", "strikes", "e"))
    pc[, n_cell := sum(N), by = .(pitcher, balls, strikes)]
    pc[, n_tot := sum(N), by = pitcher]
    pc[, p_sh := (N + K_SHRINK * n_cell / pmax(n_tot, 1) * p_lg) /
                 (n_cell + K_SHRINK * n_cell / pmax(n_tot, 1))]
    pc[is.na(p_sh) | n_cell == 0, p_sh := p_lg]
    pc[, p_sh := p_sh / sum(p_sh), by = .(pitcher, balls, strikes)]
    bp <- bip[game_year == y & pitcher %in% ids, .N, by = .(pitcher, k = pmin(outs_made, 3L))]
    bp <- as.data.table(complete(bp, pitcher, k = 0:3, fill = list(N = 0)))
    bp[, n_tot := sum(N), by = pitcher]
    bp[, p_sh := (N + K_BIP * as.numeric(bip_lg)[k + 1]) / (n_tot + K_BIP)]
    rbindlist(lapply(ids, function(pid) {
      d <- pc[pitcher == pid]; bd <- bp[pitcher == pid][order(k), p_sh]
      if (!nrow(d) || length(bd) != 4) return(NULL)
      data.table(pitcher = pid, game_year = y,
                 xouts = solve_dp(to_tp(d, "p_sh"), bd)[1, 1, 1, 1, 1])
    }))
  }))
  w <- dcast(ys, pitcher ~ game_year, value.var = "xouts")
  act <- dcast(outings[, .(o = mean(outs)), by = .(pitcher, game_year)],
               pitcher ~ game_year, value.var = "o")
  cn <- as.character(yr)
  ok <- complete.cases(w[, ..cn])
  oka <- complete.cases(act[, ..cn])
  cat(sprintf(
    "\n---- year-over-year stability (n = %d) ----\n  xOuts/start   r = %.3f\n  actual outs   r = %.3f\n",
    sum(ok), cor(w[[cn[1]]][ok], w[[cn[2]]][ok]),
    cor(act[[cn[1]]][oka], act[[cn[2]]][oka])))
  ys_wide <- w
} else ys_wide <- NULL

# -----------------------------------------------------------------------------
# 10. Per-pitcher profiles for the app's pitcher card
# -----------------------------------------------------------------------------
# Event rates by pitch type, so a card can say WHERE his outing length comes
# from -- which pitch is putting the ball in play cheaply and which is running
# up the count.
mix <- spk[pitcher %in% qual$pitcher & !is.na(pitch_type) & pitch_type != "",
  .(n = .N, velo = mean(release_speed, na.rm = TRUE),
    olv100 = 100 * mean(olv, na.rm = TRUE),
    rv100  = 100 * mean(-delta_run_exp, na.rm = TRUE),
    ball   = mean(e == "ball"), strike = mean(e == "strike"),
    foul   = mean(e == "foul"), inplay = mean(e == "inplay")),
  by = .(pitcher, pitch_type, pitch_name)]
mix[, usage := n / sum(n), by = pitcher]
mix <- mix[n >= 25]

# Same rates pooled, plus the league line to compare a card against.
prof <- spk[pitcher %in% qual$pitcher, .(
    pitches = .N,
    ball = mean(e == "ball"), strike = mean(e == "strike"),
    foul = mean(e == "foul"), inplay = mean(e == "inplay"),
    zone = mean(abs(plate_x) <= 0.83 &
                plate_z >= sz_bot & plate_z <= sz_top, na.rm = TRUE),
    first_pitch_strike = mean(balls == 0 & strikes == 0 & e != "ball")
  ), by = pitcher]
lg_prof <- spk[, .(ball = mean(e == "ball"), strike = mean(e == "strike"),
                   foul = mean(e == "foul"), inplay = mean(e == "inplay"))]

# -----------------------------------------------------------------------------
# 11. Daily tracker: how each start moved that pitcher's outing length
# -----------------------------------------------------------------------------
# The leaderboard is a season verdict. This is the game-by-game one: for a given
# day, which starters generated the events that buy outing length and which
# spent it.
#
# Two different senses of "extended or shortened" are reported, because they
# answer different questions and can disagree:
#
#   olv_start  -- the sum of per-pitch OLV across the outing. Outs of outing
#                 gained or lost against a LEAGUE-AVERAGE pitch from the same
#                 state. This is the framework-native measure and it is
#                 independent of the manager: a starter yanked after 4 innings
#                 can still post a strongly positive OLV.
#   vs_x       -- actual outs minus his own season xOuts. Did this start beat
#                 the outing length his own pitch economy predicts? This one
#                 DOES include the manager's decision, which is why both are
#                 shown rather than picking one.
TRACK_DAYS <- as.integer(Sys.getenv("OL_TRACK_DAYS", "35"))

# The opponent is whoever was batting: a Top half is the away team hitting, so
# the pitcher is the home club's and his opponent is the away side.
sp[, opp := fifelse(inning_topbot == "Top", away_team, home_team)]

starts_daily <- sp[, .(
    game_date = first(game_date),
    opp       = first(opp),
    outs      = sum(outs_made),
    pitches   = sum(pa_pitches),
    runs      = sum(runs),
    olv_start = sum(olv_pa, na.rm = TRUE),
    bb        = sum(outcome == "BB", na.rm = TRUE),
    k         = sum(outcome == "K",  na.rm = TRUE)
  ), by = .(game_pk, pitcher)]

starts_daily <- merge(starts_daily, board[, .(pitcher, name, p_throws, xouts)],
                      by = "pitcher", all.x = TRUE)
starts_daily <- starts_daily[!is.na(name)]     # qualified starters only
starts_daily[, `:=`(vs_x   = outs - xouts,
                    olv100 = 100 * olv_start / pmax(pitches, 1))]
setorder(starts_daily, -game_date, -olv_start)

cat(sprintf("\ntracker: %s starts across %s dates (%s .. %s)\n",
            format(nrow(starts_daily), big.mark = ","),
            uniqueN(starts_daily$game_date),
            min(starts_daily$game_date), max(starts_daily$game_date)))

# Pitch-level cumulative OLV, so the app can draw WHERE in the outing he gained
# or lost it. Kept only for recent dates -- the full season would be ~600k rows
# in a bundle that is otherwise under a megabyte, and nobody opens a tracker to
# read April.
recent_from <- max(as.Date(spk$game_date)) - TRACK_DAYS
track_pitches <- spk[as.Date(game_date) >= recent_from & pitcher %in% board$pitcher,
  .(game_pk, pitcher, game_date, pitch_no = p_before + 1L, inning,
    olv = round(olv, 4), e)]
setorder(track_pitches, game_pk, pitcher, pitch_no)
track_pitches[, cum_olv := round(cumsum(olv), 4), by = .(game_pk, pitcher)]
track_pitches[, olv := NULL]        # only the running total is ever plotted

cat(sprintf("tracker pitch detail: %s pitches over the last %d days (%.2f MB)\n",
            format(nrow(track_pitches), big.mark = ","), TRACK_DAYS,
            as.numeric(object.size(track_pitches)) / 1e6))

saveRDS(list(
  board = board, outings = outings, sp = sp, economy = economy,
  outcome_value = outcome_value, V_lg = V_lg, haz_mod = haz_mod,
  tp_lg = tp_lg, bip_lg = bip_lg, xouts_by_year = ys_wide,
  pitcher_cell = pitcher_cell, bip_p = bip_p, pnames = pnames,
  mix = mix, prof = prof, lg_prof = lg_prof, xo = xo,
  starts_daily = starts_daily, track_pitches = track_pitches,
  lg_xouts = V_lg[1, 1, 1, 1, 1],
  meta = list(seasons = SEASONS, PMAX = PMAX, MIN_STARTS = MIN_STARTS,
              K_SHRINK = K_SHRINK, K_BIP = K_BIP, TRACK_DAYS = TRACK_DAYS)
), OUT_RDS, compress = "xz")
cat("\nwrote ", OUT_RDS, "\n", sep = "")
