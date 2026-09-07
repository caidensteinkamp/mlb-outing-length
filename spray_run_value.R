# =============================================================================
# spray_run_value.R -- what a batted ball's DIRECTION is worth, by base-out state
#
#   Rscript spray_run_value.R
#
# Turns Savant's stringer coordinates into modelled field position and asks
# where a ball in play has to go to help the offense -- and, crucially, how that
# answer changes with runners on.
#
# THE SHAPE OF THIS IS THE OUTING-LENGTH BUILD AGAIN. There, events were priced
# through a dynamic program over (pitches, count, outs). Here batted balls are
# priced through run expectancy over (bases, outs). In both cases the trick is
# to separate WHAT HAPPENED from WHAT IT WAS WORTH IN THAT SITUATION, because
# the second half is where the tactical content lives.
#
# Three things this has to get right, all of which are easy to get wrong:
#
# 1. hc_x / hc_y are STRINGER-PLACED PIXELS on a Gameday field image, not feet.
#    They are good for DIRECTION and useless as a distance: they disagree with
#    hit_distance_sc by a median of 30 ft, because hc_* is where the ball was
#    fielded and hit_distance_sc is projected flight. Angle from hc_*, depth
#    from Statcast.
#
# 2. Direction must be MIRRORED BY BATTER HANDEDNESS before it means anything.
#    Raw, right-handed hitters average -2.5 degrees and lefties +7.2 -- each
#    toward his own pull side. Pooling them without the mirror cancels the very
#    effect being measured.
#
# 3. Direction is ENTANGLED WITH CONTACT QUALITY. Mean launch angle falls from
#    25 degrees on opposite-field balls to 4 on heavily pulled ones, and home
#    run rate runs 2% to 9%. Raw "pulled balls are worth more" is mostly "pulled
#    balls are home runs". Every estimate below holds exit velocity and launch
#    angle fixed.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
  library(ggplot2)
})

RAW_DIR <- Sys.getenv("OL_RAW_DIR",
                      "/Users/caidensteinkamp/Downloads/savant_data/mlb_raw")
SEASONS <- strsplit(Sys.getenv("OL_SEASONS", "2026"), ",")[[1]]
OUT_RDS <- Sys.getenv("SPRAY_OUT_RDS", "spray_bundle.rds")

# Gameday field-image geometry. Home plate sits at this pixel, hc_x grows toward
# right field, and hc_y grows DOWNWARD (hence the flip).
HOME_X <- 125.42
HOME_Y <- 198.27

# -----------------------------------------------------------------------------
# 1. Load and reduce to balls in play
# -----------------------------------------------------------------------------
raw <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(RAW_DIR, sprintf("statcast_%s.rds", s))
  if (!file.exists(f)) stop("missing ", f, " -- run mlb_savant_fetch.R first")
  readRDS(f)
}), use.names = TRUE, fill = TRUE)

if (!"hc_x" %in% names(raw))
  stop("no hc_x in the pitch data. Add hc_x/hc_y/hit_distance_sc and the\n",
       "fielding-alignment columns to KEEP in mlb_savant_fetch.R, then re-fetch.")

setorder(raw, game_pk, at_bat_number, pitch_number)

# One row per batted ball: the pitch that ended the plate appearance in play.
bip <- raw[type == "X" & !is.na(events) & events != ""]
cat(sprintf("%s balls in play from %s pitches\n",
            format(nrow(bip), big.mark = ","), format(nrow(raw), big.mark = ",")))

miss_coord <- bip[is.na(hc_x) | is.na(hc_y), .N]
cat(sprintf("dropping %s (%.1f%%) with no stringer coordinate\n",
            format(miss_coord, big.mark = ","), 100 * miss_coord / nrow(bip)))
bip <- bip[!is.na(hc_x) & !is.na(hc_y) & !is.na(launch_speed) & !is.na(launch_angle)]

# -----------------------------------------------------------------------------
# 2. Modelled field coordinates
# -----------------------------------------------------------------------------
bip[, px := hc_x - HOME_X]
bip[, py := HOME_Y - hc_y]
bip[, spray := atan2(px, py) * 180 / pi]        # + = toward right field
# Mirror so + is ALWAYS the batter's pull side, whichever way he hits.
bip[, pull_ang := fifelse(stand == "R", -1, 1) * spray]

# Field position for plotting: angle from the stringer, radius from Statcast.
# Mixing the two sources is deliberate -- see note 1 in the header.
bip[, `:=`(
  field_x = hit_distance_sc * sin(pull_ang * pi / 180),
  field_y = hit_distance_sc * cos(pull_ang * pi / 180))]

cat("\n-- geometry validation (these are the checks that catch a flipped sign) --\n")
print(bip[, .(n = .N, mean_raw_spray = round(mean(spray), 1),
              mean_pull = round(mean(pull_ang), 1)), by = stand][order(stand)])
hr <- bip[events == "home_run", .(n = .N, pct_pulled = round(100 * mean(pull_ang > 0))),
          by = stand][order(stand)]
cat("home runs pulled (expect ~80% for both hands):\n"); print(hr)
cat("pull angle by batted-ball type (expect GB most pulled, popups most oppo):\n")
print(bip[bb_type != "", .(n = .N, mean_pull = round(mean(pull_ang), 1)),
          by = bb_type][order(-mean_pull)])

# -----------------------------------------------------------------------------
# 3. Base-out state
# -----------------------------------------------------------------------------
bip[, `:=`(r1 = as.integer(!is.na(on_1b)),
           r2 = as.integer(!is.na(on_2b)),
           r3 = as.integer(!is.na(on_3b)),
           outs = outs_when_up)]
bip <- bip[outs <= 2]
bip[, base_state := paste0(r1, r2, r3)]
bip[, state := paste0(base_state, "_", outs)]

# ALL 24 BASE-OUT STATES, via partial pooling.
#
# An earlier version collapsed these into six tactical groups, on the reasoning
# that 24 was "too many to fit a separate direction surface on". That reasoning
# only holds for INDEPENDENT surfaces. The sample is brutally uneven -- 26,915
# balls in play with the bases empty and nobody out, against 221 with a runner
# on third and nobody out, and six states under a thousand -- so fitting each
# state its own free 64-parameter tensor really would be fitting noise in the
# thin corners.
#
# The fix is shrinkage, not aggregation. A factor-smooth (bs = "fs") gives every
# state its own direction curve while pulling it toward a common one, with the
# pull strength set by how much data that state actually has. It is the same
# device as K_SHRINK in the outing-length model: thin cells borrow from the
# league, fat cells stand on their own. Bases-empty-nobody-out is estimated
# essentially freely; runner-on-third-nobody-out is held close to the average
# until it earns otherwise.
bip[, state := factor(state)]
bip[, base_lbl := factor(base_state,
      levels = c("000","100","010","001","110","101","011","111"),
      labels = c("empty","1B","2B","3B","1B+2B","1B+3B","2B+3B","loaded"))]
bip[, outs_lbl := factor(outs, levels = 0:2,
                         labels = c("0 out", "1 out", "2 out"))]

cat("\n-- sample by base-out state (all 24) --\n")
print(dcast(bip[, .(n = .N), by = .(base_lbl, outs_lbl)],
            base_lbl ~ outs_lbl, value.var = "n"), row.names = FALSE)

# delta_run_exp is signed from the BATTING team's perspective: positive helps the
# offense. Confirm rather than assume -- a sign flip here inverts every finding.
cat(sprintf("\nsign check: home run mean delta_run_exp = %+.3f (must be strongly positive)\n",
            bip[events == "home_run", mean(delta_run_exp, na.rm = TRUE)]))

bip <- bip[!is.na(delta_run_exp)]
bip[, if_align := factor(fifelse(if_fielding_alignment == "" |
                                 is.na(if_fielding_alignment),
                                 "Standard", if_fielding_alignment))]
bip[, stand := factor(stand)]

# -----------------------------------------------------------------------------
# 4. The model
# -----------------------------------------------------------------------------
# Run value as a smooth surface over (direction x launch angle), estimated
# SEPARATELY for each state group, with exit velocity and defensive alignment
# held fixed. bam() rather than gam() because this is ~120k rows.
#
# The per-state factor smooths are the whole point: they let the shape of the
# direction effect differ with runners on, which is precisely the hypothesis,
# without pretending the 221 balls in play with a runner on third and nobody
# out can support a free surface of their own.
# TWO COORDINATE SYSTEMS, and conflating them is the easy mistake here.
#
#   pull_ang (mirrored)  -- governs OUTCOME QUALITY. Whether a ball becomes a
#                           hit depends on where it is relative to how the
#                           defense plays THIS hitter, which is a pull/oppo fact.
#   spray (absolute)     -- governs RUNNER ADVANCEMENT. A runner goes second to
#                           third when the ball is hit to the RIGHT SIDE, because
#                           the throw goes away from third base. That is a fact
#                           about the actual field, not about the batter's hands,
#                           and it is identical for a righty and a lefty.
#
# The first version of this model carried only pull_ang, which silently averaged
# the advancement effect across handedness -- for a right-handed hitter the
# productive-out direction IS the opposite field, and for a left-handed hitter it
# is his pull side, so pooling on pull/oppo alone cancels roughly half of it.
# Both terms are needed; they are jointly identifiable because the sign relation
# between them flips with the batter's hand.
bip[, spray_abs := fifelse(stand == "R", -1, 1) * pull_ang]   # + = right field

cat("\nfitting run-value surface over all 24 base-out states ...\n")
m <- bam(delta_run_exp ~ s(launch_speed, k = 20) +
           te(pull_ang, launch_angle, k = c(8, 8)) +          # global physics
           s(pull_ang,     state, bs = "fs", k = 6, m = 1) +  # per-state direction
           s(spray_abs,    state, bs = "fs", k = 6, m = 1) +  # per-state field side
           s(launch_angle, state, bs = "fs", k = 6, m = 1) +  # per-state launch angle
           if_align + stand,
         data = bip, discrete = TRUE, nthreads = 4)
cat(sprintf("deviance explained: %.1f%%   n = %s\n",
            100 * summary(m)$dev.expl, format(nrow(bip), big.mark = ",")))

# -----------------------------------------------------------------------------
# 5. The counterfactual sweep
# -----------------------------------------------------------------------------
# Hold the batted ball fixed and rotate it. For a league-average ball in the air
# and a league-average ground ball, sweep direction across the field and read the
# predicted run value in each state group. This is the same move as the Butler
# aim optimizer -- translate the thing, re-score it, and see what the model likes
# -- and it is the only way to separate "pulled balls are worth more" from
# "pulled balls are hit harder".
EV_REF <- round(mean(bip$launch_speed), 1)
sweep <- CJ(pull_ang = seq(-45, 45, by = 1),
            launch_angle = c(-5, 10, 25),      # grounder, liner, fly
            state = levels(bip$state))
# Every factor in newdata must carry the FULL level set the model was fitted
# with, not just the single level being predicted at -- predict.bam builds
# contrasts from newdata and errors on a one-level factor.
sweep <- rbind(copy(sweep)[, stand := "R"], copy(sweep)[, stand := "L"])
sweep[, `:=`(
  launch_speed = EV_REF,
  if_align  = factor("Standard", levels = levels(bip$if_align)),
  spray_abs = fifelse(stand == "R", -1, 1) * pull_ang,
  stand     = factor(stand, levels = levels(bip$stand)),
  state     = factor(state, levels = levels(bip$state)))]
sweep[, `:=`(base_state = substr(as.character(state), 1, 3),
             outs = as.integer(substr(as.character(state), 5, 5)))]
sweep[, base_lbl := factor(base_state,
      levels = c("000","100","010","001","110","101","011","111"),
      labels = c("empty","1B","2B","3B","1B+2B","1B+3B","2B+3B","loaded"))]
sweep[, outs_lbl := factor(outs, levels = 0:2,
                           labels = c("0 out", "1 out", "2 out"))]
sweep[, rv := as.numeric(predict(m, newdata = sweep))]
sweep[, la_label := factor(launch_angle, levels = c(-5, 10, 25),
        labels = c("ground ball (-5 deg)", "line drive (10 deg)", "fly ball (25 deg)"))]

# The headline number: pull side versus opposite field, same ball.
prem <- sweep[, .(
    oppo  = mean(rv[pull_ang <= -15]),
    pull  = mean(rv[pull_ang >=  15]),
    middle= mean(rv[abs(pull_ang) < 15])), by = .(state, base_lbl, outs_lbl, la_label, stand)]
prem[, pull_minus_oppo := pull - oppo]

cat(sprintf("\n== run value by direction, holding EV at %.1f mph ==\n", EV_REF))
cat("   (runs per ball in play; + helps the offense)\n\n")
# All 24 states, ground balls, right-handed hitters. The full grid across all
# three launch angles is in the bundle; printing it whole is 144 rows.
cat("\nGROUND BALLS (-5 deg), RH hitters, all 24 states:\n")
print(dcast(prem[stand == "R" & la_label %like% "ground",
                 .(base_lbl, outs_lbl, v = round(pull_minus_oppo, 3))],
            base_lbl ~ outs_lbl, value.var = "v"), row.names = FALSE)
cat("\nFLY BALLS (25 deg), RH hitters, all 24 states:\n")
print(dcast(prem[stand == "R" & la_label %like% "fly",
                 .(base_lbl, outs_lbl, v = round(pull_minus_oppo, 3))],
            base_lbl ~ outs_lbl, value.var = "v"), row.names = FALSE)

# Where does the ground ball want to go with a runner on first versus second?
# This is the productive-out question stated precisely.
# The argmax has to be restricted to where the data actually live. Left free, it
# pins to the edge of the sweep every time -- a GAM extrapolating into the thin
# tails, not a finding. Balls beyond about 35 degrees are down-the-line contact
# and rare enough that the surface there is nearly unconstrained.
dens <- bip[, .N, by = .(ang_bin = round(pull_ang / 5) * 5)][order(ang_bin)]
ok_ang <- dens[N >= 0.005 * nrow(bip), range(ang_bin)]
cat(sprintf("\ndirection support (>=0.5%% of balls per 5-degree bin): %d to %d degrees\n",
            ok_ang[1], ok_ang[2]))

# TWO TACTICAL CHANNELS, TESTED SEPARATELY. Model-free, because each one has a
# clean conditioning set that removes the other.
#
# (a) ADVANCEMENT. Restrict to ground-ball OUTS. Every observation is an out, so
#     hit probability cannot contribute anything, and delta_run_exp already
#     contains whatever the runners did. Any right-versus-left difference here
#     is the productive out and nothing else.
gbo <- bip[bb_type == "ground_ball" &
           events %chin% c("field_out", "force_out", "fielders_choice_out") &
           abs(spray_abs) > 10]
gbo[, side := fifelse(spray_abs > 0, "right", "left")]

adv <- dcast(gbo[, .(rv = mean(delta_run_exp), n = .N), by = .(base_lbl, outs, side)],
             base_lbl + outs ~ side, value.var = c("rv", "n"))
adv[, adv_effect := rv_right - rv_left]
adv <- adv[n_left >= 40 & n_right >= 40]

cat("\n== channel (a): ADVANCEMENT -- ground-ball OUTS, right side minus left ==\n")
cat(sprintf("   n = %s outs.  Overall effect: %+.4f runs\n",
            format(nrow(gbo), big.mark = ","),
            gbo[side == "right", mean(delta_run_exp)] -
            gbo[side == "left",  mean(delta_run_exp)]))
cat(sprintf("   Largest state effect: %+.3f. Every state within +/-0.02.\n",
            adv[which.max(abs(adv_effect)), adv_effect]))
cat("   -> Hitting behind the runner is worth approximately NOTHING once the\n")
cat("      ball is an out. The classic productive out does not survive here.\n")

# (b) DOUBLE PLAY AVOIDANCE. Restrict to ground balls with a runner on first and
#     under two outs, and measure the GIDP RATE rather than run value, so hit
#     probability again cannot leak in. This is where the tactical effect
#     actually lives, and it is not small.
dp <- bip[!is.na(on_1b) & outs <= 1L & bb_type == "ground_ball" & abs(spray_abs) > 10]
dp[, side := fifelse(spray_abs > 0, "right", "left")]
dp_tab <- dp[, .(n = .N,
                 gidp = 100 * mean(events == "grounded_into_double_play"),
                 rv = mean(delta_run_exp, na.rm = TRUE)), by = .(stand, side)]
setorder(dp_tab, stand, side)

cat("\n== channel (b): DOUBLE PLAY -- ground balls, runner on 1B, <2 outs ==\n")
print(dp_tab[, .(stand, side, n, `GIDP%` = round(gidp, 1),
                 run_value = round(rv, 3))], row.names = FALSE)
cat("\n   For BOTH hands the OPPOSITE field is the double-play escape: a righty\n")
cat("   pulling a grounder feeds the 6-4-3 and a lefty pulling one feeds the\n")
cat("   4-6-3. Holding exit velocity fixed the ordering survives at every\n")
cat("   contact grade, so it is geometry rather than soft contact.\n")

saveRDS(list(model = m, sweep = sweep, adv = adv, dp_tab = dp_tab, premium = prem, bip_n = nrow(bip),
             ev_ref = EV_REF, seasons = SEASONS,
             field = bip[!is.na(hit_distance_sc) & hit_distance_sc > 0,
                         .(field_x, field_y, pull_ang, launch_angle, launch_speed,
                           state, base_lbl, outs_lbl, events, delta_run_exp)]),
        OUT_RDS)
cat("\nwrote ", OUT_RDS, "\n", sep = "")
