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

# The 24 base-out states are too many to fit a separate direction surface on, and
# most of the distinctions do not change what a hitter should WANT. These six
# groups are the ones where direction plausibly pays differently, and they are
# chosen from tactics rather than from the data:
#
#   empty        -- nobody to advance; direction matters only through outcome
#   r1_dp        -- runner on first, under two out: the double-play states, where
#                   a ball to the pull side of a right-hander is the 6-4-3
#   r2_adv       -- runner on second, under two out: the classic "hit it to the
#                   right side" situation, where even an out advances him
#   r3_sac       -- runner on third, under two out: a fly ball scores him
#   multi        -- other multi-runner states under two out
#   two_out      -- two out, anybody on: productive outs do not exist, only hits
bip[, grp := fcase(
  base_state == "000",                       "empty",
  outs == 2L,                                "two_out",
  base_state == "100",                       "r1_dp",
  r3 == 1L,                                  "r3_sac",
  base_state == "010",                       "r2_adv",
  default =                                  "multi")]
bip[, grp := factor(grp, levels = c("empty", "r1_dp", "r2_adv", "r3_sac",
                                    "multi", "two_out"))]

cat("\n-- sample by state group --\n")
print(bip[, .(n = .N, mean_rv = round(mean(delta_run_exp, na.rm = TRUE), 3)), by = grp][order(grp)])

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
# The by = grp tensor product is the whole point: it lets the shape of the
# direction effect differ with runners on, which is precisely the hypothesis.
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

cat("\nfitting run-value surface ...\n")
m <- bam(delta_run_exp ~ grp + s(launch_speed, k = 20) +
           te(pull_ang, launch_angle, by = grp, k = c(8, 8)) +
           s(spray_abs, by = grp, k = 8) +
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
            grp = levels(bip$grp))
# Every factor in newdata must carry the FULL level set the model was fitted
# with, not just the single level being predicted at -- predict.bam builds
# contrasts from newdata and errors on a one-level factor.
sweep <- rbind(copy(sweep)[, stand := "R"], copy(sweep)[, stand := "L"])
sweep[, `:=`(
  launch_speed = EV_REF,
  if_align  = factor("Standard", levels = levels(bip$if_align)),
  spray_abs = fifelse(stand == "R", -1, 1) * pull_ang,
  stand     = factor(stand, levels = levels(bip$stand)),
  grp       = factor(grp,   levels = levels(bip$grp)))]
sweep[, rv := as.numeric(predict(m, newdata = sweep))]
sweep[, la_label := factor(launch_angle, levels = c(-5, 10, 25),
        labels = c("ground ball (-5 deg)", "line drive (10 deg)", "fly ball (25 deg)"))]

# The headline number: pull side versus opposite field, same ball.
prem <- sweep[, .(
    oppo  = mean(rv[pull_ang <= -15]),
    pull  = mean(rv[pull_ang >=  15]),
    middle= mean(rv[abs(pull_ang) < 15])), by = .(grp, la_label, stand)]
prem[, pull_minus_oppo := pull - oppo]

cat(sprintf("\n== run value by direction, holding EV at %.1f mph ==\n", EV_REF))
cat("   (runs per ball in play; + helps the offense)\n\n")
print(prem[stand == "R", .(grp, la_label, oppo = round(oppo, 3), middle = round(middle, 3),
               pull = round(pull, 3), `pull-oppo` = round(pull_minus_oppo, 3))],
      row.names = FALSE)

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

cat("\n== the productive-out check: ground balls, under two outs ==\n")
gb <- sweep[launch_angle == -5 & grp %in% c("empty", "r1_dp", "r2_adv") &
              pull_ang >= ok_ang[1] & pull_ang <= ok_ang[2]]
best <- gb[, .SD[which.max(rv)], by = grp]
cat("optimal ground-ball direction, within the supported range (+ = pull side):\n")
print(best[, .(grp, best_angle = pull_ang, rv = round(rv, 3))], row.names = FALSE)

# The comparison that actually isolates the productive out: with a runner on
# second and under two outs, how much does the right side gain RELATIVE to how
# it does with the bases empty? Differencing against `empty` strips out
# everything about direction that is just batted-ball physics.
rel <- dcast(sweep[launch_angle == -5 & stand == "R" &
                     pull_ang >= ok_ang[1] & pull_ang <= ok_ang[2],
                   .(grp, pull_ang, rv)], pull_ang ~ grp, value.var = "rv")
for (g in c("r1_dp", "r2_adv", "r3_sac")) rel[[paste0(g, "_vs_empty")]] <- rel[[g]] - rel$empty
cat("\nground-ball value RELATIVE to the same ball with bases empty:\n")
print(rel[pull_ang %% 10 == 0,
          .(pull_ang, r1_dp = round(r1_dp_vs_empty, 3),
            r2_adv = round(r2_adv_vs_empty, 3),
            r3_sac = round(r3_sac_vs_empty, 3))], row.names = FALSE)

# -----------------------------------------------------------------------------
# 6. Pictures
# -----------------------------------------------------------------------------
# (a) The analytical view: run value against direction, one line per state group.
#     This is the model's native space and the honest place to read effects off.
# Facet by handedness. The sweep now carries both hands, and because the
# absolute-side term makes their curves genuinely differ, overplotting them on
# one panel renders as a serrated line rather than as the two findings it is.
p_sweep <- ggplot(sweep[grp != "multi"],
                  aes(pull_ang, rv, colour = grp)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey85", linetype = "dashed") +
  geom_line(linewidth = 1) +
  facet_grid(stand ~ la_label, labeller = labeller(stand = c(R = "RH hitter", L = "LH hitter"))) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(title = "What a batted ball's direction is worth, by base-out situation",
       subtitle = sprintf(paste("Predicted run value at %.0f mph exit velocity.",
                                "Negative angle = opposite field, positive = pulled.\n",
                                "Exit velocity and launch angle held fixed, so this",
                                "is direction alone, not contact quality."), EV_REF),
       x = "Direction (degrees; + = pull side)",
       y = "Run value (runs per ball in play)") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave("spray_direction.png", p_sweep, width = 13, height = 7.5, dpi = 140)

# (b) The field view: every batted ball at its real position, coloured by the
#     run value the model assigns it. Angle is the stringer's, radius is
#     Statcast's projected distance -- the two sources measure different things
#     and only the angle is trustworthy from the stringer.
fm <- bip[!is.na(hit_distance_sc) & hit_distance_sc > 0 & abs(pull_ang) <= 50]
fm[, pred := as.numeric(predict(m, newdata = fm))]
lim <- quantile(fm$pred, c(.05, .95), na.rm = TRUE)

arc <- data.table(a = seq(-45, 45, length.out = 200))
arc[, `:=`(x = 400 * sin(a * pi / 180), y = 400 * cos(a * pi / 180))]

p_field <- ggplot(fm, aes(field_x, field_y)) +
  stat_summary_hex(aes(z = pred), bins = 26, fun = mean) +
  geom_segment(x = 0, y = 0, xend = 340 * sin(pi / 4), yend = 340 * cos(pi / 4),
               colour = "grey35", linewidth = .3) +
  geom_segment(x = 0, y = 0, xend = -340 * sin(pi / 4), yend = 340 * cos(pi / 4),
               colour = "grey35", linewidth = .3) +
  geom_path(data = arc, aes(x, y), colour = "grey35", linewidth = .3) +
  scale_fill_gradient2(low = "#b03a2e", mid = "grey92", high = "#2c6fa8",
                       midpoint = 0, limits = lim, oob = scales::squish,
                       name = "Run value") +
  coord_fixed(xlim = c(-330, 330), ylim = c(0, 430)) +
  facet_wrap(~grp, nrow = 2) +
  labs(title = "Run value by field position and base-out situation",
       subtitle = paste("Pull side is to the RIGHT of centre in every panel",
                        "(mirrored for left-handed hitters).\nBlue helps the",
                        "offense, red hurts it."),
       x = NULL, y = "Distance (ft)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), panel.grid.minor = element_blank())
ggsave("spray_field_map.png", p_field, width = 12, height = 8, dpi = 140)
cat("wrote spray_direction.png and spray_field_map.png\n")

# The advancement mechanism on its own: ground-ball outs, by ABSOLUTE field
# side, per handedness. If the productive out is real it shows up here as the
# right side beating the left for BOTH hands.
cat("\n== ground-ball value by ABSOLUTE field side (+ = right side) ==\n")
abs_tab <- sweep[launch_angle == -5 & abs(spray_abs) <= 35, .(
    left_side  = mean(rv[spray_abs <= -10]),
    right_side = mean(rv[spray_abs >=  10])), by = .(grp, stand)]
abs_tab[, right_minus_left := right_side - left_side]
print(abs_tab[grp %in% c("empty", "r2_adv", "r3_sac"),
              .(grp, stand, left = round(left_side, 3), right = round(right_side, 3),
                `right-left` = round(right_minus_left, 3))][order(grp, stand)],
      row.names = FALSE)

saveRDS(list(model = m, sweep = sweep, abs_tab = abs_tab, premium = prem, bip_n = nrow(bip),
             ev_ref = EV_REF, seasons = SEASONS,
             field = bip[!is.na(hit_distance_sc) & hit_distance_sc > 0,
                         .(field_x, field_y, pull_ang, launch_angle, launch_speed,
                           grp, events, delta_run_exp)]),
        OUT_RDS)
cat("\nwrote ", OUT_RDS, "\n", sep = "")
