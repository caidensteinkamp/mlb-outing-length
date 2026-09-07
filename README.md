# MLB Starters — Outing Length Leaderboard

Ranks major-league starting pitchers on how well they generate long outings,
from Baseball Savant pitch-level data. Rebuilds and redeploys itself every
morning via GitHub Actions.

**Live app:** https://milkmen.shinyapps.io/mlb-outing-length/

## The metric

Innings per start is mostly a measure of the *manager* — leash length, bullpen
quality, roster construction, September shutdowns. It says who was *allowed* to
go deep, not who earned it.

**xOuts/start** instead takes each pitcher's own per-pitch event rates (ball /
called-or-swinging strike / foul / in play, from each count) plus his own
outs-per-ball-in-play, and solves an outing-length dynamic program under the
*league-average* removal hazard. Holding the manager constant is the whole
trick. **Leash** is then actual outs minus xOuts: what he was given versus what
he bought.

Nothing is ranked on outing length alone. A ground ball is the cheapest out in
baseball, so optimizing innings by itself just says "pitch to contact" — every
table carries runs saved per 100 pitches beside it.

The premise, from 2026 data: a double play is worth **+1.28** outs of outing,
any batted-ball out about **+0.36**, a strikeout **+0.09**, a walk **−0.93**.
Meanwhile a strikeout and a ground-ball out are worth *identical* run
prevention (+0.267 vs +0.266). The strikeout is free on the scoreboard and
expensive on the pitch count. Across 159 qualified starters, the correlation
between strikeout rate and expected outing length is **+0.011** — nothing at
all. With walk rate it is **−0.685**.

## Daily tracker

The leaderboard is a season verdict; the **Daily tracker** tab is the
game-by-game one. Pick a date and it shows every qualified start that day, a
cumulative-OLV curve through each outing, and whether the start extended or
shortened that pitcher's outing length.

Two senses of "extended" are reported side by side, because they disagree in
informative ways:

- **OLV** — outs of outing gained or lost across the start, against a
  league-average pitch from the same count and base-out state. Framework-native
  and *independent of the manager*: a starter pulled after four innings can
  still post a strongly positive OLV, because it prices the events he generated.
- **vs xIP** — actual innings minus the outing length his own season pitch
  economy predicts. This one *does* include the manager's decision.

On 2026-09-03, Shane McClanahan went +1.97 OLV but −0.24 vs xIP (he generated
outing length and was pulled at 72 pitches anyway); Hunter Brown was the mirror
image at −0.40 OLV and +1.16 vs xIP. Neither column alone tells you that.

Pitch-level detail for the curves is retained for the last `OL_TRACK_DAYS` days
(default 35) — the full season would be ~600k rows in a bundle that is otherwise
under a megabyte, and nobody opens a tracker to read April. The per-start table
covers the whole season regardless.

## Batted-ball direction and run value

`spray_run_value.R` is a second, hitter-side model on the same data: what a ball
in play is worth given *where* it goes, and how that changes with runners on.
Outputs `spray_direction.png`, `spray_field_map.png`, and `spray_bundle.rds`.

Same two-stage shape as the outing-length build — separate what happened from
what it was worth in that situation — but priced through run expectancy over
(bases, outs) instead of a DP over (pitches, count, outs).

### Three things it has to get right

**Coordinates.** `hc_x`/`hc_y` are stringer-placed pixels on a Gameday field
image. The pixel→feet scale solves empirically to **2.303 ft/px** (R² = 0.96
against `hit_distance_sc`), but the radius is *not* usable: the two disagree by
a median of **30 ft**, because `hc_*` is where the ball was fielded and
`hit_distance_sc` is projected flight. Angle from the stringer, depth from
Statcast.

**Handedness.** Raw spray averages −3.8° for righties and +7.3° for lefties,
each toward his own pull side. Validation that the mirror is right: home runs
come out 81% / 84% pulled, and by batted-ball type the ordering is grounders
(+15.0°) → liners (+5.9°) → fly balls (−2.8°) → popups (−19.8°).

**The confound.** Direction is entangled with contact quality — mean launch
angle falls from 25° on oppo balls to 4° on heavily pulled ones, HR rate 2% → 9%.
Every number below holds exit velocity and launch angle fixed.

### Two coordinate systems, not one

- **Outcome quality** (does it become a hit?) is **pull-relative** — it depends
  on where the ball is relative to how the defense plays *this* hitter.
- **Runner advancement** (does the runner reach third?) is **absolute field
  side** — the throw goes away from third on a right-side grounder regardless of
  the batter's hands.

Carrying only pull-relative direction averages the second effect toward zero
across handedness, since the productive-out direction is the opposite field for
a righty and the pull side for a lefty. Both terms are in the model; they stay
identifiable because the sign relation between them flips with the batter's hand.

### All 24 base-out states, by shrinkage

The sample is brutally uneven — **26,915** balls in play with the bases empty and
nobody out against **221** with a runner on third and nobody out, and six states
under a thousand. Fitting each state a free 64-parameter tensor would be fitting
noise in the thin corners; collapsing them into tactical groups throws away the
question.

So each state gets its own direction curve through a factor smooth
(`bs = "fs"`), shrunk toward a common shape by its own sample size. Same device
as `K_SHRINK` in the outing-length model: thin cells borrow from the league, fat
cells stand on their own.

### What it finds

Fitted on **109,305 balls in play** (2026). The dominant effect is launch-angle
dependent and large. Holding exit velocity at 88 mph, pull minus oppo:

| Batted ball | Pull − oppo |
|---|---|
| Fly ball (25°) | **+0.34** |
| Line drive (10°) | −0.08 |
| Ground ball (−5°) | −0.11 |

Pull your fly balls; go the other way on the ground. Exit velocity and launch
angle are held fixed, so this is direction itself, not contact quality.

### The productive out does not survive; the double play does

Two tactical channels, each tested model-free on a conditioning set that removes
the other:

**Advancement — worth nothing.** Restrict to ground-ball *outs* (n = 26,717), so
hit probability cannot contribute and `delta_run_exp` already contains whatever
the runners did. Right side minus left side comes to **−0.001 runs overall**, and
no individual base-out state exceeds ±0.02. Hitting behind the runner, once the
ball is an out, is not measurably worth anything.

**Double-play avoidance — worth a lot.** Ground balls with a runner on first
under two outs, measured as GIDP *rate* so hit probability again cannot leak in:

| Hitter | Side | n | GIDP% |
|---|---|---|---|
| RH | left (pull) | 3,014 | **37.8** |
| RH | right (oppo) | 1,175 | **23.3** |
| LH | left (oppo) | 844 | **13.4** |
| LH | right (pull) | 2,986 | 25.7 |

For both hands the opposite field is the escape — a righty pulling a grounder
feeds the 6-4-3, a lefty pulling one feeds the 4-6-3. The ordering survives at
every exit-velocity grade, so it is geometry rather than soft contact.

An earlier version of this README reported an "advancement effect" of +0.034
runs for righties. That number was the *hit-probability* channel leaking in —
opposite-field grounders beat a shaded defense and become hits, and hits are
worth more with runners on. The tell was that the effect peaked with two outs,
where a productive out cannot exist. Conditioning on outs removes it entirely.

## Pipeline## Pipeline

| Script | Does | Runtime |
|---|---|---|
| `mlb_savant_fetch.R` | Pulls Savant pitch data in 3-day chunks | ~75 s incremental, ~30 min cold |
| `mlb_outing_length.R` | Economy → removal hazard → DP → OLV → leaderboard | ~45 s |
| `make_deploy.R` | Slims the bundle, assembles the app directory | instant |
| `smoke_test.R` | Forces all 15 outputs across 3 filter states | ~20 s |
| `deploy.R` | Pushes to shinyapps.io | ~1 min |

`mlb_outing_length_app.R` is the app source; `make_deploy.R` copies it to
`mlb_outing_deploy/app.R` beside a slim `data/bundle.rds`. The app resolves
`data/bundle.rds` ahead of any absolute path, so one file works both locally and
deployed.

### Two caching subtleties that matter

The Savant CSV endpoint caps a response at **25,000 rows and truncates
silently**, dropping the earliest dates in the window. 3-day chunks stay near
12,000; any chunk that still returns at the cap is bisected and re-pulled rather
than trusted.

Chunks are cached, but **a chunk is only final once its last day is well
behind us** (`OL_FRESH_DAYS`, default 4). Caching a trailing window once is
actively wrong and fails silently: the first run happened before that day's
games posted, cached zero pitches for it, and on a nightly schedule that entry
would have been honored forever — the job would keep reporting success while the
leaderboard quietly froze. The same guard applies to the season file itself.

## Scheduled updates

`.github/workflows/daily-update.yml` runs at 11:40 UTC (7:40 am ET) — after
every West Coast game is final. It restores the pitch-chunk cache, fetches only
unsettled days, rebuilds, smoke-tests, and deploys. A failed smoke test fails
the job *before* the deploy step, so a bad bundle can never replace a working
live app. Run it by hand any time from the Actions tab.

### Setup

Three repository secrets are required (Settings → Secrets and variables →
Actions). Get the values from shinyapps.io → **Account → Tokens → Show**:

| Secret | Value |
|---|---|
| `SHINYAPPS_ACCOUNT` | your shinyapps.io account name |
| `SHINYAPPS_TOKEN` | the token |
| `SHINYAPPS_SECRET` | the matching secret |

Credentials are read from the environment only — `deploy.R` contains none and
fails loudly if any are missing.

### Free-tier limits worth knowing

shinyapps.io free gives **25 active hours/month**, 5 apps, and 1 GB RAM per
instance. Active hours burn whenever someone has the app open, so a post that
lands well can exhaust the month in a day or two. The bundle is slimmed from
~88 MB in memory to ~1.5 MB partly for this reason — the app wakes fast and
holds almost nothing. If it gets traffic, the Starter plan lifts the hour cap.

## Running locally

```bash
Rscript mlb_savant_fetch.R 2026
OL_SEASONS=2026 OL_OUT_RDS=mlb_outing_bundle_2026.rds Rscript mlb_outing_length.R
OL_SRC_BUNDLE=mlb_outing_bundle_2026.rds Rscript make_deploy.R 2026
Rscript -e 'shiny::runApp("mlb_outing_deploy", launch.browser = TRUE)'
```

Every path is overridable by environment variable (`OL_RAW_DIR`, `OL_SEASONS`,
`OL_OUT_RDS`, `OL_CODE_DIR`, `OL_DEPLOY_DIR`, `OL_SRC_BUNDLE`, `OL_BUNDLE`,
`OL_FRESH_DAYS`) so the same scripts run on a Mac and on a CI runner.

## Caveats

Balls in play are credited as the outs they actually produced, so a starter in
front of a good infield does bank cheaper outs — real to his outing length
without being his skill. The **Sources** tab splits each pitcher's edge into a
count channel (his) and a contact channel (partly his defense and park).

Year over year, xOuts is *less* stable than actual innings (r = 0.39 vs 0.73),
because role assignment is stickier than pitch economy. Read the board as a
description of who is earning length now, not a projection.
