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

## Pipeline

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
