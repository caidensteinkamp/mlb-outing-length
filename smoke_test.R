# =============================================================================
# smoke_test.R -- exercise every render path in the built app, headlessly
#
#   Rscript smoke_test.R [app_dir]      # default: mlb_outing_deploy
#
# Runs before the deploy step so a bad bundle fails the job instead of replacing
# a working live app. Every output is forced, not just constructed: a Shiny
# output that is never read is never evaluated, so simply starting the app
# proves nothing. Both handedness filters are exercised because a filter that
# empties a table is exactly the case that used to break the quadrant plot.
# =============================================================================

app_dir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "mlb_outing_deploy"
stopifnot(dir.exists(app_dir))

old <- setwd(app_dir)
on.exit(setwd(old), add = TRUE)

OUTPUTS <- c("scorecard", "card_title", "t_board", "t_both", "t_mix",
             "t_long", "t_short", "t_econ", "p_quad", "p_sources",
             "p_card", "p_hist", "p_leash", "p_outcome", "p_ole")

shiny::testServer(shiny::shinyAppFile("app.R"), {
  failures <- character()

  probe <- function(label) {
    for (o in OUTPUTS) {
      r <- try(output[[o]], silent = TRUE)
      if (inherits(r, "try-error"))
        failures <<- c(failures, sprintf("%s [%s]", o, label))
    }
  }

  session$setInputs(min_gs = 14, hand = c("R", "L"))
  probe("both hands")

  session$setInputs(hand = "L")
  probe("LHP only")

  session$setInputs(hand = "R", min_gs = 25)
  probe("RHP, 25+ GS")

  if (length(failures))
    stop("broken outputs:\n  ", paste(failures, collapse = "\n  "))

  cat(sprintf("smoke test passed: %d outputs x 3 filter states\n", length(OUTPUTS)))
})
