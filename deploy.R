# =============================================================================
# deploy.R -- push the slim app directory to shinyapps.io
#
# Credentials come from the environment, never from this file. In the scheduled
# workflow they arrive as GitHub Actions secrets; to run it by hand, export the
# same three variables in your shell first. Nothing here is ever committed.
#
#   SHINY_ACCOUNT   your shinyapps.io account name
#   SHINY_TOKEN     Account -> Tokens -> Show -> token
#   SHINY_SECRET    the matching secret
# =============================================================================

need <- c("SHINY_ACCOUNT", "SHINY_TOKEN", "SHINY_SECRET")
vals <- Sys.getenv(need)
if (any(!nzchar(vals)))
  stop("missing credential(s): ", paste(need[!nzchar(vals)], collapse = ", "),
       "\nSet them as GitHub Actions secrets, or export them locally.")

APP_DIR  <- Sys.getenv("OL_DEPLOY_DIR", "mlb_outing_deploy")
APP_NAME <- Sys.getenv("SHINY_APP_NAME", "mlb-outing-length")

if (!file.exists(file.path(APP_DIR, "app.R")))
  stop("no app.R in ", APP_DIR, " -- run make_deploy.R first")
if (!file.exists(file.path(APP_DIR, "data", "bundle.rds")))
  stop("no data/bundle.rds in ", APP_DIR, " -- run make_deploy.R first")

rsconnect::setAccountInfo(name   = vals[["SHINY_ACCOUNT"]],
                          token  = vals[["SHINY_TOKEN"]],
                          secret = vals[["SHINY_SECRET"]])

# forceUpdate skips the interactive "overwrite existing app?" prompt, which
# would otherwise hang a scheduled run forever with no one there to answer it.
rsconnect::deployApp(
  appDir         = APP_DIR,
  appName        = APP_NAME,
  appTitle       = "MLB Starters -- Outing Length Leaderboard",
  account        = vals[["SHINY_ACCOUNT"]],
  forceUpdate    = TRUE,
  launch.browser = FALSE,
  logLevel       = "normal")

cat(sprintf("\ndeployed: https://%s.shinyapps.io/%s/\n",
            vals[["SHINY_ACCOUNT"]], APP_NAME))
