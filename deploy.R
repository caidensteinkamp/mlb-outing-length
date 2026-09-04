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

SCRIPT_DIR <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(dirname(f[1])) else getwd()
})

APP_DIR  <- Sys.getenv("OL_DEPLOY_DIR", file.path(SCRIPT_DIR, "mlb_outing_deploy"))
APP_NAME <- Sys.getenv("SHINY_APP_NAME", "mlb-outing-length")

# Two credential paths, because the two callers are different.
#
#   CI       -- the three env vars, injected from GitHub Actions secrets.
#   locally  -- whatever rsconnect already has saved. shinyapps.io hands you a
#               ready-made setAccountInfo() snippet; run it once in R and it is
#               stored under ~/.config/R/rsconnect, so there is no reason to
#               retype a token into a shell command (where it also lands in
#               your history in plain text).
#
# Env vars win when present so a CI run can never accidentally deploy to an
# account left configured on the machine.
need <- c("SHINY_ACCOUNT", "SHINY_TOKEN", "SHINY_SECRET")
vals <- Sys.getenv(need)
have_env <- all(nzchar(vals))

saved <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
have_saved <- !is.null(saved) && nrow(saved) > 0

if (!have_env && !have_saved)
  stop("No shinyapps.io credentials found.\n\n",
       "Local: open shinyapps.io -> Account -> Tokens -> Show, copy the\n",
       "rsconnect::setAccountInfo(...) line it gives you, and run it once in R.\n",
       "It is saved after that and this script will pick it up.\n\n",
       "CI: set SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET as\n",
       "repository secrets (the workflow maps them to SHINY_*).\n\n",
       if (any(nzchar(vals)))
         paste0("Partially set, which is why this failed: missing ",
                paste(need[!nzchar(vals)], collapse = ", "), "\n") else "")

if (have_env) {
  rsconnect::setAccountInfo(name   = vals[["SHINY_ACCOUNT"]],
                            token  = vals[["SHINY_TOKEN"]],
                            secret = vals[["SHINY_SECRET"]])
  account <- vals[["SHINY_ACCOUNT"]]
} else {
  account <- saved$name[1]
  cat("using saved rsconnect account: ", account, "\n", sep = "")
}

if (!file.exists(file.path(APP_DIR, "app.R")))
  stop("no app.R in ", APP_DIR, "\nRun: Rscript make_deploy.R 2026")
if (!file.exists(file.path(APP_DIR, "data", "bundle.rds")))
  stop("no data/bundle.rds in ", APP_DIR, "\nRun: Rscript make_deploy.R 2026")

# forceUpdate skips the interactive "overwrite existing app?" prompt, which
# would otherwise hang a scheduled run forever with no one there to answer it.
rsconnect::deployApp(
  appDir         = APP_DIR,
  appName        = APP_NAME,
  appTitle       = "MLB Starters -- Outing Length Leaderboard",
  account        = account,
  forceUpdate    = TRUE,
  launch.browser = FALSE,
  logLevel       = "normal")

cat(sprintf("\ndeployed: https://%s.shinyapps.io/%s/\n", account, APP_NAME))
