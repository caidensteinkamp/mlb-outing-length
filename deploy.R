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

# Give every repository NAME a resolvable URL before building the manifest.
#
# CI installs packages from Posit Package Manager (setup-r's use-public-rspm),
# so each package's DESCRIPTION carries `Repository: RSPM`. rsconnect/renv
# records that name in the manifest and then asks getOption("repos") to turn it
# into a URL. If "RSPM" isn't a key there, the name is emitted verbatim and the
# shinyapps.io build server fails with:
#
#   Error fetching data.table source. Error downloading package source:
#   Unsupported url scheme: RSPM/src/contrib/data.table_1.18.6.1.tar.gz
#
# Mapping RSPM to its real URL fixes that while keeping CI's fast binary
# installs. CRAN stays as the fallback for anything installed from there.
options(repos = c(
  RSPM = "https://packagemanager.posit.co/cran/latest",
  CRAN = "https://cloud.r-project.org"))

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

# Diagnostic. This failure mode is invisible from the R side -- the deploy call
# succeeds locally and only the shinyapps.io build server rejects the manifest --
# so print the two values that decide it before shipping anything.
cat("\n-- manifest inputs --\n")
print(getOption("repos"))
local({
  mf <- file.path(APP_DIR, "manifest.json")
  on.exit(unlink(mf), add = TRUE)
  try({
    rsconnect::writeManifest(APP_DIR)
    j <- jsonlite::fromJSON(mf, simplifyVector = FALSE)
    p <- j$packages[["data.table"]]
    cat("data.table  Source:", p$Source, " Repository:", p$Repository, "\n")
    if (is.null(p$Repository) || !grepl("^https?://", p$Repository))
      cat("!! Repository is not a URL -- the build server will reject this\n")
  }, silent = FALSE)
})
cat("---------------------\n\n")

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
