# Guarantee a REAL CRAN mirror URL for every R process started in this project.
#
# A fresh R session has repos = c(CRAN = "@CRAN@") -- a placeholder, not a URL.
# rsconnect builds its deploy manifest by looking each package's DESCRIPTION
# `Repository:` name (e.g. "CRAN") up in getOption("repos"). When that lookup
# yields the placeholder, the manifest carries the bare NAME where a URL belongs
# and the shinyapps.io build server fails with:
#
#   Unsupported url scheme: CRAN/src/contrib/data.table_1.18.6.1.tar.gz
#
# Setting this inside deploy.R was not enough on CI, so it is set here instead:
# .Rprofile in the working directory is sourced at startup, before any script
# runs, so every process gets it -- Rscript, the deploy, and renv's snapshot.
local({
  r <- getOption("repos")
  if (is.null(r) || is.na(r["CRAN"]) || !nzchar(r["CRAN"]) || r["CRAN"] == "@CRAN@")
    r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})
