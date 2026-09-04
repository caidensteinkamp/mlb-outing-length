# =============================================================================
# make_deploy.R -- build a self-contained, slim app directory for shinyapps.io
#
#   Rscript make_deploy.R            # pooled 2025+2026 bundle
#   Rscript make_deploy.R 2026       # 2026 only
#
# Produces mlb_outing_deploy/ containing just app.R and data/bundle.rds, which
# is everything shinyapps.io needs and nothing else.
#
# WHY THIS EXISTS. The analysis bundle is 6.7 MB on disk but expands to ~188 MB
# in memory, and the app uses almost none of it:
#
#   haz_mod       153 MB   a glm object, which carries its entire model frame
#                          (342,696 rows) around with it. Never touched by the
#                          app -- the DP was already solved offline.
#   sp             32 MB   every starter plate appearance. Also never touched.
#
# Deploying that would push ~188 MB of resident memory on an instance that gets
# 1 GB, and every cold start would pay to decompress it. Keeping only the 11
# objects the app actually reads takes it to about 2 MB. That is the difference
# between an app that wakes up in a second and one that risks being killed for
# hitting its memory ceiling while someone is reading it.
# =============================================================================

# Default to the directory this script lives in, NOT a hardcoded path. Running
# it from a clone with only OL_SRC_BUNDLE set used to silently build into
# ~/Downloads/Code and leave the checkout untouched, so the deploy step then
# looked for an app directory that was never written. Wherever this file sits is
# where its inputs and outputs belong.
SCRIPT_DIR <- local({
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(dirname(f[1])) else getwd()
})
CODE_DIR   <- Sys.getenv("OL_CODE_DIR", SCRIPT_DIR)
DEPLOY_DIR <- Sys.getenv("OL_DEPLOY_DIR", file.path(CODE_DIR, "mlb_outing_deploy"))

args   <- commandArgs(trailingOnly = TRUE)
season <- if (length(args)) args[1] else "pooled"
src <- Sys.getenv("OL_SRC_BUNDLE", "")
if (!nzchar(src)) {
  src <- if (season == "pooled") {
    file.path(CODE_DIR, "mlb_outing_bundle.rds")
  } else {
    file.path(CODE_DIR, sprintf("mlb_outing_bundle_%s.rds", season))
  }
}
if (!file.exists(src)) stop("no such bundle: ", src)

# Exactly the objects referenced as B$... in the app. Anything not on this list
# is analysis scaffolding the app has no use for.
KEEP <- c("board", "outings", "mix", "prof", "xo",
          "V_lg", "lg_xouts", "meta", "economy", "outcome_value", "lg_prof")

B <- readRDS(src)
missing <- setdiff(KEEP, names(B))
if (length(missing)) stop("bundle is missing: ", paste(missing, collapse = ", "))

slim <- B[KEEP]

dir.create(file.path(DEPLOY_DIR, "data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(slim, file.path(DEPLOY_DIR, "data", "bundle.rds"), compress = "xz")

# The app resolves data/bundle.rds ahead of any absolute path, so the same
# source file works locally and deployed -- no forked copy to keep in sync.
stopifnot(file.copy(file.path(CODE_DIR, "mlb_outing_length_app.R"),
                    file.path(DEPLOY_DIR, "app.R"), overwrite = TRUE))

cat(sprintf("season      : %s\n", season))
cat(sprintf("in-memory   : %.1f MB -> %.1f MB\n",
            as.numeric(object.size(B)) / 1e6, as.numeric(object.size(slim)) / 1e6))
cat(sprintf("bundle file : %.2f MB\n",
            file.size(file.path(DEPLOY_DIR, "data", "bundle.rds")) / 1e6))
cat(sprintf("deploy dir  : %s\n", DEPLOY_DIR))
cat("\nfiles:\n")
for (f in list.files(DEPLOY_DIR, recursive = TRUE, full.names = TRUE))
  cat(sprintf("  %-46s %8.2f MB\n", sub(paste0(DEPLOY_DIR, "/"), "", f),
              file.size(f) / 1e6))

# Smoke test: load the slim bundle the way the deployed app will and confirm
# every object the app dereferences is actually present and non-empty. Cheap
# insurance against shipping a bundle that only fails once it is live.
chk <- readRDS(file.path(DEPLOY_DIR, "data", "bundle.rds"))
bad <- KEEP[!vapply(KEEP, function(k)
  !is.null(chk[[k]]) && length(chk[[k]]) > 0, TRUE)]
cat(if (length(bad)) paste("\nWARNING empty:", paste(bad, collapse = ", "), "\n")
    else "\nsmoke test: all 11 objects present\n")
