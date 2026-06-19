# ===========================================================================
# fetch_bird_all.R — pull raw NEON Breeding landbird point counts
# (DP1.10003.001) for EVERY NEON terrestrial site in R/site_metadata.R.
# Run with R-4.1.1 (neonUtilities; R-4.5.x crashes on loadByProduct). Launch
# from PowerShell, not git-bash. Saves loadByProduct() list per site to
# ../bird-data-fetch/<SITE>_raw.rds.
#
# RESUMABLE: skips any site whose <SITE>_raw.rds already exists (so HARV/SCBI/
# WOOD from the demo pull are kept). Delete a file to re-pull it.
#   & "C:\Program Files\R\R-4.1.1\bin\Rscript.exe" scripts/fetch_bird_all.R
# Optional CLI subset:  ... scripts/fetch_bird_all.R JORN SRER CPER
# ===========================================================================
options(timeout = 3600)
suppressPackageStartupMessages(library(neonUtilities))
`%||%` <- function(a, b) if (is.null(a)) b else a

# token: bird repo .neon_token → mammal app's → NEON_TOKEN env var
tok <- ""
for (p in c(".neon_token", "../App-NEON-Small-Mammal-Tracker/.neon_token")) {
  if (file.exists(p)) { tok <- tryCatch(trimws(readLines(p, warn = FALSE))[1], error = function(e) ""); if (nzchar(tok)) break }
}
if (!nzchar(tok)) tok <- Sys.getenv("NEON_TOKEN", "")
cat(if (nzchar(tok)) "Using NEON API token (higher rate limits).\n" else "No token — anonymous rate limits.\n")

source("R/site_metadata.R")           # canonical 47-site list
sites <- neon_sites$site
args  <- commandArgs(trailingOnly = TRUE)
if (length(args)) sites <- intersect(sites, args)

outdir <- "../bird-data-fetch"; dir.create(outdir, showWarnings = FALSE)
start_d <- "2013-01"; end_d <- format(Sys.Date(), "%Y-%m")

cat(sprintf("Fetching DP1.10003.001 for %d sites (%s → %s)\n\n", length(sites), start_d, end_d))
ok <- character(0); empty <- character(0); failed <- character(0)
for (s in sites) {
  f <- file.path(outdir, paste0(s, "_raw.rds"))
  if (file.exists(f)) { cat(sprintf("• %-5s skip (exists, %.0f KB)\n", s, file.size(f)/1e3)); ok <- c(ok, s); next }
  cat(sprintf("• %-5s fetching…\n", s)); flush.console()
  res <- tryCatch(
    loadByProduct(dpID = "DP1.10003.001", site = s, startdate = start_d, enddate = end_d,
                  package = "basic", check.size = "FALSE", token = if (nzchar(tok)) tok else NA),
    error = function(e) { cat("    ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(res) || is.null(res$brd_countdata) || !nrow(res$brd_countdata)) {
    cat(sprintf("    no bird data for %s\n", s)); empty <- c(empty, s); next
  }
  saveRDS(res, f)
  cat(sprintf("    saved %s — countdata %d rows, perpoint %d rows, %.0f KB\n",
              s, nrow(res$brd_countdata), nrow(res$brd_perpoint %||% data.frame()), file.size(f)/1e3))
  ok <- c(ok, s); flush.console()
}
cat(sprintf("\nDONE. %d with data, %d empty, %d failed.\n", length(ok), length(empty), length(failed)))
if (length(empty)) cat("  empty:", paste(empty, collapse = ", "), "\n")
