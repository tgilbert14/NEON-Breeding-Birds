# ===========================================================================
# write_manifest.R — (re)generate manifest.json for a lean, bundle-only
# Posit Connect Cloud deploy (git-backed).
#
# Bundles ONLY what the running app needs: global/ui/server + R/ + www/ + the
# precomputed indexes (data/*.rds) + the per-site bundles (data/sites/*.rds) +
# the demo sample. It does NOT bundle scripts/, docs/, rsconnect/, or the README.
#
# neonUtilities is intentionally EXCLUDED — it's referenced dynamically in
# global.R (.NEON_PKG) so the dependency scanner never pins it, keeping the
# deploy lean (no wasm build; live-pull-on-cold-worker is a hang risk). The
# deployed app is bundle-only; the optional live-fetch still works in local dev.
#
# Run with an R that has the app's runtime packages (R 4.3.1 here has them all):
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/write_manifest.R
# Re-run whenever runtime dependencies change, then commit manifest.json.
# ===========================================================================
suppressMessages(library(rsconnect))

appFiles <- c(
  "global.R", "ui.R", "server.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  Sys.glob("data/*.rds"),                                       # precomputed indexes
  list.files("data/sites", pattern = "\\.rds$", full.names = TRUE),
  list.files("data/env",   pattern = "\\.rds$", full.names = TRUE),   # env overlays
  list.files("data-sample", pattern = "\\.rds$", full.names = TRUE)
)
appFiles <- unique(appFiles[file.exists(appFiles)])

cat(sprintf("Writing manifest for %d files (%d site bundles)...\n",
            length(appFiles), length(list.files("data/sites", pattern = "\\.rds$"))))
rsconnect::writeManifest(appDir = ".", appFiles = appFiles)

# ---- pin terra to the last release before the GDAL-3.8 multidim code (1.8-54) ----
# terra >= 1.8-54 ships gdal_multidimensional.cpp using a GDAL 3.8 call unguarded in
# releases, so it FAILS to compile against Connect Cloud's GDAL 3.4.1. Connect compiles
# from source regardless of repo. 1.8-50 is the last release before 1.8-54: it compiles
# on 3.4.1 and still satisfies raster's terra (>= 1.8-5). terra/raster are install-only
# (leaflet -> raster -> terra; app never calls terra) -> zero runtime impact. Also pin
# the repo to the RSPM jammy binary mirror for suite consistency.
local({
  mm <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  if (!is.null(mm$packages$terra)) {
    mm$packages$terra$description$Version <- "1.8-50"
    if (!is.null(mm$packages$terra$description$RemoteSha)) mm$packages$terra$description$RemoteSha <- "1.8-50"
    jsonlite::write_json(mm, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  mtxt <- readLines("manifest.json", warn = FALSE)
  mtxt <- gsub("https://cloud.r-project.org", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  mtxt <- gsub("https://packagemanager.posit.co/cran/latest", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  writeLines(mtxt, "manifest.json")
  cat("Pinned terra to 1.8-50 + RSPM jammy repo.\n")
})

# ---- HARD GATE: a leaked heavy-pull package must never commit silently -------
# Parse the manifest's actual package KEYS (not a substring scan — the word
# "arrow" / "data.table" appears inside other packages' Suggests/Imports text and
# would false-positive). neonUtilities + arrow are the live-fetch / columnar pull
# packages that have NO business in a bundle-only deploy; their presence is a leak
# and stop()s with a non-zero exit. data.table is a MANDATORY transitive Import of
# plotly (plotly DESCRIPTION: Imports ... data.table), so it is allowed ONLY when
# plotly is also present; data.table without plotly is a genuine leak and fails.
m    <- jsonlite::fromJSON("manifest.json")
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages.\n", length(pkgs)))

leaked <- intersect(c("neonUtilities", "arrow"), pkgs)
if ("data.table" %in% pkgs && !("plotly" %in% pkgs))
  leaked <- c(leaked, "data.table")   # data.table only legitimate via plotly

if (length(leaked)) {
  stop(sprintf(
    "LEAN-MANIFEST GATE FAILED: %s leaked into manifest.json. A bundle-only deploy must not carry it. Check the global.R .NEON_PKG split-string guard and the appFiles scope; do NOT commit this manifest.",
    paste(leaked, collapse = ", ")), call. = FALSE)
}
cat("OK: no leaked heavy-pull package (neonUtilities / arrow / stray data.table) in the manifest.\n")
if ("data.table" %in% pkgs) cat("  (data.table present as plotly's required Import — allowed.)\n")
