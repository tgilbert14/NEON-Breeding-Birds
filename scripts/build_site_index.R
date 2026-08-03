# Rebuild the Birds national site index from opportunity-complete bundles.
# Uses BIRD_OUTPUT_ROOT when validating a staged release candidate; defaults to
# the repository root for ordinary equality checks.

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("R/site_metadata.R")
source("R/bird_helpers.R")

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
SITE_DIR <- file.path(ROOT, "data", "sites")
OUT <- file.path(ROOT, "data", "site_index.rds")
files <- list.files(SITE_DIR, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
codes <- sort(sub("[.]rds$", "", basename(files)))
expected <- sort(as.character(neon_sites$site))
if (!identical(codes, expected) || length(codes) != 47L) {
  stop("Site-index build requires the exact 47-site RELEASE-2026 bundle roster.", call. = FALSE)
}

eligible_rows <- function(obs) {
  if (is.null(obs) || !nrow(obs)) return(obs)
  eligible_breeding_detections(obs)
}

rows <- lapply(files, function(path) {
  code <- sub("[.]rds$", "", basename(path))
  b <- readRDS(path)
  if (!is.list(b) || !all(c("obs", "visits", "opportunity", "points", "meta") %in% names(b)))
    stop(code, " does not implement bundle schema v4.", call. = FALSE)
  if (!identical(b$meta$release, "RELEASE-2026") || !identical(as.integer(b$meta$schema_version), 4L))
    stop(code, " has the wrong release or schema identity.", call. = FALSE)
  sp <- eligible_rows(b$obs)
  nvis <- sum(b$visits$valid_count %in% TRUE)
  nopp <- sum(b$opportunity$supported %in% TRUE)
  totals <- if (nrow(sp)) stats::aggregate(clusterSize ~ communityScientificName, sp, sum) else NULL
  top <- if (!is.null(totals) && nrow(totals)) totals$communityScientificName[which.max(totals$clusterSize)] else NA_character_
  tibble::tibble(
    site = code,
    n_species = length(unique(sp$communityScientificName)),
    n_points = sum(b$points$n_visits > 0),
    n_visits = nvis,
    n_supported_zero_counts = sum(b$visits$outcome == "supported_zero"),
    n_opportunities = nopp,
    n_supported_zero = sum(b$opportunity$outcome == "supported_zero"),
    birds_per_count = if (nvis > 0) round(sum(sp$clusterSize, na.rm = TRUE) / nvis, 3) else NA_real_,
    top_species = top,
    lat = as.numeric(b$meta$lat),
    lng = as.numeric(b$meta$lng),
    release = b$meta$release,
    schema_version = as.integer(b$meta$schema_version)
  )
})

index <- dplyr::bind_rows(rows)
if (nrow(index) != 47L || any(index$n_visits <= 0) || any(index$n_opportunities <= 0))
  stop("Site index failed roster or positive-effort validation.", call. = FALSE)
saveRDS(index, OUT, compress = "xz", version = 3)
cat(sprintf("OK: wrote %s for %d release sites.\n", OUT, nrow(index)))
