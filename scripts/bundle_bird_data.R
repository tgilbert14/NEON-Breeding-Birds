# ===========================================================================
# Bundle NEON Breeding landbird point counts (DP1.10003.001) into per-site .rds.
# Reads raw ../bird-data-fetch/<SITE>_raw.rds (fetch_bird_demo.R, R-4.1.1).
# Each bundle = list(obs, points, meta):
#   obs    — one row per DETECTION: pointkey (plotID+pointID), plotID, year, bout,
#            eventID, taxonID, scientificName, vernacularName, taxonRank, is_species,
#            observerDistance, detectionMethod, clusterSize (# birds), sexOrAge.
#   points — one row per point: pointkey, plotID, nlcdClass, lat, lng, n_visits
#            (distinct point x bout = the effort denominator), observedHabitat.
#   meta   — site, lat, lng, years, n_visits (site total point-counts).
# Abundance index = sum(clusterSize) / n point-visits = birds per point-count.
# ===========================================================================
suppressWarnings(suppressMessages({ library(dplyr) }))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
RAW <- "../bird-data-fetch"; DEMO <- "CLBJ"   # data-sample/demo.rds fallback = the app default (global.R DEMO_META); see suite data-audit (CLBJ has a stable Chao2; HARV's was a 3x extrapolation)
# bundle EVERY site we fetched (fetch_bird_all.R) — derive from the raw files present
SITES <- sort(sub("_raw\\.rds$", "", list.files(RAW, pattern = "_raw\\.rds$")))
if (!length(SITES)) stop("No <SITE>_raw.rds in ", RAW, " — run scripts/fetch_bird_all.R first.")
cat(sprintf("Bundling %d sites: %s\n", length(SITES), paste(SITES, collapse = " ")))

is_species_rank <- function(rank, sci) {
  ok <- is.na(rank) | rank %in% c("species", "subspecies", "speciesGroup")
  amb <- grepl("\\bsp\\.?$", ifelse(is.na(sci), "", sci)) | grepl("/", ifelse(is.na(sci), "", sci), fixed = TRUE)
  ok & !amb
}

build_site <- function(site) {
  f <- file.path(RAW, paste0(site, "_raw.rds")); if (!file.exists(f)) { cat("  MISSING", f, "\n"); return(NULL) }
  r <- readRDS(f)
  cd <- tibble::as_tibble(r$brd_countdata); pp <- tibble::as_tibble(r$brd_perpoint)
  num <- function(x) suppressWarnings(as.numeric(x))
  # NEON codes "distance not estimable" (flocks / flyovers / detected-but-far) as the
  # sentinel observerDistance == 999 (occasionally 9999) — a placeholder, NOT a metre
  # measurement. Recode to NA so the 999 spike (~0.3% of detections, 28 sites) cannot
  # (a) masquerade as a real long-range visual ID in bird_qc()'s "visualfar" flag,
  # (b) leak into the raw distance shown in the profile table / per-species CSV, or
  # (c) pollute any distance summary. It is already truncated out of distance_decay
  # (<=200 m); NA routes it into the honest missing-distance accounting instead.
  na_sentinel <- function(x) { x <- num(x); ifelse(x %in% c(999, 9999), NA_real_, x) }
  pk <- function(plot, pt) paste(plot, pt, sep = "_")

  obs <- cd %>%
    dplyr::filter(!is.na(.data$scientificName), num(.data$clusterSize) > 0) %>%
    dplyr::transmute(
      pointkey = pk(plotID, pointID), plotID, pointID,
      year = as.integer(substr(as.character(startDate), 1, 4)), bout = boutNumber, eventID,
      taxonID, scientificName, vernacularName, taxonRank,
      is_species = is_species_rank(taxonRank, scientificName),
      observerDistance = na_sentinel(observerDistance), detectionMethod, clusterSize = num(clusterSize),
      sexOrAge) %>%
    dplyr::filter(!is.na(.data$year))

  # effort: distinct point x bout visits (the index denominator) + point metadata
  visits <- pp %>% dplyr::filter(!(.data$samplingImpractical %in% c("OK", "Y", "yes")) | is.na(.data$samplingImpractical))
  pt_meta <- pp %>% dplyr::mutate(pointkey = pk(plotID, pointID)) %>%
    dplyr::group_by(.data$pointkey) %>%
    dplyr::summarise(plotID = mode_chr(.data$plotID), nlcdClass = mode_chr(.data$nlcdClass),
                     lat = stats::median(num(.data$decimalLatitude), na.rm = TRUE),
                     lng = stats::median(num(.data$decimalLongitude), na.rm = TRUE),
                     observedHabitat = mode_chr(.data$observedHabitat),
                     n_visits = dplyr::n_distinct(.data$eventID), .groups = "drop")
  meta <- list(site = site, lat = stats::median(pt_meta$lat, na.rm = TRUE), lng = stats::median(pt_meta$lng, na.rm = TRUE),
               years = sort(unique(obs$year)),
               n_visits = nrow(dplyr::distinct(pp, plotID, pointID, eventID)))
  list(obs = obs, points = pt_meta, meta = meta)
}

dir.create("data/sites", showWarnings = FALSE, recursive = TRUE); dir.create("data-sample", showWarnings = FALSE)
idx <- list()
for (s in SITES) {
  cat("=== bundling", s, "===\n"); b <- build_site(s); if (is.null(b)) next
  saveRDS(b, file.path("data/sites", paste0(s, ".rds")), compress = "xz")
  if (identical(s, DEMO)) saveRDS(b, file.path("data-sample", "demo.rds"), compress = "xz")
  sp <- b$obs[b$obs$is_species, ]
  ab <- aggregate(clusterSize ~ scientificName, data = sp, FUN = sum)
  top <- ab$scientificName[which.max(ab$clusterSize)]
  idx[[s]] <- data.frame(site = s, n_species = length(unique(sp$scientificName)),
                         n_points = nrow(b$points), n_visits = b$meta$n_visits,
                         birds_per_count = round(sum(sp$clusterSize[!grepl("flyover", tolower(sp$detectionMethod))]) / b$meta$n_visits, 2),  # flyovers excluded — match the live species_board() breeding index
                         top_species = top, lat = b$meta$lat, lng = b$meta$lng, stringsAsFactors = FALSE)
  cat(sprintf("  %s: %d species, %d points, %d visits, %.2f birds/count, top %s | obs %d | size %s\n",
      s, idx[[s]]$n_species, idx[[s]]$n_points, idx[[s]]$n_visits, idx[[s]]$birds_per_count, top, nrow(b$obs),
      format(file.size(file.path("data/sites", paste0(s, ".rds"))), big.mark = ",")))
}
saveRDS(dplyr::bind_rows(idx), "data/site_index.rds", compress = "xz")
cat("\nsite_index:\n"); print(dplyr::bind_rows(idx)); cat("DONE\n")
