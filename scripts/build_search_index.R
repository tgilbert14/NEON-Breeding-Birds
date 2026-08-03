# Build the network search index from the same exact 2017-2024 physical-count
# window as every other multi-site bird comparison. Lifetime site-index metrics
# are deliberately excluded from this artifact.

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("R/site_metadata.R")
source("R/bird_helpers.R")

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
SITE_DIR <- file.path(ROOT, "data", "sites")
CROSS_PATH <- file.path(ROOT, "data", "cross_site.rds")
OUT <- file.path(ROOT, "data", "search_index.rds")
files <- list.files(SITE_DIR, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
codes <- sort(sub("[.]rds$", "", basename(files)))
if (!identical(codes, sort(as.character(neon_sites$site))) || length(codes) != 47L)
  stop("Search-index build requires the exact 47-site release roster.", call. = FALSE)
if (!file.exists(CROSS_PATH))
  stop("Search-index build requires the validated cross-site artifact first.", call. = FALSE)
cross <- readRDS(CROSS_PATH)
if (!is.data.frame(cross) || nrow(cross) != 47L ||
    !identical(sort(as.character(cross$site)), codes) ||
    !identical(as.integer(attr(cross, "schema_version", exact = TRUE)), 4L) ||
    any(cross$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN) ||
    any(cross$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX))
  stop("Search-index cross-site input violates the schema-v4 window contract.", call. = FALSE)

site_meta <- function(code) {
  m <- neon_sites[neon_sites$site == code, , drop = FALSE]
  list(name = if (nrow(m)) m$name[[1]] else code,
       state = if (nrow(m)) m$state[[1]] else NA_character_)
}

rows <- lapply(files, function(path) {
  code <- sub("[.]rds$", "", basename(path))
  b <- readRDS(path)
  if (!identical(as.integer(b$meta$schema_version), 4L))
    stop(code, " is not a schema-v4 physical-count bundle.", call. = FALSE)
  effort <- bird_validate_effort(b$opportunity, b$visits, b$obs)
  visits <- effort$visits
  keep <- visits$valid_count & visits$year >= BIRD_CROSS_SITE_YEAR_MIN &
    visits$year <= BIRD_CROSS_SITE_YEAR_MAX
  visits <- visits[keep, , drop = FALSE]
  if (nrow(visits) < 1L)
    stop(code, " has no valid counts in the shared search window.", call. = FALSE)
  obs <- bird_prepare_obs(b$obs)
  obs <- obs[as.character(obs$survey_id) %in% as.character(visits$survey_id), , drop = FALSE]
  bird_validate_visits(visits, obs)
  board <- species_board(obs, points = NULL, nvis = nrow(visits),
                         opportunity = NULL, visits = visits,
                         observer_support = NULL)
  if (is.null(board) || !nrow(board)) return(NULL)
  eligible <- eligible_breeding_detections(obs)
  years <- eligible %>%
    dplyr::group_by(.data$communityScientificName) %>%
    dplyr::summarise(year_min = min(.data$year), year_max = max(.data$year), .groups = "drop") %>%
    dplyr::rename(scientificName = "communityScientificName")
  meta <- site_meta(code)
  out <- board %>% dplyr::transmute(
    scientificName = .data$scientificName,
    vernacular = .data$vernacular,
    site = code,
    name = meta$name,
    state = meta$state,
    analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
    analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
    detection_index_window = .data$index,
    detection_frequency_window = .data$detection_frequency,
    detection_rows_window = .data$detections,
    n_detected_counts_window = .data$n_detected_counts,
    n_points_detected_window = .data$n_points,
    distance_usable_pct_window = .data$distance_usable_pct,
    distance_n_used_window = .data$distance_n_used,
    distance_n_outside_truncation_window = .data$distance_n_outside_truncation,
    method = .data$method
  ) %>% dplyr::left_join(years, by = "scientificName")
  out$vernacular[is.na(out$vernacular) | !nzchar(trimws(out$vernacular))] <-
    out$scientificName[is.na(out$vernacular) | !nzchar(trimws(out$vernacular))]
  out
})

taxa <- dplyr::bind_rows(rows)
if (!nrow(taxa)) stop("Search index contains no window-qualified species.", call. = FALSE)
support <- taxa %>% dplyr::count(.data$scientificName, name = "n_sites_window")
taxa <- taxa %>% dplyr::left_join(support, by = "scientificName") %>%
  dplyr::arrange(.data$scientificName, dplyr::desc(.data$detection_index_window), .data$site)

meta <- neon_sites[match(cross$site, neon_sites$site), , drop = FALSE]
sites <- tibble::tibble(
  site = as.character(cross$site),
  name = meta$name,
  state = meta$state,
  analysis_year_min = as.integer(cross$analysis_year_min),
  analysis_year_max = as.integer(cross$analysis_year_max),
  bird_year_min = as.integer(cross$bird_year_min),
  bird_year_max = as.integer(cross$bird_year_max),
  S_obs = as.integer(cross$S_obs),
  S_rare = as.numeric(cross$S_rare),
  t_used = as.integer(cross$t_used),
  T_counts = as.integer(cross$T_counts),
  n_points_window = as.integer(cross$n_points_window),
  n_birds_window = as.numeric(cross$n_birds_window),
  n_positive_counts_window = as.integer(cross$n_positive_counts_window),
  n_supported_zero_counts_window = as.integer(cross$n_supported_zero_counts_window),
  birds_per_count_window = as.numeric(cross$birds_per_count_window),
  top_species_window = as.character(cross$top_species_window),
  coverage = as.numeric(cross$coverage)
) %>% dplyr::arrange(dplyr::desc(.data$S_obs), .data$site)

expected_taxa <- c(
  "scientificName", "vernacular", "site", "name", "state",
  "analysis_year_min", "analysis_year_max", "detection_index_window",
  "detection_frequency_window", "detection_rows_window",
  "n_detected_counts_window", "n_points_detected_window",
  "distance_usable_pct_window", "distance_n_used_window",
  "distance_n_outside_truncation_window", "method", "year_min", "year_max",
  "n_sites_window")
expected_sites <- c(
  "site", "name", "state", "analysis_year_min", "analysis_year_max",
  "bird_year_min", "bird_year_max", "S_obs", "S_rare", "t_used", "T_counts",
  "n_points_window", "n_birds_window", "n_positive_counts_window",
  "n_supported_zero_counts_window", "birds_per_count_window",
  "top_species_window", "coverage")
if (!identical(names(taxa), expected_taxa) || !identical(names(sites), expected_sites) ||
    nrow(sites) != 47L || !identical(sort(sites$site), codes) ||
    any(taxa$year_min < BIRD_CROSS_SITE_YEAR_MIN |
          taxa$year_max > BIRD_CROSS_SITE_YEAR_MAX) ||
    any(taxa$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN |
          taxa$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX) ||
    any(sites$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN |
          sites$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX) ||
    any(sites$n_positive_counts_window + sites$n_supported_zero_counts_window !=
          sites$T_counts) ||
    any(c("n_species", "n_points", "n_visits", "birds_per_count", "top_species",
          "n_opportunities") %in% names(sites)))
  stop("Search index failed its exact window-only schema validation.", call. = FALSE)

search_index <- list(
  schema_version = 4L,
  analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
  analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
  incidence_unit = "valid physical six-minute count keyed by survey_id",
  taxa = tibble::as_tibble(taxa),
  sites = sites)
saveRDS(search_index, OUT, compress = "xz", version = 3)
cat(sprintf(
  "OK: wrote %s (%d 2017-2024 taxon-site rows, %d taxa, 47 sites).\n",
  OUT, nrow(taxa), length(unique(taxa$scientificName))))
