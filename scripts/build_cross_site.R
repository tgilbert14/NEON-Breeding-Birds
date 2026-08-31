# Build the exact 2017-2024 cross-site bird comparison from valid physical
# six-minute counts. Every bird value in this artifact is window-specific; it
# never rejoins lifetime site-index metrics.

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("R/site_metadata.R")
source("R/bird_helpers.R")

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
SITE_DIR <- file.path(ROOT, "data", "sites")
OUT <- file.path(ROOT, "data", "cross_site.rds")
files <- list.files(SITE_DIR, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
codes <- sort(sub("[.]rds$", "", basename(files)))
if (!identical(codes, sort(as.character(neon_sites$site))) || length(codes) != 47L)
  stop("Cross-site build requires the exact 47-site release roster.", call. = FALSE)

incidence <- list()
base <- list()
for (path in files) {
  site <- sub("[.]rds$", "", basename(path))
  b <- readRDS(path)
  if (!identical(as.integer(b$meta$schema_version), 4L))
    stop(site, " is not a schema-v4 physical-count bundle.", call. = FALSE)
  full_effort <- bird_validate_effort(b$opportunity, b$visits, b$obs)
  visits <- full_effort$visits
  keep <- visits$valid_count & visits$year >= BIRD_CROSS_SITE_YEAR_MIN &
    visits$year <= BIRD_CROSS_SITE_YEAR_MAX
  visits <- visits[keep, setdiff(names(visits), "visit_key"), drop = FALSE]
  if (nrow(visits) < 2L)
    stop(site, " has fewer than two valid counts in the shared analysis window.", call. = FALSE)
  obs <- bird_prepare_obs(b$obs)
  obs <- obs[as.character(obs$survey_id) %in% as.character(visits$survey_id), , drop = FALSE]
  bird_validate_visits(visits, obs)

  si <- site_incidence(obs, visits)
  if (is.null(si) || si$T < 2L || !si$opportunity_complete)
    stop(site, " lacks complete valid-count incidence support.", call. = FALSE)
  incidence[[site]] <- si
  sp <- eligible_breeding_detections(obs)
  singing <- if ("method_singing" %in% names(sp)) sp$method_singing else
    grepl("sing", tolower(sp$detectionMethod))
  n_visits_window <- as.integer(nrow(visits))
  n_points_window <- as.integer(dplyr::n_distinct(visits$pointkey))
  incidence_y <- as.integer(si$Y)
  S_obs <- as.integer(length(incidence_y))
  U_incidence <- as.integer(sum(incidence_y))
  Q1_incidence <- as.integer(sum(incidence_y == 1L))
  Q2_incidence <- as.integer(sum(incidence_y == 2L))

  species_count_pairs <- if (nrow(sp)) unique(data.frame(
    communityScientificName = as.character(sp$communityScientificName),
    survey_id = as.character(sp$survey_id), stringsAsFactors = FALSE
  )) else data.frame(
    communityScientificName = character(), survey_id = character(),
    stringsAsFactors = FALSE)
  species_point_pairs <- if (nrow(sp)) unique(data.frame(
    communityScientificName = as.character(sp$communityScientificName),
    pointkey = as.character(sp$pointkey), stringsAsFactors = FALSE
  )) else data.frame(
    communityScientificName = character(), pointkey = character(),
    stringsAsFactors = FALSE)
  n_positive_counts_window <- as.integer(length(unique(species_count_pairs$survey_id)))
  n_supported_zero_counts_window <- as.integer(si$T - n_positive_counts_window)
  n_birds_window <- if (nrow(sp)) sum(sp$clusterSize) else 0

  species_birds <- if (nrow(sp)) stats::aggregate(
    clusterSize ~ communityScientificName, sp, sum
  ) else data.frame(communityScientificName = character(), clusterSize = numeric())
  if (nrow(species_birds)) {
    species_birds <- species_birds[order(
      -species_birds$clusterSize, as.character(species_birds$communityScientificName),
      method = "radix"
    ), , drop = FALSE]
    top_species <- as.character(species_birds$communityScientificName[[1]])
  } else {
    top_species <- NA_character_
  }
  point_incidence <- if (nrow(species_point_pairs))
    table(species_point_pairs$communityScientificName) else integer()
  mean_detection_frequency <- if (S_obs > 0L)
    mean(100 * incidence_y / si$T) else NA_real_
  mean_ubiquity <- if (length(point_incidence))
    mean(100 * as.numeric(point_incidence) / n_points_window) else NA_real_

  if (si$T != n_visits_window || n_points_window < 1L ||
      n_points_window > n_visits_window ||
      U_incidence != nrow(species_count_pairs) ||
      S_obs != length(unique(species_count_pairs$communityScientificName)) ||
      any(incidence_y < 1L | incidence_y > si$T) ||
      n_positive_counts_window + n_supported_zero_counts_window != si$T ||
      U_incidence < n_positive_counts_window ||
      !is.finite(n_birds_window) || n_birds_window < n_positive_counts_window)
    stop(site, " has inconsistent window-specific count incidence.", call. = FALSE)
  if (all(c("outcome", "eligible_detection_rows") %in% names(visits)) &&
      (sum(visits$outcome == "positive") != n_positive_counts_window ||
       sum(visits$outcome == "supported_zero") != n_supported_zero_counts_window ||
       any(visits$eligible_detection_rows[visits$outcome == "supported_zero"] != 0L)))
    stop(site, " has inconsistent physical-count outcome summaries.", call. = FALSE)

  base[[site]] <- tibble::tibble(
    site = site,
    analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
    analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
    bird_year_min = min(visits$year),
    bird_year_max = max(visits$year),
    T_counts = as.integer(si$T),
    n_visits_window = n_visits_window,
    n_points_window = n_points_window,
    n_positive_counts_window = n_positive_counts_window,
    n_supported_zero_counts_window = n_supported_zero_counts_window,
    n_birds_window = n_birds_window,
    S_obs = S_obs,
    U_incidence = U_incidence,
    Q1_incidence = Q1_incidence,
    Q2_incidence = Q2_incidence,
    birds_per_count_window = n_birds_window / n_visits_window,
    top_species_window = top_species,
    mean_detection_frequency = mean_detection_frequency,
    mean_ubiquity = mean_ubiquity,
    pct_singing = if (nrow(sp)) 100 * mean(singing %in% TRUE) else NA_real_
  )
}

t_common <- min(vapply(incidence, function(x) as.integer(x$T), integer(1)))
rows <- lapply(names(incidence), function(site) {
  si <- incidence[[site]]
  h <- hill_incidence(si$Y)
  dplyr::bind_cols(
    base[[site]],
    tibble::tibble(
      S_rare = rarefy_incidence(si$Y, si$T, t_common),
      t_used = as.integer(t_common),
      coverage = coverage_incidence(si$Y, si$T),
      hill_q1 = unname(h[["q1"]]),
      hill_q2 = unname(h[["q2"]])
    )
  )
})
cross_site <- dplyr::bind_rows(rows)
cross_site <- cross_site[order(cross_site$site, method = "radix"), , drop = FALSE]
rownames(cross_site) <- NULL
attr(cross_site, "schema_version") <- 4L
attr(cross_site, "release") <- "RELEASE-2026"
attr(cross_site, "analysis_year_min") <- BIRD_CROSS_SITE_YEAR_MIN
attr(cross_site, "analysis_year_max") <- BIRD_CROSS_SITE_YEAR_MAX
attr(cross_site, "incidence_unit") <-
  "valid physical six-minute count keyed by survey_id; repeated counts are protocol samples, not independent places"
attr(cross_site, "method") <- sprintf(
  paste("Eligible in-window non-flyover species incidence across valid physical counts",
        "in %d-%d; richness rarefied to %d counts. Equal count size does not",
        "equalize coverage, detectability, or spatiotemporal design."),
  BIRD_CROSS_SITE_YEAR_MIN, BIRD_CROSS_SITE_YEAR_MAX, t_common)
expected_columns <- c(
  "site", "analysis_year_min", "analysis_year_max", "bird_year_min", "bird_year_max",
  "T_counts", "n_visits_window", "n_points_window", "n_positive_counts_window",
  "n_supported_zero_counts_window", "n_birds_window", "S_obs", "U_incidence",
  "Q1_incidence", "Q2_incidence", "birds_per_count_window", "top_species_window",
  "mean_detection_frequency", "mean_ubiquity", "pct_singing", "S_rare", "t_used",
  "coverage", "hill_q1", "hill_q2"
)
nonempty <- cross_site$S_obs > 0L
coverage_supported <- cross_site$U_incidence > 0L
if (!identical(names(cross_site), expected_columns) ||
    nrow(cross_site) != 47L ||
    !identical(as.character(cross_site$site), codes) ||
    any(cross_site$T_counts != cross_site$n_visits_window) ||
    any(cross_site$n_positive_counts_window +
          cross_site$n_supported_zero_counts_window != cross_site$T_counts) ||
    any(cross_site$n_points_window < 1L |
          cross_site$n_points_window > cross_site$n_visits_window) ||
    any(cross_site$U_incidence < cross_site$n_positive_counts_window) ||
    any(cross_site$Q1_incidence + cross_site$Q2_incidence > cross_site$S_obs) ||
    any(!is.finite(cross_site$n_birds_window) | cross_site$n_birds_window < 0) ||
    any(abs(cross_site$birds_per_count_window -
              cross_site$n_birds_window / cross_site$n_visits_window) > 1e-12) ||
    any(cross_site$T_counts < t_common) || any(!is.finite(cross_site$S_rare)) ||
    length(unique(cross_site$t_used)) != 1L ||
    unique(cross_site$t_used) != t_common ||
    any(cross_site$S_rare < 0 | cross_site$S_rare > cross_site$S_obs) ||
    any(!is.finite(cross_site$coverage[coverage_supported]) |
          cross_site$coverage[coverage_supported] < 0 |
          cross_site$coverage[coverage_supported] > 1) ||
    any(!is.na(cross_site$coverage[!coverage_supported])) ||
    any(!is.finite(cross_site$hill_q1[nonempty]) |
          !is.finite(cross_site$hill_q2[nonempty])) ||
    any(!is.na(cross_site$hill_q1[!nonempty]) |
          !is.na(cross_site$hill_q2[!nonempty])) ||
    any(!is.finite(cross_site$mean_detection_frequency[nonempty]) |
          !is.finite(cross_site$mean_ubiquity[nonempty]) |
          !is.finite(cross_site$pct_singing[nonempty])) ||
    any(!is.na(cross_site$mean_detection_frequency[!nonempty]) |
          !is.na(cross_site$mean_ubiquity[!nonempty]) |
          !is.na(cross_site$pct_singing[!nonempty])) ||
    any(!is.na(cross_site$top_species_window[!nonempty])) ||
    any(is.na(cross_site$top_species_window[nonempty]) |
          !nzchar(cross_site$top_species_window[nonempty])) ||
    any(cross_site$bird_year_min < BIRD_CROSS_SITE_YEAR_MIN |
          cross_site$bird_year_max > BIRD_CROSS_SITE_YEAR_MAX |
          cross_site$bird_year_min > cross_site$bird_year_max) ||
    any(cross_site$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN) ||
    any(cross_site$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX))
  stop("Cross-site output failed exact valid-count/window validation.", call. = FALSE)
saveRDS(cross_site, OUT, compress = "xz", version = 3)
cat(sprintf(
  "OK: wrote %s for 47 sites; common target = %d valid counts in %d-%d.\n",
  OUT, t_common, BIRD_CROSS_SITE_YEAR_MIN, BIRD_CROSS_SITE_YEAR_MAX))
