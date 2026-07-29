# Build opportunity-complete site bundles for NEON DP1.10003.001 RELEASE-2026.
#
# Inputs are the immutable raw files and receipt produced by fetch_bird_all.R.
# Outputs go to a separate candidate root by default so a failed build cannot
# contaminate committed data:
#   BIRD_RAW_DIR       default build/raw/birds
#   BIRD_RECEIPT       default build/source_receipt.json
#   BIRD_OUTPUT_ROOT   default build/candidate

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(jsonlite)
  library(digest)
})

PRODUCT <- "DP1.10003.001"
RELEASE <- "RELEASE-2026"
DOI <- "10.48443/v6hs-mx57"
SCHEMA_VERSION <- 4L
PROTOCOL_MINUTES <- 6
RAW_DIR <- Sys.getenv("BIRD_RAW_DIR", "build/raw/birds")
RECEIPT_PATH <- Sys.getenv("BIRD_RECEIPT", "build/source_receipt.json")
OUT_ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", "build/candidate")
DEMO <- "CLBJ"

source("R/site_metadata.R")
source("R/bird_evidence_contract.R")
expected_sites <- sort(as.character(neon_sites$site))
if (length(expected_sites) != 47L || !"PUUM" %in% expected_sites) {
  stop("Canonical release roster must contain 47 sites including PUUM.", call. = FALSE)
}
if (!file.exists(RECEIPT_PATH)) stop("Missing source receipt: ", RECEIPT_PATH, call. = FALSE)
receipt <- jsonlite::fromJSON(RECEIPT_PATH, simplifyVector = TRUE)
if (!identical(as.integer(receipt$schema_version), 3L) ||
    !identical(receipt$product, PRODUCT) || !identical(receipt$release, RELEASE) ||
    !identical(receipt$doi, DOI)) {
  stop("Source receipt does not identify the required immutable bird release.", call. = FALSE)
}
bird_assert_receipt_evidence_contract(receipt)
if (!identical(sort(as.character(receipt$fetched_sites)), expected_sites)) {
  stop("Source receipt roster does not exactly match the canonical 47-site roster.", call. = FALSE)
}
receipt_files <- bird_receipt_records(receipt)
if (!identical(sort(as.character(receipt_files$site)), expected_sites))
  stop("Source receipt lacks exact deterministic content records.", call. = FALSE)

raw_files <- list.files(RAW_DIR, pattern = "^[A-Z]{4}_raw[.]rds$", full.names = TRUE)
raw_sites <- sort(sub("_raw[.]rds$", "", basename(raw_files)))
if (!identical(raw_sites, expected_sites)) {
  stop(sprintf("Raw staging must contain exactly the 47 release sites; got %d (%s).",
               length(raw_sites), paste(setdiff(expected_sites, raw_sites), collapse = ", ")), call. = FALSE)
}

site_dir <- file.path(OUT_ROOT, "data", "sites")
sample_dir <- file.path(OUT_ROOT, "data-sample")
if (dir.exists(site_dir) && length(list.files(site_dir, all.files = TRUE, no.. = TRUE))) {
  stop("Candidate site directory must be empty: ", site_dir, call. = FALSE)
}
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
col_chr <- function(d, name, default = NA_character_) {
  if (name %in% names(d)) as.character(d[[name]]) else rep(default, nrow(d))
}
col_num <- function(d, name) suppressWarnings(as.numeric(col_chr(d, name)))
mode_chr <- function(x) {
  x <- trimws(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
}
median_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}
year_from <- function(start_date, event_id) {
  y <- suppressWarnings(as.integer(substr(as.character(start_date), 1, 4)))
  bad <- !is.finite(y)
  if (any(bad)) {
    event <- as.character(event_id[bad])
    match <- regexpr("20[0-9]{2}", event)
    hit <- rep(NA_character_, length(event))
    has <- match > 0L
    hit[has] <- substr(event[has], match[has], match[has] + 3L)
    y[bad] <- suppressWarnings(as.integer(hit))
  }
  y
}
clean_state <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  out <- tolower(gsub("[^A-Z0-9]+", "_", raw))
  out[is.na(raw) | !nzchar(raw)] <- "source_missing"
  out
}
survey_key <- function(site, event, plot, point) paste(site, event, plot, point, sep = "|")
occasion_key <- function(site, plot, point, year) paste(site, plot, point, year, sep = "|")
point_key <- function(plot, point) paste(plot, point, sep = "_")
community_unit <- function(rank, scientific) {
  rank <- tolower(trimws(as.character(rank)))
  scientific <- as.character(scientific)
  normalized <- gsub("[[:space:]]+", " ", trimws(scientific))
  normalized[is.na(scientific) | !nzchar(normalized)] <- NA_character_
  supported <- !is.na(rank) & rank %in% c("species", "subspecies")
  state <- rep("not_species_level", length(scientific))
  state[supported & is.na(normalized)] <- "missing_scientific_name"
  name <- rep(NA_character_, length(scientific))
  for (i in which(supported & !is.na(normalized))) {
    tokens <- strsplit(normalized[[i]], " ", fixed = TRUE)[[1]]
    token_ok <- length(tokens) >= 2L && all(grepl("^[[:alpha:]][[:alpha:]'-]*$", tokens))
    rank_ok <- if (identical(rank[[i]], "species")) length(tokens) == 2L else
      length(tokens) %in% 2:3
    genus_ok <- token_ok && grepl("^[[:upper:]]", tokens[[1]])
    epithet_ok <- token_ok && grepl("^[[:lower:]]", tokens[[2]]) &&
      !tolower(tokens[[2]]) %in% c("sp", "spp", "cf", "aff", "nr")
    if (!token_ok || !rank_ok || !genus_ok || !epithet_ok) {
      state[[i]] <- "unsafe_scientific_name"
      next
    }
    genus <- paste0(toupper(substr(tokens[[1]], 1L, 1L)),
                    tolower(substr(tokens[[1]], 2L, nchar(tokens[[1]]))))
    name[[i]] <- paste(genus, tolower(tokens[[2]]))
    state[[i]] <- if (identical(rank[[i]], "subspecies"))
      "canonical_subspecies" else "canonical_species"
  }
  list(name = name, state = state,
       eligible = state %in% c("canonical_species", "canonical_subspecies"))
}
is_flyover_method <- function(x) {
  out <- grepl("flyover", tolower(as.character(x)))
  out[is.na(out)] <- FALSE
  out
}
method_channel <- function(x) {
  m <- tolower(trimws(as.character(x)))
  ifelse(is.na(m) | !nzchar(m) | m == "unknown", "unknown",
    ifelse(grepl("sing", m), "singing",
      ifelse(grepl("drum", m), "drumming",
        ifelse(grepl("call", m), "calling",
          ifelse(grepl("visual", m), "visual", "other")))))
}
distance_fields <- function(x) {
  raw <- as.character(x)
  blank <- is.na(raw) | !nzchar(trimws(raw))
  value <- suppressWarnings(as.numeric(raw))
  sentinel <- !blank & is.finite(value) & value %in% c(999, 9999)
  observed <- !blank & is.finite(value) & value >= 0 & !sentinel
  state <- ifelse(blank, "source_missing",
                  ifelse(sentinel, "sentinel_not_estimable",
                         ifelse(observed, "observed", "invalid")))
  list(raw = raw, value = ifelse(observed, value, NA_real_), state = state)
}
point_count_minute_fields <- function(x) {
  raw <- as.character(x)
  blank <- is.na(x) | !nzchar(trimws(raw))
  value <- suppressWarnings(as.numeric(raw))
  integer_value <- suppressWarnings(as.integer(value))
  whole <- !blank & is.finite(value) & !is.na(integer_value) & value == integer_value
  minute <- ifelse(whole, integer_value, NA_integer_)
  in_window <- whole & minute %in% 1:6
  state <- as.character(ifelse(blank, "source_missing",
            ifelse(in_window, "standard_minute",
              ifelse(whole & minute == 88L, "incidental_minute_88", "invalid_or_unknown"))))
  list(raw = raw, value = minute, state = state, in_window = in_window)
}
sha256_file <- function(path) digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
canonical_table <- bird_canonical_table

build_site <- function(site) {
  raw_path <- file.path(RAW_DIR, paste0(site, "_raw.rds"))
  raw <- readRDS(raw_path)
  receipt_row <- match(site, receipt_files$site)
  if (is.na(receipt_row) ||
      !identical(bird_full_source_sha256(raw),
                 as.character(receipt_files$full_source_content_sha256[[receipt_row]])) ||
      !identical(bird_evidence_projection_sha256(raw),
                 as.character(receipt_files$evidence_projection_sha256[[receipt_row]])))
    stop(site, " raw source/evidence digests do not match the deterministic source receipt.",
         call. = FALSE)
  pp <- tibble::as_tibble(canonical_table(raw$brd_perpoint))
  cd <- tibble::as_tibble(canonical_table(raw$brd_countdata))
  if (!nrow(pp)) stop(site, " has no visit rows.", call. = FALSE)
  if (!"pointCountMinute" %in% names(cd))
    stop(site, " brd_countdata lacks required pointCountMinute.", call. = FALSE)
  if (nrow(pp) != as.integer(receipt_files$brd_perpoint_rows[[receipt_row]]) ||
      nrow(cd) != as.integer(receipt_files$brd_countdata_rows[[receipt_row]]))
    stop(site, " raw tables do not match the deterministic source receipt.", call. = FALSE)

  pp_site <- col_chr(pp, "siteID", site); pp_site[is.na(pp_site) | !nzchar(pp_site)] <- site
  pp_event <- col_chr(pp, "eventID")
  pp_plot <- col_chr(pp, "plotID")
  pp_point <- col_chr(pp, "pointID")
  pp_year <- year_from(col_chr(pp, "startDate"), pp_event)
  pp_bout_raw <- col_chr(pp, "boutNumber")
  pp_bout_num <- suppressWarnings(as.numeric(pp_bout_raw))
  if (any(!is.finite(pp_bout_num) | pp_bout_num != as.integer(pp_bout_num) |
          !as.integer(pp_bout_num) %in% 1:2))
    stop(site, " has a visit outside the supported bout 1/2 domain.", call. = FALSE)
  pp_bout <- as.character(as.integer(pp_bout_num))
  pp_state_raw <- col_chr(pp, "samplingImpractical")
  pp_state <- clean_state(pp_state_raw)
  pp_valid <- toupper(trimws(pp_state_raw)) == "OK"
  pp_valid[is.na(pp_valid)] <- FALSE
  # Observer values are validation-only inputs. Public bundles retain only the
  # site/species aggregate counts needed by the support UI; neither raw values
  # nor deterministic row-level pseudonyms are serialised.
  pp_observer <- trimws(col_chr(pp, "measuredBy"))
  pp_observer[is.na(pp_observer) | !nzchar(pp_observer)] <- NA_character_

  visits <- tibble::tibble(
    survey_id = survey_key(pp_site, pp_event, pp_plot, pp_point),
    occasion_id = occasion_key(pp_site, pp_plot, pp_point, pp_year),
    site = pp_site,
    pointkey = point_key(pp_plot, pp_point),
    plotID = pp_plot,
    pointID = pp_point,
    eventID = pp_event,
    year = as.integer(pp_year),
    bout = pp_bout,
    startDate = col_chr(pp, "startDate"),
    endDate = col_chr(pp, "endDate"),
    samplingImpractical = pp_state_raw,
    sampling_state = pp_state,
    valid_count = pp_valid,
    protocol_minutes = ifelse(pp_valid, PROTOCOL_MINUTES, 0),
    samplingProtocolVersion = col_chr(pp, "samplingProtocolVersion"),
    observedHabitat = col_chr(pp, "observedHabitat"),
    nlcdClass = col_chr(pp, "nlcdClass"),
    lat = col_num(pp, "decimalLatitude"),
    lng = col_num(pp, "decimalLongitude"),
    startCloudCoverPercentage = col_num(pp, "startCloudCoverPercentage"),
    endCloudCoverPercentage = col_num(pp, "endCloudCoverPercentage"),
    observedAirTemp = col_num(pp, "observedAirTemp"),
    kmPerHourObservedWindSpeed = col_num(pp, "kmPerHourObservedWindSpeed")
  )

  key_missing <- function(x) any(is.na(x) | !nzchar(trimws(as.character(x))))
  if (anyNA(visits$year) || key_missing(visits$site) || key_missing(visits$eventID) ||
      key_missing(visits$plotID) || key_missing(visits$pointID)) {
    stop(site, " has visit rows missing a survey-key field.", call. = FALSE)
  }
  if (any(visits$site != site)) stop(site, " visit ledger contains another site.", call. = FALSE)
  duplicate_surveys <- unique(visits$survey_id[duplicated(visits$survey_id)])
  if (length(duplicate_surveys)) {
    stop(sprintf("%s has %d duplicate physical survey key(s); refusing an ambiguous join.",
                 site, length(duplicate_surveys)), call. = FALSE)
  }
  valid_bout_key <- paste(visits$occasion_id[visits$valid_count], visits$bout[visits$valid_count], sep = "|")
  if (anyDuplicated(valid_bout_key))
    stop(site, " has duplicate valid bout labels within a point-year.", call. = FALSE)

  cd_site <- col_chr(cd, "siteID", site); cd_site[is.na(cd_site) | !nzchar(cd_site)] <- site
  cd_event <- col_chr(cd, "eventID")
  cd_plot <- col_chr(cd, "plotID")
  cd_point <- col_chr(cd, "pointID")
  cd_start <- col_chr(cd, "startDate")
  cd_year <- year_from(cd_start, cd_event)
  cd_bout_raw <- col_chr(cd, "boutNumber")
  cd_bout_num <- suppressWarnings(as.numeric(cd_bout_raw))
  cd_bout <- ifelse(is.finite(cd_bout_num) & cd_bout_num == as.integer(cd_bout_num),
                    as.character(as.integer(cd_bout_num)), trimws(cd_bout_raw))
  if (any(cd_site != site)) stop(site, " detection table contains another site.", call. = FALSE)
  cd_survey <- survey_key(cd_site, cd_event, cd_plot, cd_point)
  visit_match <- match(cd_survey, visits$survey_id)
  dist <- distance_fields(col_chr(cd, "observerDistance"))
  minute <- point_count_minute_fields(col_chr(cd, "pointCountMinute"))
  method_raw <- col_chr(cd, "detectionMethod")
  method_token <- tolower(trimws(method_raw))
  method_known <- !is.na(method_token) & nzchar(method_token) & method_token != "unknown"
  method_component <- function(pattern) {
    out <- grepl(pattern, method_token)
    out[is.na(out)] <- FALSE
    out
  }
  reported_scientific <- col_chr(cd, "scientificName")
  reported_vernacular <- col_chr(cd, "vernacularName")
  reported_taxon_id <- col_chr(cd, "taxonID")
  reported_rank <- col_chr(cd, "taxonRank")
  unit <- community_unit(reported_rank, reported_scientific)
  cluster <- col_num(cd, "clusterSize")
  cluster_valid <- is.finite(cluster) & cluster > 0 & cluster == floor(cluster)
  species <- unit$eligible
  flyover <- is_flyover_method(method_raw)
  matched <- !is.na(visit_match)
  visit_index <- ifelse(matched, visit_match, 1L)
  matched_valid <- matched & visits$valid_count[visit_index]
  matched_valid[!matched] <- FALSE
  eligible <- matched_valid & minute$in_window & species & method_known &
    !flyover & cluster_valid

  # The structural visit ledger owns point, year, and bout. A detection-side
  # timestamp can be stale or formatted differently; once the immutable physical
  # survey key has joined unambiguously, use the matched visit fields for every
  # scientific grain. Keep the detection-side values as explicit audit channels.
  det_site <- cd_site; det_site[matched] <- visits$site[visit_match[matched]]
  det_plot <- cd_plot; det_plot[matched] <- visits$plotID[visit_match[matched]]
  det_point <- cd_point; det_point[matched] <- visits$pointID[visit_match[matched]]
  det_year <- cd_year; det_year[matched] <- visits$year[visit_match[matched]]
  det_bout <- cd_bout; det_bout[matched] <- visits$bout[visit_match[matched]]
  det_pointkey <- point_key(det_plot, det_point)
  det_occasion <- occasion_key(det_site, det_plot, det_point, det_year)
  year_conflict <- matched & is.finite(cd_year) & cd_year != det_year
  bout_conflict <- matched & !is.na(cd_bout) & nzchar(cd_bout) & cd_bout != det_bout
  if (any(year_conflict) || any(bout_conflict))
    stop(sprintf("%s has detection/visit grain conflicts (year=%d, bout=%d).",
                 site, sum(year_conflict), sum(bout_conflict)), call. = FALSE)

  detection <- tibble::tibble(
    survey_id = cd_survey,
    occasion_id = det_occasion,
    site = det_site,
    pointkey = det_pointkey,
    plotID = det_plot,
    pointID = det_point,
    eventID = cd_event,
    year = as.integer(det_year),
    bout = det_bout,
    detection_startDate_raw = cd_start,
    detection_year_raw = as.integer(cd_year),
    detection_bout_raw = cd_bout,
    detection_year_matches_visit = ifelse(matched & is.finite(cd_year), !year_conflict, NA),
    detection_bout_matches_visit = ifelse(matched & !is.na(cd_bout) & nzchar(cd_bout), !bout_conflict, NA),
    taxonID = reported_taxon_id,
    scientificName = unit$name,
    vernacularName = reported_vernacular,
    taxonRank = reported_rank,
    communityScientificName = unit$name,
    community_unit_state = unit$state,
    reportedTaxonID = reported_taxon_id,
    reportedScientificName = reported_scientific,
    reportedVernacularName = reported_vernacular,
    reportedTaxonRank = reported_rank,
    is_species = species,
    pointCountMinuteRaw = minute$raw,
    pointCountMinute = minute$value,
    point_count_minute_state = minute$state,
    in_protocol_window = minute$in_window,
    observerDistanceRaw = dist$raw,
    observerDistance = dist$value,
    distance_state = dist$state,
    detectionMethod = method_raw,
    detection_method_state = ifelse(method_known, "reported", "missing_or_unknown"),
    method_channel = method_channel(method_raw),
    method_singing = method_component("sing"),
    method_calling = method_component("call"),
    method_visual = method_component("visual"),
    method_drumming = method_component("drum"),
    clusterSize = cluster,
    sexOrAge = col_chr(cd, "sexOrAge"),
    matched_visit = matched,
    valid_count = matched_valid,
    is_flyover = flyover,
    enters_breeding_metrics = eligible
  )

  hold_reason <- ifelse(!matched, "orphan_detection_no_visit",
                 ifelse(!matched_valid, "detection_on_unusable_visit",
                   ifelse(minute$state == "incidental_minute_88", "incidental_outside_point_count",
                     ifelse(minute$state == "source_missing", "missing_point_count_minute",
                       ifelse(!minute$in_window, "invalid_point_count_minute",
                         ifelse(!cluster_valid, "invalid_cluster_size",
                           ifelse(is.na(reported_scientific) | !nzchar(trimws(reported_scientific)), "missing_taxon",
                             ifelse(tolower(trimws(reported_rank)) %in% c("species", "subspecies") & !species,
                                    "unsafe_species_canonicalization",
                               ifelse(!method_known,
                                      "missing_or_unknown_detection_method", NA_character_)))))))))
  detection$hold_reason <- hold_reason
  held <- detection[!is.na(hold_reason), , drop = FALSE]
  obs <- detection[is.na(hold_reason), , drop = FALSE]

  # Make the physical-count incidence denominator and supported zeros explicit.
  # A valid bout is one six-minute sample; repeated bouts/years at a point remain
  # repeated protocol samples and are never described as independent places.
  elig <- obs[obs$enters_breeding_metrics %in% TRUE, , drop = FALSE]
  visit_elig_summary <- elig %>%
    dplyr::group_by(.data$survey_id) %>%
    dplyr::summarise(
      eligible_detection_rows = dplyr::n(),
      eligible_birds = sum(.data$clusterSize, na.rm = TRUE),
      eligible_species = dplyr::n_distinct(.data$communityScientificName),
      .groups = "drop"
    )
  visit_fly_summary <- obs[obs$is_flyover %in% TRUE & obs$valid_count %in% TRUE &
      obs$in_protocol_window %in% TRUE & obs$is_species %in% TRUE &
      is.finite(obs$clusterSize) & obs$clusterSize > 0 &
      obs$clusterSize == floor(obs$clusterSize), , drop = FALSE] %>%
    dplyr::group_by(.data$survey_id) %>%
    dplyr::summarise(
      flyover_rows = dplyr::n(),
      flyover_birds = sum(.data$clusterSize, na.rm = TRUE),
      .groups = "drop"
    )
  visits <- visits %>%
    dplyr::left_join(visit_elig_summary, by = "survey_id") %>%
    dplyr::left_join(visit_fly_summary, by = "survey_id")
  visit_zero_cols <- c(
    "eligible_detection_rows", "eligible_birds", "eligible_species",
    "flyover_rows", "flyover_birds")
  for (nm in visit_zero_cols) visits[[nm]][is.na(visits[[nm]])] <- 0
  visits$support_state <- ifelse(visits$valid_count, "supported", "held")
  visits$outcome <- ifelse(!visits$valid_count, "unavailable",
    ifelse(visits$eligible_detection_rows > 0, "positive", "supported_zero"))

  opp <- visits %>%
    dplyr::group_by(.data$occasion_id, .data$site, .data$pointkey, .data$plotID,
                    .data$pointID, .data$year) %>%
    dplyr::summarise(
      n_bouts_recorded = dplyr::n(),
      n_valid_bouts = sum(.data$valid_count),
      n_held_bouts = sum(!.data$valid_count),
      n_surveyed_minutes = sum(.data$protocol_minutes),
      observedHabitat = mode_chr(.data$observedHabitat),
      nlcdClass = mode_chr(.data$nlcdClass),
      lat = median_or_na(.data$lat),
      lng = median_or_na(.data$lng),
      supported = any(.data$valid_count),
      .groups = "drop"
    )

  elig_summary <- elig %>%
    dplyr::group_by(.data$occasion_id) %>%
    dplyr::summarise(
      eligible_detection_rows = dplyr::n(),
      eligible_birds = sum(.data$clusterSize, na.rm = TRUE),
      eligible_species = dplyr::n_distinct(.data$communityScientificName),
      .groups = "drop"
    )
  fly_summary <- obs[obs$is_flyover %in% TRUE & obs$valid_count %in% TRUE &
                       obs$in_protocol_window %in% TRUE & obs$is_species %in% TRUE &
                       is.finite(obs$clusterSize) & obs$clusterSize > 0 &
                       obs$clusterSize == floor(obs$clusterSize), , drop = FALSE] %>%
    dplyr::group_by(.data$occasion_id) %>%
    dplyr::summarise(flyover_rows = dplyr::n(), flyover_birds = sum(.data$clusterSize, na.rm = TRUE),
                     .groups = "drop")
  opportunity <- opp %>% dplyr::left_join(elig_summary, by = "occasion_id") %>%
    dplyr::left_join(fly_summary, by = "occasion_id")
  zero_cols <- c("eligible_detection_rows", "eligible_birds", "eligible_species", "flyover_rows", "flyover_birds")
  for (nm in zero_cols) opportunity[[nm]][is.na(opportunity[[nm]])] <- 0
  opportunity$support_state <- ifelse(opportunity$supported, "supported", "held")
  opportunity$outcome <- ifelse(!opportunity$supported, "unavailable",
                         ifelse(opportunity$eligible_detection_rows > 0, "positive", "supported_zero"))

  points <- visits %>%
    dplyr::group_by(.data$pointkey, .data$site, .data$plotID, .data$pointID) %>%
    dplyr::summarise(
      nlcdClass = mode_chr(.data$nlcdClass),
      observedHabitat = mode_chr(.data$observedHabitat),
      lat = median_or_na(.data$lat),
      lng = median_or_na(.data$lng),
      n_visits = sum(.data$valid_count),
      n_years = dplyr::n_distinct(.data$year[.data$valid_count]),
      .groups = "drop"
    )

  meta_row <- neon_sites[neon_sites$site == site, , drop = FALSE]
  site_lat <- median_or_na(points$lat); site_lng <- median_or_na(points$lng)
  if (!is.finite(site_lat) && nrow(meta_row)) site_lat <- meta_row$lat[[1]]
  if (!is.finite(site_lng) && nrow(meta_row)) site_lng <- meta_row$lng[[1]]
  years <- sort(unique(opportunity$year[opportunity$supported]))
  valid_observer <- pp_valid & !is.na(pp_observer)
  observer_at_detection <- rep(NA_character_, nrow(cd))
  observer_at_detection[matched] <- pp_observer[visit_match[matched]]
  eligible_species <- sort(unique(unit$name[eligible]))
  observer_by_species <- if (length(eligible_species)) {
    tibble::tibble(
      scientificName = eligible_species,
      n_observers = vapply(eligible_species, function(taxon) {
        values <- observer_at_detection[eligible & unit$name == taxon]
        dplyr::n_distinct(values[!is.na(values) & nzchar(values)])
      }, integer(1))
    )
  } else {
    tibble::tibble(scientificName = character(), n_observers = integer())
  }
  observer_support <- list(
    n_observers = dplyr::n_distinct(pp_observer[valid_observer]),
    n_valid_visits = sum(pp_valid),
    n_valid_visits_with_observer = sum(valid_observer),
    complete = sum(pp_valid) > 0L && all(!is.na(pp_observer[pp_valid])),
    by_species = observer_by_species
  )
  meta <- list(
    schema_version = SCHEMA_VERSION,
    product = PRODUCT,
    release = RELEASE,
    doi = DOI,
    site = site,
    lat = site_lat,
    lng = site_lng,
    years = years,
    n_visits = sum(visits$valid_count),
    n_supported_zero_counts = sum(visits$outcome == "supported_zero"),
    n_opportunities = sum(opportunity$supported),
    n_supported_zero = sum(opportunity$outcome == "supported_zero"),
    n_unavailable = sum(opportunity$outcome == "unavailable"),
    observer_support = observer_support,
    source_receipt_sha256 = sha256_file(RECEIPT_PATH),
    full_source_content_sha256 = as.character(receipt_files$full_source_content_sha256[[receipt_row]]),
    evidence_projection_sha256 = as.character(receipt_files$evidence_projection_sha256[[receipt_row]])
  )
  list(obs = obs, visits = visits, opportunity = opportunity, points = points,
       held = held, meta = meta)
}

index_rows <- vector("list", length(expected_sites)); names(index_rows) <- expected_sites
for (site in expected_sites) {
  cat(sprintf("Bundling %s ... ", site)); flush.console()
  bundle <- build_site(site)
  path <- file.path(site_dir, paste0(site, ".rds"))
  saveRDS(bundle, path, compress = "xz", version = 3)
  if (identical(site, DEMO)) saveRDS(bundle, file.path(sample_dir, "demo.rds"), compress = "xz", version = 3)

  eligible <- bundle$obs[bundle$obs$enters_breeding_metrics %in% TRUE, , drop = FALSE]
  totals <- if (nrow(eligible)) stats::aggregate(clusterSize ~ communityScientificName, eligible, sum) else NULL
  top <- if (!is.null(totals) && nrow(totals)) totals$communityScientificName[which.max(totals$clusterSize)] else NA_character_
  index_rows[[site]] <- data.frame(
    site = site,
    n_species = length(unique(eligible$communityScientificName)),
    n_points = sum(bundle$points$n_visits > 0),
    n_visits = bundle$meta$n_visits,
    n_supported_zero_counts = bundle$meta$n_supported_zero_counts,
    n_opportunities = bundle$meta$n_opportunities,
    n_supported_zero = bundle$meta$n_supported_zero,
    birds_per_count = if (bundle$meta$n_visits > 0) round(sum(eligible$clusterSize, na.rm = TRUE) / bundle$meta$n_visits, 3) else NA_real_,
    top_species = top,
    lat = bundle$meta$lat,
    lng = bundle$meta$lng,
    release = RELEASE,
    schema_version = SCHEMA_VERSION,
    stringsAsFactors = FALSE
  )
  cat(sprintf("%d valid counts (%d supported zeros), %d point-years, %d point-year zeros, %d eligible species\n",
              bundle$meta$n_visits, bundle$meta$n_supported_zero_counts,
              bundle$meta$n_opportunities, bundle$meta$n_supported_zero,
              index_rows[[site]]$n_species))
}

site_index <- dplyr::bind_rows(index_rows)
if (nrow(site_index) != 47L || !identical(sort(site_index$site), expected_sites)) {
  stop("Built site index does not exactly match the release roster.", call. = FALSE)
}
saveRDS(site_index, file.path(OUT_ROOT, "data", "site_index.rds"), compress = "xz", version = 3)
file.copy(RECEIPT_PATH, file.path(OUT_ROOT, "data", "source_receipt.json"), overwrite = TRUE)

schema <- list(
  schema_version = SCHEMA_VERSION,
  product = PRODUCT,
  release = RELEASE,
  incidence_unit = "valid physical six-minute point-count keyed by visits.survey_id; repeated counts are protocol samples, not independent places",
  annual_audit_unit = "point x year opportunity; one or two valid bouts retained in n_valid_bouts",
  detection_index_denominator = "valid point-count bouts",
  valid_visit_rule = "samplingImpractical == OK",
  observer_support = "site/species aggregate counts only; raw measuredBy remains only in producer-local evidence",
  cross_job_evidence = "schema-v1 exact two-table scientific projection; observer identity, personnel, remarks, and source UIDs excluded",
  species_community_unit = "safe normalized genus + species binomial; parent species and subspecies collapse; ambiguous nomenclature fails closed",
  source_taxonomy_provenance = "reportedTaxonID, reportedScientificName, reportedVernacularName, and reportedTaxonRank preserve exact source values",
  breeding_detection_rule = "matched valid visit AND pointCountMinute in 1:6 AND safely canonicalized taxonRank in species/subspecies AND known detectionMethod AND not flyover AND positive finite integer clusterSize",
  visit_outcomes = c("positive", "supported_zero", "unavailable"),
  point_year_outcomes = c("positive", "supported_zero", "unavailable"),
  cross_site_rule = list(
    analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
    analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
    incidence_unit = "valid visits.survey_id physical counts",
    rarefaction_support = "minimum T_counts across the exact 47-site roster"
  ),
  climate_context_rule = list(
    analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
    analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
    realized_months = "exact distinct calendar months containing valid counts in the analysis window",
    monthly_climatology_min_coverage_year_months = 2L,
    aggregation = "equal-weight arithmetic mean across realized calendar-month climatologies",
    completeness = "every realized count month must have a coverage-qualified climatology",
    missing_policy = "fail closed; no imputation"
  ),
  detection_method_states = c("reported", "missing_or_unknown"),
  distance_states = c("observed", "sentinel_not_estimable", "source_missing", "invalid"),
  point_count_minute_states = c("standard_minute", "incidental_minute_88", "source_missing", "invalid_or_unknown")
)
jsonlite::write_json(schema, file.path(OUT_ROOT, "data", "bundle_schema.json"),
                     auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("OK: built opportunity-complete %s candidate for 47 sites under %s\n", RELEASE, OUT_ROOT))
