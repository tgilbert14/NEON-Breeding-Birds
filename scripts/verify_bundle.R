#!/usr/bin/env Rscript
# Independent oracle for the immutable, opportunity-complete Birds candidate.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })
source("R/site_metadata.R")

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
DATA <- file.path(ROOT, "data")
SITES <- file.path(DATA, "sites")
ENV <- file.path(DATA, "env")
assert <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
sha256 <- function(path) digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
same_num <- function(a, b, tolerance = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tolerance))
same_chr <- function(a, b) identical(ifelse(is.na(a), "<NA>", as.character(a)),
                                     ifelse(is.na(b), "<NA>", as.character(b)))
blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
point_count_minute_state <- function(raw) {
  token <- as.character(raw)
  empty <- blank(token)
  value <- suppressWarnings(as.numeric(token))
  integer_value <- suppressWarnings(as.integer(value))
  whole <- !empty & is.finite(value) & !is.na(integer_value) & value == integer_value
  minute <- ifelse(whole, integer_value, NA_integer_)
  in_window <- whole & minute %in% 1:6
  list(
    value = minute,
    state = as.character(ifelse(empty, "source_missing",
      ifelse(in_window, "standard_minute",
        ifelse(whole & minute == 88L, "incidental_minute_88", "invalid_or_unknown")))),
    in_window = in_window
  )
}
community_unit <- function(rank, scientific) {
  rank <- tolower(trimws(as.character(rank)))
  scientific <- as.character(scientific)
  normalized <- gsub("[[:space:]]+", " ", trimws(scientific))
  normalized[blank(scientific)] <- NA_character_
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
rarefy_incidence_oracle <- function(Y, T, t) {
  Y <- suppressWarnings(as.integer(Y))
  T <- suppressWarnings(as.integer(T)[1])
  t <- suppressWarnings(as.integer(t)[1])
  if (!is.finite(T) || !is.finite(t) || T < 1L || t < 1L || t > T ||
      any(!is.finite(Y)) || any(Y < 1L) || any(Y > T)) return(NA_real_)
  contribution <- ifelse(
    T - Y < t, 1,
    1 - exp(lchoose(T - Y, t) - lchoose(T, t))
  )
  round(sum(contribution), 1)
}
coverage_incidence_oracle <- function(Y, T) {
  Y <- suppressWarnings(as.integer(Y))
  T <- suppressWarnings(as.integer(T)[1])
  U <- sum(Y)
  if (!is.finite(T) || T < 2L || U == 0L) return(NA_real_)
  Q1 <- sum(Y == 1L)
  Q2 <- sum(Y == 2L)
  A <- if (Q2 > 0L) {
    (T - 1) * Q1 / ((T - 1) * Q1 + 2 * Q2)
  } else {
    numerator <- (T - 1) * max(Q1 - 1L, 0L)
    numerator / (numerator + 2)
  }
  1 - (Q1 / U) * A
}
hill_incidence_oracle <- function(Y) {
  Y <- suppressWarnings(as.numeric(Y))
  U <- sum(Y)
  if (!length(Y) || !is.finite(U) || U <= 0) return(c(q1 = NA_real_, q2 = NA_real_))
  p <- Y / U
  p <- p[p > 0]
  c(q1 = exp(-sum(p * log(p))), q2 = 1 / sum(p^2))
}
mode_chr_oracle <- function(x) {
  value <- trimws(as.character(x))
  value <- value[!is.na(value) & nzchar(value)]
  if (!length(value)) return(NA_character_)
  frequency <- table(value)
  names(frequency)[order(-as.integer(frequency), names(frequency), method = "radix")[[1]]]
}
canonical_method_oracle <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  missing <- is.na(x) | is.na(raw) | !nzchar(raw)
  raw[is.na(raw)] <- ""
  out <- raw
  out[!missing & grepl("flyover", raw, fixed = TRUE)] <- "flyover"
  out[!missing & grepl("visual", raw, fixed = TRUE)] <- "visual"
  out[!missing & grepl("calling", raw, fixed = TRUE)] <- "calling"
  out[!missing & grepl("drumming", raw, fixed = TRUE)] <- "drumming"
  out[!missing & grepl("singing", raw, fixed = TRUE)] <- "singing"
  out[missing] <- "unknown"
  out
}
vernacular_oracle <- function(community, reported_scientific, reported_rank,
                              reported_vernacular) {
  scientific <- gsub(
    "[[:space:]]+", " ", trimws(as.character(reported_scientific))
  )
  rank <- tolower(trimws(as.character(reported_rank)))
  vernacular <- trimws(as.character(reported_vernacular))
  available <- !is.na(vernacular) & nzchar(vernacular)
  parent <- available & rank == "species" & !is.na(scientific) &
    scientific == as.character(community)[[1]]
  value <- if (any(parent)) mode_chr_oracle(vernacular[parent]) else
    mode_chr_oracle(vernacular[available])
  if (is.na(value) || !nzchar(value)) as.character(community)[[1]] else value
}
expected_sites <- sort(as.character(neon_sites$site))
assert(length(expected_sites) == 47L && "PUUM" %in% expected_sites, "Canonical metadata is not the 47-site release roster.")
assert(identical(as.integer(BIRD_CROSS_SITE_YEAR_MIN), 2017L) &&
       identical(as.integer(BIRD_CROSS_SITE_YEAR_MAX), 2024L),
       "Canonical cross-site analysis window is not exactly 2017-2024.")

site_files <- list.files(SITES, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
env_files <- list.files(ENV, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
assert(identical(sort(sub("[.]rds$", "", basename(site_files))), expected_sites),
       "Site bundle roster is not exactly the RELEASE-2026 47-site roster.")
assert(identical(sort(sub("[.]rds$", "", basename(env_files))), expected_sites),
       "Environment bundle roster is not exactly the RELEASE-2026 47-site roster.")

required_files <- file.path(DATA, c(
  "site_index.rds", "cross_site.rds", "search_index.rds", "site_climate.rds",
  "site_month_clim.rds", "source_receipt.json", "environment_source_receipt.json",
  "release_stamp.json", "bundle_schema.json"
))
assert(all(file.exists(required_files)), paste("Missing derived file(s):", paste(basename(required_files[!file.exists(required_files)]), collapse = ", ")))

receipt <- jsonlite::fromJSON(file.path(DATA, "source_receipt.json"), simplifyVector = FALSE)
assert(identical(names(receipt), c(
  "schema_version", "product", "release", "doi", "release_generated_utc",
  "retrieval", "evidence_projection", "expected_site_count", "expected_sites",
  "fetched_site_count", "fetched_sites", "files"
)), "Bird source receipt fields or order differ from schema v3.")
assert(identical(receipt$product, "DP1.10003.001"), "Bird source receipt product mismatch.")
assert(identical(receipt$release, "RELEASE-2026"), "Bird source receipt release mismatch.")
assert(identical(receipt$doi, "10.48443/v6hs-mx57"), "Bird source receipt DOI mismatch.")
assert(identical(receipt$release_generated_utc, "2026-01-23T00:07:49Z") &&
       identical(names(receipt$retrieval), c(
         "package", "tool", "neonUtilities_version", "r_version", "token_required",
         "staging_was_empty"
       )) && identical(receipt$retrieval$package, "basic") &&
       identical(receipt$retrieval$tool, "neonUtilities::loadByProduct") &&
       nzchar(as.character(receipt$retrieval$neonUtilities_version)) &&
       identical(receipt$retrieval$r_version, "4.5.2") &&
       isTRUE(receipt$retrieval$token_required) && isTRUE(receipt$retrieval$staging_was_empty),
       "Bird source receipt release/retrieval identity mismatch.")
assert(identical(as.integer(receipt$schema_version), 3L) &&
       identical(names(receipt$evidence_projection), c("schema_version", "tables", "privacy")) &&
       identical(as.integer(receipt$evidence_projection$schema_version), 1L) &&
       identical(as.character(receipt$evidence_projection$privacy),
                 "no observer identity, personnel table, free-text sampling remarks, or source UIDs"),
       "Bird source/evidence receipt schema mismatch.")
expected_evidence_tables <- list(
  brd_perpoint = sort(c(
    "boutNumber", "decimalLatitude", "decimalLongitude", "endCloudCoverPercentage",
    "endDate", "eventID", "kmPerHourObservedWindSpeed", "nlcdClass", "observedAirTemp",
    "observedHabitat", "plotID", "pointID", "release", "samplingImpractical",
    "samplingProtocolVersion", "siteID", "startCloudCoverPercentage", "startDate"
  ), method = "radix"),
  brd_countdata = sort(c(
    "boutNumber", "clusterSize", "detectionMethod", "eventID", "observerDistance",
    "plotID", "pointCountMinute", "pointID", "release", "scientificName", "sexOrAge",
    "siteID", "startDate", "taxonID", "taxonRank", "vernacularName"
  ), method = "radix")
)
assert(identical(names(receipt$evidence_projection$tables), names(expected_evidence_tables)) &&
       all(vapply(names(expected_evidence_tables), function(table_name) {
         identical(as.character(unlist(receipt$evidence_projection$tables[[table_name]],
                                       use.names = FALSE)),
                   expected_evidence_tables[[table_name]])
       }, logical(1))), "Bird receipt evidence-table allowlist differs.")
assert(identical(as.integer(receipt$expected_site_count), 47L), "Bird receipt expected-site count mismatch.")
assert(identical(sort(unlist(receipt$expected_sites, use.names = FALSE)), expected_sites),
       "Bird receipt expected roster mismatch.")
assert(identical(as.integer(receipt$fetched_site_count), 47L),
       "Bird receipt fetched-site count mismatch.")
assert(identical(sort(unlist(receipt$fetched_sites, use.names = FALSE)), expected_sites), "Bird receipt roster mismatch.")
receipt_records <- receipt$files
receipt_sha256 <- sha256(file.path(DATA, "source_receipt.json"))
assert(length(receipt_records) == 47L, "Bird receipt must contain 47 raw-file records.")
assert(identical(sort(vapply(receipt_records, function(x) as.character(x$site), character(1))), expected_sites),
       "Bird receipt file records do not cover the exact roster.")
assert(all(vapply(receipt_records, function(x) {
  identical(names(x), c(
    "site", "file", "full_source_content_sha256", "evidence_projection_sha256",
    "brd_perpoint_rows", "brd_countdata_rows"
  )) && identical(as.character(x$file), paste0(as.character(x$site), "_raw.rds")) &&
    grepl("^[0-9a-f]{64}$", as.character(x$full_source_content_sha256)) &&
    grepl("^[0-9a-f]{64}$", as.character(x$evidence_projection_sha256)) &&
    as.integer(x$brd_perpoint_rows) > 0L &&
    as.numeric(x$brd_countdata_rows) >= 0 &&
    as.numeric(x$brd_countdata_rows) == as.integer(x$brd_countdata_rows)
}, logical(1))), "Bird receipt contains an invalid evidence identity, digest, or row count.")

schema <- jsonlite::fromJSON(file.path(DATA, "bundle_schema.json"), simplifyVector = FALSE)
expected_schema_fields <- c(
  "schema_version", "product", "release", "incidence_unit", "annual_audit_unit",
  "detection_index_denominator", "valid_visit_rule", "observer_support",
  "cross_job_evidence", "species_community_unit", "source_taxonomy_provenance",
  "breeding_detection_rule", "visit_outcomes", "point_year_outcomes",
  "cross_site_rule", "climate_context_rule", "detection_method_states",
  "distance_states", "point_count_minute_states"
)
assert(identical(names(schema), expected_schema_fields) &&
       identical(as.integer(schema$schema_version), 4L),
       "Bundle schema fields or v4 identity mismatch.")
assert(identical(schema$product, "DP1.10003.001"), "Bundle schema product mismatch.")
assert(identical(schema$release, "RELEASE-2026"), "Bundle schema release mismatch.")
assert(identical(as.character(schema$detection_index_denominator),
                 "valid point-count bouts") &&
       identical(as.character(schema$valid_visit_rule),
                 "samplingImpractical == OK"),
       "Bundle schema detection-index denominator or valid-visit rule mismatch.")
assert(grepl("visits.survey_id", as.character(schema$incidence_unit), fixed = TRUE) &&
       grepl("point x year", as.character(schema$annual_audit_unit), fixed = TRUE),
       "Bundle schema does not distinguish count incidence from the annual point-year audit.")
assert(grepl("pointCountMinute in 1:6", as.character(schema$breeding_detection_rule), fixed = TRUE),
       "Bundle schema does not declare the formal point-count minute window.")
assert(grepl("known detectionMethod", as.character(schema$breeding_detection_rule), fixed = TRUE),
       "Bundle schema does not fail closed on missing/unknown detection methods.")
assert(grepl("positive finite integer clusterSize", as.character(schema$breeding_detection_rule),
             fixed = TRUE),
       "Bundle schema does not require count-valued clusterSize eligibility.")
assert(grepl("aggregate counts only", as.character(schema$observer_support), fixed = TRUE),
       "Bundle schema does not declare aggregate-only observer support.")
assert(grepl("parent species and subspecies collapse", as.character(schema$species_community_unit),
             fixed = TRUE) &&
       grepl("reportedTaxonID", as.character(schema$source_taxonomy_provenance), fixed = TRUE),
       "Bundle schema does not declare canonical species units and source taxonomy provenance.")
assert(identical(unlist(schema$visit_outcomes, use.names = FALSE),
                 c("positive", "supported_zero", "unavailable")) &&
       identical(unlist(schema$point_year_outcomes, use.names = FALSE),
                 c("positive", "supported_zero", "unavailable")),
       "Bundle schema count or point-year outcome states mismatch.")
assert(identical(names(schema$cross_site_rule), c(
         "analysis_year_min", "analysis_year_max", "incidence_unit", "rarefaction_support"
       )) &&
       identical(as.integer(schema$cross_site_rule$analysis_year_min),
                 BIRD_CROSS_SITE_YEAR_MIN) &&
       identical(as.integer(schema$cross_site_rule$analysis_year_max),
                 BIRD_CROSS_SITE_YEAR_MAX) &&
       identical(as.character(schema$cross_site_rule$incidence_unit),
                 "valid visits.survey_id physical counts") &&
       identical(as.character(schema$cross_site_rule$rarefaction_support),
                 "minimum T_counts across the exact 47-site roster"),
       "Bundle schema cross-site count/window rule mismatch.")
assert(identical(names(schema$climate_context_rule), c(
         "analysis_year_min", "analysis_year_max", "realized_months",
         "monthly_climatology_min_coverage_year_months", "aggregation",
         "completeness", "missing_policy"
       )) &&
       identical(as.integer(schema$climate_context_rule$analysis_year_min),
                 BIRD_CROSS_SITE_YEAR_MIN) &&
       identical(as.integer(schema$climate_context_rule$analysis_year_max),
                 BIRD_CROSS_SITE_YEAR_MAX) &&
       identical(as.character(schema$climate_context_rule$realized_months),
                 "exact distinct calendar months containing valid counts in the analysis window") &&
       identical(as.integer(schema$climate_context_rule$monthly_climatology_min_coverage_year_months),
                 2L) &&
       identical(as.character(schema$climate_context_rule$aggregation),
                 "equal-weight arithmetic mean across realized calendar-month climatologies") &&
       identical(as.character(schema$climate_context_rule$completeness),
                 "every realized count month must have a coverage-qualified climatology") &&
       identical(as.character(schema$climate_context_rule$missing_policy),
                 "fail closed; no imputation"),
       "Bundle schema climate context rule mismatch.")
assert(identical(unlist(schema$point_count_minute_states, use.names = FALSE),
                 c("standard_minute", "incidental_minute_88", "source_missing", "invalid_or_unknown")),
       "Bundle schema point-count minute states mismatch.")
assert(identical(unlist(schema$detection_method_states, use.names = FALSE),
                 c("reported", "missing_or_unknown")),
       "Bundle schema detection-method states mismatch.")
assert(identical(unlist(schema$distance_states, use.names = FALSE),
                 c("observed", "sentinel_not_estimable", "source_missing", "invalid")),
       "Bundle schema distance states mismatch.")

index <- readRDS(file.path(DATA, "site_index.rds"))
assert(nrow(index) == 47L && identical(sort(as.character(index$site)), expected_sites), "Site index roster mismatch.")
expected_index_fields <- c(
  "site", "n_species", "n_points", "n_visits", "n_supported_zero_counts",
  "n_opportunities", "n_supported_zero", "birds_per_count", "top_species",
  "lat", "lng", "release", "schema_version"
)
assert(identical(names(index), expected_index_fields) &&
       all(as.character(index$release) == "RELEASE-2026") &&
       all(as.integer(index$schema_version) == 4L),
       "Site index lacks the exact schema-v4 count/opportunity contract.")

cross_oracle_base <- vector("list", length(expected_sites))
names(cross_oracle_base) <- expected_sites

for (site in expected_sites) {
  path <- file.path(SITES, paste0(site, ".rds"))
  bundle <- readRDS(path)
  assert(is.list(bundle) && identical(names(bundle),
    c("obs", "visits", "opportunity", "points", "held", "meta")),
         paste(site, "does not implement bundle schema v4."))
  assert(identical(names(bundle$meta), c(
    "schema_version", "product", "release", "doi", "site", "lat", "lng", "years",
    "n_visits", "n_supported_zero_counts", "n_opportunities", "n_supported_zero",
    "n_unavailable",
    "observer_support", "source_receipt_sha256", "full_source_content_sha256",
    "evidence_projection_sha256"
  )), paste(site, "metadata has an unexpected public field set or order."))
  assert(identical(bundle$meta$product, "DP1.10003.001") &&
         identical(bundle$meta$release, "RELEASE-2026") &&
         identical(bundle$meta$doi, "10.48443/v6hs-mx57") &&
         identical(as.integer(bundle$meta$schema_version), 4L), paste(site, "metadata identity mismatch."))
  assert(identical(bundle$meta$site, site), paste(site, "metadata site mismatch."))
  meta_count_fields <- c(
    "n_visits", "n_supported_zero_counts", "n_opportunities",
    "n_supported_zero", "n_unavailable"
  )
  meta_count_values <- suppressWarnings(as.numeric(unlist(
    bundle$meta[meta_count_fields], use.names = FALSE
  )))
  assert(length(meta_count_values) == length(meta_count_fields) &&
         all(is.finite(meta_count_values)) && all(meta_count_values >= 0) &&
         all(meta_count_values == as.integer(meta_count_values)),
         paste(site, "metadata effort/outcome counts are not nonnegative integers."))
  raw_record_index <- which(vapply(receipt_records,
    function(x) identical(as.character(x$site), site), logical(1)))
  assert(length(raw_record_index) == 1L, paste(site, "has no unique raw receipt record."))
  raw_record <- receipt_records[[raw_record_index]]
  assert(identical(as.character(bundle$meta$source_receipt_sha256), receipt_sha256),
         paste(site, "is not bound to the committed source receipt digest."))
  assert(identical(as.character(bundle$meta$full_source_content_sha256),
                   as.character(raw_record$full_source_content_sha256)) &&
         identical(as.character(bundle$meta$evidence_projection_sha256),
                   as.character(raw_record$evidence_projection_sha256)),
         paste(site, "full-source/evidence digests are not bound to the receipt."))

  visits <- as.data.frame(bundle$visits)
  opportunities <- as.data.frame(bundle$opportunity)
  obs <- as.data.frame(bundle$obs)
  held <- as.data.frame(bundle$held)
  points <- as.data.frame(bundle$points)
  visit_cols <- c(
    "survey_id", "occasion_id", "site", "pointkey", "plotID", "pointID", "eventID",
    "year", "bout", "startDate", "endDate", "samplingImpractical", "sampling_state",
    "valid_count", "protocol_minutes", "samplingProtocolVersion", "observedHabitat",
    "nlcdClass", "lat", "lng", "startCloudCoverPercentage", "endCloudCoverPercentage",
    "observedAirTemp", "kmPerHourObservedWindSpeed", "eligible_detection_rows",
    "eligible_birds", "eligible_species", "flyover_rows", "flyover_birds",
    "support_state", "outcome"
  )
  opp_cols <- c(
    "occasion_id", "site", "pointkey", "plotID", "pointID", "year",
    "n_bouts_recorded", "n_valid_bouts", "n_held_bouts", "n_surveyed_minutes",
    "observedHabitat", "nlcdClass", "lat", "lng", "supported",
    "eligible_detection_rows", "eligible_birds", "eligible_species",
    "flyover_rows", "flyover_birds", "support_state", "outcome"
  )
  obs_cols <- c(
    "survey_id", "occasion_id", "site", "pointkey", "plotID", "pointID", "eventID",
    "year", "bout", "detection_startDate_raw", "detection_year_raw", "detection_bout_raw",
    "detection_year_matches_visit", "detection_bout_matches_visit", "taxonID",
    "scientificName", "vernacularName", "taxonRank", "communityScientificName",
    "community_unit_state", "reportedTaxonID", "reportedScientificName",
    "reportedVernacularName", "reportedTaxonRank", "is_species", "pointCountMinuteRaw",
    "pointCountMinute", "point_count_minute_state", "in_protocol_window",
    "observerDistanceRaw", "observerDistance", "distance_state", "detectionMethod",
    "detection_method_state",
    "method_channel", "method_singing", "method_calling", "method_visual", "method_drumming",
    "clusterSize", "sexOrAge", "matched_visit", "valid_count", "is_flyover",
    "enters_breeding_metrics", "hold_reason"
  )
  point_cols <- c(
    "pointkey", "site", "plotID", "pointID", "nlcdClass", "observedHabitat",
    "lat", "lng", "n_visits", "n_years"
  )
  assert(identical(names(visits), visit_cols),
         paste(site, "visit ledger does not match the exact public allowlist."))
  assert(identical(names(opportunities), opp_cols),
         paste(site, "opportunity ledger does not match the exact public allowlist."))
  recorded_bouts <- suppressWarnings(as.numeric(opportunities$n_bouts_recorded))
  valid_bouts <- suppressWarnings(as.numeric(opportunities$n_valid_bouts))
  held_bouts <- suppressWarnings(as.numeric(opportunities$n_held_bouts))
  assert(all(is.finite(recorded_bouts)) && all(is.finite(valid_bouts)) &&
         all(is.finite(held_bouts)) && all(recorded_bouts >= 1) &&
         all(valid_bouts >= 0) && all(held_bouts >= 0) &&
         all(recorded_bouts == as.integer(recorded_bouts)) &&
         all(valid_bouts == as.integer(valid_bouts)) &&
         all(held_bouts == as.integer(held_bouts)) &&
         all(recorded_bouts == valid_bouts + held_bouts),
         paste(site, "point-year recorded/valid/held bout counts do not conserve."))
  assert(identical(names(obs), obs_cols) && identical(names(held), obs_cols),
         paste(site, "detection/held tables do not match the exact public allowlist."))
  assert(identical(names(points), point_cols),
         paste(site, "point table does not match the exact public allowlist."))
  assert(nrow(visits) == as.integer(raw_record$brd_perpoint_rows),
         paste(site, "visit rows do not conserve the source receipt."))
  assert(nrow(obs) + nrow(held) == as.integer(raw_record$brd_countdata_rows),
         paste(site, "detection and held rows do not conserve the source receipt."))
  assert(all(as.character(visits$site) == site) && all(as.character(opportunities$site) == site) &&
         all(as.character(obs$site) == site) && all(as.character(held$site) == site),
         paste(site, "bundle ledger contains another site."))
  public_tables <- list(visits = visits, opportunity = opportunities, obs = obs,
                        held = held, points = points)
  prohibited_public_columns <- c(
    "measuredBy", "observer_id", "observer_hash", "observer_pseudonym",
    "uid", "visit_uid", "detection_uid", "source_row", "samplingImpracticalRemarks"
  )
  for (table_name in names(public_tables)) {
    table <- public_tables[[table_name]]
    leaked <- intersect(prohibited_public_columns, names(table))
    assert(!length(leaked), paste(site, "public", table_name,
      "table exposes prohibited raw/linkable fields:", paste(leaked, collapse = ", ")))
    character_columns <- names(table)[vapply(table, is.character, logical(1))]
    observer_hash <- if (length(character_columns)) any(vapply(
      table[character_columns], function(value)
        any(grepl("^observer-sha256:[0-9a-f]{64}$", value), na.rm = TRUE), logical(1)
    )) else FALSE
    assert(!observer_hash, paste(site, "public", table_name,
      "table exposes a row-level observer pseudonym."))
  }
  assert(nrow(visits) > 0L && !anyDuplicated(visits$survey_id), paste(site, "has empty or duplicated survey keys."))
  assert(!anyDuplicated(opportunities$occasion_id), paste(site, "has duplicated point-year keys."))
  expected_survey <- paste(visits$site, visits$eventID, visits$plotID, visits$pointID, sep = "|")
  assert(identical(as.character(visits$survey_id), expected_survey), paste(site, "survey key is not authoritative."))
  expected_valid <- toupper(trimws(as.character(visits$samplingImpractical))) == "OK"
  expected_valid[is.na(expected_valid)] <- FALSE
  assert(identical(visits$valid_count %in% TRUE, expected_valid), paste(site, "valid-count state does not equal samplingImpractical == OK."))
  visit_year_value <- suppressWarnings(as.numeric(visits$year))
  visit_year <- suppressWarnings(as.integer(visit_year_value))
  assert(all(is.finite(visit_year_value)) && all(visit_year_value == visit_year) &&
         all(visit_year %in% 2013:2024),
         paste(site, "visit ledger contains a noninteger or out-of-release year."))
  assert(all(visits$protocol_minutes == ifelse(expected_valid, 6, 0)), paste(site, "protocol-minute accounting mismatch."))
  valid_bout_key <- paste(visits$occasion_id[expected_valid], visits$bout[expected_valid], sep = "|")
  assert(all(as.character(visits$bout) %in% c("1", "2")) && !anyDuplicated(valid_bout_key),
         paste(site, "visit bout domain or uniqueness mismatch."))
  visit_integer_fields <- c("eligible_detection_rows", "eligible_species", "flyover_rows")
  visit_numeric_fields <- c("eligible_birds", "flyover_birds")
  assert(all(vapply(visits[visit_integer_fields], function(value) {
           number <- suppressWarnings(as.numeric(value))
           all(is.finite(number) & number >= 0 & number == as.integer(number))
         }, logical(1))) &&
         all(vapply(visits[visit_numeric_fields], function(value) {
           number <- suppressWarnings(as.numeric(value))
           all(is.finite(number) & number >= 0)
         }, logical(1))),
         paste(site, "visit-level detection summaries contain invalid values."))
  expected_visit_support <- ifelse(expected_valid, "supported", "held")
  expected_visit_outcome_from_summary <- ifelse(
    !expected_valid, "unavailable",
    ifelse(visits$eligible_detection_rows > 0L, "positive", "supported_zero")
  )
  assert(identical(as.character(visits$support_state), expected_visit_support) &&
         identical(as.character(visits$outcome), expected_visit_outcome_from_summary) &&
         all(visits$eligible_detection_rows[!expected_valid] == 0L) &&
         all(visits$eligible_birds[!expected_valid] == 0) &&
         all(visits$eligible_species[!expected_valid] == 0L) &&
         all(visits$flyover_rows[!expected_valid] == 0L) &&
         all(visits$flyover_birds[!expected_valid] == 0),
         paste(site, "visit support/outcome state or held-count conservation mismatch."))

  observer_support <- bundle$meta$observer_support
  observer_fields <- c(
    "n_observers", "n_valid_visits", "n_valid_visits_with_observer",
    "complete", "by_species"
  )
  assert(is.list(observer_support) && identical(names(observer_support), observer_fields),
         paste(site, "observer support is not the minimized aggregate-only contract."))
  observer_counts <- suppressWarnings(as.numeric(unlist(observer_support[
    c("n_observers", "n_valid_visits", "n_valid_visits_with_observer")],
    use.names = FALSE)))
  assert(length(observer_counts) == 3L && all(is.finite(observer_counts)) &&
         all(observer_counts >= 0) && all(observer_counts == as.integer(observer_counts)) &&
         observer_counts[[1]] <= observer_counts[[3]] &&
         observer_counts[[3]] <= observer_counts[[2]] &&
         observer_counts[[2]] == sum(expected_valid),
         paste(site, "aggregate observer counts are invalid or disagree with visit effort."))
  assert(is.logical(observer_support$complete) && length(observer_support$complete) == 1L &&
         !is.na(observer_support$complete) &&
         identical(observer_support$complete,
                   observer_counts[[2]] > 0 && observer_counts[[3]] == observer_counts[[2]]),
         paste(site, "aggregate observer completeness disagrees with visit support."))
  observer_by_species <- as.data.frame(observer_support$by_species,
                                       stringsAsFactors = FALSE)
  expected_observer_species <- sort(unique(as.character(
    obs$communityScientificName[obs$enters_breeding_metrics %in% TRUE]
  )), method = "radix")
  assert(identical(names(observer_by_species), c("scientificName", "n_observers")) &&
         !anyNA(observer_by_species$scientificName) &&
         !anyDuplicated(observer_by_species$scientificName) &&
         identical(sort(as.character(observer_by_species$scientificName), method = "radix"),
                   expected_observer_species),
         paste(site, "species-level observer support has an invalid schema or universe."))
  species_observer_counts <- suppressWarnings(as.numeric(observer_by_species$n_observers))
  assert(all(is.finite(species_observer_counts)) && all(species_observer_counts >= 0) &&
         all(species_observer_counts == as.integer(species_observer_counts)) &&
         all(species_observer_counts <= observer_counts[[1]]),
         paste(site, "species-level observer support contains an invalid aggregate count."))
  meta_text <- as.character(unlist(bundle$meta, recursive = TRUE, use.names = FALSE))
  assert(!any(grepl("^observer-sha256:[0-9a-f]{64}$", meta_text), na.rm = TRUE),
         paste(site, "bundle metadata exposes a row-level observer pseudonym."))

  expected_supported <- opportunities$n_valid_bouts > 0
  assert(identical(opportunities$supported %in% TRUE, expected_supported), paste(site, "point-year support mismatch."))
  assert(identical(as.character(opportunities$support_state), ifelse(expected_supported, "supported", "held")),
         paste(site, "support_state mismatch."))
  expected_outcome <- ifelse(!expected_supported, "unavailable",
                      ifelse(opportunities$eligible_detection_rows > 0, "positive", "supported_zero"))
  assert(identical(as.character(opportunities$outcome), expected_outcome), paste(site, "opportunity outcome mismatch."))
  assert(all(opportunities$n_surveyed_minutes == 6 * opportunities$n_valid_bouts), paste(site, "survey-minute conservation failed."))
  assert(all(opportunities$eligible_detection_rows[!expected_supported] == 0), paste(site, "unsupported opportunity carries detections."))

  validate_minute_audit <- function(d, label) {
    expected <- point_count_minute_state(d$pointCountMinuteRaw)
    assert(same_num(d$pointCountMinute, expected$value) &&
           same_chr(d$point_count_minute_state, expected$state) &&
           identical(d$in_protocol_window %in% TRUE, expected$in_window),
           paste(site, label, "point-count minute audit fields do not re-derive from the raw token."))
  }
  validate_minute_audit(obs, "obs")
  validate_minute_audit(held, "held")
  assert(all(obs$in_protocol_window %in% TRUE), paste(site, "obs contains a detection outside minutes 1-6."))

  validate_taxonomy <- function(d, label) {
    unit <- community_unit(d$reportedTaxonRank, d$reportedScientificName)
    assert(same_chr(d$communityScientificName, unit$name) &&
           same_chr(d$scientificName, unit$name) &&
           same_chr(d$community_unit_state, unit$state) &&
           identical(d$is_species %in% TRUE, unit$eligible) &&
           same_chr(d$reportedTaxonID, d$taxonID) &&
           same_chr(d$reportedTaxonRank, d$taxonRank) &&
           same_chr(d$reportedVernacularName, d$vernacularName),
           paste(site, label, "taxonomy provenance or species canonicalization does not re-derive."))
    unit
  }
  obs_unit <- validate_taxonomy(obs, "obs")
  held_unit <- validate_taxonomy(held, "held")
  validate_detection_method <- function(d, label) {
    token <- tolower(trimws(as.character(d$detectionMethod)))
    known <- !blank(token) & token != "unknown"
    expected_state <- ifelse(known, "reported", "missing_or_unknown")
    assert(same_chr(d$detection_method_state, expected_state),
           paste(site, label, "detection-method state does not re-derive."))
    known
  }
  obs_method_known <- validate_detection_method(obs, "obs")
  held_method_known <- validate_detection_method(held, "held")
  derive_flyover <- function(method) {
    value <- grepl("flyover", tolower(as.character(method)))
    value[is.na(value)] <- FALSE
    value
  }
  obs_flyover <- derive_flyover(obs$detectionMethod)
  held_flyover <- derive_flyover(held$detectionMethod)
  assert(identical(obs$is_flyover %in% TRUE, obs_flyover) &&
         identical(held$is_flyover %in% TRUE, held_flyover),
         paste(site, "flyover state does not independently re-derive from detectionMethod."))

  obs_cluster_valid <- is.finite(obs$clusterSize) & obs$clusterSize > 0 &
    obs$clusterSize == floor(obs$clusterSize)
  expected_eligible <- obs_unit$eligible & obs_method_known & obs$valid_count %in% TRUE &
    obs$in_protocol_window %in% TRUE &
    !obs_flyover & obs_cluster_valid
  assert(identical(obs$enters_breeding_metrics %in% TRUE, expected_eligible),
         paste(site, "eligible protocol-filtered predicate mismatch."))
  obs_visit_match <- match(as.character(obs$survey_id), as.character(visits$survey_id))
  assert(!anyNA(obs_visit_match) &&
         same_chr(obs$occasion_id, visits$occasion_id[obs_visit_match]) &&
         same_chr(obs$site, visits$site[obs_visit_match]) &&
         same_chr(obs$pointkey, visits$pointkey[obs_visit_match]) &&
         same_chr(obs$plotID, visits$plotID[obs_visit_match]) &&
         same_chr(obs$pointID, visits$pointID[obs_visit_match]) &&
         same_chr(obs$eventID, visits$eventID[obs_visit_match]) &&
         same_num(obs$year, visit_year[obs_visit_match]) &&
         same_chr(obs$bout, visits$bout[obs_visit_match]) &&
         identical(obs$valid_count %in% TRUE, expected_valid[obs_visit_match]),
         paste(site, "obs does not rejoin exactly to its authoritative physical visit."))
  assert(all(obs$survey_id %in% visits$survey_id[expected_valid]), paste(site, "obs contains an unmatched or unusable visit."))
  assert(all(obs$occasion_id[expected_eligible] %in% opportunities$occasion_id[expected_supported]),
         paste(site, "eligible detection lacks supported point-year."))
  assert(all(obs$detection_year_matches_visit[!is.na(obs$detection_year_matches_visit)] %in% TRUE) &&
         all(obs$detection_bout_matches_visit[!is.na(obs$detection_bout_matches_visit)] %in% TRUE),
         paste(site, "detection/visit year or bout audit mismatch."))
  assert(sum(opportunities$eligible_detection_rows) == sum(expected_eligible), paste(site, "eligible-row conservation failed."))
  assert(same_num(sum(opportunities$eligible_birds), sum(obs$clusterSize[expected_eligible], na.rm = TRUE)),
         paste(site, "eligible-bird conservation failed."))
  expected_fly <- obs_flyover & obs$valid_count %in% TRUE &
    obs$in_protocol_window %in% TRUE & obs$is_species %in% TRUE & obs_cluster_valid
  assert(sum(opportunities$flyover_rows) == sum(expected_fly), paste(site, "flyover-row conservation failed."))
  assert(same_num(sum(opportunities$flyover_birds), sum(obs$clusterSize[expected_fly], na.rm = TRUE)),
         paste(site, "flyover-bird conservation failed."))
  assert(all(opportunities$eligible_detection_rows[opportunities$outcome == "supported_zero"] == 0),
         paste(site, "supported zero carries an eligible detection."))

  expected_visit_summary <- data.frame(
    survey_id = as.character(visits$survey_id),
    eligible_detection_rows = 0L, eligible_birds = 0, eligible_species = 0L,
    flyover_rows = 0L, flyover_birds = 0,
    stringsAsFactors = FALSE
  )
  add_visit_groups <- function(rows, flyover = FALSE) {
    if (!length(rows)) return(invisible(NULL))
    groups <- split(rows, as.character(obs$survey_id[rows]))
    destination <- match(names(groups), expected_visit_summary$survey_id)
    assert(!anyNA(destination), paste(site, "detection summary has an unknown survey_id."))
    if (flyover) {
      expected_visit_summary$flyover_rows[destination] <<-
        vapply(groups, length, integer(1))
      expected_visit_summary$flyover_birds[destination] <<-
        vapply(groups, function(i) sum(obs$clusterSize[i]), numeric(1))
    } else {
      expected_visit_summary$eligible_detection_rows[destination] <<-
        vapply(groups, length, integer(1))
      expected_visit_summary$eligible_birds[destination] <<-
        vapply(groups, function(i) sum(obs$clusterSize[i]), numeric(1))
      expected_visit_summary$eligible_species[destination] <<-
        vapply(groups, function(i) length(unique(obs$communityScientificName[i])), integer(1))
    }
    invisible(NULL)
  }
  add_visit_groups(which(expected_eligible))
  add_visit_groups(which(expected_fly), flyover = TRUE)
  for (field in setdiff(names(expected_visit_summary), "survey_id"))
    assert(same_num(visits[[field]], expected_visit_summary[[field]]),
           paste(site, "visit-level summary differs for", field))
  independently_expected_visit_outcome <- ifelse(
    !expected_valid, "unavailable",
    ifelse(expected_visit_summary$eligible_detection_rows > 0L,
           "positive", "supported_zero")
  )
  assert(identical(as.character(visits$outcome), independently_expected_visit_outcome) &&
         identical(as.character(visits$support_state), expected_visit_support),
         paste(site, "visit outcomes do not independently re-derive from detection evidence."))

  if (nrow(held)) {
    expected_hold_reason <- ifelse(!(held$matched_visit %in% TRUE), "orphan_detection_no_visit",
      ifelse(!(held$valid_count %in% TRUE), "detection_on_unusable_visit",
        ifelse(held$point_count_minute_state == "incidental_minute_88", "incidental_outside_point_count",
          ifelse(held$point_count_minute_state == "source_missing", "missing_point_count_minute",
            ifelse(!(held$in_protocol_window %in% TRUE), "invalid_point_count_minute",
              ifelse(!is.finite(held$clusterSize) | held$clusterSize <= 0 |
                       held$clusterSize != floor(held$clusterSize), "invalid_cluster_size",
                ifelse(is.na(held$reportedScientificName) |
                         !nzchar(trimws(as.character(held$reportedScientificName))),
                       "missing_taxon",
                  ifelse(tolower(trimws(as.character(held$reportedTaxonRank))) %in%
                           c("species", "subspecies") & !held_unit$eligible,
                         "unsafe_species_canonicalization",
                    ifelse(!held_method_known,
                           "missing_or_unknown_detection_method", NA_character_)))))))))
    assert(identical(as.character(held$hold_reason), expected_hold_reason) && !anyNA(expected_hold_reason),
           paste(site, "held reasons do not re-derive from held-row state."))
  }

  # Per-occasion conservation prevents offsetting shifts between point-years from
  # passing a site-level total check.
  visit_groups <- split(seq_len(nrow(visits)), visits$occasion_id)
  obs_groups <- split(which(expected_eligible), obs$occasion_id[expected_eligible])
  fly_groups <- split(which(expected_fly), obs$occasion_id[expected_fly])
  for (i in seq_len(nrow(opportunities))) {
    key <- as.character(opportunities$occasion_id[[i]])
    vi <- visit_groups[[key]]
    oi <- obs_groups[[key]]
    fi <- fly_groups[[key]]
    if (is.null(oi)) oi <- integer()
    if (is.null(fi)) fi <- integer()
    assert(length(vi) > 0L &&
           opportunities$n_valid_bouts[[i]] == sum(visits$valid_count[vi]) &&
           opportunities$n_held_bouts[[i]] == sum(!visits$valid_count[vi]) &&
           opportunities$eligible_detection_rows[[i]] == length(oi) &&
           same_num(opportunities$eligible_birds[[i]], sum(obs$clusterSize[oi], na.rm = TRUE)) &&
           opportunities$eligible_species[[i]] == length(unique(obs$communityScientificName[oi])) &&
           opportunities$flyover_rows[[i]] == length(fi) &&
           same_num(opportunities$flyover_birds[[i]], sum(obs$clusterSize[fi], na.rm = TRUE)),
           paste(site, "per-occasion visit/detection conservation failed for", key))
  }

  allowed_distance <- c("observed", "sentinel_not_estimable", "source_missing", "invalid")
  assert(all(obs$distance_state %in% allowed_distance), paste(site, "has an unknown distance state."))
  observed_distance <- obs$distance_state == "observed"
  assert(all(is.finite(obs$observerDistance[observed_distance]) & obs$observerDistance[observed_distance] >= 0),
         paste(site, "observed distance state is not finite nonnegative metres."))
  assert(all(is.na(obs$observerDistance[!observed_distance])), paste(site, "non-observed distance leaked into numeric metres."))
  raw_distance <- suppressWarnings(as.numeric(obs$observerDistanceRaw))
  sentinel <- obs$distance_state == "sentinel_not_estimable"
  assert(all(raw_distance[sentinel] %in% c(999, 9999)), paste(site, "distance sentinel state lost its raw code."))
  assert(!any(obs$observerDistance %in% c(999, 9999), na.rm = TRUE), paste(site, "distance sentinel leaked into analysis values."))
  assert(all(obs$method_channel %in% c("singing", "calling", "visual", "drumming", "other", "unknown")),
         paste(site, "has an unknown canonical method channel."))

  assert(sum(points$n_visits) == sum(visits$valid_count), paste(site, "point visit totals do not conserve."))
  assert(same_num(bundle$meta$n_visits, sum(visits$valid_count)),
         paste(site, "metadata visit total mismatch."))
  assert(same_num(bundle$meta$n_supported_zero_counts,
                  sum(independently_expected_visit_outcome == "supported_zero")),
         paste(site, "metadata supported-zero count total mismatch."))
  assert(same_num(bundle$meta$n_opportunities, sum(expected_supported)),
         paste(site, "metadata opportunity total mismatch."))
  assert(same_num(bundle$meta$n_supported_zero,
                  sum(opportunities$outcome == "supported_zero")),
         paste(site, "metadata supported-zero total mismatch."))
  assert(same_num(bundle$meta$n_unavailable,
                  sum(opportunities$outcome == "unavailable")),
         paste(site, "metadata unavailable total mismatch."))

  row <- index[index$site == site, , drop = FALSE]
  assert(nrow(row) == 1L, paste(site, "is missing or duplicated in site index."))
  assert(row$n_species == length(unique(obs$communityScientificName[expected_eligible])) &&
         row$n_points == sum(points$n_visits > 0) &&
         row$n_visits == sum(visits$valid_count) &&
         row$n_supported_zero_counts ==
           sum(independently_expected_visit_outcome == "supported_zero") &&
         row$n_opportunities == sum(expected_supported) &&
         row$n_supported_zero == sum(opportunities$outcome == "supported_zero"),
         paste(site, "site index does not re-derive from the bundle."))
  species_totals <- tapply(obs$clusterSize[expected_eligible],
                           obs$communityScientificName[expected_eligible], sum)
  expected_top_species <- if (length(species_totals)) {
    top_order <- order(-as.numeric(species_totals), names(species_totals), method = "radix")
    names(species_totals)[top_order[[1]]]
  } else NA_character_
  assert(same_chr(row$top_species, expected_top_species),
         paste(site, "site-index top species is not a canonical community unit."))
  expected_index <- sum(obs$clusterSize[expected_eligible], na.rm = TRUE) / sum(visits$valid_count)
  assert(same_num(row$birds_per_count, round(expected_index, 3)), paste(site, "detection index mismatch."))

  # Reconstruct the shared-window count-incidence input independently from the
  # producer and without consulting the lifetime site index.
  window_visit <- expected_valid &
    visit_year >= BIRD_CROSS_SITE_YEAR_MIN &
    visit_year <= BIRD_CROSS_SITE_YEAR_MAX
  window_visit_ids <- as.character(visits$survey_id[window_visit])
  T_counts <- as.integer(length(window_visit_ids))
  assert(T_counts >= 2L && !anyDuplicated(window_visit_ids),
         paste(site, "has incomplete or duplicated valid-count support in the cross-site window."))
  window_detection <- expected_eligible & window_visit[obs_visit_match]
  window_rows <- which(window_detection)
  species_count_pairs <- if (length(window_rows)) unique(data.frame(
    communityScientificName = as.character(obs$communityScientificName[window_rows]),
    survey_id = as.character(obs$survey_id[window_rows]),
    stringsAsFactors = FALSE
  )) else data.frame(
    communityScientificName = character(), survey_id = character(),
    stringsAsFactors = FALSE
  )
  incidence_table <- if (nrow(species_count_pairs))
    table(species_count_pairs$communityScientificName) else integer()
  incidence_y <- as.integer(incidence_table)
  U_incidence <- as.integer(sum(incidence_y))
  S_obs <- as.integer(length(incidence_y))
  Q1_incidence <- as.integer(sum(incidence_y == 1L))
  Q2_incidence <- as.integer(sum(incidence_y == 2L))
  n_positive_counts_window <- as.integer(length(unique(species_count_pairs$survey_id)))
  n_supported_zero_counts_window <- as.integer(T_counts - n_positive_counts_window)
  n_points_window <- as.integer(length(unique(as.character(visits$pointkey[window_visit]))))
  n_birds_window <- if (length(window_rows)) sum(obs$clusterSize[window_rows]) else 0

  species_point_pairs <- if (length(window_rows)) unique(data.frame(
    communityScientificName = as.character(obs$communityScientificName[window_rows]),
    pointkey = as.character(visits$pointkey[obs_visit_match[window_rows]]),
    stringsAsFactors = FALSE
  )) else data.frame(
    communityScientificName = character(), pointkey = character(),
    stringsAsFactors = FALSE
  )
  point_incidence <- if (nrow(species_point_pairs))
    table(species_point_pairs$communityScientificName) else integer()
  species_birds_window <- if (length(window_rows))
    tapply(obs$clusterSize[window_rows],
           obs$communityScientificName[window_rows], sum) else numeric()
  top_species_window <- if (length(species_birds_window)) {
    top_order <- order(-as.numeric(species_birds_window),
                       names(species_birds_window), method = "radix")
    names(species_birds_window)[top_order[[1]]]
  } else NA_character_
  singing_window <- if (length(window_rows)) {
    value <- grepl(
      "singing",
      tolower(trimws(as.character(obs$detectionMethod[window_rows]))),
      fixed = TRUE
    )
    value[is.na(value)] <- FALSE
    value
  } else logical()
  mean_detection_frequency <- if (S_obs > 0L)
    mean(100 * incidence_y / T_counts) else NA_real_
  mean_ubiquity <- if (length(point_incidence))
    mean(100 * as.numeric(point_incidence) / n_points_window) else NA_real_
  pct_singing <- if (length(window_rows)) 100 * mean(singing_window) else NA_real_

  assert(all(incidence_y >= 1L & incidence_y <= T_counts) &&
         U_incidence == nrow(species_count_pairs) &&
         S_obs == length(unique(species_count_pairs$communityScientificName)) &&
         n_positive_counts_window + n_supported_zero_counts_window == T_counts &&
         U_incidence >= n_positive_counts_window &&
         n_points_window >= 1L && n_points_window <= T_counts &&
         is.finite(n_birds_window) && n_birds_window >= n_positive_counts_window &&
         sum(independently_expected_visit_outcome[window_visit] == "positive") ==
           n_positive_counts_window &&
         sum(independently_expected_visit_outcome[window_visit] == "supported_zero") ==
           n_supported_zero_counts_window,
         paste(site, "independent cross-site incidence reconstruction failed."))

  # Independently reconstruct the taxon-by-site search rows from the same exact
  # window. No lifetime observer aggregate, site index, or helper-produced board
  # is allowed into this multi-site surface.
  taxon_groups <- split(
    window_rows,
    as.character(obs$communityScientificName[window_rows])
  )
  site_meta_row <- neon_sites[neon_sites$site == site, , drop = FALSE]
  assert(nrow(site_meta_row) == 1L,
         paste(site, "does not have one canonical metadata row."))
  search_taxa <- lapply(names(taxon_groups), function(scientific_name) {
    rows <- taxon_groups[[scientific_name]]
    visit_rows <- obs_visit_match[rows]
    distance_observed <- as.character(obs$distance_state[rows]) == "observed"
    distance_used <- distance_observed & is.finite(obs$observerDistance[rows]) &
      obs$observerDistance[rows] >= 0 & obs$observerDistance[rows] <= 200
    distance_outside <- distance_observed & is.finite(obs$observerDistance[rows]) &
      (obs$observerDistance[rows] < 0 | obs$observerDistance[rows] > 200)
    data.frame(
      scientificName = scientific_name,
      vernacular = vernacular_oracle(
        scientific_name, obs$reportedScientificName[rows],
        obs$reportedTaxonRank[rows], obs$reportedVernacularName[rows]
      ),
      site = site,
      name = as.character(site_meta_row$name[[1]]),
      state = as.character(site_meta_row$state[[1]]),
      analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
      analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
      detection_index_window = round(sum(obs$clusterSize[rows]) / T_counts, 3),
      detection_frequency_window = round(
        100 * length(unique(as.character(obs$survey_id[rows]))) / T_counts, 1
      ),
      detection_rows_window = as.integer(length(rows)),
      n_detected_counts_window = as.integer(length(unique(as.character(
        obs$survey_id[rows]
      )))),
      n_points_detected_window = as.integer(length(unique(as.character(
        visits$pointkey[visit_rows]
      )))),
      distance_usable_pct_window = round(
        100 * sum(distance_observed) /
          max(1L, sum(distance_observed) + sum(!distance_observed)),
        1
      ),
      distance_n_used_window = as.integer(sum(distance_used)),
      distance_n_outside_truncation_window = as.integer(sum(distance_outside)),
      method = mode_chr_oracle(canonical_method_oracle(
        obs$detectionMethod[rows]
      )),
      year_min = as.integer(min(visit_year[visit_rows])),
      year_max = as.integer(max(visit_year[visit_rows])),
      stringsAsFactors = FALSE
    )
  })
  search_taxa <- if (length(search_taxa)) do.call(rbind, search_taxa) else NULL
  lifetime_species <- sort(unique(as.character(
    obs$communityScientificName[expected_eligible]
  )), method = "radix")
  window_species <- sort(names(taxon_groups), method = "radix")
  prewindow_only_species <- setdiff(lifetime_species, window_species)

  cross_oracle_base[[site]] <- list(
    Y = incidence_y,
    row = data.frame(
      site = site,
      analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
      analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
      bird_year_min = min(visit_year[window_visit]),
      bird_year_max = max(visit_year[window_visit]),
      T_counts = T_counts,
      n_visits_window = T_counts,
      n_points_window = n_points_window,
      n_positive_counts_window = n_positive_counts_window,
      n_supported_zero_counts_window = n_supported_zero_counts_window,
      n_birds_window = n_birds_window,
      S_obs = S_obs,
      U_incidence = U_incidence,
      Q1_incidence = Q1_incidence,
      Q2_incidence = Q2_incidence,
      birds_per_count_window = n_birds_window / T_counts,
      top_species_window = top_species_window,
      mean_detection_frequency = mean_detection_frequency,
      mean_ubiquity = mean_ubiquity,
      pct_singing = pct_singing,
      stringsAsFactors = FALSE
    ),
    search_taxa = search_taxa,
    prewindow_only_keys = paste(site, prewindow_only_species, sep = "|")
  )
}

demo <- readRDS(file.path(ROOT, "data-sample", "demo.rds"))
assert(identical(demo, readRDS(file.path(SITES, "CLBJ.rds"))), "Demo bundle is not identical to CLBJ.")
cross <- readRDS(file.path(DATA, "cross_site.rds"))
assert(all(vapply(cross_oracle_base, function(value) is.list(value) &&
                    identical(names(value), c(
                      "Y", "row", "search_taxa", "prewindow_only_keys"
                    )), logical(1))),
       "Independent cross-site inputs are incomplete.")
t_common <- min(vapply(cross_oracle_base, function(value)
  as.integer(value$row$T_counts[[1]]), integer(1)))
expected_cross_rows <- lapply(expected_sites, function(site) {
  value <- cross_oracle_base[[site]]
  Y <- value$Y
  hill <- hill_incidence_oracle(Y)
  row <- value$row
  row$S_rare <- rarefy_incidence_oracle(Y, row$T_counts[[1]], t_common)
  row$t_used <- as.integer(t_common)
  row$coverage <- coverage_incidence_oracle(Y, row$T_counts[[1]])
  row$hill_q1 <- unname(hill[["q1"]])
  row$hill_q2 <- unname(hill[["q2"]])
  row
})
expected_cross <- do.call(rbind, expected_cross_rows)
rownames(expected_cross) <- NULL
expected_cross_fields <- c(
  "site", "analysis_year_min", "analysis_year_max", "bird_year_min", "bird_year_max",
  "T_counts", "n_visits_window", "n_points_window", "n_positive_counts_window",
  "n_supported_zero_counts_window", "n_birds_window", "S_obs", "U_incidence",
  "Q1_incidence", "Q2_incidence", "birds_per_count_window", "top_species_window",
  "mean_detection_frequency", "mean_ubiquity", "pct_singing", "S_rare", "t_used",
  "coverage", "hill_q1", "hill_q2"
)
expected_cross_method <- sprintf(
  paste("Eligible in-window non-flyover species incidence across valid physical counts",
        "in %d-%d; richness rarefied to %d counts. Equal count size does not",
        "equalize coverage, detectability, or spatiotemporal design."),
  BIRD_CROSS_SITE_YEAR_MIN, BIRD_CROSS_SITE_YEAR_MAX, t_common
)
assert(is.data.frame(cross) && identical(names(cross), expected_cross_fields) &&
       identical(names(expected_cross), expected_cross_fields) &&
       nrow(cross) == 47L &&
       identical(as.character(cross$site), expected_sites),
       "Cross-site artifact fields, order, or exact roster mismatch.")
assert(identical(as.integer(attr(cross, "schema_version", exact = TRUE)), 4L) &&
       identical(attr(cross, "release", exact = TRUE), "RELEASE-2026") &&
       identical(as.integer(attr(cross, "analysis_year_min", exact = TRUE)),
                 BIRD_CROSS_SITE_YEAR_MIN) &&
       identical(as.integer(attr(cross, "analysis_year_max", exact = TRUE)),
                 BIRD_CROSS_SITE_YEAR_MAX) &&
       identical(attr(cross, "incidence_unit", exact = TRUE),
                 paste("valid physical six-minute count keyed by survey_id;",
                       "repeated counts are protocol samples, not independent places")) &&
       identical(attr(cross, "method", exact = TRUE), expected_cross_method),
       "Cross-site artifact attributes do not bind schema v4, release, window, and count incidence.")
integer_cross_fields <- c(
  "analysis_year_min", "analysis_year_max", "bird_year_min", "bird_year_max",
  "T_counts", "n_visits_window", "n_points_window", "n_positive_counts_window",
  "n_supported_zero_counts_window", "S_obs", "U_incidence", "Q1_incidence",
  "Q2_incidence", "t_used"
)
assert(all(vapply(cross[integer_cross_fields], is.integer, logical(1))),
       "Cross-site count/window support fields are not stored as integers.")
for (field in expected_cross_fields) {
  equal <- if (is.character(expected_cross[[field]]))
    same_chr(cross[[field]], expected_cross[[field]]) else
      same_num(cross[[field]], expected_cross[[field]])
  assert(equal, paste("Cross-site field does not independently reconstruct:", field))
}
assert(all(cross$T_counts == cross$n_visits_window) &&
       all(cross$n_positive_counts_window + cross$n_supported_zero_counts_window ==
             cross$T_counts) &&
       identical(unique(cross$t_used), as.integer(t_common)) &&
       t_common == min(cross$T_counts),
       "Cross-site count, zero-support, or common-rarefaction identities mismatch.")

# The network search is another multi-site comparison and therefore must use
# this same independent 2017-2024 oracle. It cannot inherit or rename lifetime
# values from site_index.rds.
expected_search_taxa_parts <- lapply(
  expected_sites, function(site) cross_oracle_base[[site]]$search_taxa
)
expected_search_taxa_parts <- Filter(Negate(is.null), expected_search_taxa_parts)
assert(length(expected_search_taxa_parts) > 0L,
       "Independent search oracle contains no window-qualified taxa.")
expected_search_taxa <- do.call(rbind, expected_search_taxa_parts)
search_species_support <- table(expected_search_taxa$scientificName)
expected_search_taxa$n_sites_window <- as.integer(search_species_support[
  expected_search_taxa$scientificName
])
expected_search_taxa <- expected_search_taxa[order(
  expected_search_taxa$scientificName,
  -expected_search_taxa$detection_index_window,
  expected_search_taxa$site,
  method = "radix"
), , drop = FALSE]
rownames(expected_search_taxa) <- NULL
expected_search_taxa_fields <- c(
  "scientificName", "vernacular", "site", "name", "state",
  "analysis_year_min", "analysis_year_max", "detection_index_window",
  "detection_frequency_window", "detection_rows_window",
  "n_detected_counts_window", "n_points_detected_window",
  "distance_usable_pct_window", "distance_n_used_window",
  "distance_n_outside_truncation_window", "method", "year_min", "year_max",
  "n_sites_window"
)
expected_search_sites_fields <- c(
  "site", "name", "state", "analysis_year_min", "analysis_year_max",
  "bird_year_min", "bird_year_max", "S_obs", "S_rare", "t_used",
  "T_counts", "n_points_window", "n_birds_window",
  "n_positive_counts_window", "n_supported_zero_counts_window",
  "birds_per_count_window", "top_species_window", "coverage"
)
site_meta_match <- match(expected_cross$site, neon_sites$site)
assert(!anyNA(site_meta_match),
       "Independent search-site oracle lacks canonical site metadata.")
expected_search_sites <- data.frame(
  site = as.character(expected_cross$site),
  name = as.character(neon_sites$name[site_meta_match]),
  state = as.character(neon_sites$state[site_meta_match]),
  analysis_year_min = as.integer(expected_cross$analysis_year_min),
  analysis_year_max = as.integer(expected_cross$analysis_year_max),
  bird_year_min = as.integer(expected_cross$bird_year_min),
  bird_year_max = as.integer(expected_cross$bird_year_max),
  S_obs = as.integer(expected_cross$S_obs),
  S_rare = as.numeric(expected_cross$S_rare),
  t_used = as.integer(expected_cross$t_used),
  T_counts = as.integer(expected_cross$T_counts),
  n_points_window = as.integer(expected_cross$n_points_window),
  n_birds_window = as.numeric(expected_cross$n_birds_window),
  n_positive_counts_window = as.integer(expected_cross$n_positive_counts_window),
  n_supported_zero_counts_window = as.integer(
    expected_cross$n_supported_zero_counts_window
  ),
  birds_per_count_window = as.numeric(expected_cross$birds_per_count_window),
  top_species_window = as.character(expected_cross$top_species_window),
  coverage = as.numeric(expected_cross$coverage),
  stringsAsFactors = FALSE
)
expected_search_sites <- expected_search_sites[order(
  -expected_search_sites$S_obs, expected_search_sites$site, method = "radix"
), , drop = FALSE]
rownames(expected_search_sites) <- NULL

search <- readRDS(file.path(DATA, "search_index.rds"))
assert(is.list(search) && identical(names(search), c(
         "schema_version", "analysis_year_min", "analysis_year_max",
         "incidence_unit", "taxa", "sites"
       )) && identical(search$schema_version, 4L) &&
       identical(search$analysis_year_min, BIRD_CROSS_SITE_YEAR_MIN) &&
       identical(search$analysis_year_max, BIRD_CROSS_SITE_YEAR_MAX) &&
       identical(as.character(search$incidence_unit),
                 "valid physical six-minute count keyed by survey_id"),
       "Search index does not bind its exact schema-v4 physical-count window.")
search_taxa <- search$taxa
search_sites <- search$sites
assert(is.data.frame(search_taxa) && is.data.frame(search_sites) &&
       identical(names(search_taxa), expected_search_taxa_fields) &&
       identical(names(expected_search_taxa), expected_search_taxa_fields) &&
       identical(names(search_sites), expected_search_sites_fields) &&
       identical(names(expected_search_sites), expected_search_sites_fields) &&
       nrow(search_taxa) == nrow(expected_search_taxa) &&
       nrow(search_sites) == 47L,
       "Search taxon/site exact window-only field contract mismatch.")
lifetime_search_fields <- c(
  "index", "detections", "n_sites", "n_observers", "n_species", "n_points",
  "n_visits", "n_supported_zero_counts", "n_opportunities",
  "n_supported_zero", "birds_per_count", "top_species"
)
assert(!length(intersect(names(search_taxa), lifetime_search_fields)) &&
       !length(intersect(names(search_sites), lifetime_search_fields)),
       "Search index exposes a lifetime site-index or legacy taxon field.")
integer_search_taxa_fields <- c(
  "analysis_year_min", "analysis_year_max", "detection_rows_window",
  "n_detected_counts_window", "n_points_detected_window",
  "distance_n_used_window", "distance_n_outside_truncation_window",
  "year_min", "year_max", "n_sites_window"
)
integer_search_site_fields <- c(
  "analysis_year_min", "analysis_year_max", "bird_year_min", "bird_year_max",
  "S_obs", "t_used", "T_counts", "n_points_window",
  "n_positive_counts_window", "n_supported_zero_counts_window"
)
assert(all(vapply(search_taxa[integer_search_taxa_fields], is.integer, logical(1))) &&
       all(vapply(search_sites[integer_search_site_fields], is.integer, logical(1))),
       "Search window/count support fields are not stored as integers.")
for (field in expected_search_taxa_fields) {
  equal <- if (is.character(expected_search_taxa[[field]]))
    same_chr(search_taxa[[field]], expected_search_taxa[[field]]) else
      same_num(search_taxa[[field]], expected_search_taxa[[field]])
  assert(equal, paste("Search taxon field does not independently reconstruct:", field))
}
for (field in expected_search_sites_fields) {
  equal <- if (is.character(expected_search_sites[[field]]))
    same_chr(search_sites[[field]], expected_search_sites[[field]]) else
      same_num(search_sites[[field]], expected_search_sites[[field]])
  assert(equal, paste("Search site field does not independently reconstruct:", field))
}
search_keys <- paste(as.character(search_taxa$site),
                     as.character(search_taxa$scientificName), sep = "|")
expected_search_keys <- paste(as.character(expected_search_taxa$site),
                              as.character(expected_search_taxa$scientificName),
                              sep = "|")
prewindow_only_keys <- unlist(lapply(
  cross_oracle_base, function(value) value$prewindow_only_keys
), use.names = FALSE)
actual_search_support <- table(search_taxa$scientificName)
assert(!anyDuplicated(search_keys) && identical(search_keys, expected_search_keys) &&
       !any(search_keys %in% prewindow_only_keys) &&
       same_num(search_taxa$n_sites_window,
                as.integer(actual_search_support[search_taxa$scientificName])) &&
       all(search_taxa$year_min >= BIRD_CROSS_SITE_YEAR_MIN) &&
       all(search_taxa$year_max <= BIRD_CROSS_SITE_YEAR_MAX) &&
       all(search_sites$n_positive_counts_window +
             search_sites$n_supported_zero_counts_window == search_sites$T_counts) &&
       identical(unique(search_sites$t_used), as.integer(t_common)),
       paste("Search index contains a lifetime/pre-window taxon, duplicated key,",
             "or inconsistent physical-count window support."))
climate <- readRDS(file.path(DATA, "site_climate.rds"))
assert(nrow(climate) == 47L && identical(sort(as.character(climate$site)), expected_sites), "Climate site roster mismatch.")
expected_climate_fields <- c(
  "site", "lat", "lng", "domain", "mat_c", "breeding_temp_c",
  "n_realized_months", "n_supported_realized_months", "analysis_year_min",
  "analysis_year_max", "temp_amp_c", "peak_greenup_pct", "greenup_peak_month",
  "greenup_peak_lab", "precip_annual_mm", "n_precip_months",
  "n_complete_precip_years", "count_months", "count_month_min",
  "count_month_max", "count_months_lab", "env_year_min", "env_year_max"
)
assert(identical(names(climate), expected_climate_fields) &&
       all(is.na(climate$precip_annual_mm) | climate$n_complete_precip_years > 0L),
       "Climate exact field contract or complete-year precipitation support is missing.")
monthly_climate <- readRDS(file.path(DATA, "site_month_clim.rds"))
assert(is.data.frame(monthly_climate) &&
       identical(names(monthly_climate),
                 c("site", "mon", "month_lab", "temp_c", "greenup_pct")) &&
       identical(sort(unique(as.character(monthly_climate$site))), expected_sites),
       "Monthly climate roster or schema mismatch.")
assert(identical(attr(climate, "release", exact = TRUE), "RELEASE-2026") &&
       identical(attr(monthly_climate, "release", exact = TRUE), "RELEASE-2026"),
       "Climate derived tables lack exact release identity.")
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_set_label <- function(months) {
  labels <- month_labels[months]
  if (length(labels) == 1L) return(labels)
  if (length(labels) == 2L) return(paste(labels, collapse = " & "))
  paste0(paste(labels[-length(labels)], collapse = ", "), " & ", labels[[length(labels)]])
}
for (site in expected_sites) {
  env <- readRDS(file.path(ENV, paste0(site, ".rds")))
  assert(is.data.frame(env) && nrow(env) > 0L &&
         all(c("siteID", "ym", "temp_c", "greenup_pct", "precip_mm") %in% names(env)) &&
         all(as.character(env$siteID) == site),
         paste(site, "environment bundle lacks the climate-oracle schema."))
  ym <- as.character(env$ym)
  valid_ym <- !is.na(ym) & grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", ym)
  assert(all(valid_ym) && !anyDuplicated(ym),
         paste(site, "environment bundle has an invalid or duplicate year-month."))
  env_year <- as.integer(substr(ym, 1, 4))
  env_month <- as.integer(substr(ym, 6, 7))
  env_temp <- suppressWarnings(as.numeric(env$temp_c))
  env_greenup <- suppressWarnings(as.numeric(env$greenup_pct))
  env_precip <- suppressWarnings(as.numeric(env$precip_mm))
  assert(all(is.na(env$temp_c) | is.finite(env_temp)) &&
         all(is.na(env$greenup_pct) | is.finite(env_greenup)) &&
         all(is.na(env$precip_mm) | is.finite(env_precip)),
         paste(site, "environment bundle contains a non-finite non-missing climate value."))
  climatology <- function(values, month) {
    observed <- values[env_month == month & is.finite(values)]
    if (length(observed) >= 2L) mean(observed) else NA_real_
  }
  expected_temp <- vapply(1:12, function(month) climatology(env_temp, month), numeric(1))
  expected_greenup <- vapply(1:12, function(month) climatology(env_greenup, month), numeric(1))
  monthly <- monthly_climate[monthly_climate$site == site, , drop = FALSE]
  month_value <- suppressWarnings(as.numeric(monthly$mon))
  months <- suppressWarnings(as.integer(month_value))
  assert(nrow(monthly) == 12L && all(is.finite(month_value)) &&
         all(month_value == months) && identical(sort(months), 1:12) &&
         !anyDuplicated(months),
         paste(site, "monthly climate does not contain one row per calendar month."))
  monthly <- monthly[match(1:12, months), , drop = FALSE]
  assert(same_chr(monthly$month_lab, month_labels) &&
         same_num(monthly$temp_c, expected_temp) &&
         same_num(monthly$greenup_pct, expected_greenup),
         paste(site, "monthly climatology does not independently reconstruct from its environment bundle."))
  bundle <- readRDS(file.path(SITES, paste0(site, ".rds")))
  visit_year_value <- suppressWarnings(as.numeric(bundle$visits$year))
  visit_year <- suppressWarnings(as.integer(visit_year_value))
  assert(all(is.finite(visit_year_value)) && all(visit_year_value == visit_year),
         paste(site, "visit ledger has an invalid year in the climate oracle."))
  in_analysis_window <- bundle$visits$valid_count %in% TRUE &
    visit_year >= BIRD_CROSS_SITE_YEAR_MIN & visit_year <= BIRD_CROSS_SITE_YEAR_MAX
  visit_dates <- as.character(bundle$visits$startDate[in_analysis_window])
  date_token <- substr(visit_dates, 1, 10)
  parsed_dates <- suppressWarnings(as.Date(date_token, format = "%Y-%m-%d"))
  valid_dates <- !is.na(visit_dates) &
    grepl("^[0-9]{4}-(0[1-9]|1[0-2])-[0-9]{2}", visit_dates) &
    !is.na(parsed_dates) & format(parsed_dates, "%Y-%m-%d") == date_token
  assert(length(visit_dates) > 0L && all(valid_dates),
         paste(site, "has a valid count with a missing or invalid startDate."))
  realized <- sort(unique(as.integer(format(parsed_dates, "%m"))))
  values <- expected_temp[realized]
  n_supported <- sum(is.finite(values))
  row <- climate[climate$site == site, , drop = FALSE]
  realized_count <- suppressWarnings(as.numeric(row$n_realized_months))
  supported_count <- suppressWarnings(as.numeric(row$n_supported_realized_months))
  assert(nrow(row) == 1L && is.finite(realized_count) && is.finite(supported_count) &&
         realized_count == as.integer(realized_count) &&
         supported_count == as.integer(supported_count) &&
         realized_count == length(realized) && supported_count == n_supported &&
         identical(as.character(row$count_months), paste(realized, collapse = ",")) &&
         identical(as.character(row$count_months_lab), month_set_label(realized)),
         paste(site, "realized-month temperature support counts do not reconstruct."))
  assert(same_num(row$analysis_year_min, BIRD_CROSS_SITE_YEAR_MIN) &&
         same_num(row$analysis_year_max, BIRD_CROSS_SITE_YEAR_MAX),
         paste(site, "climate analysis-window identity mismatch."))
  assert(n_supported == length(realized) && is.finite(row$breeding_temp_c) &&
         same_num(row$breeding_temp_c, round(mean(values), 1)),
         paste(site, "breeding-season temperature is not supported by every realized count month."))
  expected_mat <- round(mean(env_temp[is.finite(env_temp)]), 1)
  expected_amplitude <- round(diff(range(expected_temp[is.finite(expected_temp)])), 1)
  peak_index <- if (any(is.finite(expected_greenup)))
    which.max(replace(expected_greenup, !is.finite(expected_greenup), -Inf)) else NA_integer_
  expected_peak <- if (is.na(peak_index)) NA_real_ else round(expected_greenup[[peak_index]])
  expected_peak_label <- if (is.na(peak_index)) NA_character_ else month_labels[[peak_index]]
  precip_groups <- split(which(is.finite(env_precip)), env_year[is.finite(env_precip)])
  complete_precip <- vapply(precip_groups, function(i) {
    observed <- env_month[i]
    length(observed) == 12L && !anyDuplicated(observed) && identical(sort(observed), 1:12)
  }, logical(1))
  annual_totals <- if (length(precip_groups))
    vapply(precip_groups[complete_precip], function(i) sum(env_precip[i]), numeric(1)) else numeric()
  expected_precip <- if (length(annual_totals)) round(mean(annual_totals)) else NA_real_
  assert(same_num(row$mat_c, expected_mat) &&
         same_num(row$temp_amp_c, expected_amplitude) &&
         same_num(row$peak_greenup_pct, expected_peak) &&
         same_num(row$greenup_peak_month, peak_index) &&
         same_chr(row$greenup_peak_lab, expected_peak_label) &&
         same_num(row$precip_annual_mm, expected_precip) &&
         same_num(row$n_precip_months, sum(is.finite(env_precip))) &&
         same_num(row$n_complete_precip_years, length(annual_totals)) &&
         same_num(row$count_month_min, min(realized)) &&
         same_num(row$count_month_max, max(realized)) &&
         same_num(row$env_year_min, min(env_year)) &&
         same_num(row$env_year_max, max(env_year)),
         paste(site, "site climate context does not independently reconstruct from environment and visit bundles."))
}

env_receipt <- jsonlite::fromJSON(file.path(DATA, "environment_source_receipt.json"), simplifyVector = FALSE)
expected_env_evidence_format <- paste(
  "canonical privacy-minimized selected-stream rows plus digest-bound",
  "all-stream month support; validation-only; not deployed"
)
assert(identical(names(env_receipt), c(
         "schema_version", "release", "window", "retrieval", "products",
         "validation_evidence", "files"
       )) && identical(as.integer(env_receipt$schema_version), 2L) &&
       identical(env_receipt$window$start_month, "2013-01") &&
       identical(env_receipt$window$end_month, "2024-12") &&
       identical(names(env_receipt$validation_evidence),
                 c("schema_version", "file_count", "total_bytes", "format")) &&
       identical(as.integer(env_receipt$validation_evidence$schema_version), 1L) &&
       identical(as.integer(env_receipt$validation_evidence$file_count), 47L) &&
       as.numeric(env_receipt$validation_evidence$total_bytes) > 0 &&
       identical(env_receipt$validation_evidence$format, expected_env_evidence_format),
       "Environment receipt schema/window/evidence contract mismatch.")
assert(identical(env_receipt$release, "RELEASE-2026"), "Environment receipt release mismatch.")
assert(identical(names(env_receipt$retrieval), c(
         "tool", "package", "neonUtilities_version", "r_version", "token_required"
       )) && identical(env_receipt$retrieval$tool, "neonUtilities::loadByProduct") &&
       identical(env_receipt$retrieval$package, "basic") &&
       nzchar(as.character(env_receipt$retrieval$neonUtilities_version)) &&
       identical(env_receipt$retrieval$r_version, "4.5.2") &&
       isTRUE(env_receipt$retrieval$token_required),
       "Environment receipt retrieval identity mismatch.")
assert(identical(env_receipt$products$air_temperature$doi, "10.48443/p69b-5e50") &&
       identical(env_receipt$products$precipitation$doi, "10.48443/v29j-eg88") &&
       identical(env_receipt$products$plant_phenology$doi, "10.48443/p75s-7p48"),
       "Environment receipt DOI mismatch.")
assert(as.integer(env_receipt$products$air_temperature$supported_sites) == 47L &&
       as.integer(env_receipt$products$precipitation$supported_sites) == 20L &&
       as.integer(env_receipt$products$plant_phenology$supported_sites) == 47L,
       "Environment receipt support counts mismatch.")
env_records <- env_receipt$files
assert(length(env_records) == 47L, "Environment receipt must contain 47 file records.")
env_record_sites <- vapply(env_records, function(record) as.character(record$site), character(1))
assert(!anyDuplicated(env_record_sites) && identical(sort(env_record_sites), expected_sites),
       "Environment receipt file records do not cover the exact unique 47-site roster.")
assert(identical(as.numeric(env_receipt$validation_evidence$total_bytes),
                 sum(vapply(env_records, function(record) {
                   as.numeric(record$evidence_bytes)
                 }, numeric(1)))),
       "Environment evidence total bytes do not equal the 47 file records.")
for (record in env_records) {
  site <- as.character(record$site)
  path <- file.path(ENV, paste0(site, ".rds"))
  assert(identical(names(record), c(
           "site", "rows", "month_min", "month_max", "air_temperature_supported",
           "precipitation_supported", "plant_phenology_supported", "source_tables",
           "stream_selection", "file", "sha256", "bytes", "evidence_file",
           "evidence_sha256", "evidence_bytes"
         )) && identical(as.character(record$file), paste0(site, ".rds")) &&
         file.exists(path) && identical(sha256(path), as.character(record$sha256)) &&
         identical(as.numeric(record$bytes), unname(file.info(path)$size)) &&
         identical(as.character(record$evidence_file), paste0(site, ".rds")) &&
         grepl("^[0-9a-f]{64}$", as.character(record$evidence_sha256)) &&
         is.finite(as.numeric(record$evidence_bytes)) &&
         as.numeric(record$evidence_bytes) > 0 &&
         as.numeric(record$evidence_bytes) == as.integer(record$evidence_bytes),
         paste(site, "environment public/evidence file receipt mismatch."))
  assert(!is.null(record$stream_selection$air_temperature) &&
         identical(as.character(record$stream_selection$air_temperature$rule),
                   paste(
                     "minimum finite recorded verticalPosition (ordering metadata, not metres)",
                     "max supported months", "max finalQF-passing finite values",
                     "lexical signature", sep = "; ")) &&
         identical(as.character(record$stream_selection$air_temperature$vertical_position_rule),
                   "lowest_recorded") &&
         is.finite(as.numeric(record$stream_selection$air_temperature$selected_vertical_position)) &&
         grepl("NEON.DOC.000646",
               as.character(record$stream_selection$air_temperature$vertical_position_basis),
               fixed = TRUE) &&
         nzchar(as.character(record$stream_selection$air_temperature$quality_flag_column)),
         paste(site, "air-temperature stream/QF selection is not auditable."))
  temperature_coverage <- record$stream_selection$air_temperature$monthly_coverage
  assert(!is.null(temperature_coverage) &&
         as.integer(temperature_coverage$interval_minutes) == 30L &&
         as.integer(temperature_coverage$expected_intervals_per_day) == 48L &&
         identical(as.numeric(temperature_coverage$minimum_interval_coverage), 0.75) &&
         grepl("finalQF == 0", as.character(temperature_coverage$quality_gate), fixed = TRUE),
         paste(site, "air-temperature monthly coverage rule is not auditable."))
  if (isTRUE(record$precipitation_supported))
    assert(!is.null(record$stream_selection$precipitation) &&
           nzchar(as.character(record$stream_selection$precipitation$quality_flag_column)),
           paste(site, "precipitation stream/QF selection is not auditable."))
  env <- as.data.frame(readRDS(path))
  assert(all(c("siteID", "ym") %in% names(env)),
         paste(site, "environment bundle lacks site/month keys."))
  env_months <- as.character(env$ym)
  assert(nrow(env) > 0L && identical(nrow(env), as.integer(record$rows)) &&
         all(!blank(env$siteID)) && all(as.character(env$siteID) == site),
         paste(site, "environment siteID or row-count receipt mismatch."))
  assert(!anyNA(env_months) && all(grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", env_months)) &&
         !anyDuplicated(env_months) && identical(env_months, sort(env_months, method = "radix")) &&
         identical(min(env_months), as.character(record$month_min)) &&
         identical(max(env_months), as.character(record$month_max)),
         paste(site, "environment month keys or receipt bounds mismatch."))
  required_temperature <- c(
    "temp_c", "temp_min", "temp_max", "temp_n", "temp_expected",
    "temp_coverage_pct", "temp_days", "temp_expected_days", "temp_min_n", "temp_max_n"
  )
  assert(all(required_temperature %in% names(env)) && any(is.finite(env$temp_c)),
         paste(site, "air-temperature context or coverage evidence is missing."))
  temperature_threshold <- ceiling(env$temp_expected * 0.75)
  finite_mean <- is.finite(env$temp_c)
  finite_min <- is.finite(env$temp_min)
  finite_max <- is.finite(env$temp_max)
  assert(all(env$temp_n[finite_mean] >= temperature_threshold[finite_mean] &
             env$temp_n[finite_mean] <= env$temp_expected[finite_mean] &
             env$temp_coverage_pct[finite_mean] >= 75 &
             env$temp_coverage_pct[finite_mean] <= 100) &&
         all(env$temp_min_n[finite_min] >= temperature_threshold[finite_min] &
             env$temp_min_n[finite_min] <= env$temp_expected[finite_min]) &&
         all(env$temp_max_n[finite_max] >= temperature_threshold[finite_max] &
             env$temp_max_n[finite_max] <= env$temp_expected[finite_max]),
         paste(site, "an under-covered monthly temperature summary is reportable."))
  if ("precip_mm" %in% names(env) && any(is.finite(env$precip_mm))) {
    required_precip <- c("precip_n", "precip_expected", "precip_coverage_pct")
    assert(all(required_precip %in% names(env)), paste(site, "precipitation lacks coverage channels."))
    complete <- is.finite(env$precip_mm)
    assert(all(env$precip_n[complete] == env$precip_expected[complete]) &&
           all(env$precip_coverage_pct[complete] == 100),
           paste(site, "a partial month carries a precipitation total."))
  }
}

# Re-derive the exact deploy-payload identity without invoking the stamp writer.
# This binds code, derived bytes, runtime assets, the public poster family, and
# the canonical non-file Connect manifest contract to the two acquisition
# receipts. The stamp, its Pages copy, and manifest file/checksum map are outside
# the byte payload to avoid a hash cycle.
stamp_path <- file.path(DATA, "release_stamp.json")
pages_stamp_path <- file.path(ROOT, "docs", "release.json")
manifest_path <- file.path(ROOT, "manifest.json")
stamp <- jsonlite::fromJSON(stamp_path, simplifyVector = FALSE)
stamp_fields <- c(
  "schema_version", "app_id", "product", "release", "doi",
  "source_receipt_sha256", "environment_receipt_sha256", "payload_sha256",
  "manifest_contract_sha256", "release_id"
)
assert(identical(names(stamp), stamp_fields), "Release stamp schema or field order mismatch.")
assert(identical(as.integer(stamp$schema_version), 3L) &&
       identical(stamp$app_id, "NEON-Breeding-Birds") &&
       identical(stamp$product, "DP1.10003.001") &&
       identical(stamp$release, "RELEASE-2026") &&
       identical(stamp$doi, "10.48443/v6hs-mx57"),
       "Release stamp app/source identity mismatch.")
env_receipt_sha256 <- sha256(file.path(DATA, "environment_source_receipt.json"))
assert(identical(as.character(stamp$source_receipt_sha256), receipt_sha256) &&
       identical(as.character(stamp$environment_receipt_sha256), env_receipt_sha256),
       "Release stamp is not bound to the exact acquisition receipts.")

release_files_below <- function(directory, pattern = NULL, recursive = TRUE) {
  base <- file.path(ROOT, directory)
  assert(dir.exists(base), paste("Release payload directory is missing:", directory))
  inside <- list.files(
    base, pattern = pattern, recursive = recursive, full.names = FALSE,
    all.files = FALSE, include.dirs = FALSE, no.. = TRUE
  )
  file.path(directory, inside)
}
payload_paths <- c(
  "global.R", "ui.R", "server.R",
  release_files_below("R", pattern = "[.]R$", recursive = FALSE),
  release_files_below("www"),
  release_files_below("data"),
  release_files_below("data-sample"),
  "docs/index.html", "docs/og-image-v2.png",
  release_files_below("docs/assets")
)
payload_paths <- gsub("\\\\", "/", payload_paths)
payload_paths <- setdiff(payload_paths, c("data/release_stamp.json", "docs/release.json", "manifest.json"))
payload_paths <- sort(unique(payload_paths), method = "radix")
unsafe_payload_path <- grepl("\r", payload_paths, fixed = TRUE) |
  grepl("\n", payload_paths, fixed = TRUE) |
  grepl("\t", payload_paths, fixed = TRUE)
assert(length(payload_paths) > 0L && !any(unsafe_payload_path),
       "Release payload paths are empty or contain an unsafe separator.")
payload_absolute <- file.path(ROOT, payload_paths)
assert(all(file.exists(payload_absolute)) && !any(dir.exists(payload_absolute)) &&
       !any(is.na(file.info(payload_absolute)$size)),
       "Release payload is missing one or more files.")
assert(!any(nzchar(Sys.readlink(payload_absolute))),
       "Release payload must not contain symbolic links.")
payload_entries <- paste(
  payload_paths,
  vapply(payload_absolute, sha256, character(1)),
  sep = "\t"
)
payload_material <- paste0(paste(payload_entries, collapse = "\n"), "\n")
expected_payload_sha256 <- digest::digest(
  payload_material, algo = "sha256", serialize = FALSE
)
assert(identical(as.character(stamp$payload_sha256), expected_payload_sha256),
       "Release stamp does not bind the exact deployed code/data/asset payload.")

assert(file.exists(manifest_path) && file.info(manifest_path)$size > 0,
       "Final manifest is missing from the exact release candidate.")
release_manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest_fields <- c("version", "locale", "platform", "metadata", "packages", "files", "users")
assert(length(names(release_manifest)) == length(manifest_fields) &&
       setequal(names(release_manifest), manifest_fields) &&
       is.list(release_manifest$packages) && length(release_manifest$packages) > 0L,
       "Final manifest lacks the exact runtime/dependency contract fields.")
canonical_manifest_value <- function(value) {
  if (!is.list(value)) return(value)
  keys <- names(value)
  if (is.null(keys)) return(lapply(value, canonical_manifest_value))
  order_index <- order(keys, method = "radix")
  output <- lapply(value[order_index], canonical_manifest_value)
  names(output) <- keys[order_index]
  output
}
manifest_contract <- list(
  version = release_manifest$version,
  locale = release_manifest$locale,
  platform = release_manifest$platform,
  metadata = canonical_manifest_value(release_manifest$metadata),
  packages = canonical_manifest_value(release_manifest$packages),
  users = canonical_manifest_value(release_manifest$users)
)
manifest_contract_json <- as.character(jsonlite::toJSON(
  manifest_contract, auto_unbox = TRUE, pretty = FALSE, null = "null", na = "null"
))
expected_manifest_contract_sha256 <- digest::digest(
  paste("neon-connect-manifest-contract-v1", manifest_contract_json, sep = "\n"),
  algo = "sha256", serialize = FALSE
)
assert(identical(as.character(stamp$manifest_contract_sha256),
                 expected_manifest_contract_sha256),
       "Release stamp does not bind the final Connect runtime/dependency contract.")

stamp_material <- paste(
  "neon-breeding-birds-release-instance-v3", "NEON-Breeding-Birds",
  "DP1.10003.001", "RELEASE-2026", "10.48443/v6hs-mx57",
  receipt_sha256, env_receipt_sha256, expected_payload_sha256,
  expected_manifest_contract_sha256,
  sep = "\n"
)
expected_release_id <- paste0(
  "sha256:",
  digest::digest(stamp_material, algo = "sha256", serialize = FALSE)
)
assert(identical(as.character(stamp$release_id), expected_release_id),
       "Release ID does not independently re-derive from the exact payload.")
expected_stamp <- list(
  schema_version = 3L,
  app_id = "NEON-Breeding-Birds",
  product = "DP1.10003.001",
  release = "RELEASE-2026",
  doi = "10.48443/v6hs-mx57",
  source_receipt_sha256 = receipt_sha256,
  environment_receipt_sha256 = env_receipt_sha256,
  payload_sha256 = expected_payload_sha256,
  manifest_contract_sha256 = expected_manifest_contract_sha256,
  release_id = expected_release_id
)
canonical_stamp <- paste0(
  jsonlite::toJSON(expected_stamp, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  "\n"
)
stamp_size <- file.info(stamp_path)$size
stamp_connection <- file(stamp_path, open = "rb")
stamp_bytes <- readBin(stamp_connection, what = "raw", n = stamp_size)
close(stamp_connection)
assert(identical(rawToChar(stamp_bytes), canonical_stamp),
       "Release stamp bytes are not the canonical deterministic serialization.")
assert(file.exists(pages_stamp_path) && identical(sha256(pages_stamp_path), sha256(stamp_path)),
       "docs/release.json is missing or not byte-identical to data/release_stamp.json.")

cat(sprintf(paste0(
  "OK: verified opportunity-complete %s bundles for 47 sites: ",
  "%s valid physical counts (%s supported-zero counts) and ",
  "%s supported point-years (%s supported-zero point-years).\n"),
  "RELEASE-2026", format(sum(index$n_visits), big.mark = ","),
  format(sum(index$n_supported_zero_counts), big.mark = ","),
  format(sum(index$n_opportunities), big.mark = ","),
  format(sum(index$n_supported_zero), big.mark = ",")))
