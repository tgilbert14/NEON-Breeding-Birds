#!/usr/bin/env Rscript
# Independent raw-to-bundle oracle for DP1.10003.001 RELEASE-2026.
#
# This validator runs in a clean job against the producer's receipt-bound,
# privacy-safe evidence projection. It intentionally does not source the bundler
# or bird_helpers.R. Every physical visit, detection row, eligibility decision,
# and point-year outcome is re-derived from brd_perpoint/brd_countdata before a
# candidate can be published. Observer identities never cross this boundary, so
# their published aggregate attestations are checked structurally and against
# evidence-derived upper bounds rather than recomputed by identity.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })
source("R/site_metadata.R")
source("R/bird_evidence_contract.R")

RAW_DIR <- Sys.getenv("BIRD_RAW_DIR", "raw/birds")
ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
RECEIPT_PATH <- file.path(ROOT, "data", "source_receipt.json")
SITE_DIR <- file.path(ROOT, "data", "sites")

fail <- function(...) stop(paste0(...), call. = FALSE)
assert <- function(ok, ...) if (!isTRUE(ok)) fail(...)
blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
same_chr <- function(a, b) identical(ifelse(is.na(a), "<NA>", as.character(a)),
                                     ifelse(is.na(b), "<NA>", as.character(b)))
same_num <- function(a, b, tolerance = 1e-9) isTRUE(all.equal(
  as.numeric(a), as.numeric(b), tolerance = tolerance, check.attributes = FALSE))
survey_key <- function(site, event, plot, point) paste(site, event, plot, point, sep = "|")
point_key <- function(plot, point) paste(plot, point, sep = "_")
occasion_key <- function(site, plot, point, year) paste(site, plot, point, year, sep = "|")
year_from <- function(start_date, event_id) {
  year <- suppressWarnings(as.integer(substr(as.character(start_date), 1, 4)))
  bad <- !is.finite(year)
  if (any(bad)) {
    event <- as.character(event_id[bad])
    match <- regexpr("20[0-9]{2}", event)
    hit <- rep(NA_character_, length(event))
    has <- match > 0L
    hit[has] <- substr(event[has], match[has], match[has] + 3L)
    year[bad] <- suppressWarnings(as.integer(hit))
  }
  year
}
canonical_table <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (name in names(out)) {
    value <- out[[name]]
    if (inherits(value, "POSIXt")) value <- format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    else if (inherits(value, "Date")) value <- format(value, "%Y-%m-%d")
    else if (is.factor(value)) value <- as.character(value)
    attributes(value) <- attributes(value)[intersect(names(attributes(value)), c("names", "dim", "dimnames"))]
    out[[name]] <- value
  }
  out <- out[, sort(names(out)), drop = FALSE]
  if (nrow(out) > 1L && ncol(out)) {
    unsupported <- !vapply(out, is.atomic, logical(1))
    if (any(unsupported)) fail("Raw table contains non-atomic columns: ", paste(names(out)[unsupported], collapse = ", "))
    ord <- do.call(order, c(unname(out), list(na.last = TRUE, method = "radix")))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
required <- function(d, columns, label) {
  missing <- setdiff(columns, names(d))
  if (length(missing)) fail(label, " missing: ", paste(missing, collapse = ", "))
}
col_chr <- function(d, name, default = NA_character_) {
  if (name %in% names(d)) as.character(d[[name]]) else rep(default, nrow(d))
}
col_num <- function(d, name) suppressWarnings(as.numeric(col_chr(d, name)))
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
distance_state <- function(raw) {
  token <- as.character(raw)
  value <- suppressWarnings(as.numeric(token))
  empty <- blank(token)
  sentinel <- !empty & is.finite(value) & value %in% c(999, 9999)
  observed <- !empty & is.finite(value) & value >= 0 & !sentinel
  list(
    raw = token,
    value = ifelse(observed, value, NA_real_),
    state = ifelse(empty, "source_missing", ifelse(sentinel, "sentinel_not_estimable",
      ifelse(observed, "observed", "invalid")))
  )
}
point_count_minute_state <- function(raw) {
  token <- as.character(raw)
  empty <- blank(token)
  value <- suppressWarnings(as.numeric(token))
  integer_value <- suppressWarnings(as.integer(value))
  whole <- !empty & is.finite(value) & !is.na(integer_value) & value == integer_value
  minute <- ifelse(whole, integer_value, NA_integer_)
  in_window <- whole & minute %in% 1:6
  list(
    raw = token,
    value = minute,
    state = as.character(ifelse(empty, "source_missing",
      ifelse(in_window, "standard_minute",
        ifelse(whole & minute == 88L, "incidental_minute_88", "invalid_or_unknown")))),
    in_window = in_window
  )
}
mode_count <- function(x) length(unique(x[!blank(x)]))
clean_state <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  out <- tolower(gsub("[^A-Z0-9]+", "_", raw))
  out[is.na(raw) | !nzchar(raw)] <- "source_missing"
  out
}
method_channel <- function(x) {
  method <- tolower(trimws(as.character(x)))
  ifelse(blank(method) | method == "unknown", "unknown",
    ifelse(grepl("sing", method), "singing",
      ifelse(grepl("drum", method), "drumming",
        ifelse(grepl("call", method), "calling",
          ifelse(grepl("visual", method), "visual", "other")))))
}
column_sort_token <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), "0:<NA>", ifelse(x, "2:TRUE", "1:FALSE")))
  if (is.numeric(x)) return(ifelse(is.na(x), "0:<NA>", paste0("1:", sprintf("%.17g", x))))
  value <- as.character(x)
  ifelse(is.na(value), "0:<NA>", paste0("1:", nchar(value), ":", value))
}
canonical_public_rows <- function(d, columns, label) {
  required(d, columns, label)
  out <- d[, columns, drop = FALSE]
  if (nrow(out) > 1L) {
    order_args <- lapply(out, column_sort_token)
    out <- out[do.call(order, c(order_args, list(method = "radix"))), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
assert_public_rows <- function(actual, expected, columns, label) {
  actual <- canonical_public_rows(actual, columns, paste0(label, " actual"))
  expected <- canonical_public_rows(expected, columns, paste0(label, " expected"))
  assert(nrow(actual) == nrow(expected), label, " row count differs from raw evidence.")
  for (name in columns) {
    ok <- if (is.logical(expected[[name]])) {
      identical(as.logical(actual[[name]]), as.logical(expected[[name]]))
    } else if (is.numeric(expected[[name]])) {
      same_num(actual[[name]], expected[[name]])
    } else {
      same_chr(actual[[name]], expected[[name]])
    }
    assert(ok, label, " differs from raw evidence in ", name, ".")
  }
  invisible(TRUE)
}

expected_sites <- sort(as.character(neon_sites$site))
raw_files <- list.files(RAW_DIR, pattern = "^[A-Z]{4}_raw[.]rds$", full.names = TRUE)
raw_sites <- sort(sub("_raw[.]rds$", "", basename(raw_files)))
assert(identical(raw_sites, expected_sites), "Raw evidence roster is not the exact 47-site release roster.")
assert(file.exists(RECEIPT_PATH), "Missing deterministic source receipt.")
receipt <- jsonlite::fromJSON(RECEIPT_PATH, simplifyVector = TRUE)
assert(identical(as.integer(receipt$schema_version), 3L) &&
       identical(receipt$product, "DP1.10003.001") &&
       identical(receipt$release, "RELEASE-2026") &&
       identical(receipt$doi, "10.48443/v6hs-mx57"), "Source receipt identity mismatch.")
bird_assert_receipt_evidence_contract(receipt)
records <- bird_receipt_records(receipt)
assert(identical(sort(as.character(receipt$fetched_sites), method = "radix"), expected_sites) &&
       identical(sort(as.character(records$site), method = "radix"), expected_sites),
       "Source receipt roster is not the exact 47-site release roster.")
bird_scan_evidence_directory(RAW_DIR, expected_sites, receipt)

visit_required <- c("siteID", "plotID", "pointID", "eventID", "boutNumber",
                    "startDate", "samplingImpractical", "release")
detection_required <- c("siteID", "plotID", "pointID", "eventID", "boutNumber",
                        "startDate", "taxonRank", "scientificName", "clusterSize",
                        "detectionMethod", "observerDistance", "pointCountMinute", "release")

for (site in expected_sites) {
  raw_path <- file.path(RAW_DIR, paste0(site, "_raw.rds"))
  raw <- readRDS(raw_path)
  bird_assert_evidence_projection(raw, paste0(site, " cross-job bird evidence"))
  pp <- canonical_table(raw$brd_perpoint)
  cd <- canonical_table(raw$brd_countdata)
  required(pp, visit_required, paste0(site, " brd_perpoint"))
  required(cd, detection_required, paste0(site, " brd_countdata"))
  record_index <- match(site, as.character(records$site))
  assert(!is.na(record_index), site, ": source receipt record missing.")
  assert(identical(bird_evidence_projection_sha256(raw),
                   as.character(records$evidence_projection_sha256[[record_index]])),
         site, ": evidence projection digest does not match receipt.")
  assert(nrow(pp) == as.integer(records$brd_perpoint_rows[[record_index]]) &&
         nrow(cd) == as.integer(records$brd_countdata_rows[[record_index]]),
         site, ": evidence row receipt mismatch.")

  pp_site <- as.character(pp$siteID); pp_event <- as.character(pp$eventID)
  pp_plot <- as.character(pp$plotID); pp_point <- as.character(pp$pointID)
  assert(!any(blank(pp_site) | blank(pp_event) | blank(pp_plot) | blank(pp_point) |
              blank(pp$startDate)), site, ": visit key contains missing fields.")
  assert(all(!blank(pp$release) & as.character(pp$release) == "RELEASE-2026") &&
         (!nrow(cd) || all(!blank(cd$release) & as.character(cd$release) == "RELEASE-2026")),
         site, ": raw tables contain a missing or unexpected release tag.")
  pp_year <- year_from(pp$startDate, pp_event)
  assert(!anyNA(pp_year) && all(pp_year %in% 2013:2024), site, ": visit year is outside RELEASE-2026.")
  pp_date <- suppressWarnings(as.Date(substr(as.character(pp$startDate), 1, 10)))
  assert(!anyNA(pp_date) && all(pp_date >= as.Date("2013-06-01") & pp_date <= as.Date("2024-07-31")),
         site, ": visit date is outside the published release window.")
  pp_bout_num <- suppressWarnings(as.numeric(pp$boutNumber))
  assert(!any(!is.finite(pp_bout_num) | pp_bout_num != as.integer(pp_bout_num) |
              !(as.integer(pp_bout_num) %in% 1:2)), site, ": visit bout is outside 1/2.")
  pp_bout <- as.character(as.integer(pp_bout_num))
  assert(all(pp_site == site), site, ": raw visit table contains another site.")
  pp_survey <- survey_key(pp_site, pp_event, pp_plot, pp_point)
  pp_pointkey <- point_key(pp_plot, pp_point)
  pp_occasion <- occasion_key(pp_site, pp_plot, pp_point, pp_year)
  assert(!anyDuplicated(pp_survey), site, ": raw physical survey key is duplicated.")
  pp_valid <- toupper(trimws(as.character(pp$samplingImpractical))) == "OK"
  pp_valid[is.na(pp_valid)] <- FALSE
  valid_bout_key <- paste(pp_occasion[pp_valid], pp_bout[pp_valid], sep = "|")
  assert(!anyDuplicated(valid_bout_key), site, ": raw valid bouts duplicate a point-year label.")

  bundle <- readRDS(file.path(SITE_DIR, paste0(site, ".rds")))
  assert(identical(as.integer(bundle$meta$schema_version), 4L),
         site, ": bundle schema is not v4 physical-count incidence.")
  assert(identical(as.character(bundle$meta$full_source_content_sha256),
                   as.character(records$full_source_content_sha256[[record_index]])) &&
         identical(as.character(bundle$meta$evidence_projection_sha256),
                   as.character(records$evidence_projection_sha256[[record_index]])),
         site, ": bundle metadata does not bind both receipt digests.")
  visits <- as.data.frame(bundle$visits, stringsAsFactors = FALSE)
  obs <- as.data.frame(bundle$obs, stringsAsFactors = FALSE)
  held <- as.data.frame(bundle$held, stringsAsFactors = FALSE)
  opportunity <- as.data.frame(bundle$opportunity, stringsAsFactors = FALSE)
  points <- as.data.frame(bundle$points, stringsAsFactors = FALSE)
  public_tables <- list(visits = visits, obs = obs, held = held,
                        opportunity = opportunity, points = points)
  prohibited_public_columns <- c(
    "measuredBy", "observer_id", "uid", "visit_uid", "detection_uid", "source_row",
    "samplingImpracticalRemarks")
  for (table_name in names(public_tables)) {
    tab <- public_tables[[table_name]]
    leaked <- intersect(prohibited_public_columns, names(tab))
    assert(!length(leaked), site, ": public ", table_name,
           " table exposes prohibited raw/linkable field(s): ", paste(leaked, collapse = ", "), ".")
    char_columns <- names(tab)[vapply(tab, is.character, logical(1))]
    hashed <- if (length(char_columns)) any(vapply(tab[char_columns], function(value)
      any(grepl("^observer-sha256:[0-9a-f]{64}$", value), na.rm = TRUE), logical(1))) else FALSE
    assert(!hashed, site, ": public ", table_name, " table exposes row-level observer hashes.")
  }

  expected_visits <- data.frame(
    survey_id = pp_survey,
    occasion_id = pp_occasion,
    site = pp_site,
    pointkey = pp_pointkey,
    plotID = pp_plot,
    pointID = pp_point,
    eventID = pp_event,
    year = as.integer(pp_year),
    bout = pp_bout,
    startDate = as.character(pp$startDate),
    # RELEASE-2026 brd_perpoint has no endDate. The public schema retains an
    # explicit compatibility-null column and must not consume undeclared input.
    endDate = rep(NA_character_, nrow(pp)),
    samplingImpractical = as.character(pp$samplingImpractical),
    sampling_state = clean_state(pp$samplingImpractical),
    valid_count = pp_valid,
    protocol_minutes = ifelse(pp_valid, 6, 0),
    samplingProtocolVersion = col_chr(pp, "samplingProtocolVersion"),
    observedHabitat = col_chr(pp, "observedHabitat"),
    nlcdClass = col_chr(pp, "nlcdClass"),
    lat = col_num(pp, "decimalLatitude"),
    lng = col_num(pp, "decimalLongitude"),
    startCloudCoverPercentage = col_num(pp, "startCloudCoverPercentage"),
    endCloudCoverPercentage = col_num(pp, "endCloudCoverPercentage"),
    observedAirTemp = col_num(pp, "observedAirTemp"),
    kmPerHourObservedWindSpeed = col_num(pp, "kmPerHourObservedWindSpeed"),
    stringsAsFactors = FALSE, check.names = FALSE)
  cd_site <- as.character(cd$siteID); cd_event <- as.character(cd$eventID)
  cd_plot <- as.character(cd$plotID); cd_point <- as.character(cd$pointID)
  assert(!any(blank(cd_site) | blank(cd_event) | blank(cd_plot) | blank(cd_point)),
         site, ": detection survey key contains missing fields.")
  assert(all(cd_site == site), site, ": raw detection table contains another site.")
  cd_survey <- survey_key(cd_site, cd_event, cd_plot, cd_point)
  visit_match <- match(cd_survey, pp_survey)
  matched <- !is.na(visit_match)
  matched_valid <- matched
  matched_valid[matched] <- pp_valid[visit_match[matched]]
  cd_year <- year_from(cd$startDate, cd_event)
  assert(!anyNA(cd_year) && all(cd_year %in% 2013:2024), site, ": detection year is outside RELEASE-2026.")
  cd_date <- suppressWarnings(as.Date(substr(as.character(cd$startDate), 1, 10)))
  assert(!anyNA(cd_date) && all(cd_date >= as.Date("2013-06-01") & cd_date <= as.Date("2024-07-31")),
         site, ": detection date is outside the published release window.")
  cd_bout_raw <- as.character(cd$boutNumber)
  cd_bout_num <- suppressWarnings(as.numeric(cd_bout_raw))
  cd_bout <- ifelse(is.finite(cd_bout_num) & cd_bout_num == as.integer(cd_bout_num),
                    as.character(as.integer(cd_bout_num)), trimws(cd_bout_raw))
  det_site <- cd_site; det_plot <- cd_plot; det_point <- cd_point
  det_year <- cd_year; det_bout <- cd_bout
  det_site[matched] <- pp_site[visit_match[matched]]
  det_plot[matched] <- pp_plot[visit_match[matched]]
  det_point[matched] <- pp_point[visit_match[matched]]
  det_year[matched] <- pp_year[visit_match[matched]]
  det_bout[matched] <- pp_bout[visit_match[matched]]
  det_occasion <- occasion_key(det_site, det_plot, det_point, det_year)
  year_conflict <- matched & is.finite(cd_year) & cd_year != det_year
  bout_conflict <- matched & !blank(cd_bout) & cd_bout != det_bout
  assert(!any(year_conflict) && !any(bout_conflict), site, ": detection/visit year or bout conflict.")
  rank <- as.character(cd$taxonRank); scientific <- as.character(cd$scientificName)
  reported_taxon_id <- col_chr(cd, "taxonID")
  reported_vernacular <- col_chr(cd, "vernacularName")
  unit <- community_unit(rank, scientific)
  cluster <- suppressWarnings(as.numeric(cd$clusterSize))
  cluster_valid <- is.finite(cluster) & cluster > 0 & cluster == floor(cluster)
  method <- as.character(cd$detectionMethod)
  method_token <- tolower(trimws(method))
  method_known <- !blank(method_token) & method_token != "unknown"
  minute <- point_count_minute_state(cd$pointCountMinute)
  is_species <- unit$eligible
  is_flyover <- grepl("flyover", tolower(method)); is_flyover[is.na(is_flyover)] <- FALSE
  eligible <- matched_valid & minute$in_window & is_species & method_known &
    !is_flyover & cluster_valid
  reason <- ifelse(!matched, "orphan_detection_no_visit",
            ifelse(!matched_valid, "detection_on_unusable_visit",
              ifelse(minute$state == "incidental_minute_88", "incidental_outside_point_count",
                ifelse(minute$state == "source_missing", "missing_point_count_minute",
                  ifelse(!minute$in_window, "invalid_point_count_minute",
                    ifelse(!cluster_valid, "invalid_cluster_size",
                      ifelse(blank(scientific), "missing_taxon",
                        ifelse(tolower(trimws(rank)) %in% c("species", "subspecies") & !is_species,
                               "unsafe_species_canonicalization",
                          ifelse(!method_known,
                                 "missing_or_unknown_detection_method", NA_character_)))))))))

  expected_channel <- method_channel(method)
  component <- function(pattern) {
    out <- grepl(pattern, tolower(method)); out[is.na(out)] <- FALSE; out
  }
  distance <- distance_state(cd$observerDistance)
  expected_detection <- data.frame(
    survey_id = cd_survey,
    occasion_id = det_occasion,
    site = det_site,
    pointkey = point_key(det_plot, det_point),
    plotID = det_plot,
    pointID = det_point,
    eventID = cd_event,
    year = as.integer(det_year),
    bout = det_bout,
    detection_startDate_raw = as.character(cd$startDate),
    detection_year_raw = as.integer(cd_year),
    detection_bout_raw = cd_bout,
    detection_year_matches_visit = ifelse(matched & is.finite(cd_year), !year_conflict, NA),
    detection_bout_matches_visit = ifelse(matched & !blank(cd_bout), !bout_conflict, NA),
    taxonID = reported_taxon_id,
    scientificName = unit$name,
    vernacularName = reported_vernacular,
    taxonRank = rank,
    communityScientificName = unit$name,
    community_unit_state = unit$state,
    reportedTaxonID = reported_taxon_id,
    reportedScientificName = scientific,
    reportedVernacularName = reported_vernacular,
    reportedTaxonRank = rank,
    is_species = is_species,
    pointCountMinuteRaw = minute$raw,
    pointCountMinute = minute$value,
    point_count_minute_state = minute$state,
    in_protocol_window = minute$in_window,
    observerDistanceRaw = distance$raw,
    observerDistance = distance$value,
    distance_state = distance$state,
    detectionMethod = method,
    detection_method_state = ifelse(method_known, "reported", "missing_or_unknown"),
    method_channel = expected_channel,
    method_singing = component("sing"),
    method_calling = component("call"),
    method_visual = component("visual"),
    method_drumming = component("drum"),
    clusterSize = cluster,
    sexOrAge = col_chr(cd, "sexOrAge"),
    matched_visit = matched,
    valid_count = matched_valid,
    is_flyover = is_flyover,
    enters_breeding_metrics = eligible,
    hold_reason = reason,
    stringsAsFactors = FALSE, check.names = FALSE)
  assert(identical(names(obs), names(expected_detection)) &&
         identical(names(held), names(expected_detection)),
         site, ": public detection/held schema differs from the minimized contract.")
  combined <- rbind(obs, held)
  assert_public_rows(combined, expected_detection, names(expected_detection),
                     paste0(site, " combined detection ledger"))
  assert(nrow(obs) == sum(is.na(reason)) && nrow(held) == sum(!is.na(reason)),
         site, ": detection/held partition does not conserve raw rows.")

  fly_auditable <- is_flyover & matched_valid & minute$in_window & is_species &
    cluster_valid
  visit_eligible <- split(which(eligible), cd_survey[eligible])
  visit_fly <- split(which(fly_auditable), cd_survey[fly_auditable])
  summarize_visit <- function(groups, key, fun, default) {
    if (!length(groups) || !key %in% names(groups)) return(default)
    fun(groups[[key]])
  }
  expected_visits$eligible_detection_rows <- vapply(expected_visits$survey_id,
    function(key) summarize_visit(visit_eligible, key, length, 0L), integer(1))
  expected_visits$eligible_birds <- vapply(expected_visits$survey_id,
    function(key) summarize_visit(visit_eligible, key, function(i) sum(cluster[i]), 0), numeric(1))
  expected_visits$eligible_species <- vapply(expected_visits$survey_id,
    function(key) summarize_visit(visit_eligible, key,
      function(i) length(unique(unit$name[i])), 0L), integer(1))
  expected_visits$flyover_rows <- vapply(expected_visits$survey_id,
    function(key) summarize_visit(visit_fly, key, length, 0L), integer(1))
  expected_visits$flyover_birds <- vapply(expected_visits$survey_id,
    function(key) summarize_visit(visit_fly, key,
      function(i) sum(cluster[i], na.rm = TRUE), 0), numeric(1))
  expected_visits$support_state <- ifelse(pp_valid, "supported", "held")
  expected_visits$outcome <- ifelse(!pp_valid, "unavailable",
    ifelse(expected_visits$eligible_detection_rows > 0L, "positive", "supported_zero"))
  assert(identical(names(visits), names(expected_visits)),
         site, ": public visit schema differs from the minimized v4 contract.")
  assert_public_rows(visits, expected_visits, names(expected_visits),
                     paste0(site, " visit ledger"))
  assert(nrow(visits) == nrow(pp) && !anyDuplicated(visits$survey_id),
         site, ": visit ledger does not conserve raw rows.")
  vm <- match(pp_survey, visits$survey_id)
  assert(!anyNA(vm) && setequal(visits$survey_id, pp_survey), site, ": visit ledger keys differ from raw.")
  assert(same_chr(visits$site[vm], pp_site) && same_chr(visits$eventID[vm], pp_event) &&
         same_chr(visits$plotID[vm], pp_plot) && same_chr(visits$pointID[vm], pp_point) &&
         same_num(visits$year[vm], pp_year) && same_chr(visits$bout[vm], pp_bout) &&
         same_chr(visits$startDate[vm], pp$startDate) &&
         identical(visits$valid_count[vm] %in% TRUE, pp_valid),
         site, ": visit ledger fields do not independently re-derive from raw.")
  assert(sum(visits$eligible_detection_rows) == sum(eligible) &&
         same_num(sum(visits$eligible_birds), sum(cluster[eligible], na.rm = TRUE)) &&
         sum(visits$flyover_rows) == sum(fly_auditable) &&
         same_num(sum(visits$flyover_birds), sum(cluster[fly_auditable], na.rm = TRUE)),
         site, ": visit-level detection conservation failed.")
  assert(identical(as.integer(bundle$meta$n_visits), as.integer(sum(pp_valid))) &&
         identical(as.integer(bundle$meta$n_supported_zero_counts),
                   as.integer(sum(expected_visits$outcome == "supported_zero"))),
         site, ": metadata valid-count / supported-zero-count totals do not re-derive from raw.")

  split_visits <- split(seq_len(nrow(pp)), pp_occasion)
  expected_opp <- data.frame(
    occasion_id = names(split_visits),
    n_valid_bouts = vapply(split_visits, function(i) sum(pp_valid[i]), integer(1)),
    n_held_bouts = vapply(split_visits, function(i) sum(!pp_valid[i]), integer(1)),
    stringsAsFactors = FALSE)
  expected_opp$n_bouts_recorded <- expected_opp$n_valid_bouts + expected_opp$n_held_bouts
  expected_opp$supported <- expected_opp$n_valid_bouts > 0L
  eligible_rows <- split(which(eligible), det_occasion[eligible])
  fly_rows <- split(which(fly_auditable), det_occasion[fly_auditable])
  summarize_rows <- function(groups, key, fun, default) {
    if (!length(groups) || !key %in% names(groups)) return(default)
    fun(groups[[key]])
  }
  expected_opp$eligible_detection_rows <- vapply(expected_opp$occasion_id,
    function(key) summarize_rows(eligible_rows, key, length, 0L), integer(1))
  expected_opp$eligible_birds <- vapply(expected_opp$occasion_id,
    function(key) summarize_rows(eligible_rows, key, function(i) sum(cluster[i]), 0), numeric(1))
  expected_opp$eligible_species <- vapply(expected_opp$occasion_id,
    function(key) summarize_rows(eligible_rows, key, function(i) mode_count(unit$name[i]), 0L), integer(1))
  expected_opp$flyover_rows <- vapply(expected_opp$occasion_id,
    function(key) summarize_rows(fly_rows, key, length, 0L), integer(1))
  expected_opp$flyover_birds <- vapply(expected_opp$occasion_id,
    function(key) summarize_rows(fly_rows, key, function(i) sum(cluster[i], na.rm = TRUE), 0), numeric(1))
  expected_opp$outcome <- ifelse(!expected_opp$supported, "unavailable",
    ifelse(expected_opp$eligible_detection_rows > 0L, "positive", "supported_zero"))
  om <- match(expected_opp$occasion_id, opportunity$occasion_id)
  assert(nrow(opportunity) == nrow(expected_opp) && !anyNA(om) &&
         setequal(opportunity$occasion_id, expected_opp$occasion_id),
         site, ": point-year opportunity universe differs from raw visits.")
  for (name in c("n_valid_bouts", "n_held_bouts", "n_bouts_recorded",
                 "eligible_detection_rows", "eligible_birds", "eligible_species",
                 "flyover_rows", "flyover_birds"))
    assert(same_num(opportunity[[name]][om], expected_opp[[name]]),
           site, ": per-occasion conservation mismatch for ", name, ".")
  assert(identical(opportunity$supported[om] %in% TRUE, expected_opp$supported) &&
         same_chr(opportunity$outcome[om], expected_opp$outcome),
         site, ": point-year support/outcome mismatch.")
  assert(!"n_observers" %in% names(opportunity),
         site, ": observer support is more granular than the public site/species contract.")

  observer_species <- sort(unique(unit$name[eligible]))
  observer_support <- bundle$meta$observer_support
  assert(is.list(observer_support) && identical(names(observer_support), c(
    "n_observers", "n_valid_visits", "n_valid_visits_with_observer", "complete", "by_species")),
    site, ": observer support is not the minimized site/species aggregate contract.")
  observer_integer <- function(value) {
    number <- suppressWarnings(as.numeric(value))
    length(number) == 1L && is.finite(number) && number >= 0 &&
      number == as.integer(number)
  }
  assert(observer_integer(observer_support$n_observers) &&
         observer_integer(observer_support$n_valid_visits) &&
         observer_integer(observer_support$n_valid_visits_with_observer),
         site, ": observer support contains a non-negative-integer violation.")
  n_observers <- as.integer(observer_support$n_observers)
  n_valid_visits <- as.integer(observer_support$n_valid_visits)
  n_with_observer <- as.integer(observer_support$n_valid_visits_with_observer)
  assert(identical(n_valid_visits, as.integer(sum(pp_valid))) &&
         n_with_observer <= n_valid_visits && n_observers <= n_with_observer &&
         ((n_observers == 0L) == (n_with_observer == 0L)),
         site, ": site observer aggregates violate evidence-derived bounds.")
  expected_complete <- n_valid_visits > 0L && n_with_observer == n_valid_visits
  assert(length(observer_support$complete) == 1L &&
         !is.na(observer_support$complete) &&
         identical(as.logical(observer_support$complete), expected_complete),
         site, ": observer completeness is inconsistent with aggregate visit counts.")
  actual_observer_by_species <- as.data.frame(observer_support$by_species,
                                               stringsAsFactors = FALSE)
  assert(identical(names(actual_observer_by_species), c("scientificName", "n_observers")),
         site, ": species observer support has an unexpected public schema.")
  assert(nrow(actual_observer_by_species) == length(observer_species) &&
         !anyDuplicated(as.character(actual_observer_by_species$scientificName)) &&
         identical(as.character(actual_observer_by_species$scientificName), observer_species),
         site, ": species observer support universe/order differs from eligible detections.")
  species_observers <- suppressWarnings(as.numeric(actual_observer_by_species$n_observers))
  assert(length(species_observers) == length(observer_species) &&
         all(is.finite(species_observers) & species_observers >= 0 &
             species_observers == as.integer(species_observers)),
         site, ": species observer support contains a non-negative-integer violation.")
  max_species_observers <- vapply(observer_species, function(taxon) {
    rows <- which(eligible & unit$name == taxon)
    length(unique(pp_survey[visit_match[rows]]))
  }, integer(1))
  assert(all(species_observers <= n_observers &
             species_observers <= max_species_observers),
         site, ": species observer aggregates exceed site or survey-derived bounds.")
  if (expected_complete && length(species_observers))
    assert(all(species_observers >= 1L),
           site, ": complete observer coverage cannot report zero support for a detected species.")
  assert(!any(c("measuredBy", "observer_id", "observer_hash", "observer_pseudonym") %in%
              names(bundle$meta)),
         site, ": bundle metadata exposes a row-level observer channel.")

  expected_point_visits <- tapply(pp_valid, pp_pointkey, sum)
  pm <- match(names(expected_point_visits), points$pointkey)
  assert(nrow(points) == length(expected_point_visits) && !anyNA(pm) &&
         setequal(points$pointkey, names(expected_point_visits)) &&
         same_num(points$n_visits[pm], expected_point_visits),
         site, ": point effort does not conserve raw valid visits.")
}

cat(sprintf("OK: independently reconciled every raw visit and detection into 47 %s bundles.\n",
            "RELEASE-2026"))
