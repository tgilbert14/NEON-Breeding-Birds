# Fetch the immutable NEON breeding-landbird release into an EMPTY staging area.
# This script is a read-only producer: it never writes committed app data.
#
# Required environment:
#   NEON_TOKEN    API token (release builds fail closed without it)
# Optional environment:
#   BIRD_RAW_DIR  empty output directory (default: build/raw/birds)
#   BIRD_RECEIPT  receipt path (default: build/source_receipt.json)
#
# Run from the repository root with the pinned release toolchain documented in
# docs/BUILD-TEST-HANDOFF.md.

options(timeout = 3600)
suppressPackageStartupMessages({
  library(neonUtilities)
  library(jsonlite)
  library(digest)
})

PRODUCT <- "DP1.10003.001"
RELEASE <- "RELEASE-2026"
DOI <- "10.48443/v6hs-mx57"
RELEASE_GENERATED <- "2026-01-23T00:07:49Z"
RAW_DIR <- Sys.getenv("BIRD_RAW_DIR", "build/raw/birds")
RECEIPT <- Sys.getenv("BIRD_RECEIPT", "build/source_receipt.json")

source("R/site_metadata.R")
source("R/bird_evidence_contract.R")
expected_sites <- sort(as.character(neon_sites$site))
if (length(expected_sites) != 47L || !identical(expected_sites, sort(unique(expected_sites))) ||
    !"PUUM" %in% expected_sites) {
  stop("Canonical release roster must contain 47 unique sites including PUUM.", call. = FALSE)
}

token <- trimws(Sys.getenv("NEON_TOKEN", ""))
if (!nzchar(token)) {
  stop("NEON_TOKEN is required for a release fetch; refusing an anonymous or partial build.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  if (!identical(Sys.getenv("BIRD_ALLOW_SUBSET", "0"), "1")) {
    stop("Site subsets are disabled for release builds. Set BIRD_ALLOW_SUBSET=1 only for local diagnostics.", call. = FALSE)
  }
  unknown <- setdiff(args, expected_sites)
  if (length(unknown)) stop("Unknown site code(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  sites <- sort(unique(args))
} else {
  sites <- expected_sites
}

if (dir.exists(RAW_DIR)) {
  existing <- list.files(RAW_DIR, all.files = TRUE, no.. = TRUE)
  if (length(existing)) {
    stop("BIRD_RAW_DIR must be empty; found existing entries in ", RAW_DIR,
         ". Use a fresh staging directory to prevent mixed releases.", call. = FALSE)
  }
} else {
  dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
}
dir.create(dirname(RECEIPT), recursive = TRUE, showWarnings = FALSE)

table_rows <- function(x, name) if (!is.null(x[[name]]) && is.data.frame(x[[name]])) nrow(x[[name]]) else 0L
assert_complete <- function(d, columns, label, site) {
  missing <- setdiff(columns, names(d))
  if (length(missing))
    stop(sprintf("%s %s lacks required RELEASE-2026 field(s): %s", site, label,
                 paste(missing, collapse = ", ")), call. = FALSE)
}
assert_nonblank <- function(d, columns, label, site) {
  bad <- vapply(columns, function(name) {
    value <- trimws(as.character(d[[name]]))
    any(is.na(value) | !nzchar(value))
  }, logical(1))
  if (any(bad))
    stop(sprintf("%s %s contains missing/blank key field(s): %s", site, label,
                 paste(columns[bad], collapse = ", ")), call. = FALSE)
}

cat(sprintf("Fetching %s %s for %d sites into %s\n", PRODUCT, RELEASE, length(sites), RAW_DIR))
records <- vector("list", length(sites))
names(records) <- sites

for (site in sites) {
  cat(sprintf("- %s\n", site)); flush.console()
  result <- tryCatch(
    neonUtilities::loadByProduct(
      dpID = PRODUCT,
      site = site,
      package = "basic",
      release = RELEASE,
      check.size = FALSE,
      token = token
    ),
    error = function(e) stop(sprintf("Fetch failed for %s: %s", site, conditionMessage(e)), call. = FALSE)
  )

  required <- c("brd_perpoint", "brd_countdata")
  missing_tables <- required[!vapply(required, function(nm) is.data.frame(result[[nm]]), logical(1))]
  if (length(missing_tables)) {
    stop(sprintf("%s is missing required table(s): %s", site, paste(missing_tables, collapse = ", ")), call. = FALSE)
  }
  if (!nrow(result$brd_perpoint)) stop(site, " has no brd_perpoint rows in ", RELEASE, call. = FALSE)
  visit_required <- c("siteID", "plotID", "pointID", "eventID", "boutNumber",
                      "startDate", "samplingImpractical", "measuredBy", "release")
  detection_required <- c("siteID", "plotID", "pointID", "eventID", "boutNumber",
                          "startDate", "taxonRank", "scientificName", "clusterSize",
                          "detectionMethod", "observerDistance", "pointCountMinute", "release")
  assert_complete(result$brd_perpoint, visit_required, "brd_perpoint", site)
  assert_complete(result$brd_countdata, detection_required, "brd_countdata", site)
  for (table_name in names(BIRD_EVIDENCE_TABLE_COLUMNS))
    assert_complete(result[[table_name]], BIRD_EVIDENCE_TABLE_COLUMNS[[table_name]],
                    table_name, site)
  assert_nonblank(result$brd_perpoint, c("siteID", "plotID", "pointID", "eventID", "boutNumber", "startDate"),
                  "brd_perpoint", site)
  if (nrow(result$brd_countdata))
    assert_nonblank(result$brd_countdata, c("siteID", "plotID", "pointID", "eventID", "boutNumber", "startDate"),
                    "brd_countdata", site)
  if (!all(as.character(result$brd_perpoint$siteID) == site) ||
      (nrow(result$brd_countdata) && !all(as.character(result$brd_countdata$siteID) == site)))
    stop(site, " fetch returned rows assigned to another site.", call. = FALSE)
  visit_year <- suppressWarnings(as.integer(substr(as.character(result$brd_perpoint$startDate), 1, 4)))
  detection_year <- suppressWarnings(as.integer(substr(as.character(result$brd_countdata$startDate), 1, 4)))
  if (any(!is.finite(visit_year) | !visit_year %in% 2013:2024) ||
      (length(detection_year) && any(!is.finite(detection_year) | !detection_year %in% 2013:2024)))
    stop(site, " returned a non-ISO or out-of-release survey date.", call. = FALSE)
  visit_date <- suppressWarnings(as.Date(substr(as.character(result$brd_perpoint$startDate), 1, 10)))
  detection_date <- suppressWarnings(as.Date(substr(as.character(result$brd_countdata$startDate), 1, 10)))
  release_start <- as.Date("2013-06-01")
  release_end <- as.Date("2024-07-31")
  if (any(is.na(visit_date) | visit_date < release_start | visit_date > release_end) ||
      (length(detection_date) &&
       any(is.na(detection_date) | detection_date < release_start | detection_date > release_end)))
    stop(site, " returned a survey date outside the published 2013-06 through 2024-07 window.",
         call. = FALSE)
  visit_bout <- suppressWarnings(as.numeric(result$brd_perpoint$boutNumber))
  if (any(!is.finite(visit_bout) | visit_bout != as.integer(visit_bout) |
          !(as.integer(visit_bout) %in% 1:2)))
    stop(site, " returned a visit outside the supported bout 1/2 domain.", call. = FALSE)
  visit_release <- as.character(result$brd_perpoint$release)
  detection_release <- as.character(result$brd_countdata$release)
  if (any(is.na(visit_release) | visit_release != RELEASE) ||
      (length(detection_release) && any(is.na(detection_release) | detection_release != RELEASE)))
    stop(site, " returned a missing or unexpected release tag in a required source table.",
         call. = FALSE)

  # Canonicalize every returned table/value before hashing and saving. The full
  # digest binds the complete job-local response, while a second digest binds
  # the exact privacy-safe evidence projection that may cross jobs.
  result <- bird_canonical_source(result)
  path <- file.path(RAW_DIR, paste0(site, "_raw.rds"))
  saveRDS(result, path, compress = "xz", version = 3)
  records[[site]] <- list(
    site = site,
    file = basename(path),
    full_source_content_sha256 = bird_full_source_sha256(result),
    evidence_projection_sha256 = bird_evidence_projection_sha256(result),
    brd_perpoint_rows = table_rows(result, "brd_perpoint"),
    brd_countdata_rows = table_rows(result, "brd_countdata")
  )
}

fetched_sites <- sort(vapply(records, `[[`, character(1), "site"))
if (!length(args) && !identical(fetched_sites, expected_sites)) {
  stop("Fetched roster does not exactly match the 47-site release roster.", call. = FALSE)
}

receipt <- list(
  schema_version = 3L,
  product = PRODUCT,
  release = RELEASE,
  doi = DOI,
  release_generated_utc = RELEASE_GENERATED,
  retrieval = list(
    package = "basic",
    tool = "neonUtilities::loadByProduct",
    neonUtilities_version = as.character(utils::packageVersion("neonUtilities")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    token_required = TRUE,
    staging_was_empty = TRUE
  ),
  evidence_projection = list(
    schema_version = BIRD_EVIDENCE_SCHEMA_VERSION,
    tables = BIRD_EVIDENCE_TABLE_COLUMNS,
    privacy = "no observer identity, personnel table, free-text sampling remarks, or source UIDs"
  ),
  expected_site_count = 47L,
  expected_sites = expected_sites,
  fetched_site_count = length(fetched_sites),
  fetched_sites = fetched_sites,
  files = unname(records)
)
jsonlite::write_json(receipt, RECEIPT, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(sprintf("OK: wrote %d immutable raw files and %s\n", length(fetched_sites), RECEIPT))
