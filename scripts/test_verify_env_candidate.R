#!/usr/bin/env Rscript
# Focused positive/negative controls for the independent environmental oracle.

Sys.setenv(BIRD_ENV_ORACLE_LIBRARY = "1")
source("scripts/verify_env_candidate.R")

check <- function(condition, label) {
  if (!isTRUE(condition)) stop("ENV ORACLE CHECK FAILED: ", label, call. = FALSE)
}
expect_error <- function(expr, pattern, label) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  check(inherits(error, "error") && grepl(pattern, conditionMessage(error), ignore.case = TRUE),
        label)
}

# Lowest published verticalPosition wins before support. Within a position, the
# support and lexical tie-break fields remain independently reconstructible.
streams <- data.frame(
  signature = c("A|1", "Z|2"),
  horizontalPosition = c("A", "Z"),
  verticalPosition = c("1", "2"),
  stringsAsFactors = FALSE
)
months <- data.frame(
  signature = c("A|1", "Z|2", "Z|2"),
  ym = c("2020-01", "2020-01", "2020-02"),
  rows = c(3L, 3L, 3L),
  final_qf_pass_rows = c(2L, 3L, 3L),
  passing_finite_values = c(2L, 3L, 3L),
  content_sha256 = c(strrep("a", 64), strrep("b", 64), strrep("c", 64)),
  stringsAsFactors = FALSE
)
selected <- ev_select_summary(streams, months, "tempSingleMean", "fixture temperature",
                              lowest_vertical = TRUE)
check(identical(selected$signature, "A|1") &&
        identical(selected$selection$selected_vertical_position, 1) &&
        identical(selected$selection$supported_months, 1L) &&
        identical(selected$selection$finite_values, 2L) &&
        identical(selected$selection$rejected_by_final_qf, 1L),
      "lowest-position stream selection and receipt facts")

bad_vertical <- streams
bad_vertical$verticalPosition[[1]] <- "unknown"
bad_vertical$signature[[1]] <- "A|unknown"
bad_vertical_months <- months
bad_vertical_months$signature[bad_vertical_months$signature == "A|1"] <- "A|unknown"
expect_error(
  ev_select_summary(bad_vertical, bad_vertical_months, "tempSingleMean", "bad vertical",
                    lowest_vertical = TRUE),
  "verticalPosition", "non-numeric selected vertical metadata fails closed"
)
bad_digest <- months
bad_digest$content_sha256[[1]] <- "not-a-digest"
expect_error(
  ev_select_summary(streams, bad_digest, "tempSingleMean", "bad digest",
                    lowest_vertical = TRUE),
  "digest", "malformed unselected-stream evidence digest fails closed"
)

# A selected stream carries every pre-aggregation row. Its per-month digest and
# counts must change when even one raw value changes.
days <- format(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"))
precip <- data.frame(
  startDateTime = days,
  precipBulk = rep(1, length(days)),
  finalQF = rep(0, length(days)),
  stringsAsFactors = FALSE
)
summary_one <- ev_selected_month_summary(precip, "precipBulk", "single-stream")
check(summary_one$rows == 31L && summary_one$final_qf_pass_rows == 31L &&
        summary_one$passing_finite_values == 31L &&
        grepl("^[0-9a-f]{64}$", summary_one$content_sha256),
      "selected raw rows produce canonical month support and digest")
tampered <- precip
tampered$precipBulk[[1]] <- 2
summary_two <- ev_selected_month_summary(tampered, "precipBulk", "single-stream")
check(!identical(summary_one$content_sha256, summary_two$content_sha256),
      "selected raw-value tamper changes bound month digest")

precip_month <- ev_precip_monthly(precip, "WEIPRE_daily")
check(precip_month$precip_mm == 31 && precip_month$precip_n == 31L &&
        precip_month$precip_expected == 31L && precip_month$precip_coverage_pct == 100,
      "complete finalQF-passing precipitation month is reportable")
precip_bad_qf <- precip
precip_bad_qf$finalQF[[1]] <- 1
precip_incomplete <- ev_precip_monthly(precip_bad_qf, "WEIPRE_daily")
check(is.na(precip_incomplete$precip_mm) && precip_incomplete$precip_n == 30L,
      "one failed finalQF row suppresses a precipitation month")

# Temperature is independently gated by finalQF and the 75% expected-interval
# threshold, using complete selected-stream rows rather than monthly producer
# summaries.
half_hours <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2020-01-31 23:30:00", tz = "UTC"), by = "30 min")
temperature <- data.frame(
  startDateTime = format(half_hours, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  tempSingleMean = 10,
  tempSingleMinimum = 9,
  tempSingleMaximum = 11,
  finalQF = 0,
  stringsAsFactors = FALSE
)
temperature$finalQF[[1]] <- 1
temperature$tempSingleMean[[1]] <- 999
temperature_month <- ev_temperature_monthly(temperature, "SAAT_30min")
check(temperature_month$temp_c == 10 && temperature_month$temp_n == 1487L &&
        temperature_month$temp_expected == 1488L &&
        temperature_month$temp_coverage_pct > 99,
      "temperature finalQF failure is excluded without losing a supported month")
temperature_short <- temperature[seq_len(1000), , drop = FALSE]
short_month <- ev_temperature_monthly(temperature_short, "SAAT_30min")
check(is.na(short_month$temp_c) && short_month$temp_n == 999L &&
        short_month$temp_coverage_pct < 75,
      "under-covered temperature month is unavailable")
temperature_duplicate <- rbind(temperature[1, , drop = FALSE],
                               temperature[1, , drop = FALSE])
expect_error(
  ev_temperature_monthly(temperature_duplicate, "SAAT_30min"),
  "duplicate", "duplicate selected temperature timestamp fails closed"
)

# Phenology is individual x month: repeated bouts do not overweight an
# individual, uncertain statuses leave numerator and denominator, and n<5 is
# withheld. IDs in validation evidence are irreversible fixed-domain hashes.
ids <- paste0("plant-sha256:", vapply(1:5, function(i) {
  digest::digest(paste0("fixture-", i), algo = "sha256", serialize = FALSE)
}, character(1)))
pheno <- data.frame(
  startDateTime = rep("2020-05-15", 8),
  phenophaseName = rep("Open flowers", 8),
  phenophaseStatus = c("yes", "no", "no", "yes", "no", "yes", "uncertain", "yes"),
  individualID = c(ids[[1]], ids[[1]], ids[[2]], ids[[3]], ids[[4]], ids[[5]],
                   ids[[2]], NA_character_),
  stringsAsFactors = FALSE
)
share <- ev_pheno_share(pheno, "^Open flowers$")
check(share$n == 5L && share$share == 60,
      "phenology uses distinct individuals, any-yes status, and yes/no only")
share_small <- ev_pheno_share(pheno[pheno$individualID != ids[[5]] &
                                      !is.na(pheno$individualID), , drop = FALSE],
                              "^Open flowers$")
check(share_small$n == 4L && is.na(share_small$share),
      "phenology support below five individuals is withheld")

frame <- data.frame(ym = "2020-01", value = 1, stringsAsFactors = FALSE)
expect_error(
  ev_compare_frame(frame, transform(frame, value = 2), "tampered public frame"),
  "values differ", "public bundle value mismatch fails exact comparison"
)

# Exercise the complete per-site reconstruction path with independent evidence
# objects: selection summaries choose the stream, selected raw rows produce the
# monthly sensor values, and status rows produce the plant layer.
temperature_summary <- ev_selected_month_summary(temperature, "tempSingleMean", "tower-a|1")
precip_summary <- ev_selected_month_summary(precip, "precipBulk", "gauge-a")
evidence <- list(
  schema_version = 1L,
  release = "RELEASE-2026",
  site = "ABBY",
  window = list(start_month = "2013-01", end_month = "2024-12"),
  privacy = list(
    personal_fields = "none",
    plant_individual_identity = paste(
      "domain-separated SHA-256 pseudonym; raw individualID is never written",
      "to validation evidence"
    )
  ),
  products = list(
    air_temperature = list(
      id = "DP1.00002.001", doi = "10.48443/p69b-5e50",
      source_tables = "SAAT_30min", selected_table = "SAAT_30min",
      source_columns = list(
        timestamp = "startDateTime",
        stream = c("namedLocation", "verticalPosition"),
        value = "tempSingleMean",
        companions = list(
          tempSingleMinimum = "tempSingleMinimum",
          tempSingleMaximum = "tempSingleMaximum"
        ),
        quality_flag = "finalQF"
      ),
      selection_streams = data.frame(
        signature = "tower-a|1", namedLocation = "tower-a", verticalPosition = "1",
        stringsAsFactors = FALSE
      ),
      selection_months = temperature_summary,
      selected_rows = temperature
    ),
    precipitation = list(
      id = "DP1.00044.001", doi = "10.48443/v29j-eg88",
      source_tables = "WEIPRE_daily", selected_table = "WEIPRE_daily",
      source_columns = list(
        timestamp = "startDateTime", stream = "namedLocation",
        value = "precipBulk", companions = list(), quality_flag = "finalQF"
      ),
      selection_streams = data.frame(
        signature = "gauge-a", namedLocation = "gauge-a", stringsAsFactors = FALSE
      ),
      selection_months = precip_summary,
      selected_rows = precip
    ),
    plant_phenology = list(
      id = "DP1.10055.001", doi = "10.48443/p75s-7p48",
      source_tables = "phe_statusintensity", selected_table = "phe_statusintensity",
      source_columns = list(
        timestamp = "startDateTime", phenophase = "phenophaseName",
        status = "phenophaseStatus", individual = "individualID"
      ),
      source_rows = nrow(pheno),
      rows = ev_canonical_order(pheno)
    )
  )
)
site_result <- ev_build_site("ABBY", evidence, list())
jan <- site_result$public[site_result$public$ym == "2020-01", , drop = FALSE]
may <- site_result$public[site_result$public$ym == "2020-05", , drop = FALSE]
check(nrow(jan) == 1L && jan$temp_c == 10 && jan$precip_mm == 31 &&
        nrow(may) == 1L && may$flowering_pct == 60 && may$flowering_pct_n == 5L,
      "complete site output reconstructs exactly from selected raw evidence")

# Exercise the on-disk 47-site receipt/digest/roster boundary, including JSON's
# scalar-versus-array representation. Twenty sites carry precipitation, matching
# the pinned release contract; all 47 carry temperature and phenology.
source("R/site_metadata.R")
fixture_root <- tempfile("birds-env-oracle-")
fixture_env <- file.path(fixture_root, "data", "env")
fixture_evidence <- file.path(fixture_root, "validation-evidence", "environment")
dir.create(fixture_env, recursive = TRUE)
dir.create(fixture_evidence, recursive = TRUE)
on.exit(unlink(fixture_root, recursive = TRUE, force = TRUE), add = TRUE)
fixture_sites <- sort(as.character(neon_sites$site), method = "radix")
records <- vector("list", length(fixture_sites))
for (i in seq_along(fixture_sites)) {
  site <- fixture_sites[[i]]
  site_evidence <- evidence
  site_evidence$site <- site
  if (i > 20L) {
    site_evidence$products$precipitation <- list(
      id = "DP1.00044.001", doi = "10.48443/v29j-eg88",
      source_tables = character(), selected_table = NULL, source_columns = list(),
      selection_streams = data.frame(signature = character(), stringsAsFactors = FALSE),
      selection_months = data.frame(
        signature = character(), ym = character(), rows = integer(),
        final_qf_pass_rows = integer(), passing_finite_values = integer(),
        content_sha256 = character(), stringsAsFactors = FALSE
      ),
      selected_rows = data.frame(
        startDateTime = character(), precipBulk = numeric(), finalQF = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }
  derived <- ev_build_site(site, site_evidence, list())
  public_path <- file.path(fixture_env, paste0(site, ".rds"))
  evidence_path <- file.path(fixture_evidence, paste0(site, ".rds"))
  saveRDS(derived$public, public_path, compress = FALSE, version = 3)
  saveRDS(site_evidence, evidence_path, compress = FALSE, version = 3)
  records[[i]] <- list(
    site = site,
    rows = nrow(derived$public),
    month_min = min(derived$public$ym),
    month_max = max(derived$public$ym),
    air_temperature_supported = derived$support$air_temperature,
    precipitation_supported = derived$support$precipitation,
    plant_phenology_supported = derived$support$plant_phenology,
    source_tables = derived$source_tables,
    stream_selection = derived$stream_selection,
    file = basename(public_path),
    sha256 = ev_sha256(public_path),
    bytes = unname(file.info(public_path)$size),
    evidence_file = basename(evidence_path),
    evidence_sha256 = ev_sha256(evidence_path),
    evidence_bytes = unname(file.info(evidence_path)$size)
  )
}
receipt <- list(
  schema_version = 2L,
  release = "RELEASE-2026",
  window = list(start_month = "2013-01", end_month = "2024-12"),
  retrieval = list(
    tool = "neonUtilities::loadByProduct", package = "basic",
    neonUtilities_version = "fixture",
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    token_required = TRUE
  ),
  products = list(
    air_temperature = list(
      id = "DP1.00002.001", doi = "10.48443/p69b-5e50", supported_sites = 47L
    ),
    precipitation = list(
      id = "DP1.00044.001", doi = "10.48443/v29j-eg88", supported_sites = 20L
    ),
    plant_phenology = list(
      id = "DP1.10055.001", doi = "10.48443/p75s-7p48", supported_sites = 47L
    )
  ),
  validation_evidence = list(
    schema_version = 1L, file_count = 47L,
    total_bytes = sum(vapply(records, function(record) {
      as.numeric(record$evidence_bytes)
    }, numeric(1))),
    format = paste(
      "canonical privacy-minimized selected-stream rows plus digest-bound",
      "all-stream month support; validation-only; not deployed"
    )
  ),
  files = records
)
jsonlite::write_json(
  receipt, file.path(fixture_root, "data", "environment_source_receipt.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null"
)
check(ev_verify(fixture_root, fixture_evidence),
      "full 47-site on-disk evidence/receipt round trip")
tampered_path <- file.path(fixture_env, paste0(fixture_sites[[47]], ".rds"))
tampered_public <- readRDS(tampered_path)
tampered_public$temp_c[[1]] <- tampered_public$temp_c[[1]] + 1
saveRDS(tampered_public, tampered_path, compress = FALSE, version = 3)
expect_error(
  ev_verify(fixture_root, fixture_evidence),
  "digest|values differ", "tampered public environmental bytes fail closed"
)

cat("OK: independent environmental oracle fixtures and negative controls passed.\n")
