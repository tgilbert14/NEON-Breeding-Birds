#!/usr/bin/env Rscript
# Deterministic privacy and integrity checks for the schema-v3 source receipt
# and schema-v2 cross-job breeding-bird evidence boundary.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })
source("R/site_metadata.R")
source("R/bird_evidence_contract.R")

expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), ignore.case = TRUE))
    stop("Expected error matching /", pattern, "/.", call. = FALSE)
  invisible(error)
}

make_columns <- function(columns, prefix) {
  out <- setNames(lapply(seq_along(columns), function(i)
    c(paste0(prefix, "-b-", i), paste0(prefix, "-a-", i))), columns)
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

make_full_source <- function(site) {
  pp <- make_columns(BIRD_EVIDENCE_TABLE_COLUMNS$brd_perpoint, "visit")
  pp$siteID <- site
  pp$plotID <- paste0(site, c("_001", "_001"))
  pp$pointID <- c("21", "22")
  pp$eventID <- paste(site, c("2019-06-02", "2020-06-03"), sep = ".")
  pp$boutNumber <- c("1", "2")
  pp$startDate <- c("2019-06-02T05:00:00Z", "2020-06-03T05:00:00Z")
  pp$samplingImpractical <- "OK"
  pp$release <- "RELEASE-2026"
  pp$measuredBy <- c("private-observer-2", "private-observer-1")
  pp$samplingImpracticalRemarks <- c("private free text two", "private free text one")
  pp$uid <- c("source-visit-private-2", "source-visit-private-1")
  pp$visit_uid <- c("visit-private-2", "visit-private-1")

  cd <- make_columns(BIRD_EVIDENCE_TABLE_COLUMNS$brd_countdata, "detection")
  cd$siteID <- site
  cd$plotID <- paste0(site, c("_001", "_001"))
  cd$pointID <- c("21", "22")
  cd$eventID <- paste(site, c("2019-06-02", "2020-06-03"), sep = ".")
  cd$boutNumber <- c("1", "2")
  cd$startDate <- c("2019-06-02T05:00:00Z", "2020-06-03T05:00:00Z")
  cd$scientificName <- c("Setophaga petechia", "Turdus migratorius")
  cd$taxonRank <- "species"
  cd$clusterSize <- c("1", "2")
  cd$detectionMethod <- "singing"
  cd$observerDistance <- c("10", "20")
  cd$pointCountMinute <- c("1", "2")
  cd$release <- "RELEASE-2026"
  cd$uid <- c("source-detection-private-2", "source-detection-private-1")
  cd$detection_uid <- c("detection-private-2", "detection-private-1")

  list(
    brd_personnel = data.frame(
      personnelID = c("private-2", "private-1"),
      email = c("private.two@example.org", "private.one@example.org"),
      stringsAsFactors = FALSE),
    brd_countdata = cd,
    package_metadata = list(download_note = "producer local only"),
    brd_perpoint = pp
  )
}

example <- make_full_source("ABBY")
projection <- bird_evidence_projection(example)
bird_assert_evidence_projection(projection)
stopifnot(
  !"endDate" %in% names(example$brd_perpoint),
  !"endDate" %in% names(projection$brd_perpoint),
  identical(names(projection), names(BIRD_EVIDENCE_TABLE_COLUMNS)),
  !"measuredBy" %in% names(projection$brd_perpoint),
  !"samplingImpracticalRemarks" %in% names(projection$brd_perpoint),
  !"uid" %in% names(projection$brd_perpoint),
  !"visit_uid" %in% names(projection$brd_perpoint),
  !"uid" %in% names(projection$brd_countdata),
  !"detection_uid" %in% names(projection$brd_countdata),
  !"brd_personnel" %in% names(projection)
)

# RELEASE-2026 omits brd_perpoint.endDate. Even if a future producer response
# adds that undeclared field, schema-v2 keeps it producer-local rather than
# silently widening the cross-job scientific evidence contract.
source_with_end_date <- example
source_with_end_date$brd_perpoint$endDate <-
  c("2019-06-02T05:06:00Z", "2020-06-03T05:06:00Z")
projection_with_end_date <- bird_evidence_projection(source_with_end_date)
stopifnot(
  !"endDate" %in% names(projection_with_end_date$brd_perpoint),
  !identical(bird_full_source_sha256(example),
             bird_full_source_sha256(source_with_end_date)),
  identical(projection, projection_with_end_date),
  identical(bird_evidence_projection_sha256(example),
            bird_evidence_projection_sha256(source_with_end_date))
)

# Named producer record lists must yield an unnamed canonical roster. R's
# vapply() otherwise preserves the list names, making identical() reject the
# same site values solely because one vector carries a names attribute.
named_records <- setNames(
  list(list(site = "PUUM"), list(site = "ABBY")),
  c("producer-slot-b", "producer-slot-a")
)
record_sites <- bird_receipt_record_sites(named_records)
stopifnot(
  identical(record_sites, c("ABBY", "PUUM")),
  is.null(names(record_sites))
)

private_change <- example
private_change$brd_personnel$email[[1]] <- "another.private@example.org"
stopifnot(
  !identical(bird_full_source_sha256(example), bird_full_source_sha256(private_change)),
  identical(bird_evidence_projection_sha256(example),
            bird_evidence_projection_sha256(private_change))
)

# Excluded producer-only values must not choose the public projection's row
# order. Make pointID the only allowed visit differentiator, then reverse a
# private field that alphabetically precedes pointID in the complete table.
private_order_a <- example
for (name in BIRD_EVIDENCE_TABLE_COLUMNS$brd_perpoint)
  private_order_a$brd_perpoint[[name]] <-
    rep(private_order_a$brd_perpoint[[name]][[1]], 2L)
private_order_a$brd_perpoint$pointID <- c("21", "22")
private_order_a$brd_perpoint$measuredBy <- c("private-z", "private-a")
private_order_b <- private_order_a
private_order_b$brd_perpoint$measuredBy <- rev(private_order_a$brd_perpoint$measuredBy)
private_projection_a <- bird_evidence_projection(private_order_a)
private_projection_b <- bird_evidence_projection(private_order_b)
bird_assert_evidence_projection(private_projection_a)
bird_assert_evidence_projection(private_projection_b)
stopifnot(
  identical(private_projection_a, private_projection_b),
  identical(private_projection_a$brd_perpoint$pointID, c("21", "22")),
  identical(bird_evidence_projection_sha256(private_order_a),
            bird_evidence_projection_sha256(private_order_b))
)

duplicate_source_field <- example
duplicate_source_field$brd_perpoint <- cbind(
  duplicate_source_field$brd_perpoint,
  duplicate_source_field$brd_perpoint["siteID"]
)
names(duplicate_source_field$brd_perpoint)[
  ncol(duplicate_source_field$brd_perpoint)] <- "siteID"
expect_error(bird_evidence_projection(duplicate_source_field), "duplicate source field")

reordered <- lapply(example[c(4, 2, 1, 3)], function(value) {
  if (is.data.frame(value)) value[nrow(value):1, rev(names(value)), drop = FALSE] else value
})
stopifnot(
  identical(bird_full_source_sha256(example), bird_full_source_sha256(reordered)),
  identical(bird_evidence_projection_sha256(example),
            bird_evidence_projection_sha256(reordered))
)

expected_sites <- sort(as.character(neon_sites$site), method = "radix")
test_root <- tempfile("bird-evidence-privacy-")
raw_dir <- file.path(test_root, "raw")
dir.create(raw_dir, recursive = TRUE)
records <- vector("list", length(expected_sites))
for (i in seq_along(expected_sites)) {
  site <- expected_sites[[i]]
  full <- bird_canonical_source(make_full_source(site))
  path <- file.path(raw_dir, paste0(site, "_raw.rds"))
  saveRDS(full, path, compress = "xz", version = 3)
  evidence <- bird_evidence_projection(full)
  records[[i]] <- list(
    site = site,
    file = basename(path),
    full_source_content_sha256 = bird_full_source_sha256(full),
    evidence_projection_sha256 = bird_evidence_projection_sha256(evidence),
    brd_perpoint_rows = nrow(evidence$brd_perpoint),
    brd_countdata_rows = nrow(evidence$brd_countdata)
  )
}
receipt <- list(
  schema_version = 3L,
  product = "DP1.10003.001",
  release = "RELEASE-2026",
  doi = "10.48443/v6hs-mx57",
  evidence_projection = list(
    schema_version = BIRD_EVIDENCE_SCHEMA_VERSION,
    tables = BIRD_EVIDENCE_TABLE_COLUMNS,
    privacy = "no observer identity, personnel table, free-text sampling remarks, or source UIDs"
  ),
  expected_site_count = 47L,
  expected_sites = expected_sites,
  fetched_site_count = 47L,
  fetched_sites = expected_sites,
  files = records
)
receipt_path <- file.path(test_root, "source_receipt.json")
jsonlite::write_json(receipt, receipt_path, auto_unbox = TRUE, pretty = TRUE,
                     null = "null")

# A separately owned environment-evidence subdirectory must survive byte-for-byte.
env_dir <- file.path(raw_dir, "environment")
dir.create(env_dir)
env_path <- file.path(env_dir, "sentinel.rds")
saveRDS(list(kind = "safe environment evidence", version = 1L), env_path,
        compress = "xz", version = 3)
env_before <- digest::digest(env_path, algo = "sha256", file = TRUE, serialize = FALSE)

run_script <- function(script) {
  output <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE,
    env = c(paste0("BIRD_RAW_DIR=", raw_dir),
            paste0("BIRD_RECEIPT=", receipt_path)))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L)
    stop(script, " failed:\n", paste(output, collapse = "\n"), call. = FALSE)
  invisible(output)
}
run_script("scripts/sanitize_bird_evidence.R")
stopifnot(identical(
  digest::digest(env_path, algo = "sha256", file = TRUE, serialize = FALSE),
  env_before))
run_script("scripts/scan_bird_evidence.R")

parsed_receipt <- jsonlite::fromJSON(receipt_path, simplifyVector = TRUE)
stopifnot(identical(bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt), 48L))
for (site in expected_sites) {
  path <- file.path(raw_dir, paste0(site, "_raw.rds"))
  evidence <- readRDS(path)
  bird_assert_evidence_projection(evidence)
  resaved <- file.path(test_root, paste0(site, "_resaved.rds"))
  saveRDS(evidence, resaved, compress = "xz", version = 3)
  stopifnot(identical(
    digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE),
    digest::digest(resaved, algo = "sha256", file = TRUE, serialize = FALSE)))
}

abby_path <- file.path(raw_dir, "ABBY_raw.rds")
clean_abby <- readRDS(abby_path)
leaked_column <- clean_abby
leaked_column$brd_perpoint$measuredBy <- "private-observer"
saveRDS(leaked_column, abby_path, compress = "xz", version = 3)
expect_error(bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt),
             "allowlist|prohibited")

leaked_table <- clean_abby
leaked_table$brd_personnel <- data.frame(email = "private@example.org")
saveRDS(leaked_table, abby_path, compress = "xz", version = 3)
expect_error(bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt),
             "exact evidence tables")

leaked_value <- clean_abby
leaked_value$brd_countdata$vernacularName[[1]] <- "private@example.org"
saveRDS(leaked_value, abby_path, compress = "xz", version = 3)
expect_error(bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt),
             "email-like")

digest_change <- clean_abby
digest_change$brd_countdata$scientificName[[1]] <- "Changed scientific evidence"
digest_change <- bird_evidence_projection(digest_change)
saveRDS(digest_change, abby_path, compress = "xz", version = 3)
expect_error(bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt),
             "digest differs")

saveRDS(clean_abby, abby_path, compress = "xz", version = 3)
bird_scan_evidence_directory(raw_dir, expected_sites, parsed_receipt)
unlink(test_root, recursive = TRUE, force = TRUE)

cat("OK: schema-v3 receipt / schema-v2 bird evidence is deterministic, allowlisted, and PII-safe.\n")
