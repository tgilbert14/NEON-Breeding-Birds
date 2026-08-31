#!/usr/bin/env Rscript
# Replace producer-local loadByProduct() RDS files with the exact privacy-safe
# scientific evidence projection before any cross-job artifact is packaged.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })

RAW_DIR <- Sys.getenv("BIRD_RAW_DIR", "build/raw/birds")
RECEIPT_PATH <- Sys.getenv("BIRD_RECEIPT", "build/source_receipt.json")
STAGE_NAME <- ".bird-sanitized-v2"

source("R/site_metadata.R")
source("R/bird_evidence_contract.R")

expected_sites <- sort(as.character(neon_sites$site), method = "radix")
if (length(expected_sites) != 47L || !identical(expected_sites, sort(unique(expected_sites))) ||
    !"PUUM" %in% expected_sites)
  bird_evidence_fail("Canonical release roster must contain 47 unique sites including PUUM.")
if (!file.exists(RECEIPT_PATH)) bird_evidence_fail("Missing source receipt: ", RECEIPT_PATH, ".")
receipt <- jsonlite::fromJSON(RECEIPT_PATH, simplifyVector = TRUE)
if (!identical(receipt$product, "DP1.10003.001") ||
    !identical(receipt$release, "RELEASE-2026") ||
    !identical(receipt$doi, "10.48443/v6hs-mx57"))
  bird_evidence_fail("Source receipt identity mismatch.")
bird_assert_receipt_evidence_contract(receipt)
records <- bird_receipt_records(receipt)
if (!identical(sort(as.character(receipt$fetched_sites), method = "radix"), expected_sites) ||
    !identical(sort(as.character(records$site), method = "radix"), expected_sites))
  bird_evidence_fail("Source receipt roster is not the exact 47-site release roster.")

root_entries <- list.files(RAW_DIR, full.names = TRUE, all.files = TRUE, no.. = TRUE)
root_files <- root_entries[!dir.exists(root_entries)]
expected_files <- paste0(expected_sites, "_raw.rds")
if (!identical(sort(basename(root_files), method = "radix"), expected_files))
  bird_evidence_fail("Raw staging root must contain exactly the canonical 47 bird RDS files.")
if (any(nzchar(Sys.readlink(root_entries))))
  bird_evidence_fail("Raw staging may not contain symbolic links.")

stage_dir <- file.path(RAW_DIR, STAGE_NAME)
if (file.exists(stage_dir) || dir.exists(stage_dir))
  bird_evidence_fail("Deterministic sanitizer staging path already exists: ", stage_dir, ".")
if (!dir.create(stage_dir, showWarnings = FALSE))
  bird_evidence_fail("Could not create deterministic sanitizer staging directory.")
on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

# Finish every validation and staged write before replacing any producer-local
# file. A failure therefore prevents artifact packaging without uploading a
# mixture of complete and projected source objects.
for (site in expected_sites) {
  record_index <- match(site, as.character(records$site))
  raw_path <- file.path(RAW_DIR, paste0(site, "_raw.rds"))
  raw <- tryCatch(readRDS(raw_path), error = function(error)
    bird_evidence_fail("Cannot read producer-local source for ", site, ": ",
                       conditionMessage(error), "."))
  if (!identical(bird_full_source_sha256(raw),
                 as.character(records$full_source_content_sha256[[record_index]])))
    bird_evidence_fail(site, " full source digest differs from its receipt.")
  projection <- bird_evidence_projection(raw)
  bird_assert_evidence_projection(projection, paste0(site, " staged bird evidence"))
  if (!identical(bird_evidence_projection_sha256(projection),
                 as.character(records$evidence_projection_sha256[[record_index]])))
    bird_evidence_fail(site, " evidence projection digest differs from its receipt.")
  if (nrow(projection$brd_perpoint) !=
        as.integer(records$brd_perpoint_rows[[record_index]]) ||
      nrow(projection$brd_countdata) !=
        as.integer(records$brd_countdata_rows[[record_index]]))
    bird_evidence_fail(site, " evidence projection row counts differ from its receipt.")
  saveRDS(projection, file.path(stage_dir, basename(raw_path)),
          compress = "xz", version = 3)
}

for (site in expected_sites) {
  file_name <- paste0(site, "_raw.rds")
  staged <- file.path(stage_dir, file_name)
  target <- file.path(RAW_DIR, file_name)
  if (!file.copy(staged, target, overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE))
    bird_evidence_fail("Could not replace producer-local source with sanitized evidence for ",
                       site, ".")
}

unlink(stage_dir, recursive = TRUE, force = TRUE)
bird_scan_evidence_directory(RAW_DIR, expected_sites, receipt)
cat(sprintf("OK: sanitized and verified %d bird evidence files for cross-job transfer.\n",
            length(expected_sites)))
