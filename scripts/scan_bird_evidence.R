#!/usr/bin/env Rscript
# Standalone defense-in-depth scanner for the cross-job bird evidence artifact.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })

RAW_DIR <- Sys.getenv("BIRD_RAW_DIR", "raw/birds")
output_root <- Sys.getenv("BIRD_OUTPUT_ROOT", "")
default_receipt <- if (nzchar(output_root))
  file.path(output_root, "data", "source_receipt.json") else "build/source_receipt.json"
RECEIPT_PATH <- Sys.getenv("BIRD_RECEIPT", default_receipt)

source("R/site_metadata.R")
source("R/bird_evidence_contract.R")

expected_sites <- sort(as.character(neon_sites$site), method = "radix")
if (!file.exists(RECEIPT_PATH)) bird_evidence_fail("Missing source receipt: ", RECEIPT_PATH, ".")
receipt <- jsonlite::fromJSON(RECEIPT_PATH, simplifyVector = TRUE)
if (!identical(receipt$product, "DP1.10003.001") ||
    !identical(receipt$release, "RELEASE-2026") ||
    !identical(receipt$doi, "10.48443/v6hs-mx57") ||
    !identical(sort(as.character(receipt$fetched_sites), method = "radix"), expected_sites))
  bird_evidence_fail("Source receipt identity or roster mismatch.")
count <- bird_scan_evidence_directory(RAW_DIR, expected_sites, receipt)
cat(sprintf("OK: cross-job bird evidence passed allowlist and PII scans (%d RDS files).\n",
            count))
