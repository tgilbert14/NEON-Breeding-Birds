#!/usr/bin/env Rscript
# Synthetic contracts for the exact deploy-payload and Connect-manifest stamp.
# These fixtures prove object/directory order is irrelevant while code,
# derived-data, poster, Pages-receipt, and dependency-contract tampering fail.

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

writer <- normalizePath("scripts/write_release_stamp.R", mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
fixture_parent <- tempfile("birds-release-stamp-")
dir.create(fixture_parent, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(fixture_parent, recursive = TRUE, force = TRUE), add = TRUE)

write_bytes <- function(path, bytes) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(bytes, connection)
}

read_bytes <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = file.info(path)$size)
}

fixture_manifest <- function(reverse = FALSE) {
  value <- list(
    version = 1L,
    locale = "en_US",
    platform = "4.5.2",
    metadata = list(
      appmode = "shiny", primary_rmd = NULL, primary_html = NULL,
      content_category = NULL, has_parameters = FALSE
    ),
    packages = list(
      alpha = list(
        Source = "CRAN", Repository = "https://example.invalid/snapshot",
        description = list(Package = "alpha", Version = "1.0.0", Imports = "beta")
      ),
      beta = list(
        Source = "CRAN", Repository = "https://example.invalid/snapshot",
        description = list(Package = "beta", Version = "2.0.0")
      )
    ),
    files = list("global.R" = list(checksum = "fixture-md5")),
    users = NULL
  )
  if (reverse) {
    reverse_named <- function(item) {
      if (!is.list(item)) return(item)
      keys <- names(item)
      if (is.null(keys)) return(lapply(item, reverse_named))
      index <- rev(seq_along(item))
      output <- lapply(item[index], reverse_named)
      names(output) <- keys[index]
      output
    }
    value <- reverse_named(value)
  }
  paste0(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n")
}

fixture_files <- function(reverse_manifest = FALSE) {
  source_receipt <- paste0(jsonlite::toJSON(list(
    product = "DP1.10003.001",
    release = "RELEASE-2026",
    doi = "10.48443/v6hs-mx57"
  ), auto_unbox = TRUE, pretty = TRUE), "\n")
  environment_receipt <- paste0(jsonlite::toJSON(list(
    schema_version = 1L,
    release = "RELEASE-2026"
  ), auto_unbox = TRUE, pretty = TRUE), "\n")
  values <- c(
    "global.R" = "fixture global\n",
    "ui.R" = "fixture ui\n",
    "server.R" = "fixture server\n",
    "R/helper.R" = "fixture helper\n",
    "www/app.js" = "fixture app\n",
    "www/assets/poster.webp" = "fixture runtime poster\n",
    "data/source_receipt.json" = source_receipt,
    "data/environment_source_receipt.json" = environment_receipt,
    "data/derived.rds" = "fixture derived bytes\n",
    "data-sample/demo.rds" = "fixture demo bytes\n",
    "docs/index.html" = "<main>fixture cover</main>\n",
    "docs/og-image-v2.png" = "fixture social bytes\n",
    "docs/assets/poster.webp" = "fixture pages poster\n",
    "manifest.json" = fixture_manifest(reverse_manifest)
  )
  lapply(values, charToRaw)
}

make_fixture <- function(root, reverse = FALSE) {
  files <- fixture_files(reverse_manifest = reverse)
  paths <- names(files)
  if (reverse) paths <- rev(paths)
  for (relative in paths) write_bytes(file.path(root, relative), files[[relative]])
}

run_writer <- function(root, mode, expect_success) {
  output <- suppressWarnings(system2(
    rscript,
    c("--vanilla", shQuote(writer)),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BIRD_OUTPUT_ROOT=", root),
      paste0("BIRD_RELEASE_STAMP_MODE=", mode),
      "BIRD_WRITE_PAGES_RELEASE=1"
    )
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (expect_success && status != 0L) {
    stop("Release-stamp fixture unexpectedly failed:\n", paste(output, collapse = "\n"), call. = FALSE)
  }
  if (!expect_success && status == 0L) {
    stop("Release-stamp fixture accepted a tampered payload.", call. = FALSE)
  }
  invisible(output)
}

root_a <- file.path(fixture_parent, "forward")
root_b <- file.path(fixture_parent, "reverse")
make_fixture(root_a, reverse = FALSE)
make_fixture(root_b, reverse = TRUE)
run_writer(root_a, "write", expect_success = TRUE)
run_writer(root_b, "write", expect_success = TRUE)

stamp_a <- read_bytes(file.path(root_a, "data", "release_stamp.json"))
stamp_b <- read_bytes(file.path(root_b, "data", "release_stamp.json"))
assert(identical(stamp_a, stamp_b), "Payload identity depends on file-creation order.")
stamp <- jsonlite::fromJSON(rawToChar(stamp_a), simplifyVector = FALSE)
expected_fields <- c(
  "schema_version", "app_id", "product", "release", "doi",
  "source_receipt_sha256", "environment_receipt_sha256", "payload_sha256",
  "manifest_contract_sha256", "release_id"
)
assert(identical(names(stamp), expected_fields) &&
       identical(as.integer(stamp$schema_version), 3L) &&
       grepl("^[0-9a-f]{64}$", as.character(stamp$payload_sha256)) &&
       grepl("^[0-9a-f]{64}$", as.character(stamp$manifest_contract_sha256)) &&
       grepl("^sha256:[0-9a-f]{64}$", as.character(stamp$release_id)),
       "Synthetic stamp does not implement schema v3.")

# Independently reconstruct the synthetic payload without sourcing the writer.
below <- function(directory, pattern = NULL, recursive = TRUE) {
  inside <- list.files(
    file.path(root_a, directory), pattern = pattern, recursive = recursive,
    full.names = FALSE, include.dirs = FALSE, no.. = TRUE
  )
  file.path(directory, inside)
}
expected_paths <- sort(unique(c(
  "global.R", "ui.R", "server.R",
  below("R", "[.]R$", recursive = FALSE),
  below("www"), below("data"), below("data-sample"),
  "docs/index.html", "docs/og-image-v2.png", below("docs/assets")
)), method = "radix")
expected_paths <- setdiff(expected_paths, c(
  "data/release_stamp.json", "docs/release.json", "manifest.json"
))
expected_entries <- paste(
  expected_paths,
  vapply(file.path(root_a, expected_paths), function(path) {
    digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
  }, character(1)),
  sep = "\t"
)
expected_payload <- digest::digest(
  paste0(paste(expected_entries, collapse = "\n"), "\n"),
  algo = "sha256", serialize = FALSE
)
assert(identical(as.character(stamp$payload_sha256), expected_payload),
       "Synthetic payload digest does not independently re-derive.")

synthetic_manifest <- jsonlite::fromJSON(file.path(root_a, "manifest.json"), simplifyVector = FALSE)
canonical_manifest_value <- function(value) {
  if (!is.list(value)) return(value)
  keys <- names(value)
  if (is.null(keys)) return(lapply(value, canonical_manifest_value))
  index <- order(keys, method = "radix")
  output <- lapply(value[index], canonical_manifest_value)
  names(output) <- keys[index]
  output
}
synthetic_contract <- list(
  version = synthetic_manifest$version,
  locale = synthetic_manifest$locale,
  platform = synthetic_manifest$platform,
  metadata = canonical_manifest_value(synthetic_manifest$metadata),
  packages = canonical_manifest_value(synthetic_manifest$packages),
  users = canonical_manifest_value(synthetic_manifest$users)
)
synthetic_contract_json <- as.character(jsonlite::toJSON(
  synthetic_contract, auto_unbox = TRUE, pretty = FALSE, null = "null", na = "null"
))
expected_manifest_contract <- digest::digest(
  paste("neon-connect-manifest-contract-v1", synthetic_contract_json, sep = "\n"),
  algo = "sha256", serialize = FALSE
)
assert(identical(as.character(stamp$manifest_contract_sha256), expected_manifest_contract),
       "Synthetic manifest contract does not independently re-derive.")
expected_identity <- paste(
  "neon-breeding-birds-release-instance-v3", "NEON-Breeding-Birds",
  "DP1.10003.001", "RELEASE-2026", "10.48443/v6hs-mx57",
  as.character(stamp$source_receipt_sha256),
  as.character(stamp$environment_receipt_sha256), expected_payload,
  expected_manifest_contract,
  sep = "\n"
)
assert(identical(
  as.character(stamp$release_id),
  paste0("sha256:", digest::digest(expected_identity, algo = "sha256", serialize = FALSE))
), "Synthetic release ID does not independently re-derive.")
assert(identical(
  stamp_a,
  read_bytes(file.path(root_a, "docs", "release.json"))
), "Runtime and Pages stamps are not byte-identical.")

manifest_path <- file.path(root_a, "manifest.json")
write_manifest_fixture <- function(value) {
  write_bytes(manifest_path, charToRaw(paste0(jsonlite::toJSON(
    value, auto_unbox = TRUE, pretty = TRUE, null = "null"
  ), "\n")))
}
# Simulate final manifest generation by adding the newly written stamp to only
# the excluded files/checksum map. The canonical dependency contract stays
# identical, proving the preliminary -> stamp -> final sequence has no cycle.
final_manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
final_manifest$files[["data/release_stamp.json"]] <- list(checksum = "final-stamp-md5")
write_manifest_fixture(final_manifest)
final_manifest_bytes <- read_bytes(manifest_path)
run_writer(root_a, "verify", expect_success = TRUE)

for (relative in c("global.R", "data/derived.rds", "docs/assets/poster.webp")) {
  path <- file.path(root_a, relative)
  original <- read_bytes(path)
  write_bytes(path, c(original, charToRaw("tamper\n")))
  run_writer(root_a, "verify", expect_success = FALSE)
  write_bytes(path, original)
  run_writer(root_a, "verify", expect_success = TRUE)
}

contract_tampers <- list(
  platform = function(value) { value$platform <- "9.9.9"; value },
  metadata = function(value) { value$metadata$appmode <- "static"; value },
  package = function(value) { value$packages$alpha$description$Version <- "1.0.1"; value }
)
for (label in names(contract_tampers)) {
  candidate <- jsonlite::fromJSON(rawToChar(final_manifest_bytes), simplifyVector = FALSE)
  write_manifest_fixture(contract_tampers[[label]](candidate))
  run_writer(root_a, "verify", expect_success = FALSE)
  write_bytes(manifest_path, final_manifest_bytes)
  run_writer(root_a, "verify", expect_success = TRUE)
}

pages_stamp <- file.path(root_a, "docs", "release.json")
pages_original <- read_bytes(pages_stamp)
write_bytes(pages_stamp, c(pages_original, charToRaw("tamper\n")))
run_writer(root_a, "verify", expect_success = FALSE)

cat(paste(
  "OK: schema-v3 stamp is order-invariant, cycle-free, rejects code/data/poster/Pages",
  "tampering, and rejects manifest package/platform/metadata tampering.\n"
))
