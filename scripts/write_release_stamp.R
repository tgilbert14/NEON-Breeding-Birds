#!/usr/bin/env Rscript
# Build or verify the deterministic public release-instance stamp.
#
# Schema v3 binds one public release ID to the exact bytes served by both
# surfaces: the complete Shiny runtime payload plus the Pages poster index,
# artwork, and social image. It also binds the canonical non-file Connect
# manifest contract (runtime platform, metadata, complete dependency closure,
# and users). The stamp, its Pages copy, and manifest file/checksum map are
# excluded from the byte payload so the identity has no circular dependency.
# Acquisition-receipt digests remain explicit audit fields as well as members
# of the byte payload.

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
MODE <- tolower(trimws(Sys.getenv("BIRD_RELEASE_STAMP_MODE", "verify")))
WRITE_PAGES <- identical(Sys.getenv("BIRD_WRITE_PAGES_RELEASE", "0"), "1")

if (!MODE %in% c("write", "verify")) {
  stop("BIRD_RELEASE_STAMP_MODE must be 'write' or 'verify'.", call. = FALSE)
}

APP_ID <- "NEON-Breeding-Birds"
PRODUCT <- "DP1.10003.001"
RELEASE <- "RELEASE-2026"
DOI <- "10.48443/v6hs-mx57"
DOMAIN <- "neon-breeding-birds-release-instance-v3"
MANIFEST_DOMAIN <- "neon-connect-manifest-contract-v1"

data_dir <- file.path(ROOT, "data")
source_path <- file.path(data_dir, "source_receipt.json")
environment_path <- file.path(data_dir, "environment_source_receipt.json")
stamp_path <- file.path(data_dir, "release_stamp.json")
pages_path <- file.path(ROOT, "docs", "release.json")
manifest_path <- file.path(ROOT, "manifest.json")

required_inputs <- c(source_path, environment_path, manifest_path)
missing_inputs <- required_inputs[
  !file.exists(required_inputs) |
    is.na(file.info(required_inputs)$size) |
    file.info(required_inputs)$size <= 0
]
if (length(missing_inputs)) {
  stop("Cannot stamp a release without both receipts and a preliminary manifest: ",
       paste(missing_inputs, collapse = ", "), call. = FALSE)
}

source_receipt <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)
environment_receipt <- jsonlite::fromJSON(environment_path, simplifyVector = FALSE)
if (!identical(source_receipt$product, PRODUCT) ||
    !identical(source_receipt$release, RELEASE) ||
    !identical(source_receipt$doi, DOI)) {
  stop("Bird source receipt identity does not match the release contract.", call. = FALSE)
}
if (!identical(environment_receipt$release, RELEASE)) {
  stop("Environment source receipt identity does not match the release contract.", call. = FALSE)
}

# Object-key order and pretty-printing are not semantic manifest changes. Select
# the six non-file top-level fields in a fixed order and recursively order every
# named object before hashing. The files map is excluded because it contains the
# release-stamp checksum in the final manifest; exact file entries/checksums are
# independently owned by verify_manifest.R.
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest_fields <- c("version", "locale", "platform", "metadata", "packages", "files", "users")
if (length(names(manifest)) != length(manifest_fields) ||
    !setequal(names(manifest), manifest_fields) ||
    is.null(manifest$packages) || !is.list(manifest$packages) || !length(manifest$packages)) {
  stop("Preliminary manifest lacks the exact runtime/dependency contract fields.", call. = FALSE)
}
manifest_file_paths <- if (is.list(manifest$files)) names(manifest$files) else character()
manifest_file_paths <- gsub("\\\\", "/", manifest_file_paths)
if (identical(MODE, "write") && "data/release_stamp.json" %in% manifest_file_paths) {
  stop("Write mode requires a preliminary manifest without the release stamp.", call. = FALSE)
}
if (identical(MODE, "verify") && !"data/release_stamp.json" %in% manifest_file_paths) {
  stop("Verify mode requires the final manifest containing the release stamp.", call. = FALSE)
}
canonical_object <- function(value) {
  if (!is.list(value)) return(value)
  keys <- names(value)
  if (is.null(keys)) return(lapply(value, canonical_object))
  order_index <- order(keys, method = "radix")
  output <- lapply(value[order_index], canonical_object)
  names(output) <- keys[order_index]
  output
}
manifest_contract <- list(
  version = manifest$version,
  locale = manifest$locale,
  platform = manifest$platform,
  metadata = canonical_object(manifest$metadata),
  packages = canonical_object(manifest$packages),
  users = canonical_object(manifest$users)
)
manifest_contract_json <- as.character(jsonlite::toJSON(
  manifest_contract, auto_unbox = TRUE, pretty = FALSE, null = "null", na = "null"
))
manifest_contract_sha256 <- digest::digest(
  paste(MANIFEST_DOMAIN, manifest_contract_json, sep = "\n"),
  algo = "sha256", serialize = FALSE
)

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

# Keep this allowlist byte-for-byte equivalent to the deployed Shiny files in
# write_manifest.R, then add only the public Pages cover family. Relative paths
# are part of the identity, so moving a byte is a payload change even if its file
# contents happen to match another asset.
files_below <- function(directory, pattern = NULL, recursive = TRUE) {
  base <- file.path(ROOT, directory)
  if (!dir.exists(base)) {
    stop("Release payload directory is missing: ", directory, call. = FALSE)
  }
  inside <- list.files(
    base, pattern = pattern, recursive = recursive, full.names = FALSE,
    all.files = FALSE, include.dirs = FALSE, no.. = TRUE
  )
  file.path(directory, inside)
}

payload_paths <- c(
  "global.R", "ui.R", "server.R",
  files_below("R", pattern = "[.]R$", recursive = FALSE),
  files_below("www"),
  files_below("data"),
  files_below("data-sample"),
  "docs/index.html", "docs/og-image-v2.png",
  files_below("docs/assets")
)
payload_paths <- gsub("\\\\", "/", payload_paths)
payload_paths <- setdiff(payload_paths, c("data/release_stamp.json", "docs/release.json", "manifest.json"))
payload_paths <- sort(unique(payload_paths), method = "radix")

unsafe_payload_path <- grepl("\r", payload_paths, fixed = TRUE) |
  grepl("\n", payload_paths, fixed = TRUE) |
  grepl("\t", payload_paths, fixed = TRUE)
if (!length(payload_paths) || any(unsafe_payload_path)) {
  stop("Release payload paths are empty or contain an unsafe separator.", call. = FALSE)
}
absolute_payload_paths <- file.path(ROOT, payload_paths)
missing_payload <- payload_paths[
  !file.exists(absolute_payload_paths) |
    dir.exists(absolute_payload_paths) |
    is.na(file.info(absolute_payload_paths)$size)
]
if (length(missing_payload)) {
  stop("Release payload is missing required file(s): ",
       paste(missing_payload, collapse = ", "), call. = FALSE)
}
symlink_payload <- payload_paths[nzchar(Sys.readlink(absolute_payload_paths))]
if (length(symlink_payload)) {
  stop("Release payload must not contain symbolic links: ",
       paste(symlink_payload, collapse = ", "), call. = FALSE)
}

payload_entries <- paste(
  payload_paths,
  vapply(absolute_payload_paths, sha256_file, character(1)),
  sep = "\t"
)
payload_material <- paste0(paste(payload_entries, collapse = "\n"), "\n")
payload_sha256 <- digest::digest(payload_material, algo = "sha256", serialize = FALSE)
source_sha256 <- sha256_file(source_path)
environment_sha256 <- sha256_file(environment_path)

identity_material <- paste(
  DOMAIN, APP_ID, PRODUCT, RELEASE, DOI,
  source_sha256, environment_sha256, payload_sha256, manifest_contract_sha256,
  sep = "\n"
)
release_id <- paste0(
  "sha256:",
  digest::digest(identity_material, algo = "sha256", serialize = FALSE)
)

stamp <- list(
  schema_version = 3L,
  app_id = APP_ID,
  product = PRODUCT,
  release = RELEASE,
  doi = DOI,
  source_receipt_sha256 = source_sha256,
  environment_receipt_sha256 = environment_sha256,
  payload_sha256 = payload_sha256,
  manifest_contract_sha256 = manifest_contract_sha256,
  release_id = release_id
)
canonical <- paste0(
  jsonlite::toJSON(stamp, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  "\n"
)

read_exact <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  size <- file.info(path)$size
  if (is.na(size) || size <= 0) return(NA_character_)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  rawToChar(readBin(con, what = "raw", n = size))
}
write_exact <- function(path, contents) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(contents), con)
}

if (identical(MODE, "write")) {
  write_exact(stamp_path, canonical)
  if (WRITE_PAGES) write_exact(pages_path, canonical)
} else {
  if (!identical(read_exact(stamp_path), canonical)) {
    stop("Candidate release stamp does not match its payload and manifest contract.", call. = FALSE)
  }
  if (WRITE_PAGES && !identical(read_exact(pages_path), canonical)) {
    stop("Pages release receipt does not match the exact release contract.", call. = FALSE)
  }
}

if (WRITE_PAGES && !identical(read_exact(pages_path), read_exact(stamp_path))) {
  stop("Pages release receipt is not byte-identical to the runtime stamp.", call. = FALSE)
}

cat(sprintf(
  "OK: %s exact release instance %s covers %d payload files plus the manifest contract (%s mode).\n",
  RELEASE, release_id, length(payload_paths), MODE
))
