#!/usr/bin/env Rscript
# Fail-closed verification of deploy file bytes, dependency provenance, and the
# schema-v3 release stamp's canonical non-file manifest contract.

suppressMessages({ library(jsonlite); library(digest) })

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
problems <- character(0)
note <- function(message) problems[[length(problems) + 1L]] <<- message

R_PLATFORM <- "4.5.2"
RSPM <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
RUNTIME <- c(
  "shiny", "bslib", "bsicons", "dplyr", "tidyr", "stringr", "tibble",
  "plotly", "leaflet", "DT", "shinyjs", "shinycssloaders", "RColorBrewer",
  "htmltools", "jsonlite", "digest"
)
GEO <- c(
  terra = "1.8-50", sf = "1.1-1", s2 = "1.1.11", units = "1.0-1",
  wk = "0.9.5", classInt = "0.4-11", raster = "3.6-32", sp = "2.2-1"
)
GEO_URLS <- c(
  terra = "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.8-50.tar.gz",
  sf = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/sf_1.1-1.tar.gz",
  s2 = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/s2_1.1.11.tar.gz",
  units = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/units_1.0-1.tar.gz",
  wk = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/wk_0.9.5.tar.gz",
  classInt = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/classInt_0.4-11.tar.gz",
  raster = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/raster_3.6-32.tar.gz",
  sp = "https://packagemanager.posit.co/cran/2026-07-15/src/contrib/sp_2.2-1.tar.gz"
)

runtime_files <- function(root) {
  if (!dir.exists(root)) return(character(0))
  paths <- list.files(
    root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  gsub("\\\\", "/", paths)
}

expected_files <- sort(unique(c(
  "global.R", "ui.R", "server.R",
  runtime_files("R"), runtime_files("www"), runtime_files("data"),
  runtime_files("data-sample")
)))
expected_files <- expected_files[file.exists(expected_files) & !dir.exists(expected_files)]

manifest <- tryCatch(
  jsonlite::fromJSON("manifest.json", simplifyVector = FALSE),
  error = function(error) error
)

if (inherits(manifest, "error")) {
  note(sprintf("manifest.json is missing or invalid JSON: %s", conditionMessage(manifest)))
} else {
  manifest_fields <- c("version", "locale", "platform", "metadata", "packages", "files", "users")
  exact_contract_fields <- length(names(manifest)) == length(manifest_fields) &&
    setequal(names(manifest), manifest_fields)
  if (!exact_contract_fields) {
    note("manifest top-level runtime/dependency contract fields are not exact")
  }
  files <- manifest$files
  if (is.null(files) || !is.list(files) || !length(files)) {
    note("manifest lists no runtime files")
  } else {
    manifest_files <- gsub("\\\\", "/", names(files))
    if (!"data/release_stamp.json" %in% manifest_files) {
      note("manifest does not deploy the deterministic release-instance stamp")
    }
    if (!identical(sort(manifest_files), expected_files)) {
      note(sprintf(
        "manifest file set mismatch: missing=[%s] extra=[%s]",
        paste(setdiff(expected_files, manifest_files), collapse = ","),
        paste(setdiff(manifest_files, expected_files), collapse = ",")
      ))
    }

    missing <- manifest_files[!file.exists(manifest_files)]
    if (length(missing)) note(sprintf("manifest references missing files: %s", paste(missing, collapse = ",")))
    present <- setdiff(manifest_files, missing)
    bad <- vapply(present, function(path) {
      item <- files[[match(path, manifest_files)]]
      expected <- tolower(as.character(item$checksum %||% ""))
      actual <- tolower(unname(tools::md5sum(path)))
      !nzchar(expected) || !identical(expected, actual)
    }, logical(1))
    if (any(bad)) {
      note(sprintf("manifest checksum mismatch: %s", paste(present[bad], collapse = ",")))
    }
  }

  if (!identical(as.character(manifest$platform %||% ""), R_PLATFORM)) {
    note(sprintf("manifest platform must be R %s", R_PLATFORM))
  }

  packages <- manifest$packages
  if (is.null(packages) || !is.list(packages) || !length(packages)) {
    note("manifest lists no packages")
  } else {
    keys <- names(packages)
    missing_runtime <- setdiff(RUNTIME, keys)
    if (length(missing_runtime)) {
      note(sprintf("manifest lacks runtime packages: %s", paste(missing_runtime, collapse = ",")))
    }
    leaked <- intersect(c("neonUtilities", "arrow"), keys)
    if (length(leaked)) {
      note(sprintf("manifest contains build-only packages: %s", paste(leaked, collapse = ",")))
    }
    if ("data.table" %in% keys && !"plotly" %in% keys) {
      note("manifest contains data.table without its legitimate plotly root")
    }

    for (pkg in keys) {
      item <- packages[[pkg]]
      description <- item$description %||% list()
      version <- as.character(description$Version %||% "")
      declared <- as.character(description$Package %||% "")
      source <- as.character(item$Source %||% "")
      repository <- as.character(item$Repository %||% "")
      if (!nzchar(version) || !identical(declared, pkg) || !identical(source, "CRAN")) {
        note(sprintf("manifest package identity invalid for %s", pkg))
      }

      if (pkg %in% names(GEO)) {
        expected_url <- unname(GEO_URLS[[pkg]])
        expected_ref <- paste0("url::", expected_url)
        if (!identical(version, unname(GEO[[pkg]])) ||
            !identical(repository, "https://cran.r-project.org") ||
            !identical(as.character(description$RemoteType %||% ""), "url") ||
            !identical(as.character(description$RemotePkgRef %||% ""), expected_ref) ||
            nzchar(as.character(description$Built %||% ""))) {
          note(sprintf("manifest geospatial provenance invalid for %s", pkg))
        }
      } else if (!identical(repository, RSPM)) {
        note(sprintf("manifest repository invalid for %s: %s", pkg, repository))
      }
    }

    for (pkg in names(GEO)) {
      if (!pkg %in% keys) note(sprintf("manifest is missing geospatial package %s", pkg))
    }
    if ("plotly" %in% keys &&
        !identical(as.character(packages$plotly$description$Version %||% ""), "4.12.0")) {
      note("manifest must pin plotly 4.12.0")
    }
  }

  stamp <- tryCatch(
    jsonlite::fromJSON("data/release_stamp.json", simplifyVector = FALSE),
    error = function(error) error
  )
  if (inherits(stamp, "error")) {
    note(sprintf("release stamp is missing or invalid JSON: %s", conditionMessage(stamp)))
  } else {
    stamp_fields <- c(
      "schema_version", "app_id", "product", "release", "doi",
      "source_receipt_sha256", "environment_receipt_sha256", "payload_sha256",
      "manifest_contract_sha256", "release_id"
    )
    if (!identical(names(stamp), stamp_fields) ||
        !identical(as.integer(stamp$schema_version), 3L)) {
      note("release stamp is not the exact schema-v3 contract")
    } else if (exact_contract_fields && is.list(packages) && length(packages)) {
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
        version = manifest$version,
        locale = manifest$locale,
        platform = manifest$platform,
        metadata = canonical_manifest_value(manifest$metadata),
        packages = canonical_manifest_value(manifest$packages),
        users = canonical_manifest_value(manifest$users)
      )
      manifest_contract_json <- as.character(jsonlite::toJSON(
        manifest_contract, auto_unbox = TRUE, pretty = FALSE, null = "null", na = "null"
      ))
      expected_contract_sha256 <- digest::digest(
        paste("neon-connect-manifest-contract-v1", manifest_contract_json, sep = "\n"),
        algo = "sha256", serialize = FALSE
      )
      if (!identical(as.character(stamp$manifest_contract_sha256),
                     expected_contract_sha256)) {
        note("release stamp does not bind the final manifest runtime/dependency contract")
      }
    }
  }
}

if (length(problems)) {
  for (problem in unique(problems)) {
    cat(sprintf("::error title=Breeding Birds manifest verification::%s\n", problem))
  }
  stop(sprintf(
    "Breeding Birds manifest verification FAILED with %d problem(s)",
    length(unique(problems))
  ), call. = FALSE)
}

cat(sprintf(
  "OK: exact manifest covers %d runtime files with pinned R and dependency provenance.\n",
  length(expected_files)
))
