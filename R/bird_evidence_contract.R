# Privacy-safe cross-job evidence contract for DP1.10003.001 RELEASE-2026.
#
# The producer may inspect the complete loadByProduct() result in job-local
# storage. Only this exact two-table scientific projection may cross a job or be
# uploaded as an Actions artifact.

BIRD_EVIDENCE_SCHEMA_VERSION <- 2L
BIRD_EVIDENCE_TABLE_COLUMNS <- list(
  brd_perpoint = sort(c(
    "boutNumber", "decimalLatitude", "decimalLongitude",
    "endCloudCoverPercentage", "eventID",
    "kmPerHourObservedWindSpeed", "nlcdClass", "observedAirTemp",
    "observedHabitat", "plotID", "pointID", "release",
    "samplingImpractical", "samplingProtocolVersion", "siteID",
    "startCloudCoverPercentage", "startDate"
  ), method = "radix"),
  brd_countdata = sort(c(
    "boutNumber", "clusterSize", "detectionMethod", "eventID",
    "observerDistance", "plotID", "pointCountMinute", "pointID", "release",
    "scientificName", "sexOrAge", "siteID", "startDate", "taxonID",
    "taxonRank", "vernacularName"
  ), method = "radix")
)

BIRD_EVIDENCE_PROHIBITED_NAMES <- c(
  "brdpersonnel", "detectionuid", "email", "emailaddress", "firstname",
  "lastname", "measuredby", "observeremail", "observerid", "personnelid",
  "samplingimpracticalremarks", "sourcerow", "uid", "visituid"
)
BIRD_EVIDENCE_EMAIL_PATTERN <-
  "[A-Z0-9._%+\\-]+@[A-Z0-9.\\-]+\\.[A-Z]{2,}"

bird_evidence_fail <- function(...) stop(paste0(...), call. = FALSE)

bird_canonical_table <- function(x) {
  if (!is.data.frame(x)) bird_evidence_fail("Evidence source is not a data frame.")
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (name in names(out)) {
    value <- out[[name]]
    if (inherits(value, "POSIXt"))
      value <- format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    else if (inherits(value, "Date")) value <- format(value, "%Y-%m-%d")
    else if (is.factor(value)) value <- as.character(value)
    if (!is.atomic(value))
      bird_evidence_fail("Evidence source table contains a non-atomic column: ", name, ".")
    attributes(value) <- attributes(value)[intersect(
      names(attributes(value)), c("names", "dim", "dimnames"))]
    out[[name]] <- value
  }
  out <- out[, sort(names(out), method = "radix"), drop = FALSE]
  if (nrow(out) > 1L && ncol(out)) {
    ord <- do.call(order, c(unname(out), list(na.last = TRUE, method = "radix")))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# Canonicalize the complete producer object before hashing/saving. The digest
# binds every returned table/value, including material later discarded by the
# privacy projection, without uploading that material itself.
bird_canonical_source <- function(x) {
  if (is.data.frame(x)) return(bird_canonical_table(x))
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.factor(x)) return(as.character(x))
  if (is.list(x)) {
    out <- lapply(x, bird_canonical_source)
    nm <- names(out)
    if (!is.null(nm)) {
      if (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))
        bird_evidence_fail("Full source object has missing or duplicate list names.")
      out <- out[order(nm, method = "radix")]
    }
    return(out)
  }
  if (!is.atomic(x))
    bird_evidence_fail("Full source object contains an unsupported value class: ",
                       paste(class(x), collapse = "/"), ".")
  attributes(x) <- attributes(x)[intersect(
    names(attributes(x)), c("names", "dim", "dimnames"))]
  if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  x
}

bird_full_source_sha256 <- function(x) digest::digest(
  bird_canonical_source(x), algo = "sha256", serialize = TRUE,
  serializeVersion = 3)

bird_evidence_projection <- function(x) {
  if (!is.list(x)) bird_evidence_fail("Bird source evidence must be a named list.")
  missing_tables <- setdiff(names(BIRD_EVIDENCE_TABLE_COLUMNS), names(x))
  if (length(missing_tables))
    bird_evidence_fail("Bird source evidence lacks required table(s): ",
                       paste(missing_tables, collapse = ", "), ".")
  out <- lapply(names(BIRD_EVIDENCE_TABLE_COLUMNS), function(table_name) {
    table <- bird_canonical_table(x[[table_name]])
    allowed <- BIRD_EVIDENCE_TABLE_COLUMNS[[table_name]]
    missing <- setdiff(allowed, names(table))
    if (length(missing))
      bird_evidence_fail(table_name, " lacks evidence field(s): ",
                         paste(missing, collapse = ", "), ".")
    table[, allowed, drop = FALSE]
  })
  names(out) <- names(BIRD_EVIDENCE_TABLE_COLUMNS)
  out
}

bird_evidence_projection_sha256 <- function(x) digest::digest(
  bird_evidence_projection(x), algo = "sha256", serialize = TRUE,
  serializeVersion = 3)

bird_normalize_evidence_name <- function(x)
  tolower(gsub("[^A-Za-z0-9]+", "", as.character(x)))

bird_assert_no_pii <- function(x, label = "evidence") {
  walk <- function(value, path) {
    nm <- names(value)
    if (!is.null(nm)) {
      normalized <- bird_normalize_evidence_name(nm)
      bad <- normalized %in% BIRD_EVIDENCE_PROHIBITED_NAMES
      if (any(bad))
        bird_evidence_fail(label, " exposes prohibited name(s) at ", path, ": ",
                           paste(nm[bad], collapse = ", "), ".")
    }
    if (is.character(value) || is.factor(value)) {
      text <- as.character(value)
      if (any(grepl(BIRD_EVIDENCE_EMAIL_PATTERN, text, ignore.case = TRUE,
                    perl = TRUE), na.rm = TRUE))
        bird_evidence_fail(label, " contains an email-like value at ", path, ".")
      return(invisible(TRUE))
    }
    if (is.data.frame(value) || is.list(value)) {
      for (i in seq_along(value)) {
        child <- if (!is.null(names(value)) && nzchar(names(value)[[i]]))
          paste0(path, "$", names(value)[[i]]) else paste0(path, "[[", i, "]]" )
        walk(value[[i]], child)
      }
    }
    invisible(TRUE)
  }
  walk(x, label)
  invisible(TRUE)
}

bird_assert_evidence_projection <- function(x, label = "bird evidence") {
  if (!is.list(x) || !identical(names(x), names(BIRD_EVIDENCE_TABLE_COLUMNS)))
    bird_evidence_fail(label, " must contain only the exact evidence tables: ",
                       paste(names(BIRD_EVIDENCE_TABLE_COLUMNS), collapse = ", "), ".")
  for (table_name in names(BIRD_EVIDENCE_TABLE_COLUMNS)) {
    table <- x[[table_name]]
    if (!is.data.frame(table) ||
        !identical(names(table), BIRD_EVIDENCE_TABLE_COLUMNS[[table_name]]))
      bird_evidence_fail(label, " table ", table_name,
                         " differs from the exact evidence allowlist.")
  }
  canonical <- bird_evidence_projection(x)
  if (!identical(x, canonical))
    bird_evidence_fail(label, " is not in deterministic canonical row/column order.")
  bird_assert_no_pii(x, label)
  invisible(TRUE)
}

bird_assert_receipt_evidence_contract <- function(receipt) {
  if (!identical(as.integer(receipt$schema_version), 3L))
    bird_evidence_fail("Bird source receipt schema must be 3.")
  contract <- receipt$evidence_projection
  if (!is.list(contract) ||
      !identical(as.integer(contract$schema_version), BIRD_EVIDENCE_SCHEMA_VERSION) ||
      !is.list(contract$tables) ||
      !identical(names(contract$tables), names(BIRD_EVIDENCE_TABLE_COLUMNS)))
    bird_evidence_fail("Bird source receipt lacks the evidence projection contract.")
  for (table_name in names(BIRD_EVIDENCE_TABLE_COLUMNS)) {
    actual <- as.character(unlist(contract$tables[[table_name]], use.names = FALSE))
    if (!identical(actual, BIRD_EVIDENCE_TABLE_COLUMNS[[table_name]]))
      bird_evidence_fail("Bird source receipt evidence allowlist differs for ",
                         table_name, ".")
  }
  if (!identical(as.character(contract$privacy),
                 "no observer identity, personnel table, free-text sampling remarks, or source UIDs"))
    bird_evidence_fail("Bird source receipt privacy declaration differs.")
  invisible(TRUE)
}

bird_receipt_records <- function(receipt) {
  records <- receipt$files
  if (!is.data.frame(records))
    bird_evidence_fail("Bird source receipt files must simplify to a data frame.")
  required <- c("site", "file", "full_source_content_sha256",
                "evidence_projection_sha256", "brd_perpoint_rows",
                "brd_countdata_rows")
  if (!identical(names(records), required))
    bird_evidence_fail("Bird source receipt file records differ from the exact schema-v3 fields.")
  if (anyDuplicated(as.character(records$site)) ||
      any(!grepl("^[0-9a-f]{64}$", as.character(records$full_source_content_sha256))) ||
      any(!grepl("^[0-9a-f]{64}$", as.character(records$evidence_projection_sha256))))
    bird_evidence_fail("Bird source receipt file identities are invalid or duplicated.")
  expected_file <- paste0(as.character(records$site), "_raw.rds")
  if (any(is.na(records$file)) || !identical(as.character(records$file), expected_file))
    bird_evidence_fail("Bird source receipt file names do not match their site records.")
  row_fields <- c("brd_perpoint_rows", "brd_countdata_rows")
  for (field in row_fields) {
    value <- suppressWarnings(as.numeric(records[[field]]))
    if (any(!is.finite(value) | value < 0 | value != as.integer(value)))
      bird_evidence_fail("Bird source receipt has an invalid ", field, " value.")
  }
  records
}

bird_scan_evidence_directory <- function(root, expected_sites, receipt) {
  if (!dir.exists(root)) bird_evidence_fail("Evidence artifact directory is missing: ", root, ".")
  bird_assert_receipt_evidence_contract(receipt)
  records <- bird_receipt_records(receipt)
  expected_sites <- sort(as.character(expected_sites), method = "radix")
  expected_files <- paste0(expected_sites, "_raw.rds")
  root_entries <- list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  root_files <- root_entries[!dir.exists(root_entries)]
  if (!identical(sort(basename(root_files), method = "radix"), expected_files))
    bird_evidence_fail("Evidence artifact root must contain exactly the canonical bird raw files; subdirectories may hold separately validated evidence.")
  if (length(Sys.readlink(root_entries)) && any(nzchar(Sys.readlink(root_entries))))
    bird_evidence_fail("Evidence artifact may not contain symbolic links.")
  for (site in expected_sites) {
    record_index <- match(site, as.character(records$site))
    if (is.na(record_index)) bird_evidence_fail(site, " has no source receipt record.")
    path <- file.path(root, paste0(site, "_raw.rds"))
    evidence <- readRDS(path)
    bird_assert_evidence_projection(evidence, paste0(site, " bird evidence"))
    actual <- bird_evidence_projection_sha256(evidence)
    expected <- as.character(records$evidence_projection_sha256[[record_index]])
    if (!identical(actual, expected))
      bird_evidence_fail(site, " evidence projection digest differs from its receipt.")
    if (nrow(evidence$brd_perpoint) !=
          as.integer(records$brd_perpoint_rows[[record_index]]) ||
        nrow(evidence$brd_countdata) !=
          as.integer(records$brd_countdata_rows[[record_index]]))
      bird_evidence_fail(site, " evidence projection row counts differ from its receipt.")
  }
  artifact_entries <- list.files(root, recursive = TRUE, full.names = TRUE,
                                 all.files = TRUE, no.. = TRUE,
                                 include.dirs = TRUE)
  if (any(nzchar(Sys.readlink(artifact_entries))))
    bird_evidence_fail("Evidence artifact may not contain symbolic links.")
  artifact_files <- artifact_entries
  artifact_files <- artifact_files[!dir.exists(artifact_files)]
  if (any(!grepl("[.]rds$", artifact_files, ignore.case = TRUE)))
    bird_evidence_fail("Evidence artifact contains a non-RDS file.")
  for (path in artifact_files) {
    value <- tryCatch(readRDS(path), error = function(error)
      bird_evidence_fail("Cannot read evidence artifact file ", path, ": ",
                         conditionMessage(error), "."))
    bird_assert_no_pii(value, paste0("artifact ", basename(path)))
  }
  invisible(length(artifact_files))
}
