#!/usr/bin/env Rscript
# Pure synthetic contracts for chunked, disk-sharded environmental retrieval.

source("scripts/lib/env_batch_helpers.R")

check <- function(condition, label) {
  if (!isTRUE(condition)) stop("ENV BATCH CHECK FAILED: ", label, call. = FALSE)
}

expect_error <- function(expr, pattern, label) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  check(inherits(error, "error") &&
          grepl(pattern, conditionMessage(error), ignore.case = TRUE), label)
}

file_bytes <- function(path) {
  size <- unname(file.info(path)$size)
  readBin(path, what = "raw", n = size)
}

sites <- c("ABCD", "EFGH", "IJKL", "MNOP")
check(identical(env_batch_chunk_size(), 4L), "default chunk size is four sites")
check(identical(env_batch_chunks(sites, 2L),
                list(c("ABCD", "EFGH"), c("IJKL", "MNOP"))),
      "chunk construction preserves canonical roster order")
expect_error(env_batch_chunk_size(0, length(sites)), "one integer", "zero chunk size fails")
expect_error(env_batch_chunk_size(2.5, length(sites)), "one integer", "fractional chunk size fails")
expect_error(env_batch_chunk_size(5, length(sites)), "one integer", "oversized chunk fails")
expect_error(env_batch_chunks(c("ABCD", "ABCD"), 1), "unique canonical",
             "duplicate requested sites fail")
expect_error(env_batch_chunks(c("ABCD", "bad"), 1), "unique canonical",
             "noncanonical requested sites fail")

temperature <- data.frame(
  siteID = c("EFGH", "ABCD", "MNOP", "IJKL", "ABCD", "EFGH"),
  startDateTime = as.POSIXct(
    c("2020-01-01 00:00:00", "2020-01-01 00:00:00", "2020-01-01 00:00:00",
      "2020-01-01 00:00:00", "2020-01-01 00:30:00", "2020-01-01 00:30:00"),
    tz = "UTC"
  ),
  verticalPosition = factor(c(2, 1, 1, 3, 1, 2), levels = 1:3),
  tempSingleMean = c(20, 10, NA_real_, 30, 11, 21),
  tempSingleMinimum = c(19, 9, NA_real_, 29, 10, 20),
  tempSingleMaximum = c(21, 11, NA_real_, 31, 12, 22),
  finalQF = as.integer(0),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
class(temperature) <- c("neon_fixture_table", "data.frame")
attr(temperature, "release_identity") <- "RELEASE-2026 fixture"
variables <- data.frame(fieldName = c("siteID", "tempSingleMean"),
                        units = c(NA_character_, "C"), stringsAsFactors = FALSE)
issue_log <- list(status = "fixture", rows = 0L)
temperature_result <- structure(
  list(variables = variables, SAAT_30min = temperature, issueLog = issue_log),
  class = c("neon_fixture_result", "list"),
  retrieval_identity = "synthetic-vector-call"
)

manual_site_result <- function(result, site) {
  out <- list()
  for (index in seq_along(result)) {
    object <- result[[index]]
    name <- names(result)[[index]]
    if (is.data.frame(object) && "siteID" %in% names(object)) {
      rows <- which(as.character(object$siteID) == site)
      if (!length(rows)) next
      piece <- object[rows, , drop = FALSE]
      for (attribute_name in setdiff(names(attributes(object)),
                                     c("names", "row.names", "class")))
        attr(piece, attribute_name) <- attr(object, attribute_name, exact = TRUE)
      out[[name]] <- piece
    } else {
      out[[name]] <- object
    }
  }
  attributes(out) <- within(attributes(result), names <- names(out))
  out
}

temperature_shards <- env_batch_split_result(
  temperature_result, sites, "SAAT_30min|saat.*30",
  allow_unsupported = FALSE, label = "fixture temperature"
)
for (site in sites) {
  expected <- manual_site_result(temperature_result, site)
  actual <- temperature_shards[[site]]
  check(identical(actual, expected), paste(site, "shard exactly matches the site-local result"))
  check(identical(names(actual), c("variables", "SAAT_30min", "issueLog")),
        paste(site, "retains exact table inventory and order"))
  check(identical(class(actual$SAAT_30min), class(temperature)) &&
          identical(lapply(actual$SAAT_30min, class), lapply(temperature, class)) &&
          identical(vapply(actual$SAAT_30min, typeof, character(1)),
                    vapply(temperature, typeof, character(1))) &&
          identical(attr(actual$SAAT_30min, "release_identity", exact = TRUE),
                    attr(temperature, "release_identity", exact = TRUE)),
        paste(site, "retains table, column, and metadata classes"))
  check(all(as.character(actual$SAAT_30min$siteID) == site),
        paste(site, "contains no foreign rows"))
}
check(is.numeric(temperature_shards$MNOP$SAAT_30min$tempSingleMean) &&
        is.na(temperature_shards$MNOP$SAAT_30min$tempSingleMean),
      "an all-NA site retains the batch-published numeric class")

# Same synthetic consumer on a legacy site-local list and a batch shard must
# yield byte-identical public/evidence files and consequently one receipt.
derive_site <- function(result, site) {
  table <- result$SAAT_30min
  list(
    public = data.frame(
      siteID = site,
      temp_c = mean(table$tempSingleMean, na.rm = TRUE),
      stringsAsFactors = FALSE
    ),
    evidence = list(
      site = site,
      source_tables = sort(names(result), method = "radix"),
      selected_table = "SAAT_30min",
      selected_rows = unclass(table)
    )
  )
}

parity_fixture <- function() {
  root <- tempfile("env-batch-parity-")
  dir.create(file.path(root, "legacy"), recursive = TRUE)
  dir.create(file.path(root, "batch"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  legacy_records <- list()
  batch_records <- list()
  for (site in sites) {
    legacy <- derive_site(manual_site_result(temperature_result, site), site)
    batched <- derive_site(temperature_shards[[site]], site)
    for (kind in c("public", "evidence")) {
      legacy_path <- file.path(root, "legacy", paste(site, kind, "rds", sep = "."))
      batch_path <- file.path(root, "batch", paste(site, kind, "rds", sep = "."))
      saveRDS(legacy[[kind]], legacy_path, compress = "xz", version = 3)
      saveRDS(batched[[kind]], batch_path, compress = "xz", version = 3)
      check(identical(file_bytes(legacy_path), file_bytes(batch_path)),
            paste(site, kind, "RDS bytes are legacy/batch identical"))
    }
    legacy_path <- file.path(root, "legacy", paste(site, "public", "rds", sep = "."))
    batch_path <- file.path(root, "batch", paste(site, "public", "rds", sep = "."))
    legacy_records[[site]] <- list(site = site, bytes = unname(file.info(legacy_path)$size),
                                   digest = unname(tools::md5sum(legacy_path)))
    batch_records[[site]] <- list(site = site, bytes = unname(file.info(batch_path)$size),
                                  digest = unname(tools::md5sum(batch_path)))
  }
  check(identical(legacy_records, batch_records),
        "site file sizes/digests and receipt material are legacy/batch identical")
  legacy_receipt <- file.path(root, "legacy", "receipt.rds")
  batch_receipt <- file.path(root, "batch", "receipt.rds")
  saveRDS(list(files = legacy_records), legacy_receipt, compress = "xz", version = 3)
  saveRDS(list(files = batch_records), batch_receipt, compress = "xz", version = 3)
  check(identical(file_bytes(legacy_receipt), file_bytes(batch_receipt)),
        "synthetic receipt bytes are legacy/batch identical")
}
parity_fixture()

# Precipitation may be absent. Supported sites retain only their site rows and
# shared metadata; unsupported sites receive a truly empty list so a global
# table name cannot masquerade as local support.
precip_daily <- data.frame(
  siteID = c("ABCD", "ABCD"), startDateTime = c("2020-01-01", "2020-01-02"),
  precipBulk = c(1, 2), finalQF = 0L, stringsAsFactors = FALSE
)
precip_30 <- data.frame(
  siteID = c("EFGH", "EFGH"),
  startDateTime = c("2020-01-01T00:00:00Z", "2020-01-01T00:30:00Z"),
  precipBulk = c(0.1, 0.2), finalQF = 0L, stringsAsFactors = FALSE
)
precip_result <- list(
  variables = data.frame(fieldName = "precipBulk", stringsAsFactors = FALSE),
  WEIPRE_daily = precip_daily,
  PRIPRE_30min = precip_30
)
precip_pattern <- paste(
  "(WEIPRE|PRIPRE|SECPRE)_daily|wss_daily_precip|.*daily.*[Pp]recip",
  "(WEIPRE|PRIPRE|SECPRE)_(60|30)min|.*[Pp]recip",
  sep = "|"
)
precip_shards <- env_batch_split_result(
  precip_result, sites, precip_pattern, allow_unsupported = TRUE,
  label = "fixture precipitation"
)
check(identical(names(precip_shards$ABCD), c("variables", "WEIPRE_daily")) &&
        identical(names(precip_shards$EFGH), c("variables", "PRIPRE_30min")),
      "daily and 30-minute fallback inventories remain site-specific")
check(identical(precip_shards$IJKL, list()) && identical(precip_shards$MNOP, list()),
      "unsupported precipitation sites have empty inventories")
empty_precip_result <- list(
  variables_precipitation = data.frame(fieldName = "precipBulk", stringsAsFactors = FALSE),
  WEIPRE_daily = data.frame(
    siteID = character(), startDateTime = character(), precipBulk = numeric(),
    finalQF = integer(), stringsAsFactors = FALSE
  )
)
check(identical(
  env_batch_split_result(empty_precip_result, sites, precip_pattern, TRUE,
                         "empty-table precipitation"),
  stats::setNames(rep(list(list()), length(sites)), sites)
), "an empty site-bearing table is omitted from every unsupported shard")

# Splitter failures are deliberately strict: a selected table must carry exact
# site scope, and every row/table name/column identity must be unambiguous.
foreign <- temperature_result
foreign$SAAT_30min$siteID[[1]] <- "WXYZ"
expect_error(
  env_batch_split_result(foreign, sites, "SAAT_30min", FALSE, "foreign fixture"),
  "unrequested siteID", "foreign rows fail closed"
)
blank <- temperature_result
blank$SAAT_30min$siteID[[1]] <- NA_character_
expect_error(
  env_batch_split_result(blank, sites, "SAAT_30min", FALSE, "blank fixture"),
  "blank, missing", "missing siteID fails closed"
)
padded <- temperature_result
padded$SAAT_30min$siteID[[1]] <- "EFGH "
expect_error(
  env_batch_split_result(padded, sites, "SAAT_30min", FALSE, "padded fixture"),
  "padded", "padded siteID fails closed"
)
missing_column <- temperature_result
missing_column$SAAT_30min$siteID <- NULL
expect_error(
  env_batch_split_result(missing_column, sites, "SAAT_30min", FALSE, "missing fixture"),
  "no exact siteID", "required table without siteID fails closed"
)
missing_site <- temperature_result
missing_site$SAAT_30min <- missing_site$SAAT_30min[
  missing_site$SAAT_30min$siteID != "MNOP", , drop = FALSE
]
expect_error(
  env_batch_split_result(missing_site, sites, "SAAT_30min", FALSE, "support fixture"),
  "MNOP", "required product missing one site fails closed"
)
duplicate_columns <- temperature_result
names(duplicate_columns$SAAT_30min)[[2]] <- "siteID"
expect_error(
  env_batch_split_result(duplicate_columns, sites, "SAAT_30min", FALSE,
                         "duplicate-column fixture"),
  "duplicate column", "duplicate fields fail closed"
)
duplicate_tables <- unclass(temperature_result)
names(duplicate_tables)[[2]] <- names(duplicate_tables)[[1]]
expect_error(
  env_batch_split_result(duplicate_tables, sites, "SAAT_30min", FALSE,
                         "duplicate-table fixture"),
  "duplicate table", "duplicate table names fail closed"
)
expect_error(
  env_batch_split_result(data.frame(x = 1), sites, "SAAT_30min", FALSE,
                         "non-list fixture"),
  "list of tables", "non-list batch result fails closed"
)
expect_error(
  env_batch_split_result(list(temperature), sites, "SAAT_30min", FALSE,
                         "unnamed fixture"),
  "table names", "unnamed batch result fails closed"
)
expect_error(
  env_batch_split_result(NULL, sites, "precip", TRUE, "empty precipitation"),
  "no auditable", "an optional null result fails closed rather than hiding a fetch error"
)
metadata_only <- list(variables_fixture = data.frame(
  fieldName = "precipBulk", stringsAsFactors = FALSE
))
check(identical(
  env_batch_split_result(metadata_only, sites, "precip", TRUE,
                         "unsupported precipitation"),
  stats::setNames(rep(list(list()), length(sites)), sites)
), "a successful metadata-only response becomes exact unsupported per-site shards")
unknown_metadata <- list(mystery = data.frame(field = "value", stringsAsFactors = FALSE))
expect_error(
  env_batch_split_result(unknown_metadata, sites, "precip", TRUE,
                         "unknown metadata fixture"),
  "unknown non-site-bearing table", "unknown shared tables fail closed"
)
expect_error(
  env_batch_split_result(list(mystery = list(secret = "raw")), sites, "precip", TRUE,
                         "unknown object fixture"),
  "unknown non-site-bearing object", "unknown shared objects fail closed"
)

private_shard_fixture <- function() {
  parent <- tempfile("env-batch-private-")
  dir.create(parent, mode = "0700")
  on.exit(unlink(parent, recursive = TRUE, force = TRUE), add = TRUE)
  candidate <- file.path(parent, "candidate")
  dir.create(candidate)
  root <- file.path(parent, "shards")
  root <- env_batch_create_private_root(root, parent, forbidden_paths = candidate)
  check(dir.exists(root), "private root is created inside the allowed job temp")
  if (!identical(Sys.info()[["sysname"]], "Windows"))
    check(identical(as.character(file.info(root)$mode), "700"),
          "private root permissions are 0700")

  env_batch_write_product(
    temperature_result, sites, root, "DP1.00002.001",
    "SAAT_30min|saat.*30", FALSE, "fixture temperature"
  )
  expect_error(
    env_batch_write_product(
      temperature_result, sites, root, "DP1.00002.001",
      "SAAT_30min|saat.*30", FALSE, "fixture overwrite"
    ),
    "refusing to replace", "an existing private shard cannot be overwritten"
  )
  for (site in sites) {
    path <- env_batch_product_path(root, site, "DP1.00002.001")
    check(file.exists(path) && identical(env_batch_read_product(
      root, site, "DP1.00002.001"
    ), temperature_shards[[site]]), paste(site, "private shard round-trips exactly"))
    if (!identical(Sys.info()[["sysname"]], "Windows"))
      check(identical(as.character(file.info(path)$mode), "600"),
            paste(site, "private shard permissions are 0600"))
  }

  tampered_path <- env_batch_product_path(root, "ABCD", "DP1.00002.001")
  tampered <- readRDS(tampered_path)
  tampered$SAAT_30min$siteID[[1]] <- "EFGH"
  saveRDS(tampered, tampered_path, compress = "xz", version = 3)
  expect_error(env_batch_read_product(root, "ABCD", "DP1.00002.001"),
               "outside site", "read-time isolation audit rejects a tampered shard")

  for (site in sites) env_batch_remove_site(root, site)
  check(identical(list.files(root, all.files = TRUE, no.. = TRUE),
                  ".bird-environment-shard-root"),
        "site shards are deleted immediately after consumption")
  env_batch_remove_root(root)
  check(!dir.exists(root), "private root is deleted after the chunk")

  expect_error(
    env_batch_create_private_root(candidate, parent, forbidden_paths = candidate),
    "already exist|overlaps", "a shard root cannot reuse a candidate path"
  )
  outside <- tempfile("env-batch-outside-", tmpdir = dirname(parent))
  expect_error(env_batch_create_private_root(outside, parent), "job-local temp",
               "a shard root outside the allowed parent fails closed")
}
private_shard_fixture()

cleanup_on_error_fixture <- function() {
  parent <- tempfile("env-batch-cleanup-")
  dir.create(parent)
  root_path <- file.path(parent, "shards")
  run <- function() {
    root <- env_batch_create_private_root(root_path, parent)
    on.exit(env_batch_remove_root(root), add = TRUE)
    env_batch_write_product(
      temperature_result, sites, root, "DP1.00002.001",
      "SAAT_30min", FALSE, "cleanup fixture"
    )
    stop("synthetic producer failure", call. = FALSE)
  }
  error <- tryCatch({ run(); NULL }, error = identity)
  check(inherits(error, "error") && !dir.exists(root_path),
        "function-level cleanup removes raw shards after producer failure")
  unlink(parent, recursive = TRUE, force = TRUE)
}
cleanup_on_error_fixture()

# The actual 47-site release roster forms 12 conservative chunks: eleven groups
# of four and one final group of three, with PUUM retained at the tail.
if (requireNamespace("tibble", quietly = TRUE)) {
  source("R/site_metadata.R")
  release_chunks <- env_batch_chunks(as.character(neon_sites$site), 4L)
  check(length(release_chunks) == 12L &&
          identical(vapply(release_chunks, length, integer(1)), c(rep(4L, 11L), 3L)) &&
          identical(tail(release_chunks[[12]], 1), "PUUM"),
        "47-site release roster yields deterministic 4-site chunks including PUUM")
}

cat("OK: chunked environmental splitter, parity, privacy, and negative fixtures passed.\n")
