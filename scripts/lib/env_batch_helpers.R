# Conservative, dependency-free build helpers for batching NEON environmental pulls.
#
# loadByProduct() may combine several requested sites into each returned table.
# These helpers split that result before any scientific table or stream selection,
# write one private shard per product/site, and fail closed on ambiguous scope.

env_batch_stop <- function(...) stop(..., call. = FALSE)

env_batch_validate_sites <- function(sites, label = "environment batch") {
  if (!is.character(sites) || !length(sites) || anyNA(sites) ||
      any(!nzchar(trimws(sites))) || any(sites != trimws(sites)) ||
      any(!grepl("^[A-Z]{4}$", sites)) || anyDuplicated(sites))
    env_batch_stop(label, " requires unique canonical four-letter site IDs.")
  sites
}

env_batch_chunk_size <- function(value = Sys.getenv("BIRD_ENV_BATCH_SIZE", "4"),
                                 site_count = 47L) {
  numeric_value <- suppressWarnings(as.numeric(value))
  integer_value <- suppressWarnings(as.integer(numeric_value))
  if (length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value != integer_value || integer_value < 1L ||
      integer_value > as.integer(site_count))
    env_batch_stop("BIRD_ENV_BATCH_SIZE must be one integer from 1 through ",
                   as.integer(site_count), ".")
  integer_value
}

env_batch_chunks <- function(sites, chunk_size = env_batch_chunk_size(site_count = length(sites))) {
  sites <- env_batch_validate_sites(sites)
  chunk_size <- env_batch_chunk_size(chunk_size, length(sites))
  unname(split(sites, ceiling(seq_along(sites) / chunk_size)))
}

env_batch_validate_result <- function(result, label) {
  if (is.null(result)) return(invisible(TRUE))
  if (is.data.frame(result) || !is.list(result))
    env_batch_stop(label, " did not return a top-level list of tables.")
  if (!length(result)) return(invisible(TRUE))
  table_names <- names(result)
  if (is.null(table_names) || length(table_names) != length(result) ||
      anyNA(table_names) || any(!nzchar(trimws(table_names))) ||
      any(table_names != trimws(table_names)) || anyDuplicated(table_names))
    env_batch_stop(label, " returned missing, blank, or duplicate table names.")
  invisible(TRUE)
}

env_batch_subset_table <- function(table, rows, label) {
  out <- table[rows, , drop = FALSE]
  custom_attributes <- setdiff(names(attributes(table)), c("names", "row.names", "class"))
  for (name in custom_attributes) attr(out, name) <- attr(table, name, exact = TRUE)
  if (!identical(class(out), class(table)) || !identical(names(out), names(table)))
    env_batch_stop(label, " changed table class or column order while isolating a site.")
  input_classes <- lapply(table, class)
  output_classes <- lapply(out, class)
  input_types <- vapply(table, typeof, character(1))
  output_types <- vapply(out, typeof, character(1))
  if (!identical(input_classes, output_classes) || !identical(input_types, output_types))
    env_batch_stop(label, " changed one or more column classes or types while isolating a site.")
  out
}

env_batch_restore_result_attributes <- function(shard, result) {
  result_attributes <- attributes(result)
  if (is.null(result_attributes)) return(shard)
  result_attributes$names <- names(shard)
  attributes(shard) <- result_attributes
  shard
}

env_batch_is_shared_metadata <- function(table_name) {
  grepl(
    "^(readme|variables|validation|categoricalCodes|issueLog|citation)(_|$)",
    table_name, ignore.case = TRUE, perl = TRUE
  )
}

env_batch_product_plan <- function(result, requested_sites, required_table_pattern,
                                   allow_unsupported, label) {
  requested_sites <- env_batch_validate_sites(requested_sites, label)
  if (!is.character(required_table_pattern) ||
      length(required_table_pattern) != 1L || is.na(required_table_pattern) ||
      !nzchar(required_table_pattern))
    env_batch_stop(label, " requires one nonblank required-table pattern.")
  if (!is.logical(allow_unsupported) || length(allow_unsupported) != 1L ||
      is.na(allow_unsupported))
    env_batch_stop(label, " allow_unsupported must be TRUE or FALSE.")
  if (is.null(result) || !length(result))
    env_batch_stop(label, " returned no auditable batch result.")
  env_batch_validate_result(result, label)
  required_support <- stats::setNames(rep(FALSE, length(requested_sites)), requested_sites)

  for (index in seq_along(result)) {
    table_name <- names(result)[[index]]
    object <- result[[index]]
    is_shared <- env_batch_is_shared_metadata(table_name)
    is_required <- !is_shared && grepl(
      required_table_pattern, table_name, ignore.case = TRUE, perl = TRUE
    )

    if (!is.data.frame(object)) {
      if (is_required)
        env_batch_stop(label, " required table ", table_name, " is not tabular.")
      if (!is_shared)
        env_batch_stop(label, " returned unknown non-site-bearing object ",
                       table_name, ".")
      next
    }
    if (anyDuplicated(names(object)))
      env_batch_stop(label, " table ", table_name, " has duplicate column names.")

    has_site_id <- "siteID" %in% names(object)
    if (is_required && !has_site_id)
      env_batch_stop(label, " required table ", table_name,
                     " has no exact siteID column.")
    if (!has_site_id) {
      if (!is_shared)
        env_batch_stop(label, " returned unknown non-site-bearing table ",
                       table_name, ".")
      next
    }

    raw_site <- as.character(object$siteID)
    if (length(raw_site) != nrow(object) || anyNA(raw_site) ||
        any(!nzchar(trimws(raw_site))) || any(raw_site != trimws(raw_site)))
      env_batch_stop(label, " table ", table_name,
                     " has blank, missing, or padded siteID values.")
    foreign <- sort(setdiff(unique(raw_site), requested_sites), method = "radix")
    if (length(foreign))
      env_batch_stop(label, " table ", table_name,
                     " contains unrequested siteID values: ",
                     paste(foreign, collapse = ", "), ".")
    if (is_required)
      required_support[unique(raw_site)] <- TRUE
  }

  missing_required <- names(required_support)[!required_support]
  if (length(missing_required) && !allow_unsupported)
    env_batch_stop(label, " lacks a required site-scoped table for: ",
                   paste(missing_required, collapse = ", "), ".")
  list(required_support = required_support, missing_required = missing_required)
}

env_batch_extract_site <- function(result, site, supported, label) {
  if (!isTRUE(supported)) return(list())
  shard <- list()
  for (index in seq_along(result)) {
    table_name <- names(result)[[index]]
    object <- result[[index]]
    if (is.data.frame(object) && "siteID" %in% names(object)) {
      rows <- which(as.character(object$siteID) == site)
      if (!length(rows)) next
      shard[[table_name]] <- env_batch_subset_table(
        object, rows, paste(label, table_name, site, sep = " / ")
      )
    } else {
      shard[[table_name]] <- object
    }
  }
  env_batch_restore_result_attributes(shard, result)
}

env_batch_split_result <- function(result, requested_sites, required_table_pattern,
                                   allow_unsupported = FALSE,
                                   label = "environment product") {
  requested_sites <- env_batch_validate_sites(requested_sites, label)
  if (!is.character(required_table_pattern) ||
      length(required_table_pattern) != 1L || is.na(required_table_pattern) ||
      !nzchar(required_table_pattern))
    env_batch_stop(label, " requires one nonblank required-table pattern.")
  if (!is.logical(allow_unsupported) || length(allow_unsupported) != 1L ||
      is.na(allow_unsupported))
    env_batch_stop(label, " allow_unsupported must be TRUE or FALSE.")

  plan <- env_batch_product_plan(
    result, requested_sites, required_table_pattern, allow_unsupported, label
  )
  shards <- stats::setNames(lapply(requested_sites, function(site) {
    env_batch_extract_site(result, site, plan$required_support[[site]], label)
  }), requested_sites)
  shards
}

env_batch_safe_component <- function(value, pattern, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl(pattern, value))
    env_batch_stop(label, " is not a safe path component.")
  value
}

env_batch_product_path <- function(shard_root, site, product_id) {
  site <- env_batch_safe_component(site, "^[A-Z]{4}$", "site ID")
  product_id <- env_batch_safe_component(product_id, "^DP1[.][0-9]{5}[.][0-9]{3}$",
                                         "product ID")
  file.path(shard_root, site, paste0(product_id, ".rds"))
}

env_batch_root_marker <- function(shard_root) {
  file.path(shard_root, ".bird-environment-shard-root")
}

env_batch_require_mode <- function(path, expected, label) {
  if (identical(Sys.info()[["sysname"]], "Windows")) return(invisible(TRUE))
  actual <- as.character(file.info(path)$mode)
  if (!identical(actual, expected))
    env_batch_stop(label, " permissions must be ", expected, "; found ", actual, ".")
  invisible(TRUE)
}

env_batch_audit_private_root <- function(shard_root) {
  if (!dir.exists(shard_root))
    env_batch_stop("Private shard root is missing: ", shard_root)
  root <- normalizePath(shard_root, winslash = "/", mustWork = TRUE)
  marker <- env_batch_root_marker(root)
  if (!file.exists(marker) || isTRUE(file.info(marker)$isdir))
    env_batch_stop("Private shard root has no ownership marker: ", root)
  identity <- readLines(marker, warn = FALSE)
  if (length(identity) != 3L ||
      !identical(identity[[1]], "NEON-Breeding-Birds environment shards v1") ||
      !startsWith(identity[[2]], "root=") || !startsWith(identity[[3]], "parent="))
    env_batch_stop("Private shard root ownership marker is malformed: ", root)
  recorded_root <- sub("^root=", "", identity[[2]])
  recorded_parent <- sub("^parent=", "", identity[[3]])
  if (!identical(recorded_root, root) ||
      !env_batch_is_within(root, recorded_parent) || identical(root, recorded_parent))
    env_batch_stop("Private shard root ownership marker does not match its path.")
  invisible(root)
}

env_batch_write_private_rds <- function(object, path) {
  if (file.exists(path)) env_batch_stop("Refusing to replace private shard: ", path)
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, mode = "0700", showWarnings = FALSE))
    env_batch_stop("Could not create private shard directory: ", directory)
  Sys.chmod(directory, mode = "0700")
  env_batch_require_mode(directory, "700", "Private shard directory")
  temporary <- tempfile(".partial-", tmpdir = directory, fileext = ".rds")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  # Shards live only until one chunk is consumed. Avoid expensive xz work that
  # would erase the runtime benefit of batching; the enclosing chunk bounds disk.
  saveRDS(object, temporary, compress = FALSE, version = 3)
  Sys.chmod(temporary, mode = "0600")
  env_batch_require_mode(temporary, "600", "Private shard partial file")
  if (!file.rename(temporary, path))
    env_batch_stop("Could not atomically publish private shard: ", path)
  Sys.chmod(path, mode = "0600")
  env_batch_require_mode(path, "600", "Private shard file")
  invisible(path)
}

env_batch_write_product <- function(result, requested_sites, shard_root, product_id,
                                    required_table_pattern, allow_unsupported = FALSE,
                                    label = product_id) {
  env_batch_audit_private_root(shard_root)
  requested_sites <- env_batch_validate_sites(requested_sites, label)
  plan <- env_batch_product_plan(
    result, requested_sites, required_table_pattern, allow_unsupported, label
  )
  paths <- stats::setNames(character(length(requested_sites)), requested_sites)
  for (site in requested_sites) {
    shard <- env_batch_extract_site(
      result, site, plan$required_support[[site]], label
    )
    path <- env_batch_product_path(shard_root, site, product_id)
    env_batch_write_private_rds(shard, path)
    paths[[site]] <- path
    rm(shard)
    invisible(gc(verbose = FALSE))
  }
  invisible(paths)
}

env_batch_audit_site_result <- function(result, site, label) {
  env_batch_validate_sites(site, label)
  env_batch_validate_result(result, label)
  if (is.null(result) || !length(result)) return(invisible(TRUE))
  for (index in seq_along(result)) {
    object <- result[[index]]
    if (!is.data.frame(object)) next
    if (anyDuplicated(names(object)))
      env_batch_stop(label, " table ", names(result)[[index]],
                     " has duplicate column names.")
    if (!"siteID" %in% names(object)) next
    raw_site <- as.character(object$siteID)
    if (anyNA(raw_site) || any(!nzchar(trimws(raw_site))) ||
        any(raw_site != site))
      env_batch_stop(label, " contains rows outside site ", site, ".")
  }
  invisible(TRUE)
}

env_batch_read_product <- function(shard_root, site, product_id) {
  env_batch_audit_private_root(shard_root)
  path <- env_batch_product_path(shard_root, site, product_id)
  if (!file.exists(path) || isTRUE(file.info(path)$isdir) || file.info(path)$size <= 0)
    env_batch_stop("Missing private environmental shard: ", path)
  env_batch_require_mode(path, "600", "Private shard file")
  result <- readRDS(path)
  env_batch_audit_site_result(result, site, paste(site, product_id))
  result
}

env_batch_normalize_new_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    env_batch_stop("Private shard root must be one nonblank path.")
  parent <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  file.path(parent, basename(path))
}

env_batch_is_within <- function(path, parent) {
  path <- sub("/+$", "", path)
  parent <- sub("/+$", "", parent)
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

env_batch_create_private_root <- function(path, allowed_parent,
                                          forbidden_paths = character()) {
  allowed_parent <- normalizePath(allowed_parent, winslash = "/", mustWork = TRUE)
  path <- env_batch_normalize_new_path(path)
  if (file.exists(path) || dir.exists(path))
    env_batch_stop("Private shard root must not already exist: ", path)
  if (!env_batch_is_within(path, allowed_parent) || identical(path, allowed_parent))
    env_batch_stop("Private shard root must be a new child of the job-local temp directory.")
  for (forbidden in forbidden_paths) {
    if (!nzchar(forbidden)) next
    forbidden_parent <- normalizePath(dirname(forbidden), winslash = "/", mustWork = TRUE)
    forbidden_path <- file.path(forbidden_parent, basename(forbidden))
    if (env_batch_is_within(path, forbidden_path) ||
        env_batch_is_within(forbidden_path, path))
      env_batch_stop("Private shard root overlaps a candidate or evidence path.")
  }
  if (!dir.create(path, recursive = FALSE, mode = "0700", showWarnings = FALSE))
    env_batch_stop("Could not create private shard root: ", path)
  complete <- FALSE
  on.exit(if (!complete && dir.exists(path))
    unlink(path, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.chmod(path, mode = "0700")
  env_batch_require_mode(path, "700", "Private shard root")
  marker <- env_batch_root_marker(path)
  writeLines(c(
    "NEON-Breeding-Birds environment shards v1",
    paste0("root=", path),
    paste0("parent=", allowed_parent)
  ), marker, useBytes = TRUE)
  Sys.chmod(marker, mode = "0600")
  env_batch_require_mode(marker, "600", "Private shard ownership marker")
  env_batch_audit_private_root(path)
  complete <- TRUE
  path
}

env_batch_remove_site <- function(shard_root, site) {
  env_batch_audit_private_root(shard_root)
  site <- env_batch_safe_component(site, "^[A-Z]{4}$", "site ID")
  path <- file.path(shard_root, site)
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  if (dir.exists(path) || file.exists(path))
    env_batch_stop("Could not remove private site shards: ", path)
  invisible(TRUE)
}

env_batch_remove_root <- function(shard_root) {
  if (!dir.exists(shard_root) && !file.exists(shard_root)) return(invisible(TRUE))
  env_batch_audit_private_root(shard_root)
  if (dir.exists(shard_root)) unlink(shard_root, recursive = TRUE, force = TRUE)
  if (dir.exists(shard_root) || file.exists(shard_root))
    env_batch_stop("Could not remove private shard root: ", shard_root)
  invisible(TRUE)
}
