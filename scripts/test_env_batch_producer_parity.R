#!/usr/bin/env Rscript
# Full producer parity using pure synthetic RELEASE-2026-shaped product lists.

Sys.setenv(BIRD_ENV_PRODUCER_LIBRARY = "1")
source("scripts/refresh_env_data.R")

check <- function(condition, label) {
  if (!isTRUE(condition)) stop("ENV PRODUCER PARITY FAILED: ", label, call. = FALSE)
}

file_bytes <- function(path) {
  readBin(path, what = "raw", n = unname(file.info(path)$size))
}

contains_raw_plant_id <- function(object) {
  values <- unlist(lapply(object, function(value) {
    if (is.list(value) || is.data.frame(value)) {
      unlist(value, recursive = TRUE, use.names = FALSE)
    } else value
  }), recursive = TRUE, use.names = FALSE)
  any(grepl("RAW-PLANT-", as.character(values), fixed = TRUE), na.rm = TRUE)
}

release_sites <- as.character(neon_sites$site)
check(length(release_sites) == 47L && identical(tail(release_sites, 1), "PUUM"),
      "fixture uses the exact 47-site release roster")

loadByProduct <- function(...) NULL
no_files_result <- load_environment_batch("DP1.00044.001", release_sites[1:4])
check(identical(names(no_files_result), "validation_no_files") &&
        grepl("returned no files", no_files_result$validation_no_files$status,
              fixed = TRUE),
      "a successful loadByProduct no-files NULL becomes explicit local metadata")
loadByProduct <- function(...) stop("synthetic network failure", call. = FALSE)
fetch_error <- tryCatch({
  load_environment_batch("DP1.00044.001", release_sites[1:4])
  NULL
}, error = identity)
check(inherits(fetch_error, "error") &&
        grepl("batch fetch failed", conditionMessage(fetch_error), fixed = TRUE),
      "a batch exception stays fatal and cannot become unsupported precipitation")
rm(loadByProduct, no_files_result, fetch_error)

invalid_precip_site <- release_sites[[21L]]

half_hours <- seq(
  as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
  as.POSIXct("2020-01-31 23:30:00", tz = "UTC"), by = "30 min"
)
hours <- seq(
  as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
  as.POSIXct("2020-01-31 23:00:00", tz = "UTC"), by = "hour"
)
days <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")

metadata <- list(
  temperature = data.frame(fieldName = c("siteID", "tempSingleMean"),
                           units = c(NA_character_, "C"), stringsAsFactors = FALSE),
  precipitation = data.frame(fieldName = c("siteID", "precipBulk"),
                             units = c(NA_character_, "mm"), stringsAsFactors = FALSE),
  phenology = data.frame(fieldName = c("siteID", "individualID"),
                         units = NA_character_, stringsAsFactors = FALSE)
)

fixture_metadata <- function(dpnum, variables) {
  out <- stats::setNames(list(
    data.frame(V1 = paste("Synthetic readme", dpnum), stringsAsFactors = FALSE),
    variables,
    data.frame(check = "fixture", status = "valid", stringsAsFactors = FALSE),
    data.frame(fieldName = "fixtureCode", code = "A", stringsAsFactors = FALSE),
    data.frame(issue = character(), stringsAsFactors = FALSE),
    paste("Synthetic RELEASE-2026 citation", dpnum)
  ), c(
    paste0("readme_", dpnum), paste0("variables_", dpnum),
    paste0("validation_", dpnum), paste0("categoricalCodes_", dpnum),
    paste0("issueLog_", dpnum), paste0("citation_", dpnum, "_RELEASE-2026")
  ))
  out[sort(names(out), method = "radix")]
}

fixture_metadata_names <- unique(c(
  names(fixture_metadata("00002", metadata$temperature)),
  names(fixture_metadata("00044", metadata$precipitation)),
  names(fixture_metadata("10055", metadata$phenology))
))

temperature_table <- function(site) {
  site_index <- match(site, release_sites)
  primary_horizontal <- if (identical(site, release_sites[[2L]])) "B" else "A"
  primary_vertical <- if (identical(site, release_sites[[2L]])) 1 else
    if (identical(site, release_sites[[1L]])) 1 else 2
  primary <- data.frame(
    siteID = site,
    startDateTime = format(half_hours, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    horizontalPosition = primary_horizontal,
    verticalPosition = primary_vertical,
    tempSingleMean = 5 + site_index,
    tempSingleMinimum = 4 + site_index,
    tempSingleMaximum = 6 + site_index,
    finalQF = 0L,
    stringsAsFactors = FALSE
  )
  if (!site %in% release_sites[1:2]) return(primary)
  secondary <- primary
  secondary$horizontalPosition <- if (identical(site, release_sites[[1L]])) "B" else "A"
  secondary$verticalPosition <- if (identical(site, release_sites[[1L]])) 2 else 3
  secondary$tempSingleMean <- secondary$tempSingleMean + 100
  secondary$tempSingleMinimum <- secondary$tempSingleMinimum + 100
  secondary$tempSingleMaximum <- secondary$tempSingleMaximum + 100
  rbind(primary, secondary)
}

precip_table <- function(site) {
  site_index <- match(site, release_sites)
  if (site %in% release_sites[1:7]) {
    return(list(name = "WEIPRE_daily", table = data.frame(
      siteID = site, startDateTime = format(days), horizontalPosition = "P1",
      precipBulk = rep(site_index / 100, length(days)), finalQF = 0L,
      stringsAsFactors = FALSE
    )))
  }
  if (site %in% release_sites[8:14]) {
    return(list(name = "PRIPRE_30min", table = data.frame(
      siteID = site,
      startDateTime = format(half_hours, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      horizontalPosition = "P1", precipBulk = rep(site_index / 1000, length(half_hours)),
      finalQF = 0L, stringsAsFactors = FALSE
    )))
  }
  if (site %in% release_sites[15:20]) {
    return(list(name = "SECPRE_60min", table = data.frame(
      siteID = site,
      startDateTime = format(hours, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      horizontalPosition = "P1", precipBulk = rep(site_index / 500, length(hours)),
      finalQF = 0L, stringsAsFactors = FALSE
    )))
  }
  if (identical(site, invalid_precip_site)) {
    return(list(name = "WEIPRE_daily", table = data.frame(
      siteID = site, startDateTime = format(days), horizontalPosition = "P1",
      precipBulk = rep(1, length(days)), finalQF = 1L, stringsAsFactors = FALSE
    )))
  }
  NULL
}

phenology_table <- function(site) {
  individuals <- sprintf("RAW-PLANT-%s-%02d", site, seq_len(5L))
  phases <- c("Open flowers", "Breaking leaf buds", "Fruits")
  rows <- expand.grid(individualID = individuals, phenophaseName = phases,
                      stringsAsFactors = FALSE)
  rows$siteID <- site
  rows$startDateTime <- "2020-01-15T12:00:00Z"
  rows$phenophaseStatus <- ifelse(seq_len(nrow(rows)) %% 3L == 0L, "no", "yes")
  rows[c("siteID", "startDateTime", "phenophaseName",
         "phenophaseStatus", "individualID")]
}

direct_products <- stats::setNames(lapply(release_sites, function(site) {
  precipitation <- precip_table(site)
  temperature_result <- c(
    fixture_metadata("00002", metadata$temperature),
    list(SAAT_30min = temperature_table(site))
  )
  temperature_result <- temperature_result[sort(names(temperature_result), method = "radix")]
  precipitation_result <- if (is.null(precipitation)) list() else {
    result <- c(
      fixture_metadata("00044", metadata$precipitation),
      stats::setNames(list(precipitation$table), precipitation$name)
    )
    result[sort(names(result), method = "radix")]
  }
  phenology_result <- c(
    fixture_metadata("10055", metadata$phenology),
    list(phe_statusintensity = phenology_table(site))
  )
  phenology_result <- phenology_result[sort(names(phenology_result), method = "radix")]
  list(
    "DP1.00002.001" = temperature_result,
    "DP1.00044.001" = precipitation_result,
    "DP1.10055.001" = phenology_result
  )
}), release_sites)

direct_loader <- function(dpID, site, timeIndex = NULL) {
  if (!identical(timeIndex, if (identical(dpID, "DP1.00002.001")) 30 else NULL))
    stop("fixture received an unexpected timeIndex", call. = FALSE)
  direct_products[[site]][[dpID]]
}

combine_chunk <- function(dpID, chunk_sites, reverse_rows = FALSE) {
  names_in_chunk <- unique(unlist(lapply(chunk_sites, function(site)
    names(direct_products[[site]][[dpID]])), use.names = FALSE))
  # A successful no-files response still carries auditable product metadata.
  if (!length(names_in_chunk) && identical(dpID, "DP1.00044.001"))
    return(list(validation_no_files = data.frame(
      dpID = dpID, requested_sites = paste(chunk_sites, collapse = ","),
      status = "successful loadByProduct query returned no files",
      stringsAsFactors = FALSE
    )))
  ordered_names <- sort(as.character(names_in_chunk), method = "radix")
  out <- list()
  for (name in ordered_names) {
    values <- lapply(chunk_sites, function(site) direct_products[[site]][[dpID]][[name]])
    values <- Filter(Negate(is.null), values)
    # This fixture allowlist is intentionally independent of the production
    # metadata predicate so a production classification regression is visible.
    if (name %in% fixture_metadata_names) {
      out[[name]] <- values[[1L]]
    } else {
      table <- do.call(rbind, values)
      rownames(table) <- NULL
      if (reverse_rows) table <- table[rev(seq_len(nrow(table))), , drop = FALSE]
      out[[name]] <- table
    }
  }
  out
}

write_site_outputs <- function(env, output_root, evidence_root, site) {
  public_path <- file.path(output_root, paste0(site, ".rds"))
  evidence_path <- file.path(evidence_root, paste0(site, ".rds"))
  saveRDS(tibble::as_tibble(env), public_path, compress = "xz", version = 3)
  saveRDS(attr(env, "validation_evidence"), evidence_path, compress = "xz", version = 3)
  support <- attr(env, "support")
  list(
    site = site,
    rows = nrow(env),
    month_min = min(env$ym),
    month_max = max(env$ym),
    air_temperature_supported = isTRUE(support$air_temperature),
    precipitation_supported = isTRUE(support$precipitation),
    plant_phenology_supported = isTRUE(support$plant_phenology),
    source_tables = support$source_tables,
    stream_selection = support$stream_selection,
    file = basename(public_path),
    sha256 = digest::digest(public_path, algo = "sha256", file = TRUE, serialize = FALSE),
    bytes = unname(file.info(public_path)$size),
    evidence_file = basename(evidence_path),
    evidence_sha256 = digest::digest(
      evidence_path, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    evidence_bytes = unname(file.info(evidence_path)$size)
  )
}

write_fixture_receipt <- function(records, path) {
  temp_n <- sum(vapply(records, function(x) x$air_temperature_supported, logical(1)))
  precip_n <- sum(vapply(records, function(x) x$precipitation_supported, logical(1)))
  pheno_n <- sum(vapply(records, function(x) x$plant_phenology_supported, logical(1)))
  receipt <- list(
    schema_version = 2L,
    release = RELEASE,
    window = list(start_month = start_d, end_month = end_d),
    retrieval = list(
      tool = "neonUtilities::loadByProduct", package = "basic",
      neonUtilities_version = "fixture",
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      token_required = TRUE
    ),
    products = list(
      air_temperature = list(id = "DP1.00002.001", doi = "10.48443/p69b-5e50",
                             supported_sites = temp_n),
      precipitation = list(id = "DP1.00044.001", doi = "10.48443/v29j-eg88",
                           supported_sites = precip_n),
      plant_phenology = list(id = "DP1.10055.001", doi = "10.48443/p75s-7p48",
                             supported_sites = pheno_n)
    ),
    validation_evidence = list(
      schema_version = 1L, file_count = length(records),
      total_bytes = sum(vapply(records, function(record)
        as.numeric(record$evidence_bytes), numeric(1))),
      format = paste(
        "canonical privacy-minimized selected-stream rows plus digest-bound",
        "all-stream month support; validation-only; not deployed"
      )
    ),
    files = unname(records)
  )
  jsonlite::write_json(receipt, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(receipt)
}

run_fixture <- function(name, chunk_size = NULL, roster = release_sites,
                        reverse_batch_rows = FALSE) {
  root <- tempfile(paste0("env-producer-parity-", name, "-"))
  dir.create(file.path(root, "public"), recursive = TRUE)
  dir.create(file.path(root, "evidence"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  output_root <- file.path(root, "public")
  evidence_root <- file.path(root, "evidence")
  records <- stats::setNames(vector("list", length(release_sites)), release_sites)
  built <- stats::setNames(vector("list", length(release_sites)), release_sites)

  if (is.null(chunk_size)) {
    for (site in release_sites) {
      env <- build_site_env(site, direct_loader)
      built[[site]] <- env
      records[[site]] <- write_site_outputs(env, output_root, evidence_root, site)
    }
  } else {
    private_parent <- tempfile(paste0("env-producer-private-", name, "-"))
    dir.create(private_parent, mode = "0700")
    on.exit(unlink(private_parent, recursive = TRUE, force = TRUE), add = TRUE)
    shard_root <- env_batch_create_private_root(
      file.path(private_parent, "shards"), private_parent,
      forbidden_paths = c(output_root, evidence_root)
    )
    on.exit(env_batch_remove_root(shard_root), add = TRUE)
    chunks <- env_batch_chunks(roster, chunk_size)
    for (chunk_sites in chunks) {
      env_batch_write_product(
        combine_chunk("DP1.00044.001", chunk_sites, reverse_batch_rows),
        chunk_sites, shard_root, "DP1.00044.001",
        paste(
          "(WEIPRE|PRIPRE|SECPRE)_daily|wss_daily_precip|.*daily.*[Pp]recip",
          "(WEIPRE|PRIPRE|SECPRE)_(60|30)min|.*[Pp]recip", sep = "|"
        ), TRUE, "fixture precipitation"
      )
      env_batch_write_product(
        combine_chunk("DP1.00002.001", chunk_sites, reverse_batch_rows),
        chunk_sites, shard_root, "DP1.00002.001", "SAAT_30min|saat.*30",
        FALSE, "fixture temperature"
      )
      env_batch_write_product(
        combine_chunk("DP1.10055.001", chunk_sites, reverse_batch_rows),
        chunk_sites, shard_root, "DP1.10055.001", "phe_statusintensity",
        FALSE, "fixture phenology"
      )
      shard_loader <- function(dpID, site, timeIndex = NULL) {
        if (!identical(timeIndex, if (identical(dpID, "DP1.00002.001")) 30 else NULL))
          stop("fixture shard received an unexpected timeIndex", call. = FALSE)
        env_batch_read_product(shard_root, site, dpID)
      }
      for (site in chunk_sites) {
        env <- build_site_env(site, shard_loader)
        built[[site]] <- env
        records[[site]] <- write_site_outputs(env, output_root, evidence_root, site)
        env_batch_remove_site(shard_root, site)
      }
    }
    remaining <- setdiff(
      list.files(shard_root, all.files = TRUE, no.. = TRUE),
      basename(env_batch_root_marker(shard_root))
    )
    check(!length(remaining), paste(name, "consumes every private shard"))
  }

  receipt_path <- file.path(root, "environment_source_receipt.json")
  receipt <- write_fixture_receipt(records, receipt_path)
  snapshots <- list(
    public = stats::setNames(lapply(release_sites, function(site)
      file_bytes(file.path(output_root, paste0(site, ".rds")))), release_sites),
    evidence = stats::setNames(lapply(release_sites, function(site)
      file_bytes(file.path(evidence_root, paste0(site, ".rds")))), release_sites),
    receipt = file_bytes(receipt_path)
  )
  check(!any(vapply(built, function(env) contains_raw_plant_id(list(
    public = as.data.frame(env),
    evidence = attr(env, "validation_evidence")
  )), logical(1))),
        paste(name, "public/evidence objects contain no raw plant IDs"))
  list(built = built, records = records, receipt = receipt, snapshots = snapshots)
}

legacy <- run_fixture("legacy")
check(sum(vapply(legacy$records, function(x) x$air_temperature_supported, logical(1))) == 47L &&
        sum(vapply(legacy$records, function(x) x$precipitation_supported, logical(1))) == 20L &&
        sum(vapply(legacy$records, function(x) x$plant_phenology_supported, logical(1))) == 47L,
      "synthetic producer enforces exact 47/20/47 support")
check(identical(
  attr(legacy$built[[invalid_precip_site]], "support")$precipitation,
  FALSE
), "a published precipitation table with no viable finalQF stream is unsupported")
check(identical(
  attr(legacy$built[[release_sites[[8L]]]],
       "validation_evidence")$products$precipitation$selected_table,
  "PRIPRE_30min"
), "a site without daily data retains its 30-minute fallback")
check(identical(
  attr(legacy$built[[release_sites[[15L]]]],
       "validation_evidence")$products$precipitation$selected_table,
  "SECPRE_60min"
), "a site without daily/30-minute data retains its 60-minute fallback")
check(identical(
  attr(legacy$built[[release_sites[[1L]]]],
       "support")$stream_selection$air_temperature$signature,
  "A|1"
) && identical(
  attr(legacy$built[[release_sites[[2L]]]],
       "support")$stream_selection$air_temperature$signature,
  "B|1"
), "temperature tower selection remains site-local across shared signatures")
check(all(vapply(release_sites, function(site) {
  evidence <- attr(legacy$built[[site]], "validation_evidence")
  identical(evidence$products$plant_phenology$source_rows, 15L) &&
    all(grepl("^plant-sha256:[0-9a-f]{64}$",
              evidence$products$plant_phenology$rows$individualID))
}, logical(1))), "phenology source_rows and pseudonyms remain exact per site")

modes <- list(
  chunk_1 = list(size = 1L, roster = release_sites, reverse = FALSE),
  chunk_4 = list(size = 4L, roster = release_sites, reverse = FALSE),
  chunk_8 = list(size = 8L, roster = release_sites, reverse = FALSE),
  chunk_4_permuted = list(size = 4L, roster = rev(release_sites), reverse = TRUE)
)
for (mode_name in names(modes)) {
  mode <- modes[[mode_name]]
  actual <- run_fixture(mode_name, mode$size, mode$roster, mode$reverse)
  for (site in release_sites) {
    check(identical(actual$built[[site]], legacy$built[[site]]),
          paste(mode_name, site, "producer object is direct-loader identical"))
    check(identical(actual$snapshots$public[[site]], legacy$snapshots$public[[site]]),
          paste(mode_name, site, "public RDS bytes are identical"))
    check(identical(actual$snapshots$evidence[[site]], legacy$snapshots$evidence[[site]]),
          paste(mode_name, site, "evidence RDS bytes are identical"))
  }
  check(identical(actual$snapshots$receipt, legacy$snapshots$receipt),
        paste(mode_name, "receipt JSON bytes are identical"))
}

cat(paste(
  "OK: full 47-site producer direct/sharded parity passed for chunk sizes 1/4/8,",
  "permuted order, 47/20/47 support, fallbacks, stream locality, and privacy.\n"
))
