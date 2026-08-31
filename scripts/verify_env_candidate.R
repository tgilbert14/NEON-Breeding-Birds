#!/usr/bin/env Rscript
# Independent oracle for RELEASE-2026 environmental context.
#
# This file intentionally does not source R/env_helpers.R, refresh_env_data.R,
# or any producer aggregation code. It reconstructs stream choice, quality
# filtering, monthly support, individual-month phenology, every public site
# bundle, and every environmental receipt fact from bound pre-aggregation
# evidence.
#
# To keep the cross-job artifact bounded, complete pre-aggregation rows are kept
# for each selected sensor stream. Unselected sensor streams are retained as
# canonical per-stream/month support counts plus content digests. Thus public
# values, selected-stream QF filtering, interval coverage, and phenology are
# row-recomputed; the existence/support ordering of unselected streams is
# digest-bound and summary-verified rather than interval-row-replayed.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })

ev_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

ev_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

ev_sha256 <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

ev_chr <- function(x) {
  if (is.null(x)) character() else unname(as.character(unlist(x, use.names = FALSE)))
}

ev_canonical_order <- function(tb) {
  if (is.null(tb) || !nrow(tb)) {
    rownames(tb) <- NULL
    return(tb)
  }
  keys <- lapply(tb, function(x) {
    key <- as.character(x)
    key[is.na(key)] <- "<NA>"
    key
  })
  idx <- do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
  out <- tb[idx, , drop = FALSE]
  rownames(out) <- NULL
  out
}

ev_days_in_month <- function(ym) vapply(ym, function(key) {
  first <- as.Date(paste0(key, "-01"))
  as.integer(seq(first, by = "month", length.out = 2L)[[2]] - first)
}, integer(1))

ev_pick_table <- function(inventory, pattern) {
  matches <- sort(grep(pattern, inventory, value = TRUE), method = "radix")
  if (length(matches)) matches[[1]] else NULL
}

ev_check_canonical_table <- function(tb, expected_names, label) {
  ev_assert(is.data.frame(tb), paste(label, "is not a data frame."))
  ev_assert(identical(names(tb), expected_names),
            paste(label, "has an unexpected field set or field order."))
  ev_assert(all(vapply(tb, function(x) is.atomic(x) && !is.factor(x), logical(1))),
            paste(label, "contains a non-atomic or factor channel."))
  actual <- tb
  rownames(actual) <- NULL
  expected <- ev_canonical_order(tb)
  ev_assert(identical(actual, expected), paste(label, "is not in canonical row order."))
  character_fields <- vapply(tb, is.character, logical(1))
  if (any(character_fields)) {
    values <- unlist(tb[character_fields], use.names = FALSE)
    ev_assert(!any(grepl("@", values, fixed = TRUE), na.rm = TRUE),
              paste(label, "contains an email-like token."))
  }
  invisible(TRUE)
}

ev_equal_tree <- function(actual, expected, label) {
  if (is.null(actual) || is.null(expected)) {
    ev_assert(is.null(actual) && is.null(expected), paste(label, "null state differs."))
    return(invisible(TRUE))
  }
  if (is.list(expected) && !is.data.frame(expected)) {
    ev_assert(is.list(actual) && identical(names(actual), names(expected)),
              paste(label, "object fields differ."))
    for (name in names(expected))
      ev_equal_tree(actual[[name]], expected[[name]], paste0(label, ".", name))
    return(invisible(TRUE))
  }
  actual <- unlist(actual, use.names = FALSE)
  expected <- unlist(expected, use.names = FALSE)
  if (is.numeric(expected)) {
    ev_assert(length(actual) == length(expected) &&
                identical(as.numeric(actual), as.numeric(expected)),
              paste(label, "numeric value differs."))
  } else if (is.logical(expected)) {
    ev_assert(length(actual) == length(expected) &&
                identical(as.logical(actual), as.logical(expected)),
              paste(label, "logical value differs."))
  } else {
    ev_assert(identical(as.character(actual), as.character(expected)),
              paste(label, "value differs."))
  }
  invisible(TRUE)
}

ev_canonical_token <- function(x) {
  if (length(x) != 1L || is.na(x)) return("<NA>")
  if (is.numeric(x)) {
    if (is.nan(x)) return("<NaN>")
    if (is.infinite(x)) return(if (x > 0) "<Inf>" else "<-Inf>")
    return(sprintf("%.17g", x))
  }
  enc2utf8(as.character(x))
}

ev_selected_month_summary <- function(tb, value_name, signature) {
  if (!nrow(tb)) return(data.frame(
    signature = character(), ym = character(), rows = integer(),
    final_qf_pass_rows = integer(), passing_finite_values = integer(),
    content_sha256 = character(), stringsAsFactors = FALSE
  ))
  timestamp <- as.character(tb$startDateTime)
  ym <- substr(timestamp, 1, 7); ym[is.na(ym)] <- "<NA>"
  value <- suppressWarnings(as.numeric(tb[[value_name]]))
  qf <- suppressWarnings(as.numeric(tb$finalQF))
  qf_pass <- is.finite(qf) & qf == 0
  groups <- split(seq_len(nrow(tb)), ym)
  out <- do.call(rbind, lapply(groups, function(i) {
    row_material <- vapply(i, function(j) paste(
      ev_canonical_token(timestamp[[j]]),
      ev_canonical_token(value[[j]]),
      ev_canonical_token(qf[[j]]),
      sep = "\t"
    ), character(1))
    data.frame(
      signature = signature,
      ym = ym[[i[[1]]]],
      rows = length(i),
      final_qf_pass_rows = sum(qf_pass[i]),
      passing_finite_values = sum(qf_pass[i] & is.finite(value[i])),
      content_sha256 = digest::digest(
        paste0(paste(sort(row_material, method = "radix"), collapse = "\n"), "\n"),
        algo = "sha256", serialize = FALSE
      ),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  ev_canonical_order(out)
}

ev_select_summary <- function(streams, months, value_name, label,
                              lowest_vertical = FALSE) {
  stream_columns <- intersect(
    c("namedLocation", "sensorLocation", "sensorPositionID",
      "horizontalPosition", "verticalPosition"), names(streams)
  )
  ev_check_canonical_table(streams, c("signature", stream_columns),
                           paste(label, "stream summary"))
  ev_check_canonical_table(
    months,
    c("signature", "ym", "rows", "final_qf_pass_rows",
      "passing_finite_values", "content_sha256"),
    paste(label, "stream-month summary")
  )
  if (!nrow(streams) && !nrow(months)) return(NULL)
  ev_assert(nrow(streams) > 0L && nrow(months) > 0L &&
              !anyDuplicated(streams$signature) &&
              !anyDuplicated(paste(months$signature, months$ym, sep = "\x1f")) &&
              all(months$signature %in% streams$signature) &&
              all(streams$signature %in% months$signature),
            paste(label, "stream summaries have missing, duplicate, or orphan keys."))
  ev_assert(all(months$rows > 0L & months$final_qf_pass_rows >= 0L &
                  months$final_qf_pass_rows <= months$rows &
                  months$passing_finite_values >= 0L &
                  months$passing_finite_values <= months$final_qf_pass_rows) &&
              all(grepl("^[0-9a-f]{64}$", months$content_sha256)),
            paste(label, "stream-month counts or digests are malformed."))
  ev_assert(all(grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", months$ym)) &&
              all(months$ym >= "2013-01" & months$ym <= "2024-12"),
            paste(label, "stream-month evidence lies outside the pinned window."))
  if (length(stream_columns)) {
    metadata <- lapply(streams[stream_columns], function(x) {
      value <- trimws(as.character(x)); value[ev_blank(value)] <- "<NA>"; value
    })
    expected_signature <- do.call(paste, c(metadata, sep = "|"))
  } else expected_signature <- rep("single-stream", nrow(streams))
  ev_assert(identical(as.character(streams$signature), expected_signature),
            paste(label, "stream signatures do not derive from stream metadata."))

  valid_month <- grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", months$ym)
  support <- lapply(streams$signature, function(signature) {
    rows <- months$signature == signature
    data.frame(
      signature = signature,
      months = length(unique(months$ym[rows & valid_month &
                                         months$passing_finite_values > 0L])),
      values = sum(months$passing_finite_values[rows]),
      rejected = sum(months$rows[rows] - months$final_qf_pass_rows[rows]),
      stringsAsFactors = FALSE
    )
  })
  support <- do.call(rbind, support)
  support <- support[support$months > 0L & support$values > 0L, , drop = FALSE]
  if (!nrow(support)) return(NULL)
  if (isTRUE(lowest_vertical)) {
    ev_assert("verticalPosition" %in% names(streams),
              paste(label, "has no verticalPosition for the lowest-position rule."))
    vertical <- suppressWarnings(as.numeric(streams$verticalPosition[
      match(support$signature, streams$signature)
    ]))
    ev_assert(all(is.finite(vertical)),
              paste(label, "has missing or non-numeric verticalPosition metadata."))
    support$vertical_position <- vertical
    support <- support[order(support$vertical_position, -support$months,
                             -support$values, support$signature), , drop = FALSE]
    rule <- paste(
      "minimum finite recorded verticalPosition (ordering metadata, not metres)",
      "max supported months", "max finalQF-passing finite values",
      "lexical signature", sep = "; "
    )
  } else {
    support <- support[order(-support$months, -support$values,
                             support$signature), , drop = FALSE]
    rule <- "max supported months; max finalQF-passing finite values; lexical signature"
  }
  chosen <- support$signature[[1]]
  selection <- list(
    columns = stream_columns,
    signature = chosen,
    value_column = value_name,
    timestamp_column = "startDateTime",
    supported_months = support$months[[1]],
    finite_values = support$values[[1]],
    quality_flag_column = "finalQF",
    rejected_by_final_qf = support$rejected[[1]],
    candidate_streams = nrow(support),
    vertical_position_rule = if (isTRUE(lowest_vertical)) "lowest_recorded" else "none",
    selected_vertical_position = if (isTRUE(lowest_vertical))
      unname(support$vertical_position[[1]]) else NULL,
    vertical_position_basis = if (isTRUE(lowest_vertical)) paste(
      "NEON variables metadata defines verticalPosition as the site vertical-location index;",
      "NEON.DOC.000646 defines index 001 as the lowest SAAT/boom"
    ) else NULL,
    rule = rule
  )
  list(signature = chosen, selection = selection)
}

ev_temperature_monthly <- function(tb, source_table, minimum_coverage = 0.75) {
  ev_assert(grepl("30min", source_table, ignore.case = TRUE),
            "Temperature evidence is not from an identified 30-minute table.")
  required <- c("startDateTime", "tempSingleMean", "tempSingleMinimum",
                "tempSingleMaximum", "finalQF")
  ev_assert(identical(names(tb), required),
            "Selected temperature evidence has an unexpected schema.")
  time <- as.character(tb$startDateTime)
  ym <- substr(time, 1, 7)
  date <- substr(time, 1, 10)
  valid_time <- !ev_blank(time) &
    grepl("^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])", time)
  values <- list(
    mean = suppressWarnings(as.numeric(tb$tempSingleMean)),
    minimum = suppressWarnings(as.numeric(tb$tempSingleMinimum)),
    maximum = suppressWarnings(as.numeric(tb$tempSingleMaximum))
  )
  any_finite <- Reduce(`|`, lapply(values, is.finite))
  ev_assert(!any(!valid_time & any_finite),
            "Temperature evidence has a finite value without a valid timestamp.")
  ev_assert(!anyDuplicated(time[any_finite]),
            "Temperature evidence has duplicate finite-value timestamps.")
  qf <- suppressWarnings(as.numeric(tb$finalQF))
  qf_pass <- is.finite(qf) & qf == 0
  values <- lapply(values, function(x) {
    x[!qf_pass] <- NA_real_
    x
  })
  groups <- split(seq_len(nrow(tb)), ym)
  groups <- groups[!is.na(names(groups)) &
                     grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", names(groups))]
  if (!length(groups)) return(NULL)
  keys <- names(groups)
  expected <- ev_days_in_month(keys) * 48L
  threshold <- ceiling(expected * minimum_coverage)
  observed <- lapply(values, function(x) {
    vapply(groups, function(i) sum(is.finite(x[i])), integer(1))
  })
  supported <- lapply(observed, function(n) n >= threshold & n <= expected)
  summarise_channel <- function(x, support, fun) {
    vapply(seq_along(groups), function(j) {
      i <- groups[[j]]
      if (!support[[j]]) NA_real_ else fun(x[i][is.finite(x[i])])
    }, numeric(1))
  }
  data.frame(
    ym = keys,
    temp_c = summarise_channel(values$mean, supported$mean, mean),
    temp_min = summarise_channel(values$minimum, supported$minimum, min),
    temp_max = summarise_channel(values$maximum, supported$maximum, max),
    temp_n = unname(observed$mean),
    temp_expected = expected,
    temp_coverage_pct = round(100 * unname(observed$mean) / expected, 1),
    temp_days = vapply(groups, function(i) {
      length(unique(date[i][is.finite(values$mean[i])]))
    }, integer(1)),
    temp_expected_days = ev_days_in_month(keys),
    temp_min_n = unname(observed$minimum),
    temp_max_n = unname(observed$maximum),
    stringsAsFactors = FALSE
  )
}

ev_precip_monthly <- function(tb, source_table) {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  time <- as.character(tb$startDateTime)
  ym <- substr(time, 1, 7)
  value <- suppressWarnings(as.numeric(tb$precipBulk))
  finite <- is.finite(value)
  ev_assert(!any(ev_blank(time[finite])),
            "Precipitation evidence has a finite selected value without a timestamp.")
  ev_assert(!anyDuplicated(time[finite]),
            "Precipitation evidence has duplicate selected finite-value timestamps.")
  qf <- suppressWarnings(as.numeric(tb$finalQF))
  qf_pass <- is.finite(qf) & qf == 0
  value[!qf_pass] <- NA_real_
  groups <- split(seq_len(nrow(tb)), ym)
  groups <- groups[!is.na(names(groups)) & grepl("^[0-9]{4}-[0-9]{2}$", names(groups))]
  if (!length(groups)) return(NULL)
  per_day <- if (grepl("daily", source_table, ignore.case = TRUE)) 1L else
    if (grepl("30min", source_table, ignore.case = TRUE)) 48L else
      if (grepl("60min", source_table, ignore.case = TRUE)) 24L else
        stop("Precipitation evidence has an unsupported source interval.", call. = FALSE)
  keys <- names(groups)
  expected <- ev_days_in_month(keys) * per_day
  observed <- vapply(groups, function(i) sum(is.finite(value[i])), integer(1))
  total <- vapply(groups, function(i) sum(value[i], na.rm = TRUE), numeric(1))
  data.frame(
    ym = keys,
    precip_mm = ifelse(observed == expected, total, NA_real_),
    precip_n = observed,
    precip_expected = expected,
    precip_coverage_pct = round(100 * observed / expected, 1),
    stringsAsFactors = FALSE
  )
}

ev_pheno_share <- function(tb, pattern) {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  status <- tolower(trimws(as.character(tb$phenophaseStatus)))
  keep <- grepl(pattern, tb$phenophaseName) & status %in% c("yes", "no") &
    !ev_blank(tb$individualID)
  if (!any(keep)) return(NULL)
  individual <- as.character(tb$individualID[keep])
  ym <- substr(as.character(tb$startDateTime[keep]), 1, 7)
  yes <- as.integer(status[keep] == "yes")
  keep_month <- !is.na(ym)
  individual <- individual[keep_month]
  ym <- ym[keep_month]
  yes <- yes[keep_month]
  if (!length(ym)) return(NULL)
  individual_month_key <- paste(individual, ym, sep = "\x1f")
  groups <- split(seq_along(yes), individual_month_key)
  individual_month <- data.frame(
    individual = vapply(groups, function(i) individual[[i[[1]]]], character(1)),
    ym = vapply(groups, function(i) ym[[i[[1]]]], character(1)),
    yes = vapply(groups, function(i) max(yes[i]), integer(1)),
    stringsAsFactors = FALSE
  )
  months <- split(seq_len(nrow(individual_month)), individual_month$ym)
  out <- data.frame(
    ym = names(months),
    share = vapply(months, function(i) 100 * mean(individual_month$yes[i]), numeric(1)),
    n = vapply(months, length, integer(1)),
    stringsAsFactors = FALSE
  )
  out$share[out$n < 5L] <- NA_real_
  out
}

ev_left_join_month <- function(out, monthly) {
  if (is.null(monthly)) return(out)
  match_index <- match(out$ym, monthly$ym)
  for (name in setdiff(names(monthly), "ym")) out[[name]] <- monthly[[name]][match_index]
  out
}

ev_add_pheno <- function(out, tb, pattern, name) {
  monthly <- ev_pheno_share(tb, pattern)
  if (is.null(monthly)) {
    out[[name]] <- NA_real_
    out[[paste0(name, "_n")]] <- NA_integer_
  } else {
    match_index <- match(out$ym, monthly$ym)
    out[[name]] <- monthly$share[match_index]
    out[[paste0(name, "_n")]] <- monthly$n[match_index]
  }
  out
}

ev_compare_frame <- function(actual, expected, label) {
  actual <- as.data.frame(actual, stringsAsFactors = FALSE)
  expected <- as.data.frame(expected, stringsAsFactors = FALSE)
  rownames(actual) <- NULL
  rownames(expected) <- NULL
  ev_assert(identical(names(actual), names(expected)), paste(label, "column order differs."))
  ev_assert(nrow(actual) == nrow(expected), paste(label, "row count differs."))
  for (name in names(expected)) {
    a <- actual[[name]]
    e <- expected[[name]]
    ev_assert(identical(class(a), class(e)), paste(label, name, "class differs."))
    ev_assert(identical(unname(a), unname(e)), paste(label, name, "values differ."))
  }
  invisible(TRUE)
}

ev_validate_source_columns <- function(product, label) {
  columns <- product$source_columns
  ev_assert(is.list(columns), paste(label, "source-column map is missing."))
  if (!length(columns)) return(invisible(TRUE))
  ev_assert(identical(names(columns),
                      c("timestamp", "stream", "value", "companions", "quality_flag")),
            paste(label, "source-column map schema differs."))
  ev_assert(length(ev_chr(columns$timestamp)) == 1L &&
              length(ev_chr(columns$value)) == 1L &&
              length(ev_chr(columns$quality_flag)) == 1L,
            paste(label, "source-column map contains an ambiguous scalar."))
  ev_assert(grepl("finalQF$", ev_chr(columns$quality_flag), ignore.case = TRUE),
            paste(label, "source quality field is not a finalQF channel."))
  allowed_stream <- c("namedLocation", "sensorLocation", "sensorPositionID",
                      "horizontalPosition", "verticalPosition")
  ev_assert(all(ev_chr(columns$stream) %in% allowed_stream),
            paste(label, "source stream-column map contains an unknown field."))
  invisible(TRUE)
}

ev_build_site <- function(site, evidence, record) {
  ev_assert(identical(names(evidence),
                      c("schema_version", "release", "site", "window", "privacy", "products")),
            paste(site, "evidence schema fields differ."))
  ev_assert(identical(as.integer(evidence$schema_version), 1L) &&
              identical(evidence$release, "RELEASE-2026") &&
              identical(evidence$site, site),
            paste(site, "evidence identity differs."))
  ev_assert(identical(evidence$window,
                      list(start_month = "2013-01", end_month = "2024-12")),
            paste(site, "evidence window differs."))
  ev_assert(identical(evidence$privacy$personal_fields, "none") &&
              grepl("raw individualID is never written",
                    evidence$privacy$plant_individual_identity, fixed = TRUE),
            paste(site, "evidence privacy declaration differs."))
  ev_assert(identical(names(evidence$products),
                      c("air_temperature", "precipitation", "plant_phenology")),
            paste(site, "evidence product set differs."))

  temperature <- evidence$products$air_temperature
  precipitation <- evidence$products$precipitation
  phenology <- evidence$products$plant_phenology
  ev_assert(identical(names(temperature),
                      c("id", "doi", "source_tables", "selected_table", "source_columns",
                        "selection_streams", "selection_months", "selected_rows")),
            paste(site, "temperature evidence fields differ."))
  ev_assert(identical(names(precipitation),
                      c("id", "doi", "source_tables", "selected_table", "source_columns",
                        "selection_streams", "selection_months", "selected_rows")),
            paste(site, "precipitation evidence fields differ."))
  ev_assert(identical(names(phenology),
                      c("id", "doi", "source_tables", "selected_table", "source_columns",
                        "source_rows", "rows")),
            paste(site, "phenology evidence fields differ."))
  identities <- list(
    air_temperature = c("DP1.00002.001", "10.48443/p69b-5e50"),
    precipitation = c("DP1.00044.001", "10.48443/v29j-eg88"),
    plant_phenology = c("DP1.10055.001", "10.48443/p75s-7p48")
  )
  for (name in names(identities)) {
    product <- evidence$products[[name]]
    ev_assert(identical(c(product$id, product$doi), identities[[name]]),
              paste(site, name, "product identity differs."))
    inventory <- as.character(product$source_tables)
    ev_assert(identical(inventory, sort(unique(inventory), method = "radix")),
              paste(site, name, "source-table inventory is not canonical."))
  }

  expected_temp_table <- ev_pick_table(temperature$source_tables, "SAAT_30min|saat.*30")
  expected_pheno_table <- ev_pick_table(phenology$source_tables, "phe_statusintensity")
  expected_precip_table <- ev_pick_table(
    precipitation$source_tables,
    "(WEIPRE|PRIPRE|SECPRE)_daily|wss_daily_precip|.*daily.*[Pp]recip"
  )
  if (is.null(expected_precip_table)) expected_precip_table <- ev_pick_table(
    precipitation$source_tables,
    "(WEIPRE|PRIPRE|SECPRE)_(60|30)min|.*[Pp]recip"
  )
  ev_assert(identical(temperature$selected_table, expected_temp_table) &&
              !is.null(expected_temp_table),
            paste(site, "temperature source-table choice is not independently reproducible."))
  ev_assert(identical(phenology$selected_table, expected_pheno_table) &&
              !is.null(expected_pheno_table),
            paste(site, "phenology source-table choice is not independently reproducible."))
  ev_assert(identical(precipitation$selected_table, expected_precip_table),
            paste(site, "precipitation source-table choice is not independently reproducible."))
  ev_validate_source_columns(temperature, paste(site, "temperature"))
  ev_validate_source_columns(precipitation, paste(site, "precipitation"))
  ev_assert(identical(ev_chr(temperature$source_columns$stream),
                      setdiff(names(temperature$selection_streams), "signature")),
            paste(site, "temperature stream fields differ from their source map."))
  if (length(precipitation$source_columns))
    ev_assert(identical(ev_chr(precipitation$source_columns$stream),
                        setdiff(names(precipitation$selection_streams), "signature")),
              paste(site, "precipitation stream fields differ from their source map."))

  ev_check_canonical_table(
    temperature$selected_rows,
    c("startDateTime", "tempSingleMean", "tempSingleMinimum",
      "tempSingleMaximum", "finalQF"),
    paste(site, "selected temperature evidence")
  )
  ev_assert(length(ev_chr(temperature$source_columns$companions)) == 2L &&
              grepl("tempSingleMinimum", ev_chr(temperature$source_columns$companions)[[1]],
                    ignore.case = TRUE) &&
              grepl("tempSingleMaximum", ev_chr(temperature$source_columns$companions)[[2]],
                    ignore.case = TRUE),
            paste(site, "temperature companion source channels differ."))
  temp_selected <- ev_select_summary(
    temperature$selection_streams, temperature$selection_months,
    "tempSingleMean", paste(site, "temperature"), lowest_vertical = TRUE
  )
  ev_assert(!is.null(temp_selected), paste(site, "has no supported temperature stream."))
  temp_selected_summary <- ev_selected_month_summary(
    temperature$selected_rows, "tempSingleMean", temp_selected$signature
  )
  temp_summary_rows <- temperature$selection_months$signature == temp_selected$signature
  ev_compare_frame(
    temp_selected_summary,
    temperature$selection_months[temp_summary_rows, , drop = FALSE],
    paste(site, "selected-temperature raw/summary evidence")
  )
  temp_month <- ev_temperature_monthly(temperature$selected_rows,
                                       temperature$selected_table)
  ev_assert(!is.null(temp_month) && any(is.finite(temp_month$temp_c)),
            paste(site, "has no coverage-qualified temperature month."))
  temp_selected$selection$monthly_coverage <- list(
    interval_minutes = 30L,
    expected_intervals_per_day = 48L,
    minimum_interval_coverage = 0.75,
    threshold_type = "analysis eligibility rule, not a NEON completeness designation",
    quality_gate = "selected finalQF == 0 and finite channel value",
    rule = "report a monthly channel only at or above its 75.0% expected-interval threshold"
  )

  ev_check_canonical_table(
    precipitation$selected_rows,
    c("startDateTime", "precipBulk", "finalQF"),
    paste(site, "selected precipitation evidence")
  )
  precip_selected <- ev_select_summary(
    precipitation$selection_streams, precipitation$selection_months,
    "precipBulk", paste(site, "precipitation"), lowest_vertical = FALSE
  )
  if (is.null(precipitation$selected_table))
    ev_assert(is.null(precip_selected),
              paste(site, "has precipitation stream support without a source table."))
  if (is.null(precip_selected)) {
    ev_assert(!nrow(precipitation$selected_rows),
              paste(site, "has selected precipitation rows without a viable stream."))
  } else {
    precip_selected_summary <- ev_selected_month_summary(
      precipitation$selected_rows, "precipBulk", precip_selected$signature
    )
    precip_summary_rows <- precipitation$selection_months$signature ==
      precip_selected$signature
    ev_compare_frame(
      precip_selected_summary,
      precipitation$selection_months[precip_summary_rows, , drop = FALSE],
      paste(site, "selected-precipitation raw/summary evidence")
    )
  }
  precip_month <- ev_precip_monthly(precipitation$selected_rows,
                                    precipitation$selected_table)

  ev_check_canonical_table(
    phenology$rows,
    c("startDateTime", "phenophaseName", "phenophaseStatus", "individualID"),
    paste(site, "plant-phenology evidence")
  )
  ev_assert(length(phenology$source_rows) == 1L &&
              is.finite(as.numeric(phenology$source_rows)) &&
              as.numeric(phenology$source_rows) == as.integer(phenology$source_rows) &&
              as.numeric(phenology$source_rows) >= nrow(phenology$rows) &&
              as.numeric(phenology$source_rows) > 0,
            paste(site, "phenology source-row support is missing or smaller than evidence."))
  individual <- as.character(phenology$rows$individualID)
  nonblank <- !ev_blank(individual)
  ev_assert(all(grepl("^plant-sha256:[0-9a-f]{64}$", individual[nonblank])),
            paste(site, "phenology evidence contains a raw or malformed individual identity."))
  allowed_phase <- grepl(
    "^Open flowers$|[Bb]reaking leaf buds|[Ee]merging needles|[Yy]oung (leaves|needles)|[Ii]ncreasing leaf size|[Ii]nitial growth|^Fruits$",
    phenology$rows$phenophaseName
  )
  ev_assert(all(allowed_phase), paste(site, "phenology evidence contains an unused phenophase."))
  pheno_month <- substr(as.character(phenology$rows$startDateTime), 1, 7)
  ev_assert(all(grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", pheno_month)) &&
              all(pheno_month >= "2013-01" & pheno_month <= "2024-12"),
            paste(site, "phenology evidence lies outside the pinned window."))

  months <- format(seq(as.Date("2013-01-01"), as.Date("2024-12-01"), by = "month"),
                   "%Y-%m")
  out <- data.frame(
    siteID = rep(site, length(months)),
    ym = months,
    date = as.Date(paste0(months, "-01")),
    stringsAsFactors = FALSE
  )
  if (is.null(precip_month)) {
    out$precip_mm <- NA_real_
    out$precip_n <- NA_integer_
    out$precip_expected <- NA_integer_
    out$precip_coverage_pct <- NA_real_
  } else out <- ev_left_join_month(out, precip_month)
  out <- ev_left_join_month(out, temp_month)
  out <- ev_add_pheno(out, phenology$rows, "^Open flowers$", "flowering_pct")
  out <- ev_add_pheno(
    out, phenology$rows,
    "[Bb]reaking leaf buds|[Ee]merging needles|[Yy]oung (leaves|needles)|[Ii]ncreasing leaf size|[Ii]nitial growth",
    "greenup_pct"
  )
  out <- ev_add_pheno(out, phenology$rows, "^Fruits$", "fruiting_pct")
  out$source <- "NEON RELEASE-2026"
  primary <- c("precip_mm", "temp_c", "temp_min", "temp_max",
               "flowering_pct", "greenup_pct", "fruiting_pct")
  out <- out[rowSums(!is.na(out[primary])) > 0L, , drop = FALSE]
  rownames(out) <- NULL

  derived <- list(
    public = out,
    support = list(
      air_temperature = !is.null(temp_selected) && nrow(temperature$selected_rows) > 0L,
      precipitation = !is.null(precip_selected) && nrow(precipitation$selected_rows) > 0L,
      plant_phenology = as.numeric(phenology$source_rows) > 0
    ),
    source_tables = list(
      air_temperature = temperature$source_tables,
      precipitation = precipitation$source_tables,
      plant_phenology = phenology$source_tables
    ),
    stream_selection = list(
      air_temperature = temp_selected$selection,
      precipitation = if (is.null(precip_selected)) NULL else precip_selected$selection
    )
  )
  derived
}

ev_verify <- function(root = Sys.getenv("BIRD_OUTPUT_ROOT", "."),
                      evidence_dir = Sys.getenv(
                        "BIRD_ENV_EVIDENCE_DIR",
                        file.path(root, "validation-evidence", "environment")
                      )) {
  source("R/site_metadata.R")
  expected_sites <- sort(as.character(neon_sites$site), method = "radix")
  ev_assert(length(expected_sites) == 47L && "PUUM" %in% expected_sites,
            "Canonical metadata is not the exact 47-site roster.")
  receipt_path <- file.path(root, "data", "environment_source_receipt.json")
  env_dir <- file.path(root, "data", "env")
  ev_assert(file.exists(receipt_path), "Environmental source receipt is missing.")
  ev_assert(dir.exists(env_dir), "Public environmental bundle directory is missing.")
  ev_assert(dir.exists(evidence_dir), "Environmental validation evidence directory is missing.")

  receipt <- jsonlite::fromJSON(receipt_path, simplifyVector = FALSE)
  ev_assert(identical(names(receipt),
                      c("schema_version", "release", "window", "retrieval", "products",
                        "validation_evidence", "files")),
            "Environmental receipt schema fields differ.")
  ev_assert(identical(as.integer(receipt$schema_version), 2L) &&
              identical(receipt$release, "RELEASE-2026") &&
              identical(receipt$window$start_month, "2013-01") &&
              identical(receipt$window$end_month, "2024-12"),
            "Environmental receipt release/window identity differs.")
  ev_assert(identical(names(receipt$retrieval),
                      c("tool", "package", "neonUtilities_version", "r_version", "token_required")) &&
              identical(receipt$retrieval$tool, "neonUtilities::loadByProduct") &&
              identical(receipt$retrieval$package, "basic") &&
              nzchar(as.character(receipt$retrieval$neonUtilities_version)) &&
              identical(receipt$retrieval$r_version,
                        paste(R.version$major, R.version$minor, sep = ".")) &&
              isTRUE(receipt$retrieval$token_required),
            "Environmental receipt retrieval identity differs from the validator runtime.")
  ev_assert(identical(names(receipt$validation_evidence),
                      c("schema_version", "file_count", "total_bytes", "format")) &&
              identical(as.integer(receipt$validation_evidence$schema_version), 1L) &&
              identical(as.integer(receipt$validation_evidence$file_count), 47L) &&
              is.finite(as.numeric(receipt$validation_evidence$total_bytes)) &&
              as.numeric(receipt$validation_evidence$total_bytes) > 0 &&
              identical(receipt$validation_evidence$format,
                        paste(
                          "canonical privacy-minimized selected-stream rows plus digest-bound",
                          "all-stream month support; validation-only; not deployed"
                        )),
            "Environmental evidence receipt contract differs.")

  product_contract <- list(
    air_temperature = list(id = "DP1.00002.001", doi = "10.48443/p69b-5e50",
                           supported_sites = 47L),
    precipitation = list(id = "DP1.00044.001", doi = "10.48443/v29j-eg88",
                         supported_sites = 20L),
    plant_phenology = list(id = "DP1.10055.001", doi = "10.48443/p75s-7p48",
                           supported_sites = 47L)
  )
  ev_equal_tree(receipt$products, product_contract, "receipt.products")

  env_files <- sort(list.files(env_dir, pattern = "^[A-Z]{4}[.]rds$"), method = "radix")
  evidence_files <- sort(list.files(evidence_dir, pattern = "^[A-Z]{4}[.]rds$"),
                         method = "radix")
  expected_files <- paste0(expected_sites, ".rds")
  ev_assert(identical(env_files, expected_files),
            "Public environmental files are not the exact 47-site roster.")
  ev_assert(identical(evidence_files, expected_files),
            "Environmental evidence files are not the exact 47-site roster.")
  records <- receipt$files
  ev_assert(length(records) == 47L, "Environmental receipt does not contain 47 records.")
  record_sites <- vapply(records, function(record) as.character(record$site), character(1))
  ev_assert(!anyDuplicated(record_sites) && identical(sort(record_sites), expected_sites),
            "Environmental receipt records are not the exact unique roster.")
  ev_assert(identical(
    as.numeric(receipt$validation_evidence$total_bytes),
    sum(vapply(records, function(record) as.numeric(record$evidence_bytes), numeric(1)))
  ), "Environmental evidence total-byte receipt does not equal its file records.")

  support <- list(air_temperature = logical(), precipitation = logical(),
                  plant_phenology = logical())
  for (site in expected_sites) {
    record <- records[[match(site, record_sites)]]
    expected_record_names <- c(
      "site", "rows", "month_min", "month_max",
      "air_temperature_supported", "precipitation_supported",
      "plant_phenology_supported", "source_tables", "stream_selection",
      "file", "sha256", "bytes", "evidence_file", "evidence_sha256",
      "evidence_bytes"
    )
    ev_assert(identical(names(record), expected_record_names),
              paste(site, "receipt record fields differ."))
    public_path <- file.path(env_dir, paste0(site, ".rds"))
    evidence_path <- file.path(evidence_dir, paste0(site, ".rds"))
    ev_assert(identical(record$file, paste0(site, ".rds")) &&
                identical(record$evidence_file, paste0(site, ".rds")) &&
                identical(record$sha256, ev_sha256(public_path)) &&
                identical(record$evidence_sha256, ev_sha256(evidence_path)) &&
                identical(as.numeric(record$bytes), unname(file.info(public_path)$size)) &&
                identical(as.numeric(record$evidence_bytes),
                          unname(file.info(evidence_path)$size)),
              paste(site, "receipt file identity, digest, or byte count differs."))
    evidence <- readRDS(evidence_path)
    derived <- ev_build_site(site, evidence, record)
    actual <- readRDS(public_path)
    ev_compare_frame(actual, derived$public, paste(site, "public environmental bundle"))
    ev_assert(identical(as.integer(record$rows), nrow(derived$public)) &&
                identical(record$month_min, min(derived$public$ym)) &&
                identical(record$month_max, max(derived$public$ym)),
              paste(site, "receipt row/month facts do not re-derive."))
    for (name in names(derived$support)) {
      field <- paste0(name, "_supported")
      ev_assert(identical(isTRUE(record[[field]]), isTRUE(derived$support[[name]])),
                paste(site, name, "support receipt differs."))
      support[[name]] <- c(support[[name]], isTRUE(derived$support[[name]]))
    }
    for (name in names(derived$source_tables)) {
      ev_assert(identical(ev_chr(record$source_tables[[name]]),
                          as.character(derived$source_tables[[name]])),
                paste(site, name, "source-table inventory receipt differs."))
    }
    ev_equal_tree(record$stream_selection, derived$stream_selection,
                  paste(site, "stream_selection"))
  }
  ev_assert(sum(support$air_temperature) == 47L &&
              sum(support$precipitation) == 20L &&
              sum(support$plant_phenology) == 47L,
            "Re-derived environmental support is not temperature 47/47, precipitation 20/47, phenology 47/47.")
  cat(paste0(
    "OK: independently verified 47 canonical environmental evidence files; ",
    "exact RELEASE-2026 stream/QF/month/phenology derivation; and byte-exact public bundles ",
    "(temperature 47/47, precipitation 20/47, phenology 47/47).\n"
  ))
  invisible(TRUE)
}

if (!identical(Sys.getenv("BIRD_ENV_ORACLE_LIBRARY"), "1")) ev_verify()
