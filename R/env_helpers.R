# Deterministic helpers for contextual NEON sensor streams. These functions are
# dependency-light so producer and validator fixtures exercise identical logic.

env_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

env_pick_col_name <- function(tb, col_rx) {
  if (is.null(tb)) return(NULL)
  names <- grep(col_rx, names(tb), value = TRUE)
  value_names <- names[!grepl("finalQF$", names, ignore.case = TRUE)]
  if (length(value_names)) names <- value_names
  exact <- names[tolower(names) == tolower(col_rx)]
  if (length(exact)) names <- exact
  if (length(names)) names[[1]] else NULL
}

env_time_col <- function(tb) {
  preferred <- c("startDateTime", "endDateTime", "date", "collectDate", "endDate")
  exact <- preferred[preferred %in% names(tb)]
  if (length(exact)) return(exact[[1]])
  names <- grep("endDateTime|startDateTime|^date$|collectDate|^endDate$", names(tb), value = TRUE)
  if (length(names)) names[[1]] else NULL
}

env_month_key <- function(tb) {
  name <- env_time_col(tb)
  if (is.null(name)) rep(NA_character_, nrow(tb)) else substr(as.character(tb[[name]]), 1, 7)
}

env_select_single_stream <- function(tb, value_rx, label = "sensor", require_final_qf = TRUE,
                                     vertical_position_rule = c("none", "lowest_recorded")) {
  vertical_position_rule <- match.arg(vertical_position_rule)
  if (is.null(tb) || !nrow(tb)) return(NULL)
  source_table <- attr(tb, "source_table", exact = TRUE)
  value_name <- env_pick_col_name(tb, value_rx)
  if (is.null(value_name)) return(NULL)
  qf_names <- grep("finalQF$", names(tb), value = TRUE, ignore.case = TRUE)
  exact_qf <- qf_names[tolower(qf_names) == "finalqf"]
  value_qf <- qf_names[tolower(qf_names) == tolower(paste0(value_name, "FinalQF"))]
  qf_name <- if (length(value_qf) == 1L) value_qf[[1]] else
    if (length(exact_qf)) exact_qf[[1]] else if (length(qf_names) == 1L) qf_names[[1]] else NULL
  if (is.null(qf_name) && require_final_qf)
    stop(label, " stream has no unambiguous finalQF pass channel.", call. = FALSE)
  qf_pass <- if (is.null(qf_name)) rep(TRUE, nrow(tb)) else {
    qf <- suppressWarnings(as.numeric(tb[[qf_name]]))
    is.finite(qf) & qf == 0
  }
  stream_cols <- intersect(
    c("namedLocation", "sensorLocation", "sensorPositionID",
      "horizontalPosition", "verticalPosition"), names(tb))
  if (length(stream_cols)) {
    stream <- lapply(tb[stream_cols], function(x) {
      x <- trimws(as.character(x)); x[env_blank(x)] <- "<NA>"; x
    })
    signature <- do.call(paste, c(stream, sep = "|"))
  } else signature <- rep("single-stream", nrow(tb))
  ym <- env_month_key(tb)
  valid_ym <- !is.na(ym) & grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", ym)
  value <- suppressWarnings(as.numeric(tb[[value_name]]))
  value[!qf_pass] <- NA_real_
  candidates <- split(seq_len(nrow(tb)), signature)
  support <- data.frame(
    signature = names(candidates),
    months = vapply(candidates, function(i)
      length(unique(ym[i][valid_ym[i] & is.finite(value[i])])), integer(1)),
    values = vapply(candidates, function(i) sum(is.finite(value[i])), integer(1)),
    stringsAsFactors = FALSE)
  viable <- support$months > 0L & support$values > 0L
  if (!any(viable)) return(NULL)
  support <- support[viable, , drop = FALSE]

  vertical_position <- rep(NA_real_, nrow(support))
  if (identical(vertical_position_rule, "lowest_recorded")) {
    if (!"verticalPosition" %in% names(tb))
      stop(label, " stream has no verticalPosition metadata for the required lowest-position rule.",
           call. = FALSE)
    vertical_raw <- vapply(support$signature, function(sig) {
      i <- candidates[[sig]]
      raw <- unique(trimws(as.character(tb$verticalPosition[i])))
      raw <- raw[!env_blank(raw)]
      if (length(raw) == 1L) raw[[1]] else NA_character_
    }, character(1))
    vertical_position <- suppressWarnings(as.numeric(vertical_raw))
    if (any(!is.finite(vertical_position)))
      stop(label, " has a supported stream with missing, non-numeric, or conflicting verticalPosition metadata.",
           call. = FALSE)
    support$vertical_position <- vertical_position
    support <- support[order(support$vertical_position, -support$months,
                             -support$values, support$signature), , drop = FALSE]
    selection_rule <- paste(
      "minimum finite recorded verticalPosition (ordering metadata, not metres)",
      "max supported months", "max finalQF-passing finite values",
      "lexical signature", sep = "; ")
  } else {
    support <- support[order(-support$months, -support$values, support$signature), , drop = FALSE]
    selection_rule <- "max supported months; max finalQF-passing finite values; lexical signature"
  }
  chosen <- support$signature[[1]]
  out <- tb[signature == chosen, , drop = FALSE]
  selected_pass <- qf_pass[signature == chosen]
  out$.env_qf_pass <- selected_pass
  time_name <- env_time_col(out)
  if (is.null(time_name)) stop(label, " selected stream has no timestamp field.", call. = FALSE)
  time <- as.character(out[[time_name]])
  finite <- is.finite(suppressWarnings(as.numeric(out[[value_name]])))
  if (any(env_blank(time[finite])))
    stop(label, " selected stream has a finite value without a timestamp.", call. = FALSE)
  if (anyDuplicated(time[finite]))
    stop(label, " selected stream has duplicate finite-value timestamps.", call. = FALSE)
  order_keys <- lapply(out[c(time_name, sort(setdiff(names(out), time_name)))], function(x)
    as.character(x))
  row_order <- do.call(order, c(order_keys, list(na.last = TRUE, method = "radix")))
  out <- out[row_order, , drop = FALSE]
  rownames(out) <- NULL
  selected_pass <- selected_pass[row_order]
  out$.env_qf_pass <- selected_pass
  attr(out, "stream_selection") <- list(
    columns = stream_cols,
    signature = chosen,
    value_column = value_name,
    timestamp_column = time_name,
    supported_months = support$months[[1]],
    finite_values = support$values[[1]],
    quality_flag_column = qf_name,
    rejected_by_final_qf = sum(!selected_pass),
    candidate_streams = nrow(support),
    vertical_position_rule = vertical_position_rule,
    selected_vertical_position = if (identical(vertical_position_rule, "lowest_recorded"))
      unname(support$vertical_position[[1]]) else NULL,
    vertical_position_basis = if (identical(vertical_position_rule, "lowest_recorded"))
      paste("NEON variables metadata defines verticalPosition as the site vertical-location index;",
            "NEON.DOC.000646 defines index 001 as the lowest SAAT/boom") else NULL,
    rule = selection_rule)
  attr(out, "source_table") <- source_table
  out
}

env_days_in_month <- function(ym) vapply(ym, function(key) {
  first <- as.Date(paste0(key, "-01"))
  as.integer(seq(first, by = "month", length.out = 2L)[[2]] - first)
}, integer(1))

# Monthly air-temperature context from one selected 30-minute SAAT stream.
# A month is reportable only when at least 75% of its expected half-hour intervals
# carry a finite value and pass the selected finalQF channel. The published
# verticalPosition field is used only to order streams; it is never converted to
# or described as a physical height.
env_monthly_temperature <- function(tb, mean_rx = "tempSingleMean",
                                    min_rx = "tempSingleMinimum",
                                    max_rx = "tempSingleMaximum",
                                    label = "air temperature",
                                    minimum_coverage = 0.75) {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  if (!is.numeric(minimum_coverage) || length(minimum_coverage) != 1L ||
      !is.finite(minimum_coverage) || minimum_coverage <= 0 || minimum_coverage > 1)
    stop(label, " minimum coverage must be in (0, 1].", call. = FALSE)
  source_table <- attr(tb, "source_table", exact = TRUE)
  if (is.null(source_table) || !grepl("30min", source_table, ignore.case = TRUE))
    stop(label, " must come from an identified 30-minute source table.", call. = FALSE)
  time_name <- env_time_col(tb)
  value_names <- vapply(
    c(mean = mean_rx, minimum = min_rx, maximum = max_rx),
    function(rx) {
      name <- env_pick_col_name(tb, rx)
      if (is.null(name)) NA_character_ else name
    }, character(1))
  if (is.null(time_name) || any(is.na(value_names)) || any(!nzchar(value_names)))
    stop(label, " lacks a timestamp or required mean/minimum/maximum channel.", call. = FALSE)
  if (!".env_qf_pass" %in% names(tb))
    stop(label, " has not been gated by an unambiguous finalQF channel.", call. = FALSE)

  time <- as.character(tb[[time_name]])
  ym <- substr(time, 1, 7)
  date <- substr(time, 1, 10)
  valid_time <- !env_blank(time) &
    grepl("^[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])", time)
  values <- lapply(value_names, function(name) suppressWarnings(as.numeric(tb[[name]])))
  any_finite <- Reduce(`|`, lapply(values, is.finite))
  if (any(!valid_time & any_finite))
    stop(label, " has a finite value without a valid calendar timestamp.", call. = FALSE)
  if (anyDuplicated(time[any_finite]))
    stop(label, " selected stream has duplicate finite-value timestamps.", call. = FALSE)
  qf_pass <- tb$.env_qf_pass %in% TRUE
  values <- lapply(values, function(x) { x[!qf_pass] <- NA_real_; x })

  groups <- split(seq_len(nrow(tb)), ym)
  group_names <- names(groups)
  groups <- groups[!is.na(group_names) &
                     grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", group_names)]
  if (!length(groups)) return(NULL)
  keys <- names(groups)
  expected <- env_days_in_month(keys) * 48L
  threshold <- ceiling(expected * minimum_coverage)
  observed <- lapply(values, function(x)
    vapply(groups, function(i) sum(is.finite(x[i])), integer(1)))
  supported <- lapply(observed, function(n) n >= threshold & n <= expected)
  summarise_channel <- function(x, support, fun) vapply(seq_along(groups), function(j) {
    i <- groups[[j]]
    if (!support[[j]]) NA_real_ else fun(x[i][is.finite(x[i])])
  }, numeric(1))
  mean_value <- summarise_channel(values$mean, supported$mean, mean)
  min_value <- summarise_channel(values$minimum, supported$minimum, min)
  max_value <- summarise_channel(values$maximum, supported$maximum, max)
  days_observed <- vapply(groups, function(i)
    length(unique(date[i][is.finite(values$mean[i])])), integer(1))

  out <- data.frame(
    ym = keys,
    temp_c = mean_value,
    temp_min = min_value,
    temp_max = max_value,
    temp_n = unname(observed$mean),
    temp_expected = expected,
    temp_coverage_pct = round(100 * unname(observed$mean) / expected, 1),
    temp_days = days_observed,
    temp_expected_days = env_days_in_month(keys),
    temp_min_n = unname(observed$minimum),
    temp_max_n = unname(observed$maximum),
    stringsAsFactors = FALSE)
  attr(out, "monthly_coverage") <- list(
    interval_minutes = 30L,
    expected_intervals_per_day = 48L,
    minimum_interval_coverage = minimum_coverage,
    threshold_type = "analysis eligibility rule, not a NEON completeness designation",
    quality_gate = "selected finalQF == 0 and finite channel value",
    rule = sprintf("report a monthly channel only at or above its %.1f%% expected-interval threshold",
                   100 * minimum_coverage))
  out
}

# Produce precipitation only for complete calendar months after finalQF filtering.
# Strict completeness avoids turning a partial sensor record into a low-biased
# monthly or annual total.
env_monthly_precip <- function(tb, value_rx, label = "precipitation") {
  if (is.null(tb) || !nrow(tb)) return(NULL)
  value_name <- env_pick_col_name(tb, value_rx)
  time_name <- env_time_col(tb)
  source_table <- attr(tb, "source_table", exact = TRUE)
  if (is.null(value_name) || is.null(time_name) || is.null(source_table))
    stop(label, " lacks a value, timestamp, or source-table identity.", call. = FALSE)
  time <- as.character(tb[[time_name]])
  ym <- substr(time, 1, 7)
  value <- suppressWarnings(as.numeric(tb[[value_name]]))
  if (".env_qf_pass" %in% names(tb)) value[!(tb$.env_qf_pass %in% TRUE)] <- NA_real_
  groups <- split(seq_len(nrow(tb)), ym)
  groups <- groups[!is.na(names(groups)) & grepl("^[0-9]{4}-[0-9]{2}$", names(groups))]
  if (!length(groups)) return(NULL)
  per_day <- if (grepl("daily", source_table, ignore.case = TRUE)) 1L else
    if (grepl("30min", source_table, ignore.case = TRUE)) 48L else
      if (grepl("60min", source_table, ignore.case = TRUE)) 24L else
        stop(label, " source table has an unsupported interval: ", source_table, call. = FALSE)
  keys <- names(groups)
  expected <- env_days_in_month(keys) * per_day
  observed <- vapply(groups, function(i) sum(is.finite(value[i])), integer(1))
  total <- vapply(groups, function(i) sum(value[i], na.rm = TRUE), numeric(1))
  complete <- observed == expected
  data.frame(
    ym = keys,
    value = ifelse(complete, total, NA_real_),
    precip_n = observed,
    precip_expected = expected,
    precip_coverage_pct = round(100 * observed / expected, 1),
    stringsAsFactors = FALSE)
}

env_complete_annual_precip <- function(year, month, precip_mm) {
  year <- suppressWarnings(as.integer(year))
  month <- suppressWarnings(as.integer(month))
  precip_mm <- suppressWarnings(as.numeric(precip_mm))
  good <- is.finite(year) & month %in% 1:12 & is.finite(precip_mm)
  if (!any(good)) return(data.frame(year=integer(), n=integer(), total=numeric()))
  idx <- which(good)
  rows <- split(idx, year[good])
  out <- data.frame(
    year = as.integer(names(rows)),
    n = vapply(rows, function(i) length(unique(month[i])), integer(1)),
    duplicate_month = vapply(rows, function(i) anyDuplicated(month[i]) > 0L, logical(1)),
    total = vapply(rows, function(i) sum(precip_mm[i]), numeric(1)),
    stringsAsFactors = FALSE)
  out[out$n == 12L & !out$duplicate_month, c("year", "n", "total"), drop = FALSE]
}

# Summarize the temperature climatology only when every distinct calendar month
# in the realized bird-count window has support. A partial realized window must
# never be silently relabeled as the site's breeding-season temperature.
env_realized_window_temperature <- function(monthly, realized_months) {
  if (!is.data.frame(monthly) || !all(c("mon", "temp_c") %in% names(monthly)))
    stop("Realized-window temperature requires monthly mon and temp_c columns.", call. = FALSE)
  mon_value <- suppressWarnings(as.numeric(monthly$mon))
  mon <- suppressWarnings(as.integer(mon_value))
  if (any(!is.finite(mon_value)) || any(mon_value != mon) ||
      any(!mon %in% 1:12) || anyDuplicated(mon))
    stop("Monthly temperature support must contain unique calendar months 1-12.", call. = FALSE)
  realized_value <- suppressWarnings(as.numeric(realized_months))
  realized_integer <- suppressWarnings(as.integer(realized_value))
  if (!length(realized_value) || any(!is.finite(realized_value)) ||
      any(realized_value != realized_integer) || any(!realized_integer %in% 1:12))
    stop("Every realized bird-count month must be an integer calendar month 1-12.", call. = FALSE)
  realized <- sort(unique(realized_integer))
  values <- suppressWarnings(as.numeric(monthly$temp_c[match(realized, mon)]))
  supported <- is.finite(values)
  complete <- all(supported)
  list(
    temp_c = if (complete) mean(values) else NA_real_,
    n_realized_months = as.integer(length(realized)),
    n_supported_realized_months = as.integer(sum(supported)),
    complete = complete,
    realized_months = realized
  )
}

env_realized_visit_months <- function(visits, year_min, year_max) {
  if (!is.data.frame(visits) || !all(c("startDate", "year", "valid_count") %in% names(visits)))
    stop("Realized climate months require visit startDate, year, and valid_count.", call. = FALSE)
  bounds <- suppressWarnings(as.numeric(c(year_min, year_max)))
  if (length(bounds) != 2L || any(!is.finite(bounds)) || any(bounds != as.integer(bounds)) ||
      bounds[[1]] > bounds[[2]])
    stop("Climate analysis years must be ordered integers.", call. = FALSE)
  year_value <- suppressWarnings(as.numeric(visits$year))
  year_integer <- suppressWarnings(as.integer(year_value))
  if (any(!is.finite(year_value)) || any(year_value != year_integer))
    stop("Visit ledger has an invalid climate-analysis year.", call. = FALSE)
  keep <- visits$valid_count %in% TRUE &
    year_integer >= bounds[[1]] & year_integer <= bounds[[2]]
  dates <- as.character(visits$startDate[keep])
  date_token <- substr(dates, 1, 10)
  parsed <- suppressWarnings(as.Date(date_token, format = "%Y-%m-%d"))
  valid <- !is.na(dates) & grepl("^[0-9]{4}-(0[1-9]|1[0-2])-[0-9]{2}", dates) &
    !is.na(parsed) & format(parsed, "%Y-%m-%d") == date_token
  if (!length(dates) || any(!valid))
    stop("Climate analysis has no valid visits or a retained visit has an invalid startDate.",
         call. = FALSE)
  as.integer(format(parsed, "%m"))
}
