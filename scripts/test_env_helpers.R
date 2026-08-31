source("R/env_helpers.R")

check <- function(ok, label) if (!isTRUE(ok)) stop("ENV CHECK FAILED: ", label, call. = FALSE)
fixture <- data.frame(
  startDateTime = c("2020-01-01", "2020-02-01", "2020-02-02",
                    "2020-01-01", "2020-02-01"),
  horizontalPosition = c(1, 1, 1, 2, 2),
  precipBulk = c(1, 2, 3, 10, 20), finalQF = 0, stringsAsFactors = FALSE)
attr(fixture, "source_table") <- "WEIPRE_daily"
selected <- env_select_single_stream(fixture, "precipBulk", "fixture")
selection <- attr(selected, "stream_selection")
check(nrow(selected) == 3L && identical(selection$signature, "1") &&
      selection$candidate_streams == 2L && identical(selection$quality_flag_column, "finalQF"),
      "single most-supported finalQF-governed stream is selected")

vertical_fixture <- data.frame(
  startDateTime = c("2020-01-01", "2020-02-01", "2020-01-01",
                    "2020-01-01", "2020-02-01", "2020-03-01"),
  horizontalPosition = c("B", "B", "A", "Z", "Z", "Z"),
  verticalPosition = c(1, 1, 1, 2, 2, 2),
  tempSingleMean = c(10, 11, 12, 20, 21, 22),
  finalQF = 0, stringsAsFactors = FALSE)
attr(vertical_fixture, "source_table") <- "SAAT_30min"
vertical_selected <- env_select_single_stream(
  vertical_fixture, "tempSingleMean", "temperature fixture",
  vertical_position_rule = "lowest_recorded")
vertical_selection <- attr(vertical_selected, "stream_selection")
check(identical(vertical_selection$selected_vertical_position, 1) &&
      identical(vertical_selection$signature, "B|1") && nrow(vertical_selected) == 2L,
      "lowest recorded vertical position precedes within-position support ranking")
check(grepl("not metres", vertical_selection$rule, fixed = TRUE),
      "vertical selection receipt forbids an undocumented height interpretation")
check(grepl("NEON.DOC.000646", vertical_selection$vertical_position_basis, fixed = TRUE),
      "vertical selection receipt records the official lowest-index basis")
vertical_reversed <- vertical_fixture[rev(seq_len(nrow(vertical_fixture))), , drop = FALSE]
attr(vertical_reversed, "source_table") <- "SAAT_30min"
vertical_reversed_selected <- env_select_single_stream(
  vertical_reversed, "tempSingleMean", "temperature fixture reversed",
  vertical_position_rule = "lowest_recorded")
check(identical(vertical_selected, vertical_reversed_selected) &&
      identical(attr(vertical_selected, "stream_selection"),
                attr(vertical_reversed_selected, "stream_selection")),
      "multi-stream vertical selection is invariant to input row order")

bad_vertical <- vertical_fixture
bad_vertical$verticalPosition <- "unknown"
attr(bad_vertical, "source_table") <- "SAAT_30min"
err <- tryCatch({
  env_select_single_stream(bad_vertical, "tempSingleMean", "temperature fixture",
                           vertical_position_rule = "lowest_recorded")
  NULL
}, error = identity)
check(inherits(err, "error") && grepl("verticalPosition", conditionMessage(err)),
      "unsupported vertical-position metadata fails closed")

duplicate <- rbind(fixture[1, ], fixture[1, ])
err <- tryCatch({ env_select_single_stream(duplicate, "precipBulk", "fixture"); NULL }, error=identity)
check(inherits(err, "error") && grepl("duplicate", conditionMessage(err)),
      "duplicate timestamps fail closed")

daily <- data.frame(
  startDateTime = format(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by="day")),
  horizontalPosition = 1, precipBulk = 1, finalQF = 0, stringsAsFactors = FALSE)
attr(daily, "source_table") <- "WEIPRE_daily"
daily_selected <- env_select_single_stream(daily, "precipBulk", "fixture daily")
daily_month <- env_monthly_precip(daily_selected, "precipBulk", "fixture daily")
check(daily_month$value == 31 && daily_month$precip_coverage_pct == 100,
      "complete finalQF-passing month receives a precipitation total")
daily$finalQF[[3]] <- 1
attr(daily, "source_table") <- "WEIPRE_daily"
partial <- env_monthly_precip(env_select_single_stream(daily, "precipBulk", "fixture partial"),
                              "precipBulk", "fixture partial")
check(is.na(partial$value) && partial$precip_n == 30L,
      "one rejected day suppresses the partial monthly precipitation total")

half_hours <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
                  as.POSIXct("2020-01-31 23:30:00", tz = "UTC"), by = "30 min")
temperature <- data.frame(
  startDateTime = format(half_hours, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  namedLocation = "tower-a", horizontalPosition = 1, verticalPosition = 1,
  tempSingleMean = 10, tempSingleMinimum = 9, tempSingleMaximum = 11,
  finalQF = 0, stringsAsFactors = FALSE)
attr(temperature, "source_table") <- "SAAT_30min"
temperature_selected <- env_select_single_stream(
  temperature, "tempSingleMean", "temperature complete",
  vertical_position_rule = "lowest_recorded")
temperature_month <- env_monthly_temperature(temperature_selected,
                                             label = "temperature complete")
check(nrow(temperature_month) == 1L && temperature_month$temp_c == 10 &&
      temperature_month$temp_min == 9 && temperature_month$temp_max == 11 &&
      temperature_month$temp_n == 1488L && temperature_month$temp_expected == 1488L &&
      temperature_month$temp_coverage_pct == 100,
      "complete 30-minute month receives coverage-qualified temperature summaries")

temperature_qf <- temperature
temperature_qf$tempSingleMean[[1]] <- 999
temperature_qf$finalQF[[1]] <- 1
attr(temperature_qf, "source_table") <- "SAAT_30min"
qf_month <- env_monthly_temperature(env_select_single_stream(
  temperature_qf, "tempSingleMean", "temperature QF",
  vertical_position_rule = "lowest_recorded"), label = "temperature QF")
check(qf_month$temp_c == 10 && qf_month$temp_n == 1487L,
      "failed finalQF interval is excluded from monthly temperature")

temperature_incomplete <- temperature[seq_len(1000), , drop = FALSE]
attr(temperature_incomplete, "source_table") <- "SAAT_30min"
incomplete_month <- env_monthly_temperature(env_select_single_stream(
  temperature_incomplete, "tempSingleMean", "temperature incomplete",
  vertical_position_rule = "lowest_recorded"), label = "temperature incomplete")
check(is.na(incomplete_month$temp_c) && incomplete_month$temp_n == 1000L &&
      incomplete_month$temp_expected == 1488L && incomplete_month$temp_coverage_pct < 75,
      "month below 75% expected-interval coverage is unavailable")

set.seed(20260728)
temperature_shuffled <- temperature[sample.int(nrow(temperature)), , drop = FALSE]
attr(temperature_shuffled, "source_table") <- "SAAT_30min"
shuffled_selected <- env_select_single_stream(
  temperature_shuffled, "tempSingleMean", "temperature shuffled",
  vertical_position_rule = "lowest_recorded")
shuffled_month <- env_monthly_temperature(shuffled_selected,
                                          label = "temperature shuffled")
check(identical(temperature_selected, shuffled_selected) &&
      identical(temperature_month, shuffled_month),
      "stream selection and monthly aggregation are invariant to input row order")

temperature_duplicate <- rbind(temperature[1, , drop = FALSE],
                               temperature[1, , drop = FALSE])
attr(temperature_duplicate, "source_table") <- "SAAT_30min"
err <- tryCatch({
  env_select_single_stream(temperature_duplicate, "tempSingleMean",
                           "temperature duplicate",
                           vertical_position_rule = "lowest_recorded")
  NULL
}, error = identity)
check(inherits(err, "error") && grepl("duplicate", conditionMessage(err)),
      "duplicate temperature timestamps fail closed")

annual <- env_complete_annual_precip(c(rep(2020, 12), rep(2021, 6)),
                                     c(1:12, 1:6), c(rep(10, 12), rep(20, 6)))
check(nrow(annual) == 1L && annual$year == 2020L && annual$total == 120,
      "only complete 12-month years enter annual precipitation")
duplicate_year <- env_complete_annual_precip(rep(2022, 12), c(1:11, 11), rep(1, 12))
check(!nrow(duplicate_year), "duplicate months cannot masquerade as a complete year")

realized_monthly <- data.frame(mon = 1:12, temp_c = seq_len(12), stringsAsFactors = FALSE)
realized_complete <- env_realized_window_temperature(realized_monthly, c(5, 6, 7, 6))
check(isTRUE(realized_complete$complete) && realized_complete$temp_c == 6 &&
      realized_complete$n_realized_months == 3L &&
      realized_complete$n_supported_realized_months == 3L &&
      identical(realized_complete$realized_months, 5:7),
      "complete realized bird months receive an all-month temperature mean")
realized_monthly$temp_c[[6]] <- NA_real_
realized_partial <- env_realized_window_temperature(realized_monthly, 5:7)
check(!isTRUE(realized_partial$complete) && is.na(realized_partial$temp_c) &&
      !is.nan(realized_partial$temp_c) &&
      realized_partial$n_realized_months == 3L &&
      realized_partial$n_supported_realized_months == 2L,
      "one unsupported realized bird month suppresses only the breeding-window mean")
realized_none_monthly <- realized_monthly
realized_none_monthly$temp_c[5:7] <- NA_real_
realized_none <- env_realized_window_temperature(realized_none_monthly, 5:7)
check(!isTRUE(realized_none$complete) && is.na(realized_none$temp_c) &&
      !is.nan(realized_none$temp_c) &&
      realized_none$n_realized_months == 3L &&
      realized_none$n_supported_realized_months == 0L,
      "zero supported realized months remain explicit without imputation")
duplicate_monthly <- rbind(realized_monthly, realized_monthly[1, , drop = FALSE])
err <- tryCatch({ env_realized_window_temperature(duplicate_monthly, 5:7); NULL },
                error = identity)
check(inherits(err, "error") && grepl("unique calendar months", conditionMessage(err)),
      "duplicate monthly climatology rows fail closed")
err <- tryCatch({ env_realized_window_temperature(realized_monthly, c(5, 13)); NULL },
                error = identity)
check(inherits(err, "error") && grepl("integer calendar month", conditionMessage(err)),
      "one valid realized month cannot hide an invalid month")
err <- tryCatch({ env_realized_window_temperature(realized_monthly, c(5, 5.9)); NULL },
                error = identity)
check(inherits(err, "error") && grepl("integer calendar month", conditionMessage(err)),
      "fractional realized months cannot be truncated")
nonrealized_missing <- realized_monthly
nonrealized_missing$temp_c[[1]] <- NA_real_
realized_noncontiguous <- env_realized_window_temperature(nonrealized_missing, c(5, 7))
check(isTRUE(realized_noncontiguous$complete) && realized_noncontiguous$temp_c == 6 &&
      identical(realized_noncontiguous$realized_months, c(5L, 7L)),
      "missing non-realized months do not suppress an exact noncontiguous realized set")
visit_month_fixture <- data.frame(
  startDate = c("2016-04-03T05:00:00Z", "2017-05-03T05:00:00Z",
                "2018-07-04 05:00:00", "2024-07-05T05:00:00Z"),
  year = c(2016L, 2017L, 2018L, 2024L), valid_count = TRUE,
  stringsAsFactors = FALSE)
check(identical(env_realized_visit_months(visit_month_fixture, 2017L, 2024L),
                c(5L, 7L, 7L)),
      "realized visit months retain exact valid counts in the shared analysis window")
bad_visit_date <- visit_month_fixture
bad_visit_date$startDate[[2]] <- "2017-13-03"
err <- tryCatch({ env_realized_visit_months(bad_visit_date, 2017L, 2024L); NULL },
                error = identity)
check(inherits(err, "error") && grepl("invalid startDate", conditionMessage(err)),
      "one malformed retained visit date cannot be silently discarded")
fractional_visit_year <- visit_month_fixture
fractional_visit_year$year[[2]] <- 2017.5
err <- tryCatch({ env_realized_visit_months(fractional_visit_year, 2017L, 2024L); NULL },
                error = identity)
check(inherits(err, "error") && grepl("invalid climate-analysis year", conditionMessage(err)),
      "fractional visit years cannot be truncated")
cat("OK: deterministic environmental stream, temperature, and annual-coverage fixtures passed.\n")
