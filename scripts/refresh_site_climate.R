# Build cross-site contextual climate tables from the immutable RELEASE-2026
# environmental bundles and the validated bird visit ledger. Environmental
# values are context, never bird measurements or causal drivers.

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("R/site_metadata.R")
source("R/env_helpers.R")

ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", ".")
ENV_DIR <- file.path(ROOT, "data", "env")
SITE_DIR <- file.path(ROOT, "data", "sites")
CLIMATE_OUT <- file.path(ROOT, "data", "site_climate.rds")
MONTH_OUT <- file.path(ROOT, "data", "site_month_clim.rds")
MONTH_LABELS <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

env_files <- list.files(ENV_DIR, pattern = "^[A-Z]{4}[.]rds$", full.names = TRUE)
env_codes <- sort(sub("[.]rds$", "", basename(env_files)))
expected <- sort(as.character(neon_sites$site))
if (!identical(env_codes, expected) || length(env_codes) != 47L)
  stop("Climate build requires the exact 47-site RELEASE-2026 environmental roster.", call. = FALSE)

count_months <- function(site) {
  path <- file.path(SITE_DIR, paste0(site, ".rds"))
  if (!file.exists(path)) stop("Missing site bundle for climate window: ", site, call. = FALSE)
  visits <- readRDS(path)$visits
  tryCatch(
    env_realized_visit_months(
      visits, BIRD_CROSS_SITE_YEAR_MIN, BIRD_CROSS_SITE_YEAR_MAX),
    error = function(error) stop(site, ": ", conditionMessage(error), call. = FALSE))
}

month_set_label <- function(months) {
  labels <- MONTH_LABELS[sort(unique(months))]
  if (length(labels) == 1L) return(labels)
  if (length(labels) == 2L) return(paste(labels, collapse = " & "))
  paste0(paste(labels[-length(labels)], collapse = ", "), " & ", labels[[length(labels)]])
}

climate_rows <- list(); month_rows <- list()
for (path in env_files) {
  site <- sub("[.]rds$", "", basename(path))
  env <- tibble::as_tibble(readRDS(path))
  required <- c("ym", "temp_c", "greenup_pct", "precip_mm")
  if (!nrow(env) || !all(required %in% names(env)))
    stop(site, " has an empty or malformed environmental bundle.", call. = FALSE)
  env$mon <- suppressWarnings(as.integer(substr(env$ym, 6, 7)))
  env$year <- suppressWarnings(as.integer(substr(env$ym, 1, 4)))

  monthly <- env %>%
    dplyr::group_by(.data$mon) %>%
    dplyr::summarise(
      temp_c = if (sum(!is.na(.data$temp_c)) >= 2) mean(.data$temp_c, na.rm = TRUE) else NA_real_,
      greenup_pct = if (sum(!is.na(.data$greenup_pct)) >= 2) mean(.data$greenup_pct, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::right_join(tibble::tibble(mon = 1:12), by = "mon") %>%
    dplyr::arrange(.data$mon)
  monthly$site <- site
  monthly$month_lab <- MONTH_LABELS[monthly$mon]
  month_rows[[site]] <- monthly[, c("site", "mon", "month_lab", "temp_c", "greenup_pct")]

  if (!any(is.finite(monthly$temp_c))) stop(site, " has no usable RELEASE-2026 temperature context.")
  mat <- mean(env$temp_c, na.rm = TRUE)
  amplitude <- diff(range(monthly$temp_c, na.rm = TRUE))
  peak_index <- if (any(!is.na(monthly$greenup_pct)))
    which.max(replace(monthly$greenup_pct, is.na(monthly$greenup_pct), -Inf)) else NA_integer_
  peak_greenup <- if (is.na(peak_index)) NA_real_ else monthly$greenup_pct[[peak_index]]
  peak_month <- if (is.na(peak_index)) NA_integer_ else monthly$mon[[peak_index]]

  annual_precip <- env_complete_annual_precip(env$year, env$mon, env$precip_mm)
  precip <- if (nrow(annual_precip)) round(mean(annual_precip$total)) else NA_real_

  realized <- count_months(site)
  breeding_months <- sort(unique(realized))
  breeding_support <- env_realized_window_temperature(monthly, breeding_months)
  if (!isTRUE(breeding_support$complete))
    stop(sprintf(
      "%s has temperature support for only %d of %d realized bird-count months.",
      site, breeding_support$n_supported_realized_months,
      breeding_support$n_realized_months), call. = FALSE)
  meta <- neon_sites[neon_sites$site == site, , drop = FALSE]
  climate_rows[[site]] <- tibble::tibble(
    site = site,
    lat = meta$lat[[1]],
    lng = meta$lng[[1]],
    domain = meta$domain[[1]],
    mat_c = round(mat, 1),
    breeding_temp_c = round(breeding_support$temp_c, 1),
    n_realized_months = breeding_support$n_realized_months,
    n_supported_realized_months = breeding_support$n_supported_realized_months,
    analysis_year_min = BIRD_CROSS_SITE_YEAR_MIN,
    analysis_year_max = BIRD_CROSS_SITE_YEAR_MAX,
    temp_amp_c = round(amplitude, 1),
    peak_greenup_pct = round(peak_greenup),
    greenup_peak_month = peak_month,
    greenup_peak_lab = if (is.na(peak_month)) NA_character_ else MONTH_LABELS[[peak_month]],
    precip_annual_mm = precip,
    n_precip_months = sum(!is.na(env$precip_mm)),
    n_complete_precip_years = nrow(annual_precip),
    count_months = paste(breeding_support$realized_months, collapse = ","),
    count_month_min = min(realized),
    count_month_max = max(realized),
    count_months_lab = month_set_label(breeding_support$realized_months),
    env_year_min = min(env$year, na.rm = TRUE),
    env_year_max = max(env$year, na.rm = TRUE)
  )
}

climate <- dplyr::bind_rows(climate_rows)
monthly_climate <- dplyr::bind_rows(month_rows)
if (nrow(climate) != 47L || any(!is.finite(climate$breeding_temp_c)) ||
    any(climate$n_realized_months < 1L) ||
    any(climate$n_supported_realized_months != climate$n_realized_months) ||
    any(is.na(climate$count_months) | !nzchar(climate$count_months)) ||
    any(is.na(climate$count_months_lab) | !nzchar(climate$count_months_lab)))
  stop("Climate output failed exact roster, temperature, or realized-window validation.", call. = FALSE)
attr(climate, "release") <- "RELEASE-2026"
attr(monthly_climate, "release") <- "RELEASE-2026"
saveRDS(climate, CLIMATE_OUT, compress = "xz", version = 3)
saveRDS(monthly_climate, MONTH_OUT, compress = "xz", version = 3)
cat(sprintf("OK: wrote climate context for 47 sites; precipitation summaries at %d sites; green-up at %d sites.\n",
            sum(!is.na(climate$precip_annual_mm)), sum(!is.na(climate$peak_greenup_pct))))
