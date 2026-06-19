# ===========================================================================
# refresh_site_climate.R — precompute the cross-site CLIMATE tables the app
# loads at boot, so it never scans 46 env files inside a reactive.
#
# Reads, per site:
#   data/env/<SITE>.rds            — monthly precip/temp/phenology (refresh_env_data.R)
#   ../bird-data-fetch/<SITE>_raw.rds — raw brd_countdata, for the REALIZED count
#                                       months (the bundled obs keeps only year+bout)
#
# Writes two tiny tables:
#   data/site_climate.rds   — ONE row per site (the gradient + headline climate):
#     site, lat, lng, mat_c (mean annual air temp, 46/46 sites),
#     breeding_temp_c (mean temp over the site's realized count months),
#     temp_amp_c (warmest-minus-coldest month, annual amplitude),
#     peak_greenup_pct, greenup_peak_month, greenup_peak_lab,
#     precip_annual_mm (NA where NEON has no gauge), n_precip_months,
#     count_month_min/max, count_months_lab (the realized breeding window),
#     env_year_min/max (so the UI can say "NEON record", not "30-yr normal").
#   data/site_month_clim.rds — site x month (1-12) climatology for the seasonal
#     band: temp_c, greenup_pct (averaged across years; NA where <2 obs).
#
# DEFENSIVE per the Fauna/playbook review: precip is present at only ~19/46
# sites, so it is OPTIONAL and never imputed; a missing env or raw file degrades
# to NA, never a crash. Run:  Rscript scripts/refresh_site_climate.R
# ===========================================================================
suppressMessages({ library(dplyr); library(tibble) })
source("R/site_metadata.R")

ENV_DIR <- "data/env"; RAW_DIR <- "../bird-data-fetch"
MON_LAB <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

env_files <- list.files(ENV_DIR, pattern = "\\.rds$", full.names = TRUE)
if (!length(env_files)) stop("No env files in ", ENV_DIR, " — copy data/env/<SITE>.rds first.")

# realized count months for a site, from the raw countdata startDate (build-time
# only; the runtime bundle deliberately doesn't carry the date). Returns integer
# months present, or NULL if the raw file is missing.
count_months <- function(site) {
  f <- file.path(RAW_DIR, paste0(site, "_raw.rds"))
  if (!file.exists(f)) return(NULL)
  cd <- tryCatch(readRDS(f)$brd_countdata, error = function(e) NULL)
  if (is.null(cd) || !"startDate" %in% names(cd) || !nrow(cd)) return(NULL)
  m <- suppressWarnings(as.integer(substr(as.character(cd$startDate), 6, 7)))
  m <- m[is.finite(m)]; if (!length(m)) return(NULL)
  m
}

# label a contiguous-ish month set, e.g. c(5,6,7) -> "May-Jul". Use – (en-dash)
# via Unicode escape so the literal is locale-safe: R-4.1.1 reading this UTF-8 source
# under a Windows-1252 locale would otherwise mojibake a raw "–" into "â€“".
month_span_lab <- function(mons) {
  if (is.null(mons) || !length(mons)) return(NA_character_)
  r <- range(mons); if (r[1] == r[2]) MON_LAB[r[1]] else paste0(MON_LAB[r[1]], intToUtf8(8211L), MON_LAB[r[2]])
}

clim_rows <- list(); month_rows <- list()
for (f in env_files) {
  s <- sub("\\.rds$", "", basename(f))
  e <- tryCatch(tibble::as_tibble(readRDS(f)), error = function(ee) NULL)
  if (is.null(e) || !nrow(e)) next
  e$mon <- suppressWarnings(as.integer(substr(e$ym, 6, 7)))
  e$yr  <- substr(e$ym, 1, 4)

  # month climatology (average across years); NA where <2 observations
  mc <- e %>% group_by(mon) %>% summarise(
      temp_c = if (sum(!is.na(temp_c)) >= 2) mean(temp_c, na.rm = TRUE) else NA_real_,
      greenup_pct = if (sum(!is.na(greenup_pct)) >= 2) mean(greenup_pct, na.rm = TRUE) else NA_real_,
      .groups = "drop") %>%
    right_join(tibble(mon = 1:12), by = "mon") %>% arrange(mon)
  mc$site <- s; mc$month_lab <- MON_LAB[mc$mon]
  month_rows[[s]] <- mc[, c("site","mon","month_lab","temp_c","greenup_pct")]

  # annual climate
  mat <- mean(e$temp_c, na.rm = TRUE)
  temp_amp <- if (any(!is.na(mc$temp_c))) diff(range(mc$temp_c, na.rm = TRUE)) else NA_real_
  gp_i <- if (any(!is.na(mc$greenup_pct))) which.max(replace(mc$greenup_pct, is.na(mc$greenup_pct), -Inf)) else NA_integer_
  peak_gp  <- if (!is.na(gp_i)) mc$greenup_pct[gp_i] else NA_real_
  peak_gpm <- if (!is.na(gp_i)) mc$mon[gp_i] else NA_integer_

  # annual precip: only where NEON actually has a gauge (>=6 months of data)
  pr_by_yr <- e %>% group_by(yr) %>% summarise(n = sum(!is.na(precip_mm)),
                 tot = sum(precip_mm, na.rm = TRUE), .groups = "drop") %>% filter(n >= 6)
  precip_annual <- if (nrow(pr_by_yr)) round(mean(pr_by_yr$tot)) else NA_real_
  n_precip <- sum(!is.na(e$precip_mm))

  # realized count window + breeding-season temp from those months
  cm <- count_months(s)
  cm_min <- if (!is.null(cm)) min(cm) else NA_integer_
  cm_max <- if (!is.null(cm)) max(cm) else NA_integer_
  bwin <- if (!is.null(cm)) sort(unique(cm)) else 5:7   # fallback: protocol window
  breeding_temp <- if (any(!is.na(mc$temp_c[mc$mon %in% bwin]))) mean(mc$temp_c[mc$mon %in% bwin], na.rm = TRUE) else NA_real_

  meta <- neon_sites[neon_sites$site == s, ]
  clim_rows[[s]] <- tibble(
    site = s,
    lat = if (nrow(meta)) meta$lat[1] else NA_real_,
    lng = if (nrow(meta)) meta$lng[1] else NA_real_,
    domain = if (nrow(meta)) meta$domain[1] else NA_character_,
    mat_c = round(mat, 1),
    breeding_temp_c = round(breeding_temp, 1),
    temp_amp_c = round(temp_amp, 1),
    peak_greenup_pct = round(peak_gp),
    greenup_peak_month = peak_gpm,
    greenup_peak_lab = if (!is.na(peak_gpm)) MON_LAB[peak_gpm] else NA_character_,
    precip_annual_mm = precip_annual,
    n_precip_months = n_precip,
    count_month_min = cm_min,
    count_month_max = cm_max,
    count_months_lab = month_span_lab(if (!is.null(cm)) cm else NULL),
    env_year_min = suppressWarnings(min(as.integer(e$yr), na.rm = TRUE)),
    env_year_max = suppressWarnings(max(as.integer(e$yr), na.rm = TRUE)))
}

clim <- bind_rows(clim_rows)
mclim <- bind_rows(month_rows)
saveRDS(clim,  "data/site_climate.rds",    compress = "xz")
saveRDS(mclim, "data/site_month_clim.rds", compress = "xz")

cat(sprintf("site_climate.rds: %d sites | temp 46/46, precip %d sites, greenup %d sites\n",
            nrow(clim), sum(!is.na(clim$precip_annual_mm)), sum(!is.na(clim$peak_greenup_pct))))
cat(sprintf("realized count window known for %d/%d sites\n",
            sum(!is.na(clim$count_months_lab)), nrow(clim)))
print(clim[order(clim$mat_c), c("site","mat_c","breeding_temp_c","peak_greenup_pct","greenup_peak_lab","count_months_lab","precip_annual_mm")], n = nrow(clim))
