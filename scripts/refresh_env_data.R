# ===========================================================================
# refresh_env_data.R — build the bundled per-site ENVIRONMENTAL overlays
#
# For every release site, download the three co-located contextual products from
# immutable RELEASE-2026, aggregate them to one value per calendar month, and
# write a tiny staged data/env/<SITE>.rds plus a source receipt. This is a clean
# release producer: it refuses a non-empty destination and never top-ups or
# silently carries prior bytes.
#
# Output schema (one row per site-month), matching global.R ENV_LAYERS:
#   siteID, ym ("YYYY-MM"), date (first of month),
#   precip_mm   (monthly SUM,  DP1.00044.001 weighing-gauge precipitation)
#   temp_c/min/max (monthly MEAN/MIN/MAX, DP1.00002.001 single-aspirated air temp)
#   flowering_pct (monthly STATUS yes-share, "Open flowers",  DP1.10055.001)
#   greenup_pct   (monthly STATUS yes-share, early leaf-out bundle, DP1.10055.001)
#   fruiting_pct  (monthly STATUS yes-share, "Fruits" exact,   DP1.10055.001)
#   <col>_n       (distinct individuals behind each phenology share; <5 -> share NA)
#   source = "neon"
#
# Run from the project root with NEON_TOKEN and BIRD_OUTPUT_ROOT set.
#
# IMPORTANT — verify table/column names once before a full run:
#   neonUtilities::loadByProduct("DP1.00044.001", site="JORN",
#       startdate="2018-07", enddate="2018-09", check.size="F") |> names()
# NEON occasionally renames published tables; the matchers below are deliberately
# pattern-based, but every required release channel still fails closed when its
# table or auditable metadata cannot be identified.
# Sensor products also return MANY sub-streams. For air temperature, selection
# uses the smallest finite RELEASE-2026 verticalPosition strictly as published
# ordering metadata (never as metres), then support and a lexical tie-break.
# Complete pre-aggregation rows/QF are retained for each selected sensor stream.
# To bound the cross-job artifact, unselected streams retain canonical
# per-stream/month counts and content digests rather than every 30-minute row.
# ===========================================================================

options(timeout = 3600)
suppressMessages({
  library(neonUtilities)
  library(dplyr)
  library(tibble)
  library(jsonlite)
  library(digest)
})

RELEASE <- "RELEASE-2026"
ROOT <- Sys.getenv("BIRD_OUTPUT_ROOT", "build/candidate")
ENV_RECEIPT <- Sys.getenv("BIRD_ENV_RECEIPT", file.path(ROOT, "data", "environment_source_receipt.json"))
ENV_EVIDENCE_DIR <- Sys.getenv(
  "BIRD_ENV_EVIDENCE_DIR",
  file.path(ROOT, "validation-evidence", "environment")
)
.neon_token <- trimws(Sys.getenv("NEON_TOKEN", ""))
if (!nzchar(.neon_token))
  stop("NEON_TOKEN is required for an environmental release build.", call. = FALSE)

source("R/site_metadata.R")  # canonical site list
source("R/env_helpers.R")

out_dir <- file.path(ROOT, "data", "env")
if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE)))
  stop("Staged environment directory must be empty: ", out_dir, call. = FALSE)
if (dir.exists(ENV_EVIDENCE_DIR) &&
    length(list.files(ENV_EVIDENCE_DIR, all.files = TRUE, no.. = TRUE)))
  stop("Staged environmental evidence directory must be empty: ",
       ENV_EVIDENCE_DIR, call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ENV_EVIDENCE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(ENV_RECEIPT), recursive = TRUE, showWarnings = FALSE)

start_d <- "2013-01"
end_d   <- "2024-12"
sites   <- neon_sites$site
if (length(sites) != 47L || !"PUUM" %in% sites)
  stop("Canonical environmental context roster must contain 47 sites including PUUM.", call. = FALSE)

# ---- generic helpers ------------------------------------------------------

# Pull the first table in a loadByProduct() result whose name matches `tbl_rx`.
pick_table <- function(dl, tbl_rx) {
  if (is.null(dl)) return(NULL)
  nm <- sort(grep(tbl_rx, names(dl), value = TRUE), method = "radix")
  if (!length(nm)) return(NULL)
  out <- tibble::as_tibble(dl[[nm[1]]])
  attr(out, "source_table") <- nm[[1]]
  out
}

source_table_names <- function(dl) {
  if (is.null(dl)) character() else
    sort(unique(as.character(names(dl))), method = "radix")
}

canonical_order <- function(tb) {
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

resolve_final_qf <- function(tb, value_name, label) {
  qf_names <- grep("finalQF$", names(tb), value = TRUE, ignore.case = TRUE)
  exact_qf <- qf_names[tolower(qf_names) == "finalqf"]
  value_qf <- qf_names[tolower(qf_names) == tolower(paste0(value_name, "FinalQF"))]
  qf_name <- if (length(value_qf) == 1L) value_qf[[1]] else
    if (length(exact_qf)) exact_qf[[1]] else
      if (length(qf_names) == 1L) qf_names[[1]] else NULL
  if (is.null(qf_name))
    stop(label, " has no unambiguous finalQF pass channel.", call. = FALSE)
  qf_name
}

canonical_stream_input <- function(tb, value_rx, label,
                                   output_value_name,
                                   extra_channels = character()) {
  if (is.null(tb)) return(NULL)
  source_table <- attr(tb, "source_table", exact = TRUE)
  value_name <- env_pick_col_name(tb, value_rx)
  time_name <- env_time_col(tb)
  if (is.null(source_table) || is.null(value_name) || is.null(time_name))
    stop(label, " lacks a source table, value, or timestamp field.", call. = FALSE)
  qf_name <- resolve_final_qf(tb, value_name, label)
  stream_columns <- intersect(
    c("namedLocation", "sensorLocation", "sensorPositionID",
      "horizontalPosition", "verticalPosition"),
    names(tb)
  )
  extra_names <- vapply(extra_channels, function(rx) {
    name <- env_pick_col_name(tb, rx)
    if (is.null(name)) NA_character_ else name
  }, character(1))
  if (length(extra_names) && any(is.na(extra_names)))
    stop(label, " lacks a required companion value channel.", call. = FALSE)

  out <- data.frame(startDateTime = as.character(tb[[time_name]]),
                    stringsAsFactors = FALSE, check.names = FALSE)
  for (name in stream_columns) out[[name]] <- as.character(tb[[name]])
  out[[output_value_name]] <- suppressWarnings(as.numeric(tb[[value_name]]))
  if (length(extra_names)) {
    for (i in seq_along(extra_names))
      out[[names(extra_channels)[[i]]]] <- suppressWarnings(as.numeric(tb[[extra_names[[i]]]]))
  }
  out$finalQF <- suppressWarnings(as.numeric(tb[[qf_name]]))
  out <- canonical_order(out)
  attr(out, "source_table") <- source_table
  list(
    table = out,
    source_columns = list(
      timestamp = time_name,
      stream = stream_columns,
      value = value_name,
      companions = as.list(extra_names),
      quality_flag = qf_name
    )
  )
}

canonical_token <- function(x) {
  if (length(x) != 1L || is.na(x)) return("<NA>")
  if (is.numeric(x)) {
    if (is.nan(x)) return("<NaN>")
    if (is.infinite(x)) return(if (x > 0) "<Inf>" else "<-Inf>")
    return(sprintf("%.17g", x))
  }
  enc2utf8(as.character(x))
}

stream_evidence_summary <- function(tb, value_name) {
  stream_columns <- intersect(
    c("namedLocation", "sensorLocation", "sensorPositionID",
      "horizontalPosition", "verticalPosition"), names(tb)
  )
  empty_streams <- data.frame(signature = character(), stringsAsFactors = FALSE)
  for (name in stream_columns) empty_streams[[name]] <- character()
  empty_months <- data.frame(
    signature = character(), ym = character(), rows = integer(),
    final_qf_pass_rows = integer(), passing_finite_values = integer(),
    content_sha256 = character(), stringsAsFactors = FALSE
  )
  if (is.null(tb) || !nrow(tb))
    return(list(streams = empty_streams, months = empty_months))

  if (length(stream_columns)) {
    metadata <- lapply(tb[stream_columns], function(x) {
      value <- trimws(as.character(x)); value[env_blank(value)] <- "<NA>"; value
    })
    signature <- do.call(paste, c(metadata, sep = "|"))
  } else {
    metadata <- list()
    signature <- rep("single-stream", nrow(tb))
  }
  stream_groups <- split(seq_len(nrow(tb)), signature)
  streams <- data.frame(signature = names(stream_groups), stringsAsFactors = FALSE)
  for (name in stream_columns) {
    streams[[name]] <- vapply(stream_groups, function(i) {
      values <- unique(metadata[[name]][i])
      if (length(values) != 1L)
        stop("Canonical stream signature does not identify one ", name, ".", call. = FALSE)
      values[[1]]
    }, character(1))
  }
  streams <- canonical_order(streams)

  timestamp <- as.character(tb$startDateTime)
  ym <- substr(timestamp, 1, 7)
  ym[is.na(ym)] <- "<NA>"
  value <- suppressWarnings(as.numeric(tb[[value_name]]))
  qf <- suppressWarnings(as.numeric(tb$finalQF))
  qf_pass <- is.finite(qf) & qf == 0
  group_key <- paste(signature, ym, sep = "\x1f")
  groups <- split(seq_len(nrow(tb)), group_key)
  months <- do.call(rbind, lapply(groups, function(i) {
    row_material <- vapply(i, function(j) paste(
      canonical_token(timestamp[[j]]),
      canonical_token(value[[j]]),
      canonical_token(qf[[j]]),
      sep = "\t"
    ), character(1))
    data.frame(
      signature = signature[[i[[1]]]],
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
  rownames(months) <- NULL
  list(streams = streams, months = canonical_order(months))
}

canonical_pheno_input <- function(tb, label) {
  required <- c("phenophaseName", "phenophaseStatus", "individualID")
  if (is.null(tb) || !nrow(tb)) return(NULL)
  if (!all(required %in% names(tb)))
    stop(label, " lacks a required phenophase/status/individual field.", call. = FALSE)
  time_name <- env_time_col(tb)
  if (is.null(time_name)) stop(label, " lacks a timestamp field.", call. = FALSE)
  phase <- as.character(tb$phenophaseName)
  target <- grepl(
    "^Open flowers$|[Bb]reaking leaf buds|[Ee]merging needles|[Yy]oung (leaves|needles)|[Ii]ncreasing leaf size|[Ii]nitial growth|^Fruits$",
    phase
  )
  raw_id <- as.character(tb$individualID)
  valid_id <- !env_blank(raw_id)
  individual_key <- rep(NA_character_, length(raw_id))
  unique_id <- unique(raw_id[valid_id])
  unique_key <- vapply(unique_id, function(value) paste0(
    "plant-sha256:", digest::digest(
      paste("NEON-Breeding-Birds plant individual evidence v1", value, sep = "\n"),
      algo = "sha256", serialize = FALSE
    )
  ), character(1))
  individual_key[valid_id] <- unique_key[match(raw_id[valid_id], unique_id)]
  out <- data.frame(
    startDateTime = as.character(tb[[time_name]])[target],
    phenophaseName = phase[target],
    phenophaseStatus = as.character(tb$phenophaseStatus)[target],
    individualID = individual_key[target],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- canonical_order(out)
  list(
    table = out,
    source_rows = nrow(tb),
    source_columns = list(
      timestamp = time_name,
      phenophase = "phenophaseName",
      status = "phenophaseStatus",
      individual = "individualID"
    )
  )
}

# A "YYYY-MM" key from whatever date/time column a table carries.
month_key <- function(tb) env_month_key(tb)

# Monthly STATUS yes-share for a phenophase group (DP1.10055.001 phe_statusintensity),
# built defensibly per the phenology review:
#  - only status yes/no count (uncertain/blank dropped from BOTH num & denom — the
#    old fruit code folded them in as 0, biasing the share down);
#  - grain = individual x month (a high-cadence month doesn't over-weight): an
#    individual counts "yes" if seen in-phenophase at >=1 bout that month;
#  - returns a companion n (distinct individuals); months with n<5 -> share NA.
# Returns a data.frame(ym, share, n) or NULL when the phenophase isn't recorded.
pheno_share <- function(pht, name_rx) {
  if (is.null(pht) || !nrow(pht)) return(NULL)
  if (!all(c("phenophaseName", "phenophaseStatus", "individualID") %in% names(pht))) return(NULL)
  st   <- tolower(trimws(as.character(pht$phenophaseStatus)))
  keep <- grepl(name_rx, pht$phenophaseName) & st %in% c("yes", "no") &
    !env_blank(pht$individualID)
  if (!any(keep)) return(NULL)
  d <- tibble::tibble(individualID = pht$individualID[keep],
                      ym  = month_key(pht[keep, , drop = FALSE]),
                      yes = as.integer(st[keep] == "yes"))
  d <- d[!is.na(d$ym), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  im <- d %>% dplyr::group_by(.data$individualID, .data$ym) %>%
    dplyr::summarise(yes = max(.data$yes), .groups = "drop")
  mo <- im %>% dplyr::group_by(.data$ym) %>%
    dplyr::summarise(share = 100 * mean(.data$yes), n = dplyr::n(), .groups = "drop")
  mo$share[mo$n < 5] <- NA_real_
  as.data.frame(mo)
}

safe_load <- function(dpID, site, timeIndex = NULL) {
  # timeIndex (e.g. 30) restricts a sensor product to ONE averaging interval,
  # so we don't download the high-volume 1-min tables we'd only discard.
  args <- list(dpID = dpID, site = site, release = RELEASE,
               startdate = start_d, enddate = end_d,
               package = "basic", check.size = FALSE, token = .neon_token)
  if (!is.null(timeIndex)) args$timeIndex <- timeIndex
  tryCatch(do.call(loadByProduct, args),
    error = function(e) { cat(sprintf("      ! %s: %s\n", dpID, conditionMessage(e))); NULL })
}

# ---- per-site build -------------------------------------------------------

build_site_env <- function(site) {
  # full monthly skeleton across the whole window
  months <- format(seq(as.Date(paste0(start_d, "-01")),
                       as.Date(paste0(end_d, "-01")), by = "month"), "%Y-%m")
  out <- tibble::tibble(siteID = site, ym = months,
                        date = as.Date(paste0(months, "-01")))

  # 1) precipitation — weighing gauge, DAILY table, monthly SUM (mm)
  pr <- safe_load("DP1.00044.001", site)
  # NEON publishes precip as WEIPRE_* (weighing gauge), PRIPRE_* (primary) or
  # SECPRE_* (secondary tipping bucket); prefer the DAILY table, fall back to
  # 60/30-min. Sum to a monthly total (mm). Some arid sites (e.g. JORN) have no
  # precip deployment at all -> stays NA, which the UI handles gracefully.
  prt_raw <- pick_table(pr, "(WEIPRE|PRIPRE|SECPRE)_daily|wss_daily_precip|.*daily.*[Pp]recip")
  if (is.null(prt_raw))
    prt_raw <- pick_table(pr, "(WEIPRE|PRIPRE|SECPRE)_(60|30)min|.*[Pp]recip")
  pr_canonical <- if (is.null(prt_raw)) NULL else canonical_stream_input(
    prt_raw, "[Pp]recipBulk|secPrecipBulk|priPrecipBulk",
    paste(site, "precipitation"), "precipBulk"
  )
  prt_all <- if (is.null(pr_canonical)) NULL else pr_canonical$table
  prt <- env_select_single_stream(prt_all, "precipBulk", paste(site, "precipitation"))
  pr_selection <- if (is.null(prt)) NULL else attr(prt, "stream_selection")
  precip_month <- env_monthly_precip(prt, "precipBulk",
                                     paste(site, "precipitation"))
  if (is.null(precip_month)) {
    out$precip_mm <- NA_real_; out$precip_n <- NA_integer_
    out$precip_expected <- NA_integer_; out$precip_coverage_pct <- NA_real_
  } else {
    names(precip_month)[names(precip_month) == "value"] <- "precip_mm"
    out <- dplyr::left_join(out, precip_month, by = "ym")
  }

  # 2) air temperature — single aspirated, 30-min ONLY (timeIndex=30 skips the
  #    high-volume 1-min table we'd discard anyway); keep one tower level
  at <- safe_load("DP1.00002.001", site, timeIndex = 30)
  att_raw <- pick_table(at, "SAAT_30min|saat.*30")
  if (is.null(att_raw) || !nrow(att_raw)) stop(site, " has no RELEASE-2026 air-temperature support.")
  at_canonical <- canonical_stream_input(
    att_raw, "tempSingleMean", paste(site, "air temperature"), "tempSingleMean",
    c(tempSingleMinimum = "tempSingleMinimum", tempSingleMaximum = "tempSingleMaximum")
  )
  att_all <- at_canonical$table
  att <- env_select_single_stream(
    att_all, "tempSingleMean", paste(site, "air temperature"),
    vertical_position_rule = "lowest_recorded")
  if (is.null(att) || !nrow(att)) stop(site, " has no deterministic air-temperature stream.")
  at_selection <- attr(att, "stream_selection")
  temp_month <- env_monthly_temperature(att, label = paste(site, "air temperature"))
  if (is.null(temp_month) || !any(is.finite(temp_month$temp_c)))
    stop(site, " has no coverage-qualified monthly air-temperature context.", call. = FALSE)
  at_selection$monthly_coverage <- attr(temp_month, "monthly_coverage")
  out <- dplyr::left_join(out, temp_month, by = "ym")

  # (Relative humidity DP1.00098.001 and soil water content DP1.00094.001 are
  #  intentionally NOT built — soil water especially is a very-high-volume 30-min
  #  product. The bundled overlays are precipitation + temperature + three
  #  plant-phenology status summaries; ENV_LAYERS in global.R lists the public
  #  channels.)

  # 3) plant phenology — DP1.10055.001 phe_statusintensity. Three monthly STATUS
  #    yes-share signals (via pheno_share, with the yes/no filter, individual x
  #    month grain, and n<5 -> NA guardrails):
  #      flowering_pct  "Open flowers"        — seed-crop precursor (arid LEAD driver)
  #      greenup_pct    early leaf-out bundle  — precip-pulse proxy / forage (arid LEAD)
  #      fruiting_pct   "Fruits" (exact)      — mast signal (mesic/forest LEAD)
  #    Arid sites (SRER/JORN) have NO Fruits but rich flowers + green-up, so this
  #    gives them a real phenology signal the old fruit-only build missed. Each
  #    layer also gets a <col>_n companion (distinct individuals behind the share).
  ph  <- safe_load("DP1.10055.001", site)
  pht_raw <- pick_table(ph, "phe_statusintensity")
  if (is.null(pht_raw) || !nrow(pht_raw)) stop(site, " has no RELEASE-2026 plant-phenology support.")
  ph_canonical <- canonical_pheno_input(pht_raw, paste(site, "plant phenology"))
  pht <- ph_canonical$table
  join_pheno <- function(out, rx, col) {
    sh <- pheno_share(pht, rx)
    if (is.null(sh)) { out[[col]] <- NA_real_; out[[paste0(col, "_n")]] <- NA_integer_; return(out) }
    j <- dplyr::left_join(out["ym"], sh, by = "ym")
    out[[col]]               <- j$share
    out[[paste0(col, "_n")]] <- j$n
    out
  }
  out <- join_pheno(out, "^Open flowers$", "flowering_pct")
  out <- join_pheno(out, "[Bb]reaking leaf buds|[Ee]merging needles|[Yy]oung (leaves|needles)|[Ii]ncreasing leaf size|[Ii]nitial growth", "greenup_pct")
  out <- join_pheno(out, "^Fruits$", "fruiting_pct")

  out$source <- "NEON RELEASE-2026"
  # drop months with no data at all (keeps files lean)
  keep_cols <- c("precip_mm", "temp_c", "temp_min", "temp_max",
                 "flowering_pct", "greenup_pct", "fruiting_pct")
  has_any <- rowSums(!is.na(out[keep_cols])) > 0
  ans <- out[has_any, , drop = FALSE]
  attr(ans, "support") <- list(
    air_temperature = !is.null(att) && nrow(att) > 0,
    precipitation = !is.null(prt) && nrow(prt) > 0,
    plant_phenology = !is.null(pht_raw) && nrow(pht_raw) > 0,
    source_tables = list(
      air_temperature = source_table_names(at),
      precipitation = source_table_names(pr),
      plant_phenology = source_table_names(ph)
    ),
    stream_selection = list(air_temperature = at_selection, precipitation = pr_selection)
  )
  temperature_evidence_summary <- stream_evidence_summary(att_all, "tempSingleMean")
  precipitation_evidence_summary <- stream_evidence_summary(prt_all, "precipBulk")
  temperature_selected_columns <- c(
    "startDateTime", "tempSingleMean", "tempSingleMinimum",
    "tempSingleMaximum", "finalQF"
  )
  precipitation_selected_rows <- if (is.null(prt)) data.frame(
    startDateTime = character(), precipBulk = numeric(), finalQF = numeric(),
    stringsAsFactors = FALSE
  ) else canonical_order(as.data.frame(
    prt[c("startDateTime", "precipBulk", "finalQF")], stringsAsFactors = FALSE
  ))
  attr(ans, "validation_evidence") <- list(
    schema_version = 1L,
    release = RELEASE,
    site = site,
    window = list(start_month = start_d, end_month = end_d),
    privacy = list(
      personal_fields = "none",
      plant_individual_identity = paste(
        "domain-separated SHA-256 pseudonym; raw individualID is never written",
        "to validation evidence"
      )
    ),
    products = list(
      air_temperature = list(
        id = "DP1.00002.001", doi = "10.48443/p69b-5e50",
        source_tables = source_table_names(at),
        selected_table = attr(att_all, "source_table", exact = TRUE),
        source_columns = at_canonical$source_columns,
        selection_streams = temperature_evidence_summary$streams,
        selection_months = temperature_evidence_summary$months,
        selected_rows = canonical_order(as.data.frame(
          att[temperature_selected_columns], stringsAsFactors = FALSE
        ))
      ),
      precipitation = list(
        id = "DP1.00044.001", doi = "10.48443/v29j-eg88",
        source_tables = source_table_names(pr),
        selected_table = if (is.null(prt_all)) NULL else
          attr(prt_all, "source_table", exact = TRUE),
        source_columns = if (is.null(pr_canonical)) list() else pr_canonical$source_columns,
        selection_streams = precipitation_evidence_summary$streams,
        selection_months = precipitation_evidence_summary$months,
        selected_rows = precipitation_selected_rows
      ),
      plant_phenology = list(
        id = "DP1.10055.001", doi = "10.48443/p75s-7p48",
        source_tables = source_table_names(ph),
        selected_table = attr(pht_raw, "source_table", exact = TRUE),
        source_columns = ph_canonical$source_columns,
        source_rows = ph_canonical$source_rows,
        rows = canonical_order(as.data.frame(pht, stringsAsFactors = FALSE))
      )
    )
  )
  ans
}

# ---- run ------------------------------------------------------------------

cat(sprintf("Refreshing environmental overlays for %d sites (%s → %s) into %s/\n\n",
            length(sites), start_d, end_d, out_dir))

records <- vector("list", length(sites)); names(records) <- sites
for (s in sites) {
  f <- file.path(out_dir, paste0(s, ".rds"))
  evidence_file <- file.path(ENV_EVIDENCE_DIR, paste0(s, ".rds"))
  cat(sprintf("• %-5s building from %s…\n", s, RELEASE))
  env <- build_site_env(s)
  support <- attr(env, "support")
  evidence <- attr(env, "validation_evidence")
  if (is.null(env) || !nrow(env)) stop("No environmental context rows produced for ", s, call. = FALSE)
  if (is.null(evidence) || !identical(evidence$site, s))
    stop("Missing canonical environmental validation evidence for ", s, call. = FALSE)
  saveRDS(tibble::as_tibble(env), f, compress = "xz", version = 3)
  saveRDS(evidence, evidence_file, compress = "xz", version = 3)
  records[[s]] <- list(
    site = s,
    rows = nrow(env),
    month_min = min(env$ym),
    month_max = max(env$ym),
    air_temperature_supported = isTRUE(support$air_temperature),
    precipitation_supported = isTRUE(support$precipitation),
    plant_phenology_supported = isTRUE(support$plant_phenology),
    source_tables = support$source_tables,
    stream_selection = support$stream_selection,
    file = basename(f),
    sha256 = digest::digest(f, algo = "sha256", file = TRUE, serialize = FALSE),
    bytes = unname(file.info(f)$size),
    evidence_file = basename(evidence_file),
    evidence_sha256 = digest::digest(
      evidence_file, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    evidence_bytes = unname(file.info(evidence_file)$size)
  )
  cat(sprintf("    saved %s: %d months, %.1f KB public + %.1f MB validation evidence\n",
              s, nrow(env), file.size(f) / 1e3, file.size(evidence_file) / 1e6))
}

env_files <- list.files(out_dir, pattern = "^[A-Z]{4}[.]rds$")
evidence_files <- list.files(ENV_EVIDENCE_DIR, pattern = "^[A-Z]{4}[.]rds$")
expected_files <- paste0(sort(as.character(sites), method = "radix"), ".rds")
if (!identical(sort(env_files, method = "radix"), expected_files))
  stop("Environmental candidate does not contain the exact 47-site file roster.", call. = FALSE)
if (!identical(sort(evidence_files, method = "radix"), expected_files))
  stop("Environmental validation evidence does not contain exactly 47 site files.", call. = FALSE)
temp_n <- sum(vapply(records, function(x) isTRUE(x$air_temperature_supported), logical(1)))
precip_n <- sum(vapply(records, function(x) isTRUE(x$precipitation_supported), logical(1)))
pheno_n <- sum(vapply(records, function(x) isTRUE(x$plant_phenology_supported), logical(1)))
if (temp_n != 47L || precip_n != 20L || pheno_n != 47L)
  stop(sprintf("Unexpected RELEASE-2026 environmental support: temp %d/47, precip %d/47, phenology %d/47.",
               temp_n, precip_n, pheno_n), call. = FALSE)

receipt <- list(
  schema_version = 2L,
  release = RELEASE,
  window = list(start_month = start_d, end_month = end_d),
  retrieval = list(tool = "neonUtilities::loadByProduct", package = "basic",
                   neonUtilities_version = as.character(utils::packageVersion("neonUtilities")),
                   r_version = paste(R.version$major, R.version$minor, sep = "."), token_required = TRUE),
  products = list(
    air_temperature = list(id = "DP1.00002.001", doi = "10.48443/p69b-5e50", supported_sites = temp_n),
    precipitation = list(id = "DP1.00044.001", doi = "10.48443/v29j-eg88", supported_sites = precip_n),
    plant_phenology = list(id = "DP1.10055.001", doi = "10.48443/p75s-7p48", supported_sites = pheno_n)
  ),
  validation_evidence = list(
    schema_version = 1L,
    file_count = length(evidence_files),
    total_bytes = sum(vapply(records, function(record) {
      as.numeric(record$evidence_bytes)
    }, numeric(1))),
    format = paste(
      "canonical privacy-minimized selected-stream rows plus digest-bound",
      "all-stream month support; validation-only; not deployed"
    )
  ),
  files = unname(records)
)
jsonlite::write_json(receipt, ENV_RECEIPT, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(sprintf(paste0(
  "\nOK: 47 environmental bundles and 47 bound evidence files; ",
  "temp %d/47, precip %d/47, phenology %d/47; evidence %.1f MB; receipt %s\n"
), temp_n, precip_n, pheno_n,
sum(vapply(records, function(record) as.numeric(record$evidence_bytes), numeric(1))) / 1e6,
ENV_RECEIPT))
