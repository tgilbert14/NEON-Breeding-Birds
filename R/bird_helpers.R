# ===========================================================================
# NEON Breeding Bird Explorer — bird_helpers.R
# Point-count / avian-survey analyses on DP1.10003.001. The unit is a SPECIES
# detected at a site (community grain WITH detection). The honesty backbone:
# raw point-count totals are detection-confounded (a loud flycatcher and a quiet
# sparrow at equal density give unequal counts), so the abundance axis is a
# DETECTION INDEX (birds per point-count), never "population". observerDistance
# powers the per-species detection-decay panel. See docs/neonize-playbook.md.
# ===========================================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
mode_chr <- function(x){ x<-x[!is.na(x)]; if(!length(x)) return(NA_character_); names(sort(table(x),decreasing=TRUE))[1] }
short_point <- function(p) sub("^[A-Z]{4}_", "", as.character(p))

species_level_only <- function(d){
  if (is.null(d) || !nrow(d)) return(d)
  if ("is_species" %in% names(d)) return(d[d$is_species %in% TRUE, , drop = FALSE])
  ok <- is.na(d$taxonRank) | d$taxonRank %in% c("species","subspecies","speciesGroup")
  d[ok, , drop = FALSE]
}
# stable species->color by primary detection method (no family in the basic package)
METHOD_COLS <- c(singing = "#1a7f37", calling = "#2f7fb5", visual = "#c1502e",
                 drumming = "#9c6644", "non-vocal" = "#9c6644", other = "#9aa6b2", unknown = "#9aa6b2")
method_col <- function(m) { m2 <- ifelse(m %in% names(METHOD_COLS), m, "other"); unname(METHOD_COLS[m2]) }

# ---------------------------------------------------------------------------
# species_board(): one row per species — the Bird Board. ubiquity = % of points
# where ever detected (a less count-biased axis than the index, though still a
# detection floor); abundance = a detection index = birds per point-count
# (sum clusterSize / site point-visits).
# ---------------------------------------------------------------------------
species_board <- function(obs, points, nvis) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  np <- max(1L, nrow(points)); nv <- max(1L, nvis)
  sp %>% dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      vernacular = mode_chr(.data$vernacularName),
      detections = dplyr::n(),
      total_birds = sum(.data$clusterSize, na.rm = TRUE),
      n_points = dplyr::n_distinct(.data$pointkey),
      n_grids  = dplyr::n_distinct(.data$plotID),
      mean_cluster = round(mean(.data$clusterSize, na.rm = TRUE), 2),
      method = mode_chr(.data$detectionMethod),
      .groups = "drop") %>%
    dplyr::mutate(ubiquity = round(100 * .data$n_points / np, 1),
                  index = round(.data$total_birds / nv, 3)) %>%   # detection index: birds / point-count
    dplyr::arrange(dplyr::desc(.data$index))
}

# site headline
site_birds <- function(obs, points, nvis) {
  brd <- species_board(obs, points, nvis); if (is.null(brd)) return(NULL)
  list(n_species = nrow(brd), birds_per_count = round(sum(brd$total_birds) / max(1L, nvis), 2),
       n_points = nrow(points), n_visits = nvis,
       top = brd$vernacular[which.max(brd$index)] %||% brd$scientificName[which.max(brd$index)])
}

# A "sampling occasion" = one point counted in one year (pointkey × year). NEON
# re-surveys the same points yearly, so this is the correct incidence replicate;
# pooling a point's revisits as if they were distinct PLACES inflates richness and
# the Chao2 estimator (the playbook's year-pooling rule). Used by chao2 + accum.
sampling_occasion <- function(sp) paste(sp$pointkey, sp$year)

# ---------------------------------------------------------------------------
# Incidence-based richness estimate (Chao2) — species incidence across SAMPLING
# OCCASIONS (point × year), the right replicate for presence/point data. The unit
# is a point-count, not a place. Chao 1987; Colwell et al. 2012.
# ---------------------------------------------------------------------------
chao2_points <- function(obs, points) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  occ <- sampling_occasion(sp); m <- length(unique(occ))   # # point-count occasions
  inc <- tapply(occ, sp$scientificName, function(o) length(unique(o)))
  inc <- as.numeric(inc); S <- length(inc); Q1 <- sum(inc == 1); Q2 <- sum(inc == 2)
  if (S == 0 || m < 2) return(NULL)
  corr <- (m - 1) / m
  chao <- if (Q2 > 0) S + corr * Q1^2 / (2 * Q2) else S + corr * Q1 * (Q1 - 1) / 2
  list(S_obs = S, chao2 = round(chao, 1), m = m, Q1 = Q1, Q2 = Q2, unstable = Q2 < 3)
}

# sample-based species accumulation over point-count OCCASIONS (point × year),
# mean over permutations. x-axis is point-counts, not unique places.
bird_accum <- function(obs, points, perms = 40) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  byp <- split(sp$scientificName, sampling_occasion(sp)); k <- length(byp); if (k < 2) return(NULL)
  seeds <- 1:perms
  mat <- vapply(seeds, function(s) {
    ord <- byp[order((seq_len(k) * 7919 + s * 104729) %% k)]   # deterministic shuffle, no RNG-state dep
    seen <- character(0); out <- integer(k)
    for (i in seq_len(k)) { seen <- union(seen, ord[[i]]); out[i] <- length(seen) }
    out
  }, numeric(k))
  data.frame(points = seq_len(k), richness = round(rowMeans(mat), 1))
}

# ---------------------------------------------------------------------------
# Cross-site effort standardization (dependency-light, no iNEXT). Raw richness is
# an effort artifact — sites differ 13–144 points — so the gradient compares
# richness rarefied to a common number of point-count occasions.
#   Y = per-species incidence counts (# occasions detected); T = total occasions.
# Incidence rarefaction (Colwell et al. 2012; Chao & Jost 2012); Hill q1/q2 on
# incidence frequencies are effort-robust common/dominant diversity.
# ---------------------------------------------------------------------------
site_incidence <- function(obs) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  occ <- sampling_occasion(sp)
  Y <- tapply(occ, sp$scientificName, function(o) length(unique(o)))
  list(Y = as.integer(Y), T = length(unique(occ)))
}
rarefy_incidence <- function(Y, T, t) {                # E[species] in t of T occasions
  if (is.na(t) || t < 1 || t > T) return(NA_real_)
  contrib <- ifelse(T - Y < t, 1, 1 - exp(lchoose(T - Y, t) - lchoose(T, t)))
  round(sum(contrib), 1)
}
coverage_incidence <- function(Y, T) {                 # sample completeness, 0–1
  U <- sum(Y); if (U == 0 || T < 2) return(NA_real_)
  Q1 <- sum(Y == 1); Q2 <- sum(Y == 2)
  1 - (Q1 / U) * ((T - 1) * Q1 / ((T - 1) * Q1 + 2 * max(Q2, 1)))
}
hill_incidence <- function(Y) {                        # q1 = exp(Shannon), q2 = invSimpson
  p <- Y / sum(Y); p <- p[p > 0]
  c(q1 = round(exp(-sum(p * log(p))), 1), q2 = round(1 / sum(p^2), 1))
}

# ---------------------------------------------------------------------------
# Per-species detail (the Species Profile card).
# ---------------------------------------------------------------------------
species_detail <- function(obs, sci) {
  d <- obs[obs$scientificName == sci & !is.na(obs$scientificName), , drop = FALSE]
  if (!nrow(d)) return(NULL); d
}
# detection-decay: detections by observerDistance, AREA-CORRECTED. A raw count
# histogram for a POINT count rises then falls — far annuli cover more ground
# (area ∝ 2π·r·Δr), so raw counts are detection g(d) × annulus area, not g(d).
# Dividing each band's count by its annulus area recovers apparent density per
# ring, which IS the monotone-declining detectability signature. The unbounded
# far tail is truncated at 200 m (Buckland et al. 2001 distance-sampling practice).
distance_decay <- function(obs, sci) {
  d <- species_detail(obs, sci); if (is.null(d)) return(NULL)
  v <- d$observerDistance[is.finite(d$observerDistance) & d$observerDistance >= 0 & d$observerDistance <= 200]
  if (length(v) < 8) return(NULL)   # n-gate: 3 detections across 6 bands is noise (Colwell/Buckland)
  brks <- c(0, 25, 50, 75, 100, 150, 200); labs <- c("0–25","25–50","50–75","75–100","100–150","150–200")
  cl <- cut(v, breaks = brks, labels = labs, right = FALSE)
  tab <- as.data.frame(table(band = cl), responseName = "n")
  area_ha <- (pi * (brks[-1]^2 - brks[-length(brks)]^2)) / 10000   # annulus area per ring, hectares
  out <- dplyr::left_join(data.frame(band = factor(labs, levels = labs), area_ha = area_ha), tab, by = "band")
  out$n <- ifelse(is.na(out$n), 0L, out$n)
  out$density <- round(out$n / out$area_ha, 2)   # detections per hectare per ring = detectability signature
  out
}
# detections by bout/year (the seasonal/effort texture)
detection_by_year <- function(obs, sci) {
  d <- species_detail(obs, sci); if (is.null(d)) return(NULL)
  d %>% dplyr::group_by(.data$year) %>%
    dplyr::summarise(birds = sum(.data$clusterSize, na.rm = TRUE), .groups = "drop")
}
# primary detection method mix for a species
method_mix <- function(obs, sci) {
  d <- species_detail(obs, sci); if (is.null(d)) return(NULL)
  d %>% dplyr::count(.data$detectionMethod, name = "n") %>% dplyr::arrange(dplyr::desc(.data$n))
}

# every species detected at one grid (plotID) — powers the map click panel + CSV
grid_species <- function(obs, plotid) {
  sp <- species_level_only(obs); sp <- sp[!is.na(sp$plotID) & sp$plotID == plotid, , drop = FALSE]
  if (!nrow(sp)) return(NULL)
  sp %>% dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(vernacular  = mode_chr(.data$vernacularName),
                     detections  = dplyr::n(),
                     birds       = sum(.data$clusterSize, na.rm = TRUE),
                     method      = mode_chr(.data$detectionMethod),
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$birds), dplyr::desc(.data$detections))
}

# ---------------------------------------------------------------------------
# bird_qc(): the data-quality-flag system for ONE species (the Species Profile).
# The family's gold-standard feature, ported from the Small Mammal Tracker's
# individual_qc_flags()/flagged_measure_captures(). Returns ranked "verify, not
# wrong" flags PLUS the exact offending detections behind each, so the UI can
# list them (clickable) and download a QC report. Thresholds grounded in BBS /
# IMBCR / distance-sampling practice (Fauna review; see docs/neonize-playbook.md):
#   high = almost certainly an error (vernacular drift; distance > 1 km)
#   warn = worth a look (exact-0 / visual-far distance; solitary-species mega-cluster; under-effort points)
#   info = a note (missing-distance share; flocking-species log-scale outliers)
# Flagged-far / large-cluster records are RETAINED for modelling (truncated only
# at analysis, per Buckland) — a flag means "review", never "delete".
# Returns list(flags = <list(level,title,key,n,detail)>, sets = <named list of data.frames>).
# ---------------------------------------------------------------------------
bird_qc <- function(obs, sci, points = NULL) {
  out <- list(flags = list(), sets = list())
  d <- species_detail(obs, sci); if (is.null(d) || !nrow(d)) return(out)
  cols <- intersect(c("vernacularName","pointkey","plotID","year","bout","observerDistance","detectionMethod","clusterSize"), names(d))
  tidy <- function(rows, label) { x <- d[rows, cols, drop = FALSE]; if (!nrow(x)) return(NULL); x$flag <- label; x }
  add <- function(level, title, key, rows, detail) {
    rows <- rows[!is.na(rows)]; n <- length(rows); if (!n) return(invisible())
    out$flags[[length(out$flags) + 1L]] <<- list(level = level, title = title, key = key, n = n, detail = detail)
    out$sets[[key]] <<- tidy(rows, title)
  }
  od   <- if ("observerDistance" %in% names(d)) suppressWarnings(as.numeric(d$observerDistance)) else rep(NA_real_, nrow(d))
  cs   <- if ("clusterSize"      %in% names(d)) suppressWarnings(as.numeric(d$clusterSize))      else rep(NA_real_, nrow(d))
  meth <- if ("detectionMethod"  %in% names(d)) as.character(d$detectionMethod)                  else rep(NA_character_, nrow(d))

  # 1 — vernacularName drift for one scientificName (deterministic, ~0 false positives)
  if ("vernacularName" %in% names(d)) {
    vn <- unique(trimws(d$vernacularName[!is.na(d$vernacularName) & nzchar(trimws(d$vernacularName))]))
    if (length(vn) > 1)
      add("high", "Two common names for one scientific name", "vernacular", seq_len(nrow(d)),
          sprintf("Recorded under %d different common names (%s). One accepted scientific name should map to one common name — usually a join or mid-dataset taxonomy revision to reconcile.", length(vn), paste(vn, collapse = " / ")))
  }
  # 2 — implausibly far (> 1 km): a units / transcription error, not a real far bird
  add("high", "Detection beyond 1 km", "far", which(is.finite(od) & od > 1000),
      "Recorded farther than 1 km from the observer — implausible for a 6-minute landbird count and almost always a units or transcription error. (Legitimate far birds are truncated at analysis, not flagged.)")
  # 3 — exact-0 distance (heaping at the origin / placeholder entry)
  add("warn", "Distance recorded as exactly 0 m", "zero", which(is.finite(od) & od == 0),
      "A distance of exactly 0 m puts the bird on the observer — usually a default/placeholder. Distance sampling assumes near-perfect detection AT the point, so a pile-up of zeros distorts the curve's origin; verify these are real at-point detections.")
  # 4 — visual ID at long range. 500 m (not 250) so it doesn't cry wolf on open
  # grassland, where conspicuous birds ARE legitimately seen far — past ~500 m an
  # unaided visual species ID of a landbird is not credible regardless of habitat.
  add("warn", "Visual ID at long range (> 500 m)", "visualfar", which(meth %in% "visual" & is.finite(od) & od > 500),
      "An unaided visual identification past ~500 m is not credible for a landbird even on open ground. Worth confirming the species and the distance.")
  # 5 — clusterSize: solitary-species mega-cluster (warn) OR flocking-species log-outlier (info).
  # "Effectively solitary" = 99% of detections are 1–3 birds (p99 <= 3): this keeps a genuine
  # flocking species (which is USUALLY counted as singletons on a breeding point count but has a
  # real tail of flocks, e.g. Red-winged Blackbird) OUT of the solitary branch — its p99 is high.
  csf <- cs[is.finite(cs)]
  if (length(csf) >= 10) {
    p99 <- stats::quantile(csf, 0.99, names = FALSE)
    if (is.finite(p99) && p99 <= 3) {
      add("warn", "Large flock for a typically solitary species", "cluster", which(is.finite(cs) & cs >= 6),
          "99% of this species' detections are 1–3 birds, yet these report 6+ in one cluster — a likely transcription error or misidentification for a territorial species.")
    } else {
      l <- log1p(csf); mads <- stats::mad(l); thr <- stats::median(l) + 5 * mads
      if (is.finite(thr) && mads > 0)
        add("info", "Unusually large flock (vs this species)", "cluster", which(is.finite(cs) & log1p(cs) > thr),
            "Flock size far above this species' own typical range (judged on the log scale, so ordinary flocks don't flag). Often genuine for gregarious species — noted for review, not presumed wrong.")
    }
  }
  # 6 — missing distance: only surface when it's a MEANINGFUL share (>=10%). A handful of
  # NA-distance flyovers is routine and not worth a flag (that just cries wolf on every species).
  miss <- which(is.na(od)); pct <- if (nrow(d)) round(100 * length(miss) / nrow(d)) else 0
  if (length(miss) && pct >= 25)
    add("info", sprintf("Missing distance on %d%% of detections", pct), "missing", miss,
        "A large share of detections have no observerDistance, so they can't enter a distance/detectability model — weakening any density estimate (often documented flyovers).")
  # 7 — under-effort points (within-site robust MAD): low effort biases detection
  if (!is.null(points) && all(c("n_visits","pointkey") %in% names(points)) && "pointkey" %in% names(d)) {
    nv <- suppressWarnings(as.numeric(points$n_visits)); good <- is.finite(nv)
    if (sum(good) >= 5) {
      smed <- stats::median(nv[good]); smad <- stats::mad(nv[good])
      if (is.finite(smad) && smad > 0) {
        low_pts <- points$pointkey[good & nv < smed - 3 * smad]
        add("warn", "Detected at under-sampled point(s)", "loweffort", which(d$pointkey %in% low_pts),
            sprintf("Some detections fall on points visited far less than the site norm (< %.0f visits vs a site median of %.0f). Under-effort points under-detect species, biasing richness and any across-point comparison.", smed - 3 * smad, smed))
      }
    }
  }
  out
}

# every flagged detection for a species, across all flag types (the QC report CSV)
bird_qc_report <- function(obs, sci, points = NULL) {
  q <- bird_qc(obs, sci, points); if (!length(q$sets)) return(NULL)
  do.call(rbind, c(q$sets, list(make.row.names = FALSE)))
}

# per-POINT (grid) summary for the map: richness + birds/visit per point
point_summary <- function(obs, points) {
  sp <- species_level_only(obs)
  per <- sp %>% dplyr::group_by(.data$pointkey) %>%
    dplyr::summarise(richness = dplyr::n_distinct(.data$scientificName),
                     birds = sum(.data$clusterSize, na.rm = TRUE), .groups = "drop")
  out <- dplyr::left_join(points, per, by = "pointkey")
  out$richness <- ifelse(is.na(out$richness), 0L, out$richness)
  out$per_visit <- ifelse(out$n_visits > 0, round(out$birds / out$n_visits, 1), NA_real_)
  out
}
