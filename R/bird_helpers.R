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
# stable species->color by primary detection method (no family in the basic package).
# LUMINANCE-SEPARATED so the three core methods stay distinct under colour-vision
# deficiency: singing L=.19 (green) / visual L=.22 (rust) / calling L=.25 (blue),
# min adjacent gap .030. The old set put singing/visual within .016 L (a CVD wash).
# Hues stay on the Field Guide theme; the three are also distinguished by hue family.
METHOD_COLS <- c(singing = "#1a8a5a", calling = "#3a8fd6", visual = "#D55E00",
                 drumming = "#7a4a2a", "non-vocal" = "#7a4a2a", other = "#9aa6b2", unknown = "#9aa6b2")
# Map compound NEON methods to their dominant breeding-signal component before colouring,
# so "calling and singing"/"visual and singing" read as the territorial-singing signal they
# carry instead of dumping into grey "other". A singing component IS a breeding signal.
canon_method <- function(m) {
  m <- tolower(trimws(as.character(m)))
  ifelse(is.na(m) | m == "", "unknown",
  ifelse(grepl("singing", m), "singing",
  ifelse(grepl("drumming", m), "drumming",
  ifelse(grepl("calling", m), "calling",
  ifelse(grepl("visual",  m), "visual",
  ifelse(grepl("flyover", m), "flyover", m))))))
}
method_col <- function(m) { m2 <- canon_method(m); m2 <- ifelse(m2 %in% names(METHOD_COLS), m2, "other"); unname(METHOD_COLS[m2]) }
# Flyovers are birds passing overhead, NOT holding a breeding territory at the point
# (IMBCR/BBS convention excludes them from a breeding-density index). Quarantine them
# from the detection index so 300–500-bird flyover flocks don't distort it. Kept in the
# raw data and the QC/profile surfaces — only excluded from the index denominator-math.
is_flyover <- function(x) grepl("flyover", tolower(as.character(x)), fixed = FALSE)

# ---------------------------------------------------------------------------
# species_board(): one row per species — the Bird Board. ubiquity = % of points
# where ever detected (a less count-biased axis than the index, though still a
# detection floor); abundance = a detection index = birds per point-count
# (sum clusterSize / site point-visits).
# ---------------------------------------------------------------------------
species_board <- function(obs, points, nvis) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  np <- max(1L, nrow(points)); nv <- max(1L, nvis)
  # flyovers are summed for honest total_birds/detections, but EXCLUDED from the
  # breeding detection index (index_birds) — passing flocks aren't territory holders.
  sp$.fly <- is_flyover(sp$detectionMethod)
  sp %>% dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(
      vernacular = mode_chr(.data$vernacularName),
      detections = dplyr::n(),
      total_birds = sum(.data$clusterSize, na.rm = TRUE),
      index_birds = sum(.data$clusterSize[!.data$.fly], na.rm = TRUE),   # flyovers removed
      flyover_birds = sum(.data$clusterSize[.data$.fly], na.rm = TRUE),
      n_points = dplyr::n_distinct(.data$pointkey),
      n_grids  = dplyr::n_distinct(.data$plotID),
      mean_cluster = round(mean(.data$clusterSize, na.rm = TRUE), 2),
      method = mode_chr(.data$detectionMethod),
      .groups = "drop") %>%
    dplyr::mutate(ubiquity = round(100 * .data$n_points / np, 1),
                  index = round(.data$index_birds / nv, 3)) %>%   # detection index: breeding birds / point-count (flyovers excluded)
    dplyr::arrange(dplyr::desc(.data$index))
}

# site headline. birds_per_count is the breeding detection index — flyovers excluded
# (passing flocks aren't territory holders). flyover_birds carries the quarantined
# count so the UI can disclose it behind a click without altering the headline.
site_birds <- function(obs, points, nvis) {
  brd <- species_board(obs, points, nvis); if (is.null(brd)) return(NULL)
  list(n_species = nrow(brd), birds_per_count = round(sum(brd$index_birds) / max(1L, nvis), 2),
       flyover_birds = sum(brd$flyover_birds), n_points = nrow(points), n_visits = nvis,
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
  f0 <- max(0, chao - S)   # estimated undetected species
  # analytic log-normal 95% CI for the Chao2 estimate (Chao 1987 variance of the
  # added f0 term, Colwell et al. 2012 eq. for the asymmetric log-normal interval).
  var_f0 <- if (Q2 > 0)
    corr * (Q1 * (Q1 - 1)) / (2 * (Q2 + 1)) +
      corr^2 * (Q1 * (2 * Q1 - 1)^2) / (4 * (Q2 + 1)^2) +
      corr^2 * (Q1^2 * Q2 * (Q1 - 1)^2) / (4 * (Q2 + 1)^4)
  else
    corr * (Q1 * (Q1 - 1)) / 2 + corr^2 * (Q1 * (2 * Q1 - 1)^2) / 4 -
      corr^2 * (Q1^4) / (4 * chao)
  var_f0 <- max(var_f0, 0)
  ci_lo <- ci_hi <- NA_real_
  if (f0 > 0 && var_f0 > 0) {
    K <- exp(1.96 * sqrt(log(1 + var_f0 / f0^2)))
    ci_lo <- S + f0 / K; ci_hi <- S + f0 * K
  } else if (f0 == 0) { ci_lo <- ci_hi <- S }
  list(S_obs = S, chao2 = round(chao, 1), m = m, Q1 = Q1, Q2 = Q2, unstable = Q2 < 3,
       ci_lo = round(ci_lo, 1), ci_hi = round(ci_hi, 1))
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
# sample-coverage completeness for ONE site's detections (Chao & Jost 2012). The
# honest completeness story to lead with when the Chao2 point estimate is unstable.
site_coverage <- function(obs) {
  si <- site_incidence(obs); if (is.null(si)) return(NA_real_)
  coverage_incidence(si$Y, si$T)
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
  # tidy carries flag (title) + flag_key + flag_level so the QC report CSV is
  # self-describing and re-derivable per the codebook (no orphan codebook rows).
  tidy <- function(rows, label, key, level) {
    x <- d[rows, cols, drop = FALSE]; if (!nrow(x)) return(NULL)
    x$flag <- label; x$flag_key <- key; x$flag_level <- level; x }
  add <- function(level, title, key, rows, detail) {
    rows <- rows[!is.na(rows)]; n <- length(rows); if (!n) return(invisible())
    out$flags[[length(out$flags) + 1L]] <<- list(level = level, title = title, key = key, n = n, detail = detail)
    out$sets[[key]] <<- tidy(rows, title, key, level)
  }
  od   <- if ("observerDistance" %in% names(d)) suppressWarnings(as.numeric(d$observerDistance)) else rep(NA_real_, nrow(d))
  cs   <- if ("clusterSize"      %in% names(d)) suppressWarnings(as.numeric(d$clusterSize))      else rep(NA_real_, nrow(d))
  meth <- if ("detectionMethod"  %in% names(d)) as.character(d$detectionMethod)                  else rep(NA_character_, nrow(d))

  # 1 — vernacularName drift for one scientificName (deterministic, ~0 false positives)
  if ("vernacularName" %in% names(d)) {
    vn <- unique(trimws(d$vernacularName[!is.na(d$vernacularName) & nzchar(trimws(d$vernacularName))]))
    if (length(vn) > 1)
      add("high", "Two common names for one scientific name", "vernacular", seq_len(nrow(d)),
          sprintf("Recorded under %d different common names (%s). One accepted scientific name should map to one common name, usually a join or mid-dataset taxonomy revision to reconcile.", length(vn), paste(vn, collapse = " / ")))
  }
  # 2 — implausibly far (> 1 km): a units / transcription error, not a real far bird
  add("high", "Detection beyond 1 km", "far", which(is.finite(od) & od > 1000),
      "Recorded farther than 1 km from the observer, implausible for a 6-minute landbird count and almost always a units or transcription error. (Legitimate far birds are truncated at analysis, not flagged.)")
  # 3 — exact-0 distance (heaping at the origin / placeholder entry)
  add("warn", "Distance recorded as exactly 0 m", "zero", which(is.finite(od) & od == 0),
      "A distance of exactly 0 m puts the bird on the observer, usually a default/placeholder. Distance sampling assumes near-perfect detection AT the point, so a pile-up of zeros distorts the curve's origin; verify these are real at-point detections.")
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
          "99% of this species' detections are 1–3 birds, yet these report 6+ in one cluster, a likely transcription error or misidentification for a territorial species.")
    } else {
      l <- log1p(csf); mads <- stats::mad(l); thr <- stats::median(l) + 5 * mads
      if (is.finite(thr) && mads > 0)
        add("info", "Unusually large flock (vs this species)", "cluster", which(is.finite(cs) & log1p(cs) > thr),
            "Flock size far above this species' own typical range (judged on the log scale, so ordinary flocks don't flag). Often genuine for gregarious species, noted for review, not presumed wrong.")
    }
  }
  # 6 — missing distance: only surface when it's a MEANINGFUL share (>=10%). A handful of
  # NA-distance flyovers is routine and not worth a flag (that just cries wolf on every species).
  miss <- which(is.na(od)); pct <- if (nrow(d)) round(100 * length(miss) / nrow(d)) else 0
  if (length(miss) && pct >= 25)
    add("info", sprintf("Missing distance on %d%% of detections", pct), "missing", miss,
        "A large share of detections have no observerDistance, so they can't enter a distance/detectability model, weakening any density estimate (often documented flyovers).")
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

# ---------------------------------------------------------------------------
# EXPORT KEEP-VECTORS — the single source of truth for what each CSV download
# emits AND for the codebook. The codebook (below) is GENERATED by iterating the
# union of these vectors against BIRD_COL_DICT, so a column can never be exported
# without a documented codebook entry, and the codebook can never list a column no
# export emits (the codebook-from-keep-vector standard). Edit a keep-vector here
# and the codebook follows automatically; an undocumented column stop()s the boot.
# ---------------------------------------------------------------------------
SPCSV_KEEP <- c("scientificName","vernacularName","pointkey","plotID","year","bout",
                "observerDistance","detectionMethod","clusterSize","is_flyover","enters_index")
BOARD_KEEP <- c("scientificName","vernacular","method","index","ubiquity","detections",
                "total_birds","index_birds","flyover_birds","n_points","n_grids","mean_cluster")
GRADIENT_KEEP <- c("site","name","state","biome_lab","breeding_temp_c","precip_annual_mm",
                   "n_species","n_points","n_visits","birds_per_count","S_obs","S_rare","t_used",
                   "coverage","hill_q1","mean_ubiquity","pct_singing","top_species")
GRIDCSV_KEEP <- c("scientificName","vernacularName","total_birds","detections","primary_method")
# QC report / inspector exports carry these flag columns (added by bird_qc tidy()):
QC_KEEP    <- c("vernacularName","pointkey","plotID","year","bout","observerDistance",
                "detectionMethod","clusterSize","flag","flag_key","flag_level")

# Master column dictionary: every column ANY export can emit -> units + NA-semantics.
# One row per column name (keyed); the codebook is a lookup over the keep-vectors.
BIRD_COL_DICT <- list(
  scientificName  = c("", "Accepted scientific (Latin) name of the species detected."),
  vernacularName  = c("", "Common (English) name; one accepted scientificName should map to one vernacular. NA = no common name recorded."),
  vernacular      = c("", "Common (English) name (Bird Board column name for vernacularName). NA = none recorded."),
  pointkey        = c("", "Point identifier = plotID_pointID; the fixed spot an observer stands for a 6-minute count."),
  plotID          = c("", "NEON plot (grid) identifier."),
  pointID         = c("", "Point identifier within a plot."),
  year            = c("year", "Calendar year of the point-count."),
  bout            = c("bout #", "Bout (visit) number within the breeding season; a point may be counted 1–2x per year."),
  observerDistance= c("metres", "Observer-ESTIMATED distance from observer to bird, in metres. NA = distance NOT ESTIMABLE (flocks / flyovers / detected-but-far; the former 999/9999 sentinel, recoded to NA at build). NA is not 0; it means no usable distance."),
  detectionMethod = c("category", "How the bird was first detected (singing / calling / visual / drumming / flyover / compound, e.g. 'visual and singing'). NA = not recorded."),
  clusterSize     = c("# birds", "Number of birds in the detection (a flock counts as one detection row with clusterSize > 1)."),
  is_flyover      = c("logical", "TRUE if this detection is a flyover (a bird passing overhead, not holding a breeding territory at the point). Flyovers are EXCLUDED from the breeding detection index per IMBCR/BBS convention."),
  enters_index    = c("logical", "TRUE if this detection enters the breeding detection index (= NOT a flyover). Re-derive the index as sum(clusterSize[enters_index]) / point-counts. Exported so a downstream sum() cannot silently recreate the flyover-inflated total."),
  primary_method  = c("category", "Modal (most-frequent) first-detection method across a species' detections at the grid (singing / calling / visual / drumming / flyover / compound). NA = none recorded."),
  method          = c("category", "Modal first-detection method across the species' detections (Bird Board / grid column). NA = none recorded."),
  index           = c("birds / point-count", "Detection index = sum(clusterSize, FLYOVERS EXCLUDED) / point-counts run. A relative detection index, NOT a population. Detectability differs by species."),
  ubiquity        = c("% of points", "Naive occupancy floor = % of points where the species was EVER detected. Less count-biased than the index, but still effort/detection-dependent (under-counts quiet/secretive birds)."),
  detections      = c("# detection rows", "Number of detection rows for the species (a count of records, not birds)."),
  total_birds     = c("# birds", "Total birds summed across all detections (INCLUDES flyovers; the index excludes them)."),
  index_birds     = c("# birds", "Birds entering the index = sum(clusterSize) with flyovers EXCLUDED."),
  flyover_birds   = c("# birds", "Birds in flyover detections (quarantined from the index). total_birds = index_birds + flyover_birds."),
  mean_cluster    = c("# birds", "Mean clusterSize across the species' detections."),
  n_points        = c("# points", "Number of distinct points where the species was detected (per-species) / count points at the site (per-site)."),
  n_grids         = c("# grids", "Number of distinct grids (plots) where the species was detected."),
  n_visits        = c("# point-counts", "Number of point-count occasions (point x year x bout) run at the site = the effort denominator."),
  flag            = c("category", "Which data-quality review flag this row tripped (the flag's title). 'verify, not wrong'; flagged rows are RETAINED, never deleted."),
  flag_key        = c("", "Machine key of the QC flag (vernacular / far / zero / visualfar / cluster / missing / loweffort)."),
  flag_level      = c("category", "Severity of the QC flag: high (almost certainly an error) / warn (worth a look) / info (a note)."),
  n_species       = c("# species", "Observed species richness at the site = number of species-level taxa detected (= S_obs)."),
  S_obs           = c("# species", "Observed species richness at the site (species detected)."),
  chao2           = c("# species", "Chao2 incidence-based richness estimate (a bias-corrected MINIMUM; unstable when <3 species are detected at exactly two occasions). Chao 1987."),
  coverage        = c("0-1", "Sample-coverage completeness, 0-1 (fraction of the community detected). Chao & Jost 2012."),
  S_rare          = c("# species", "Species richness RAREFIED to a common number of point-count occasions across sites (incidence rarefaction; Colwell et al. 2012), comparable across sites of unequal effort. NA = below the common rarefaction target."),
  t_used          = c("# point-counts", "The common number of point-count occasions S_rare is rarefied to (shared across all sites)."),
  birds_per_count = c("birds / point-count", "Site detection index = total breeding birds (flyovers excluded) / point-counts run."),
  pct_singing     = c("% of detections", "Singing share = % of detections where the bird was singing (a habitat/detectability signature)."),
  hill_q1         = c("# species", "Hill q1 = exp(Shannon) on incidence frequencies = effective number of COMMON species (effort-robust)."),
  mean_ubiquity   = c("% of points", "Community mean ubiquity = average across species of the % of points where each is detected."),
  breeding_temp_c = c("degrees C", "Mean breeding-season air temperature from the co-located NEON air-temperature sensor (storage unit degrees C; the app converts for display)."),
  precip_annual_mm= c("mm / year", "Mean annual precipitation from the NEON gauge. NA = no precipitation gauge at this site (19/46 sites have one; never imputed)."),
  site            = c("", "NEON 4-letter site code."),
  name            = c("", "NEON site name."),
  state           = c("", "US state / territory of the site."),
  biome_lab       = c("", "Biome class used for the gradient colour/legend (Forest / Grassland / Desert / Tundra / Tropical dry forest)."),
  top_species     = c("", "Most-detected species at the site (by the detection index).")
)

# ---------------------------------------------------------------------------
# bird_codebook(): machine-readable FAIR data dictionary, GENERATED from the export
# keep-vectors so it cannot drift from the columns actually emitted. Iterates the
# union of every keep-vector (plus the QC flag_key/flag_level meta-columns) against
# BIRD_COL_DICT; an emitted column missing a dictionary entry stop()s (caught at
# boot / build), so a new export column can never ship undocumented.
# ---------------------------------------------------------------------------
bird_codebook <- function() {
  cols <- unique(c(SPCSV_KEEP, BOARD_KEEP, GRADIENT_KEEP, GRIDCSV_KEEP, QC_KEEP,
                   "flag_key", "flag_level",
                   "S_obs","chao2","coverage"))   # site-summary columns surfaced elsewhere
  missing <- setdiff(cols, names(BIRD_COL_DICT))
  if (length(missing))
    stop(sprintf("bird_codebook(): %d exported column(s) have no BIRD_COL_DICT entry: %s",
                 length(missing), paste(missing, collapse = ", ")), call. = FALSE)
  data.frame(
    column      = cols,
    units       = vapply(cols, function(c) BIRD_COL_DICT[[c]][1], character(1)),
    description = vapply(cols, function(c) BIRD_COL_DICT[[c]][2], character(1)),
    row.names   = NULL, stringsAsFactors = FALSE)
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
