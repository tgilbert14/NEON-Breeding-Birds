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
