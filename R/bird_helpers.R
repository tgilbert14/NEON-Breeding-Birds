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
# where ever detected (the LEAST detection-biased axis); abundance = a detection
# index = birds per point-count (sum clusterSize / site point-visits).
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

# ---------------------------------------------------------------------------
# Incidence-based richness estimate (Chao2) — species incidence across POINTS
# (the sampling unit). The right estimator for presence/point data. Chao 1987.
# ---------------------------------------------------------------------------
chao2_points <- function(obs, points) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  m <- max(1L, nrow(points))
  inc <- tapply(sp$pointkey, sp$scientificName, function(p) length(unique(p)))
  inc <- as.numeric(inc); S <- length(inc); Q1 <- sum(inc == 1); Q2 <- sum(inc == 2)
  if (S == 0 || m < 2) return(NULL)
  corr <- (m - 1) / m
  chao <- if (Q2 > 0) S + corr * Q1^2 / (2 * Q2) else S + corr * Q1 * (Q1 - 1) / 2
  list(S_obs = S, chao2 = round(chao, 1), m = m, Q1 = Q1, Q2 = Q2, unstable = Q2 < 3)
}

# sample-based species accumulation over points (mean over permutations)
bird_accum <- function(obs, points, perms = 40) {
  sp <- species_level_only(obs); if (is.null(sp) || !nrow(sp)) return(NULL)
  byp <- split(sp$scientificName, sp$pointkey); k <- length(byp); if (k < 2) return(NULL)
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
  if (length(v) < 3) return(NULL)
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
