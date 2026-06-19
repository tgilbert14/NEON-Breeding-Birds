# ===========================================================================
# build_cross_site.R — precompute data/cross_site.rds: the EFFORT-STANDARDIZED
# community metrics the "Across the continent" gradient tab reads. Raw richness
# is an effort artifact (sites differ 13–144 points), so we rarefy every site's
# richness to a COMMON number of point-count occasions and also report coverage
# and Hill q1/q2. Runtime must never recompute this over 46 incidence matrices.
#
# One row per site: site, T_occ (point-count occasions), S_obs, S_rare (richness
# rarefied to t_used), t_used, coverage (0–1), hill_q1, hill_q2, mean_ubiquity,
# pct_singing. Run:  Rscript scripts/build_cross_site.R
# ===========================================================================
suppressMessages({ library(dplyr) })
source("R/bird_helpers.R")

files <- list.files("data/sites", pattern = "\\.rds$", full.names = TRUE)
if (!length(files)) stop("No site bundles in data/sites — run scripts/bundle_bird_data.R first.")

# pass 1: per-site incidence vector (Y, T) + community summaries
inc <- list(); base <- list()
for (f in files) {
  s <- sub("\\.rds$", "", basename(f))
  b <- tryCatch(readRDS(f), error = function(e) NULL); if (is.null(b) || is.null(b$obs)) next
  si <- site_incidence(b$obs); if (is.null(si) || si$T < 2) next
  inc[[s]] <- si
  brd <- species_board(b$obs, b$points, b$meta$n_visits %||% si$T)
  sp <- species_level_only(b$obs)
  pct_singing <- if (nrow(sp)) round(100 * mean(sp$detectionMethod == "singing", na.rm = TRUE), 1) else NA_real_
  base[[s]] <- data.frame(site = s, T_occ = si$T, S_obs = length(si$Y),
                          mean_ubiquity = round(mean(brd$ubiquity), 1), pct_singing = pct_singing)
}

# common rarefaction target: the smallest site's occasion count (every site can be
# interpolated down to it; none extrapolated up). Stated on the chart.
t_common <- min(vapply(inc, function(x) x$T, integer(1)))
cat(sprintf("Common rarefaction target t = %d point-count occasions (min over %d sites)\n",
            t_common, length(inc)))

rows <- lapply(names(inc), function(s) {
  Y <- inc[[s]]$Y; T <- inc[[s]]$T; h <- hill_incidence(Y)
  cbind(base[[s]],
        S_rare = rarefy_incidence(Y, T, t_common),
        t_used = t_common,
        coverage = round(coverage_incidence(Y, T), 3),
        hill_q1 = unname(h["q1"]), hill_q2 = unname(h["q2"]))
})
cs <- dplyr::bind_rows(rows)
attr(cs, "method") <- sprintf("Richness rarefied to %d point-count occasions (incidence rarefaction; Colwell et al. 2012). Hill q1/q2 on incidence frequencies. Coverage = sample completeness (Chao & Jost 2012).", t_common)
saveRDS(cs, "data/cross_site.rds", compress = "xz")

cat(sprintf("cross_site.rds: %d sites | S_obs %d–%d, S_rare(@%d) %.0f–%.0f, coverage %.3f–%.3f\n",
            nrow(cs), min(cs$S_obs), max(cs$S_obs), t_common,
            min(cs$S_rare, na.rm = TRUE), max(cs$S_rare, na.rm = TRUE),
            min(cs$coverage, na.rm = TRUE), max(cs$coverage, na.rm = TRUE)))
print(cs[order(-cs$S_rare), c("site","T_occ","S_obs","S_rare","coverage","hill_q1","mean_ubiquity","pct_singing")], row.names = FALSE)
