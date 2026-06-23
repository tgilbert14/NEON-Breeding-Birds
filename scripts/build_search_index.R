# ===========================================================================
# build_search_index.R — precompute the "Search the network" index.
#
# Reads the COMMITTED bundles (data/sites/<SITE>.rds) — NOT a live fetch — and
# writes one small table to data/search_index.rds. The app loads it once at
# boot (like site_index) and filters it in memory, so the network search stays
# instant and the bundled-load story is unchanged.
#
# The index is a tidy taxon x site occurrence table: one row per
# (scientificName, site) where that breeding-bird species was DETECTED, with:
#   vernacular   — the common name (display label in the autocomplete)
#   site, name, state
#   index        — the per-site DETECTION INDEX (breeding birds per point-count),
#                  computed by the SAME species_board() the Overview uses, so
#                  flyovers are excluded (the app's honesty rule) and it matches
#                  the headline. NOT an absolute density.
#   detections   — raw # detections of that species at the site (context only)
#   year_min/max — the species' detected-year span at the site
#   n_sites      — # of sites the species occurs at (precomputed for the
#                  "detected at > N sites" threshold query)
#
# It also carries the per-site headline metrics (richness, birds/count) as a
# small companion table for the "site richness > X" threshold query — reusing
# site_index so the numbers are identical to the picker map.
#
# Run after the bundles refresh:
#   "/c/Program Files/R/R-4.5.2/bin/Rscript.exe" scripts/build_search_index.R
# ===========================================================================
suppressPackageStartupMessages({ library(dplyr) })
source("R/site_metadata.R")
source("R/bird_helpers.R")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

SITE_DIR <- "data/sites"
files <- list.files(SITE_DIR, pattern = "\\.rds$", full.names = TRUE)
if (!length(files)) stop("No bundles in ", SITE_DIR, " — run scripts/bundle_bird_data.R first.")
cat(sprintf("Indexing %d bundled sites for network search...\n", length(files)))

site_meta <- function(code) {
  m <- neon_sites[neon_sites$site == code, ]
  list(name  = if (nrow(m)) m$name[1]  else code,
       state = if (nrow(m)) m$state[1] else NA_character_)
}

rows <- lapply(files, function(f) {
  code <- sub("\\.rds$", "", basename(f))
  b <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(b) || is.null(b$obs) || !nrow(b$obs)) return(NULL)
  obs <- b$obs
  nvis <- b$meta$n_visits %||% (if (!is.null(b$points$n_visits)) sum(b$points$n_visits, na.rm = TRUE) else NA_integer_)

  # per-species per-site detection index, EXACTLY as the Overview computes it
  # (species_board: species-level only, flyovers excluded from index_birds).
  brd <- species_board(obs, b$points, nvis)
  if (is.null(brd) || !nrow(brd)) return(NULL)

  # year span per species at this site (species-level detections only)
  sp <- species_level_only(obs)
  yr <- sp %>%
    dplyr::filter(!is.na(.data$scientificName), nzchar(.data$scientificName)) %>%
    dplyr::group_by(.data$scientificName) %>%
    dplyr::summarise(year_min = suppressWarnings(min(.data$year, na.rm = TRUE)),
                     year_max = suppressWarnings(max(.data$year, na.rm = TRUE)), .groups = "drop")

  m <- site_meta(code)
  brd %>%
    dplyr::filter(!is.na(.data$scientificName), nzchar(.data$scientificName)) %>%
    dplyr::transmute(
      scientificName = .data$scientificName,
      vernacular     = .data$vernacular %||% .data$scientificName,
      site = code, name = m$name, state = m$state,
      index = round(.data$index, 3),               # birds per point-count, flyovers excluded
      detections = .data$detections) %>%
    dplyr::left_join(yr, by = "scientificName")
})

taxa <- dplyr::bind_rows(rows)
# clean vernacular: fall back to sci name when missing/blank
taxa$vernacular <- ifelse(is.na(taxa$vernacular) | !nzchar(trimws(taxa$vernacular)),
                          taxa$scientificName, taxa$vernacular)
# n_sites per species (drives the "detected at > N sites" query)
ns <- taxa %>% dplyr::count(.data$scientificName, name = "n_sites")
taxa <- taxa %>% dplyr::left_join(ns, by = "scientificName")
taxa <- taxa[order(taxa$scientificName, -taxa$index), , drop = FALSE]

# companion: per-site headline metrics for the richness threshold query.
# Reuse site_index so these match the picker map exactly.
si <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
sites <- if (!is.null(si)) {
  m <- neon_sites[match(si$site, neon_sites$site), ]
  dplyr::tibble(site = si$site, name = m$name, state = m$state,
                n_species = si$n_species, n_points = si$n_points,
                n_visits = si$n_visits, birds_per_count = si$birds_per_count,
                top_species = si$top_species)
} else {
  taxa %>% dplyr::group_by(.data$site, .data$name, .data$state) %>%
    dplyr::summarise(n_species = dplyr::n_distinct(.data$scientificName), .groups = "drop")
}
sites <- sites[order(-sites$n_species), , drop = FALSE]

out <- list(taxa = tibble::as_tibble(taxa), sites = tibble::as_tibble(sites))
saveRDS(out, "data/search_index.rds", compress = "xz")

sz <- file.size("data/search_index.rds")
cat(sprintf("Wrote data/search_index.rds: %d taxon x site rows, %d distinct species, %d sites | %s\n",
            nrow(taxa), length(unique(taxa$scientificName)), nrow(sites),
            format(structure(sz, class = "object_size"), units = "auto")))
# 10 most widespread species (sanity check)
top <- taxa %>% dplyr::distinct(.data$scientificName, .data$vernacular, .data$n_sites) %>%
  dplyr::arrange(-.data$n_sites) %>% utils::head(10)
print(as.data.frame(top))
