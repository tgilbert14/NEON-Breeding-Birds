#!/usr/bin/env Rscript
# Required Shiny lifecycle gate against the committed validated release corpus.

Sys.setenv(BRD_LIVE = "0")
suppressPackageStartupMessages(library(shiny))
source("global.R")
source("ui.R")
app_server <- source("server.R")$value

stopifnot(is.function(app_server), !is.null(SITE_INDEX), nrow(SITE_INDEX) == 47L,
          identical(sort(as.character(SITE_INDEX$site)), sort(as.character(neon_sites$site))))

candidate <- NULL
for (site in as.character(SITE_INDEX$site)) {
  bundle <- load_site_bundle(site)
  if (is.null(bundle)) next
  board <- species_board(bundle$obs, bundle$points, bundle$meta$n_visits,
                         bundle$opportunity, bundle$visits,
                         bundle$meta$observer_support)
  if (!is.null(board) && nrow(board)) {
    candidate <- bundle
    break
  }
}
stopifnot(!is.null(candidate))

shiny::testServer(app_server, {
  ok <- ingest(candidate, paste(candidate$meta$site, "runtime fixture"))
  session$flushReact()
  stopifnot(isTRUE(ok), identical(rv$site, candidate$meta$site),
            rv$nvis == sum(candidate$visits$valid_count),
            rv$nopp == sum(candidate$opportunity$supported), nrow(rv$board) > 0L)

  # Force both a plot and its surrounding UI through their real render paths.
  distance_support <- aggregate(
    as.integer(candidate$obs$distance_state == "observed" &
      is.finite(candidate$obs$observerDistance) & candidate$obs$observerDistance >= 0 &
      candidate$obs$observerDistance <= 200 & candidate$obs$enters_breeding_metrics),
    list(scientificName = candidate$obs$communityScientificName), sum, na.rm = TRUE)
  names(distance_support)[2] <- "n"
  usable <- distance_support$scientificName[distance_support$n >= 8L]
  if (length(usable)) {
    rv$sp <- usable[[1]]
    session$flushReact()
    stopifnot(!is.null(output$decayPlot), !is.null(output$speciesProfile))
  }
  stopifnot(!is.null(output$topBar), !is.null(output$heroStats))

  # A fully sampled all-zero record is valid evidence. It must replace the prior
  # site's state atomically, stay navigable, and never print an NA Chao2 estimate.
  zero <- candidate
  zero$obs <- candidate$obs[0, , drop = FALSE]
  zero$opportunity$eligible_detection_rows <- 0L
  zero$opportunity$eligible_birds <- 0
  zero$opportunity$eligible_species <- 0L
  zero$opportunity$flyover_rows <- 0L
  zero$opportunity$flyover_birds <- 0
  zero$opportunity$outcome <- ifelse(zero$opportunity$supported,
                                     "supported_zero", "unavailable")
  zero$visits$eligible_detection_rows <- 0L
  zero$visits$eligible_birds <- 0
  zero$visits$eligible_species <- 0L
  zero$visits$flyover_rows <- 0L
  zero$visits$flyover_birds <- 0
  zero$visits$support_state <- ifelse(zero$visits$valid_count, "supported", "held")
  zero$visits$outcome <- ifelse(zero$visits$valid_count, "supported_zero", "unavailable")
  zero$meta$n_supported_zero_counts <- sum(zero$visits$valid_count)
  zero$meta$n_supported_zero <- sum(zero$opportunity$supported)
  zero$meta$observer_support$by_species <- data.frame(
    scientificName = character(), n_observers = integer(),
    stringsAsFactors = FALSE
  )
  ok_zero <- ingest(zero, paste(zero$meta$site, "all-zero runtime fixture"))
  session$flushReact()
  stopifnot(isTRUE(ok_zero), identical(rv$site, zero$meta$site), nrow(rv$board) == 0L,
            rv$nvis == sum(zero$visits$valid_count), rv$nopp == sum(zero$opportunity$supported))
  overview <- paste(as.character(output$overviewInsight), collapse = " ")
  chao <- paste(as.character(output$chaoBanner), collapse = " ")
  species_empty <- paste(as.character(output$speciesProfile), collapse = " ")
  zero_report <- bird_report_export(
    rv$board, rv$site, NEON_RELEASE,
    site_birds(rv$obs, rv$points, rv$nvis, rv$opportunity,
               rv$visits, rv$observer_support),
    rv$visits, rv$opportunity)
  stopifnot(grepl("zero eligible", overview, ignore.case = TRUE),
            grepl("not estimable", chao, ignore.case = TRUE),
            !grepl("estimates NA|NA species", chao, ignore.case = TRUE),
            grepl("no eligible species profile", species_empty, ignore.case = TRUE),
            !is.null(output$topBar), !is.null(output$heroStats),
            !is.null(output$siteInsights), !is.null(output$accumPlot),
            !is.null(output$birdBoard), !is.null(output$map), !is.null(output$gridPanel))
  stopifnot(nrow(zero_report) == 1L,
            identical(zero_report$row_type, "site_summary"),
            is.na(zero_report$scientificName[[1]]),
            zero_report$site_n_species[[1]] == 0L,
            zero_report$site_n_valid_counts[[1]] == sum(zero$visits$valid_count),
            zero_report$site_n_supported_zero_counts[[1]] == sum(zero$visits$valid_count),
            zero_report$site_n_supported_point_years[[1]] == sum(zero$opportunity$supported),
            zero_report$site_n_supported_zero_point_years[[1]] == sum(zero$opportunity$supported))
})

server_source <- paste(readLines("server.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("y=~relative_rate", server_source, fixed = TRUE),
          !grepl("y=~density", server_source, fixed = TRUE),
          grepl("Observer-estimated distance band (0–200 m)", server_source, fixed = TRUE),
          grepl("size = sub$n_points_window", server_source, fixed = TRUE),
          grepl("col = \"S_obs\"", server_source, fixed = TRUE),
          grepl("col = \"birds_per_count_window\"", server_source, fixed = TRUE),
          grepl("Unstandardized sample-incidence Hill q1", server_source, fixed = TRUE),
          grepl("Descriptive Spearman", server_source, fixed = TRUE),
          !grepl("Fisher-z", server_source, fixed = TRUE),
          grepl("count_months", server_source, fixed = TRUE),
          grepl("d <- SEARCH_SITES", server_source, fixed = TRUE),
          !grepl("d <- site_table;", server_source, fixed = TRUE),
          grepl("SEARCH_SITES$S_rare", server_source, fixed = TRUE),
          !grepl("SEARCH_SITES$n_species", server_source, fixed = TRUE),
          !grepl("scientificName = c(\"# SITE\"", server_source, fixed = TRUE),
          grepl("row_type = \"site_summary\"", server_source, fixed = TRUE),
          grepl("body[, REPORT_KEEP", server_source, fixed = TRUE),
          grepl("eligible_birds\",\"detections", server_source, fixed = TRUE),
          grepl("distance_decay(rv$obs, sci, rv$opportunity, rv$visits, rv$nvis)",
                server_source, fixed = TRUE))
search_source <- paste(readLines("scripts/build_search_index.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("visits$year >= BIRD_CROSS_SITE_YEAR_MIN", search_source, fixed = TRUE),
          grepl("visits$year <= BIRD_CROSS_SITE_YEAR_MAX", search_source, fixed = TRUE),
          grepl("CROSS_PATH", search_source, fixed = TRUE),
          !grepl("INDEX_PATH", search_source, fixed = TRUE),
          !grepl("site_index$n_species", search_source, fixed = TRUE))
cat("OK: real positive-site, distance-profile, and opportunity-complete all-zero Shiny lifecycles passed.\n")
