# ===========================================================================
# NEON Breeding Bird Explorer — global.R
# A NEONize sibling (Desert Data Labs) for Breeding landbird point counts
# (DP1.10003.001). Chrome + bundling spine + pin-card interaction ported from
# the prior siblings; the analysis layer is point-count / avian-survey native.
# ===========================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons)
  library(dplyr); library(tidyr); library(stringr); library(tibble)
  library(plotly); library(leaflet); library(DT)
  library(shinyjs); library(shinycssloaders); library(RColorBrewer); library(htmltools)
  library(jsonlite); library(digest)
})
# ---- basemap --------------------------------------------------------------
# CARTO watermarks unauthenticated basemaps.cartocdn.com raster tiles ("API KEY
# REQUIRED", since 2026-08-26; suite record: NEON-Driver-Cascade
# docs/SUITE-BASEMAP-INCIDENT-2026-08.md). The key rides in the tile URL, so it
# is a public rate-limited identifier, not a credential; Sys.getenv keeps it out
# of git and makes rotation a Connect Cloud setting. addProviderTiles() cannot
# carry it (the bundled CartoDB template has no {apikey} slot), hence addTiles().
# Accepts either a leaflet provider name or a CARTO variant, so ui.R basemap
# choices stay exactly as they are and any non-CARTO provider passes straight
# through. Without the key it falls back to Esri's keyless grey canvas — clean,
# but content-free past z16 at rural sites, so the cap keeps the zoom honest.
add_suite_basemap <- function(map, basemap = "light_all", noWrap = FALSE) {
  variant <- switch(basemap,
    "light_all" = ,
    "CartoDB.Positron" = "light_all",
    "dark_all" = ,
    "CartoDB.DarkMatter" = "dark_all",
    NULL)
  if (is.null(variant))
    return(leaflet::addProviderTiles(map, basemap,
      options = leaflet::providerTileOptions(noWrap = noWrap)))
  key <- Sys.getenv("CARTO_BASEMAP_KEY", "")
  if (nzchar(key)) {
    leaflet::addTiles(map,
      urlTemplate = sprintf(
        "https://{s}.basemaps.cartocdn.com/%s/{z}/{x}/{y}{r}.png?key=%s", variant, key),
      attribution = paste(
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
        '&copy; <a href="https://carto.com/attributions">CARTO</a>'),
      options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20, noWrap = noWrap))
  } else {
    leaflet::addTiles(map,
      urlTemplate = sprintf(
        "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_%s_Gray_Base/MapServer/tile/{z}/{y}/{x}",
        if (identical(variant, "dark_all")) "Dark" else "Light"),
      attribution = 'Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors',
      options = leaflet::tileOptions(maxNativeZoom = 16, maxZoom = 19, noWrap = noWrap))
  }
}

source("R/site_metadata.R", local = FALSE)
source("R/bird_helpers.R", local = FALSE)

NEON_DPID <- "DP1.10003.001"   # Breeding landbird point counts
NEON_RELEASE <- "RELEASE-2026"
NEON_DOI <- "10.48443/v6hs-mx57"
APP_RELEASE_MARKER <- "breeding-birds-release-2026-v1"
LIVE_FETCH <- FALSE              # production and local runtime are bundle-only

# A deterministic exact-payload identity is written only after the clean
# validator rebuilds every derived artifact. It binds the acquisition receipts,
# Shiny runtime bytes, Pages poster bytes, and canonical Connect dependency
# contract to one public ID. Keep it in the initial HTML so a production probe
# can prove that Pages and Shiny expose the same candidate—not merely two generic
# RELEASE-2026 shells.
RELEASE_STAMP_PATH <- "data/release_stamp.json"
RELEASE_STAMP <- tryCatch(
  jsonlite::fromJSON(RELEASE_STAMP_PATH, simplifyVector = FALSE),
  error = function(error) {
    stop("Cannot read deterministic release stamp: ", conditionMessage(error), call. = FALSE)
  }
)
stamp_chr <- function(field) {
  value <- RELEASE_STAMP[[field]]
  if (is.null(value) || length(value) != 1L || is.na(value)) "" else as.character(value)
}
stamp_hash <- function(field) stamp_chr(field)
required_stamp_fields <- c(
  "schema_version", "app_id", "product", "release", "doi",
  "source_receipt_sha256", "environment_receipt_sha256", "payload_sha256",
  "manifest_contract_sha256", "release_id"
)
if (!identical(names(RELEASE_STAMP), required_stamp_fields) ||
    !identical(as.integer(RELEASE_STAMP$schema_version), 3L) ||
    !identical(stamp_chr("app_id"), "NEON-Breeding-Birds") ||
    !identical(stamp_chr("product"), NEON_DPID) ||
    !identical(stamp_chr("release"), NEON_RELEASE) ||
    !identical(stamp_chr("doi"), NEON_DOI) ||
    !grepl("^[0-9a-f]{64}$", stamp_hash("source_receipt_sha256")) ||
    !grepl("^[0-9a-f]{64}$", stamp_hash("environment_receipt_sha256")) ||
    !grepl("^[0-9a-f]{64}$", stamp_hash("payload_sha256")) ||
    !grepl("^[0-9a-f]{64}$", stamp_hash("manifest_contract_sha256"))) {
  stop("Deterministic release stamp does not match the app release contract.", call. = FALSE)
}
release_identity_material <- paste(
  "neon-breeding-birds-release-instance-v3", "NEON-Breeding-Birds",
  NEON_DPID, NEON_RELEASE, NEON_DOI,
  stamp_hash("source_receipt_sha256"), stamp_hash("environment_receipt_sha256"),
  stamp_hash("payload_sha256"), stamp_hash("manifest_contract_sha256"),
  sep = "\n"
)
RELEASE_STAMP_ID <- paste0(
  "sha256:",
  digest::digest(release_identity_material, algo = "sha256", serialize = FALSE)
)
if (!identical(stamp_chr("release_id"), RELEASE_STAMP_ID)) {
  stop("Deterministic release ID does not re-derive from its exact payload stamp.", call. = FALSE)
}

SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/demo.rds"
# Demo fallback is the CLBJ oak-savanna bundle; ordinary entry asks the visitor to
# choose any of the 47 release sites, so the demo is only a local resilience path.
DEMO_META <- list(site = "CLBJ", label = "CLBJ · LBJ National Grassland · demo")

read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) { warning(sprintf("read_bundle('%s'): %s", f, conditionMessage(e))); NULL })
  if (is.null(out)) return(NULL)
  if (!is.list(out) || !all(c("obs", "visits", "opportunity", "points", "held", "meta") %in% names(out))) return(NULL)
  if (!identical(out$meta$release, NEON_RELEASE) || !identical(as.integer(out$meta$schema_version), 4L)) return(NULL)
  if (!nrow(out$opportunity)) NULL else out
}
load_site_bundle <- function(site) read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
load_demo <- function() { b <- load_site_bundle(DEMO_META$site); if (!is.null(b)) b else read_bundle(DEMO_PATH) }

SITE_INDEX <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
BUNDLED <- if (!is.null(SITE_INDEX)) SITE_INDEX$site else character(0)

# ---- network search index (built by scripts/build_search_index.R) -----------
# One small .rds loaded ONCE at boot. Both $taxa and $sites are derived only from
# valid physical counts in the fixed 2017-2024 window; lifetime SITE_INDEX values
# are not permitted in this multi-site comparison artifact.
SEARCH_INDEX <- tryCatch(readRDS("data/search_index.rds"), error = function(e) NULL)
search_taxa_fields <- c(
  "scientificName", "vernacular", "site", "name", "state",
  "analysis_year_min", "analysis_year_max", "detection_index_window",
  "detection_frequency_window", "detection_rows_window",
  "n_detected_counts_window", "n_points_detected_window",
  "distance_usable_pct_window", "distance_n_used_window",
  "distance_n_outside_truncation_window", "method", "year_min", "year_max",
  "n_sites_window")
search_site_fields <- c(
  "site", "name", "state", "analysis_year_min", "analysis_year_max",
  "bird_year_min", "bird_year_max", "S_obs", "S_rare", "t_used", "T_counts",
  "n_points_window", "n_birds_window", "n_positive_counts_window",
  "n_supported_zero_counts_window", "birds_per_count_window",
  "top_species_window", "coverage")
if (is.null(SEARCH_INDEX) || !is.list(SEARCH_INDEX) ||
    !identical(names(SEARCH_INDEX), c("schema_version", "analysis_year_min",
      "analysis_year_max", "incidence_unit", "taxa", "sites")) ||
    !identical(as.integer(SEARCH_INDEX$schema_version), 4L) ||
    !identical(as.integer(SEARCH_INDEX$analysis_year_min), BIRD_CROSS_SITE_YEAR_MIN) ||
    !identical(as.integer(SEARCH_INDEX$analysis_year_max), BIRD_CROSS_SITE_YEAR_MAX) ||
    !is.data.frame(SEARCH_INDEX$taxa) || !is.data.frame(SEARCH_INDEX$sites) ||
    !identical(names(SEARCH_INDEX$taxa), search_taxa_fields) ||
    !identical(names(SEARCH_INDEX$sites), search_site_fields) ||
    nrow(SEARCH_INDEX$sites) != 47L ||
    any(SEARCH_INDEX$taxa$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN) ||
    any(SEARCH_INDEX$taxa$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX) ||
    any(SEARCH_INDEX$sites$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN) ||
    any(SEARCH_INDEX$sites$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX))
  SEARCH_INDEX <- NULL
SEARCH_TAXA  <- if (!is.null(SEARCH_INDEX)) SEARCH_INDEX$taxa  else NULL
SEARCH_SITES <- if (!is.null(SEARCH_INDEX)) SEARCH_INDEX$sites else NULL
# autocomplete choices: "Common Name · Scientific name" -> scientificName
SEARCH_SPECIES_CHOICES <- if (!is.null(SEARCH_TAXA)) {
  u <- SEARCH_TAXA[!duplicated(SEARCH_TAXA$scientificName), c("scientificName", "vernacular")]
  u <- u[order(u$vernacular), ]
  setNames(u$scientificName, sprintf("%s · %s", u$vernacular, u$scientificName))
} else character(0)
site_table <- if (length(BUNDLED)) {
  m <- neon_sites[match(BUNDLED, neon_sites$site), ]
  cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site),
    c("n_species", "n_points", "n_visits", "n_supported_zero_counts",
      "n_opportunities", "n_supported_zero", "birds_per_count", "top_species")])
} else neon_sites[0, ]

bird_state_choices <- function() {
  st <- sort(unique(site_table$state)); if (!length(st)) return(NULL)
  setNames(st, sprintf("%s (%d)", state_names[st] %||% st, as.integer(table(site_table$state)[st])))
}
bird_sites_in_state <- function(stt) {
  rows <- site_table[site_table$state == stt, ]; rows <- rows[order(rows$name), ]
  if (!nrow(rows)) return(character(0))
  setNames(rows$site, sprintf("%s · %s", rows$site, rows$name))
}

# Field Guide palette (parchment / ink / rust / goldfinch — an Audubon-plate look,
# deliberately distinct from the mammal app's navy/cardinal house style). OLD key
# names are kept and remapped so existing references (e.g. server.R's DDL$sky) keep
# working; the detection-method data colors (sing/call/vis) are LOCKED.
DDL <- list(
  parchment = "#f7f3e9", paper = "#fffdf6", bg = "#f7f3e9",
  ink = "#2b2722", ink2 = "#4a443c", muted = "#7a6f5d", line = "#e3d9c4",
  rust = "#c1502e", rust2 = "#a23f22", goldfinch = "#e8a317", gold_ink = "#9a6b0f",
  sing = "#1a8a5a", call = "#3a8fd6", vis = "#D55E00",            # detection palette (luminance-separated, CVD-safe; mirrors METHOD_COLS)
  dawn1 = "#f6c89a", dawn2 = "#e8a37a", dawn3 = "#c98ba0", dawn4 = "#8fb0c9",
  # legacy aliases -> Field Guide, so old code paths stay on-theme
  navy = "#2b2722", navy2 = "#4a443c", cardinal = "#c1502e",
  gold = "#e8a317", gold2 = "#9a6b0f", sky = "#2f7fb5",
  green = "#1a7f37", green2 = "#12612a")
# The names below are local/system fallback stacks only. No font is downloaded at
# app startup or in the browser; this keeps cold starts and offline source checks
# independent of Google Fonts or another network service.
rubik_stack <- bslib::font_collection(
  "Rubik", "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")
app_theme <- bs_theme(version = 5, bg = "#fffdf6", fg = DDL$ink,
  primary = DDL$rust, secondary = DDL$goldfinch, success = DDL$sing, info = DDL$call,
  warning = DDL$goldfinch, danger = DDL$rust2,
  base_font = rubik_stack, heading_font = rubik_stack, "border-radius" = "10px")

asset_url <- function(path) { f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L; sprintf("%s?v=%s", path, v) }
spin <- function(x, img = NULL) shinycssloaders::withSpinner(x, color = DDL$sky, type = 6)
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")), ..., title = title, placement = placement)
insight_banner <- function(icon, ..., tone = "navy")
  div(class = paste("chart-insight", paste0("ci-", tone)), bsicons::bs_icon(icon), div(class = "ci-text", ...))
glow_badge <- function(label, color = "#c1502e", glow = color)
  span(class = "glow-badge", style = sprintf("color:#fff; background:%s; border-color:%s;", color, color), label)
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)

# temperature display — Fahrenheit (default, US audience) or Celsius. Stored data
# is always °C; these convert for display only. (Spearman/rank stats are unit-free.)
temp_val  <- function(c, unit = "F") if (identical(unit, "C")) c else c * 9 / 5 + 32
temp_unit_lab <- function(unit = "F") if (identical(unit, "C")) "°C" else "°F"
temp_disp <- function(c, unit = "F") {                       # vectorised; NA -> "—"
  c <- suppressWarnings(as.numeric(c))
  s <- if (identical(unit, "C")) sprintf("%.1f°C", c) else sprintf("%.0f°F", c * 9 / 5 + 32)
  s[is.na(c)] <- "—"; s
}

# ---- biome classification (cross-site gradient color / legend) --------------
# Manual per-site biome (domain alone mixes biomes); anything unlisted = forest.
SITE_BIOME <- c(
  BARR="tundra", TOOL="tundra", NIWO="tundra",
  JORN="desert", SRER="desert", MOAB="desert", ONAQ="desert",
  WOOD="grassland", DCFS="grassland", NOGP="grassland", KONZ="grassland", KONA="grassland",
  CPER="grassland", STER="grassland", OAES="grassland", CLBJ="grassland", YELL="grassland", SJER="grassland",
  GUAN="tropical", LAJA="tropical")
biome_of  <- function(site) { b <- unname(SITE_BIOME[site]); ifelse(is.na(b), "forest", b) }
# Biome hues are LUMINANCE-LADDERED (Okabe-Ito-derived) so the 5 biomes stay
# distinguishable for colour-vision-deficient readers and in grayscale: relative
# luminances run desert .10 < forest .19 < tropical .26 < grassland .42 < tundra
# .51 (min adjacent gap .066). The old set put forest/desert/tropical within .02 L,
# which washed out under CVD. Each biome also carries a redundant plotly marker
# SYMBOL (BIOME_SYM) so colour is never the only channel on the gradient scatter.
BIOME_COL <- c(forest="#1a8a5a", grassland="#E69F00", desert="#9c3a17", tundra="#7fc7ec", tropical="#b07aa1")
BIOME_SYM <- c(forest="circle", grassland="square", desert="diamond", tundra="triangle-up", tropical="cross")
BIOME_LAB <- c(forest="Forest", grassland="Grassland / prairie", desert="Desert / shrub",
               tundra="Tundra / alpine", tropical="Tropical dry forest")
biome_col <- function(b) { out <- unname(BIOME_COL[b]); ifelse(is.na(out), "#9aa6b2", out) }

# ---- precomputed climate / cross-site tables (built by scripts/, loaded once) -
SITE_CLIMATE    <- tryCatch(readRDS("data/site_climate.rds"),    error = function(e) NULL)
SITE_MONTH_CLIM <- tryCatch(readRDS("data/site_month_clim.rds"), error = function(e) NULL)
CROSS_SITE      <- tryCatch(readRDS("data/cross_site.rds"),      error = function(e) NULL)

# One row per site for the "Across the continent" tab. Every bird metric comes
# exclusively from the 2017-2024 cross-site artifact; lifetime SITE_INDEX values
# are deliberately not rejoined. Climate realized-month support uses the same
# bird-window constants and remains explicit in the export.
GRADIENT <- local({
  if (is.null(SITE_CLIMATE) || is.null(CROSS_SITE)) return(NULL)
  climate_fields <- setdiff(names(SITE_CLIMATE), c("analysis_year_min", "analysis_year_max"))
  g <- merge(SITE_CLIMATE[, climate_fields, drop = FALSE], CROSS_SITE,
             by = "site", all = FALSE)
  if (nrow(g) != 47L || any(g$analysis_year_min != BIRD_CROSS_SITE_YEAR_MIN) ||
      any(g$analysis_year_max != BIRD_CROSS_SITE_YEAR_MAX)) return(NULL)
  m <- neon_sites[match(g$site, neon_sites$site), ]
  g$name <- m$name; g$state <- m$state; g$bio <- m$bio
  g$biome <- biome_of(g$site); g$biome_col <- biome_col(g$biome); g$biome_lab <- unname(BIOME_LAB[g$biome])
  # Order the comparison by the exact realized-month temperature estimand.
  # Available-record MAT remains contextual metadata and never substitutes for
  # an unsupported breeding window.
  g[order(g$breeding_temp_c, g$site, na.last = TRUE, method = "radix"), ]
})
