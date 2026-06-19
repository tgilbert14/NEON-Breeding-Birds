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
})
source("R/site_metadata.R", local = FALSE)
source("R/bird_helpers.R", local = FALSE)

NEON_DPID <- "DP1.10003.001"   # Breeding landbird point counts
.NEON_PKG <- paste0("neon", "Utilities")
LIVE_FETCH <- (Sys.getenv("BRD_LIVE", "0") != "0") && requireNamespace(.NEON_PKG, quietly = TRUE)

SITE_DIR  <- "data/sites"
DEMO_PATH <- "data-sample/demo.rds"
DEMO_META <- list(site = "HARV", label = "HARV · Harvard Forest — demo")

read_bundle <- function(f) {
  if (!file.exists(f)) return(NULL)
  out <- tryCatch(readRDS(f), error = function(e) { warning(sprintf("read_bundle('%s'): %s", f, conditionMessage(e))); NULL })
  if (is.null(out)) return(NULL)
  if (is.data.frame(out)) return(out)
  if (is.null(out$obs) || !nrow(out$obs)) NULL else out
}
load_site_bundle <- function(site) read_bundle(file.path(SITE_DIR, paste0(site, ".rds")))
load_demo <- function() { b <- load_site_bundle(DEMO_META$site); if (!is.null(b)) b else read_bundle(DEMO_PATH) }

SITE_INDEX <- tryCatch(readRDS("data/site_index.rds"), error = function(e) NULL)
BUNDLED <- if (!is.null(SITE_INDEX)) SITE_INDEX$site else character(0)
site_table <- if (length(BUNDLED)) {
  m <- neon_sites[match(BUNDLED, neon_sites$site), ]
  cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site), c("n_species", "n_points", "n_visits", "birds_per_count", "top_species")])
} else neon_sites[0, ]

bird_state_choices <- function() {
  st <- sort(unique(site_table$state)); if (!length(st)) return(NULL)
  setNames(st, sprintf("%s (%d)", state_names[st] %||% st, as.integer(table(site_table$state)[st])))
}
bird_sites_in_state <- function(stt) {
  rows <- site_table[site_table$state == stt, ]; rows <- rows[order(rows$name), ]
  if (!nrow(rows)) return(character(0))
  setNames(rows$site, sprintf("%s — %s", rows$site, rows$name))
}

# Field Guide palette (parchment / ink / rust / goldfinch — an Audubon-plate look,
# deliberately distinct from the mammal app's navy/cardinal house style). OLD key
# names are kept and remapped so existing references (e.g. server.R's DDL$sky) keep
# working; the detection-method data colors (sing/call/vis) are LOCKED.
DDL <- list(
  parchment = "#f7f3e9", paper = "#fffdf6", bg = "#f7f3e9",
  ink = "#2b2722", ink2 = "#4a443c", muted = "#7a6f5d", line = "#e3d9c4",
  rust = "#c1502e", rust2 = "#a23f22", goldfinch = "#e8a317", gold_ink = "#9a6b0f",
  sing = "#1a7f37", call = "#2f7fb5", vis = "#c1502e",            # detection palette (locked)
  dawn1 = "#f6c89a", dawn2 = "#e8a37a", dawn3 = "#c98ba0", dawn4 = "#8fb0c9",
  # legacy aliases -> Field Guide, so old code paths stay on-theme
  navy = "#2b2722", navy2 = "#4a443c", cardinal = "#c1502e",
  gold = "#e8a317", gold2 = "#9a6b0f", sky = "#2f7fb5",
  green = "#1a7f37", green2 = "#12612a")
# Body stays Rubik (sans); Fraunces serif display headings are applied in bird.css
# (NOT via heading_font here — avoids a double @font-face import that would break
# html-to-image's already-loaded-font path). See www/bird.css.
app_theme <- bs_theme(version = 5, bg = "#fffdf6", fg = DDL$ink,
  primary = DDL$rust, secondary = DDL$goldfinch, success = DDL$sing, info = DDL$call,
  warning = DDL$goldfinch, danger = DDL$rust2,
  base_font = font_google("Rubik"), heading_font = font_google("Rubik"), "border-radius" = "10px")

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

# ---- biome classification (cross-site gradient color / legend) --------------
# Manual per-site biome (domain alone mixes biomes); anything unlisted = forest.
SITE_BIOME <- c(
  BARR="tundra", TOOL="tundra", NIWO="tundra",
  JORN="desert", SRER="desert", MOAB="desert", ONAQ="desert",
  WOOD="grassland", DCFS="grassland", NOGP="grassland", KONZ="grassland", KONA="grassland",
  CPER="grassland", STER="grassland", OAES="grassland", CLBJ="grassland", YELL="grassland", SJER="grassland",
  GUAN="tropical", LAJA="tropical")
biome_of  <- function(site) { b <- unname(SITE_BIOME[site]); ifelse(is.na(b), "forest", b) }
BIOME_COL <- c(forest="#2f7f4f", grassland="#e8a317", desert="#c1502e", tundra="#7fa8c9", tropical="#9c5fb0")
BIOME_LAB <- c(forest="Forest", grassland="Grassland / prairie", desert="Desert / shrub",
               tundra="Tundra / alpine", tropical="Tropical dry forest")
biome_col <- function(b) { out <- unname(BIOME_COL[b]); ifelse(is.na(out), "#9aa6b2", out) }

# ---- precomputed climate / cross-site tables (built by scripts/, loaded once) -
SITE_CLIMATE    <- tryCatch(readRDS("data/site_climate.rds"),    error = function(e) NULL)
SITE_MONTH_CLIM <- tryCatch(readRDS("data/site_month_clim.rds"), error = function(e) NULL)
CROSS_SITE      <- tryCatch(readRDS("data/cross_site.rds"),      error = function(e) NULL)

# One row per site for the "Across the continent" tab: climate + richness (raw +
# effort-rarefied) + biome, joined once at boot (46 rows). NULL-safe so a missing
# precompute degrades the tab, never crashes boot.
GRADIENT <- local({
  if (is.null(SITE_CLIMATE) || is.null(SITE_INDEX)) return(NULL)
  g <- merge(SITE_CLIMATE,
             SITE_INDEX[, c("site","n_species","n_points","n_visits","birds_per_count","top_species")],
             by = "site", all.x = TRUE)
  if (!is.null(CROSS_SITE)) g <- merge(g, CROSS_SITE, by = "site", all.x = TRUE)
  m <- neon_sites[match(g$site, neon_sites$site), ]
  g$name <- m$name; g$state <- m$state; g$bio <- m$bio
  g$biome <- biome_of(g$site); g$biome_col <- biome_col(g$biome); g$biome_lab <- unname(BIOME_LAB[g$biome])
  g[order(g$mat_c), ]
})
