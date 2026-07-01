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
# Demo defaults to CLBJ (LBJ National Grassland, TX oak savanna): the richest site
# in the set AND one with a STABLE Chao2 (Q2=13), so the first estimate a new user
# meets is honest, not the old HARV demo's 3x Q2=1 extrapolation (suite data-audit).
DEMO_META <- list(site = "CLBJ", label = "CLBJ · LBJ National Grassland · demo")

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

# ---- network search index (built by scripts/build_search_index.R) -----------
# One small .rds loaded ONCE at boot: $taxa = tidy (species x site) occurrence
# table with the per-site detection index + year span; $sites = per-site
# headline metrics (mirrors site_index). The Search tab filters these in memory,
# so the network search is instant with no live fetch. NULL-safe.
SEARCH_INDEX <- tryCatch(readRDS("data/search_index.rds"), error = function(e) NULL)
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
  cbind(m, SITE_INDEX[match(m$site, SITE_INDEX$site), c("n_species", "n_points", "n_visits", "birds_per_count", "top_species")])
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
# Body stays Rubik (sans); Fraunces serif display headings are applied in bird.css
# (NOT via heading_font here — avoids a double @font-face import that would break
# html-to-image's already-loaded-font path). See www/bird.css.
# Rubik is named as a PLAIN CSS font-family here (a bslib font_collection of bare
# strings), NOT font_google("Rubik"). font_google() defaults to local = TRUE, which
# makes bslib DOWNLOAD the font from Google and compile it into the theme AT APP
# STARTUP. On Connect Cloud that live fetch runs on every cold start against an empty
# cache; when Google Fonts is slow/unreachable the Sass compile blocks/fails during
# boot -> black screen / "start-up error" (republish only re-primes the cache until the
# next recycle). Naming the family as a string does ZERO network at boot; the real
# Rubik glyphs are still delivered client-side by the <link> in ui.R (display=swap),
# with a system-sans fallback. Fraunces serif display headings are applied in
# www/bird.css (already fallback-stacked: Fraunces, Georgia, "Times New Roman", serif).
# See docs/neonize-playbook.md §4.
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

# The app mascot — a flat (no-gradient, no-id so it's safely reusable) cheerful
# goldfinch in the Field Guide accent. Used as the loading spinner, the splash
# guide, and the celebration hop. Parts are classed so the CSS can wiggle "ears"
# (the wing tufts) / blink eyes.
MASCOT_CRITTER <- htmltools::HTML(paste0(
  '<svg class="mascot" viewBox="0 0 120 120" aria-hidden="true">',
  '<g fill="#f0b94a"><path d="M54,30 L57,16 L62,30 Z"/><path d="M62,30 L65,14 L70,30 Z"/></g>',
  '<ellipse cx="60" cy="66" rx="32" ry="33" fill="#ffce5a"/>',
  '<ellipse cx="60" cy="76" rx="19" ry="21" fill="#fff3d6"/>',
  '<g class="mascot-ear-l"><path d="M30,58 Q14,66 22,86 Q34,80 40,64 Z" fill="#e0714a"/></g>',
  '<g class="mascot-ear-r"><path d="M90,58 Q106,66 98,86 Q86,80 80,64 Z" fill="#e0714a"/></g>',
  '<path d="M54,68 L66,68 L60,80 Z" fill="#f0993a"/>',
  '<g class="mascot-eyes"><circle cx="50" cy="60" r="6.5" fill="#2a160a"/><circle cx="70" cy="60" r="6.5" fill="#2a160a"/>',
  '<circle cx="48" cy="57.5" r="2.4" fill="#ffffff"/><circle cx="68" cy="57.5" r="2.4" fill="#ffffff"/></g>',
  '</svg>'))
