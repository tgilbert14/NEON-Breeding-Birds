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

DDL <- list(
  navy = "#0C234B", navy2 = "#16386e", cardinal = "#AB0520", gold = "#FFD200",
  gold2 = "#c9a300", sky = "#2f7fb5", green = "#1a7f37", green2 = "#12612a",
  ink = "#1c2733", muted = "#6b7a89", bg = "#eef2f8", paper = "#ffffff", line = "#dbe2ec")
app_theme <- bs_theme(version = 5, bg = "#ffffff", fg = DDL$ink,
  primary = DDL$navy, secondary = DDL$cardinal, success = DDL$green, info = DDL$sky,
  warning = DDL$gold, danger = DDL$cardinal,
  base_font = font_google("Rubik"), heading_font = font_google("Rubik"), "border-radius" = "10px")

asset_url <- function(path) { f <- file.path("www", path)
  v <- if (file.exists(f)) as.integer(as.numeric(file.mtime(f))) else 0L; sprintf("%s?v=%s", path, v) }
spin <- function(x, img = NULL) shinycssloaders::withSpinner(x, color = DDL$sky, type = 6)
info_pop <- function(title, ..., placement = "auto")
  bslib::popover(tags$span(class = "info-dot", bsicons::bs_icon("info-circle")), ..., title = title, placement = placement)
insight_banner <- function(icon, ..., tone = "navy")
  div(class = paste("chart-insight", paste0("ci-", tone)), bsicons::bs_icon(icon), div(class = "ci-text", ...))
glow_badge <- function(label, color = "#0C234B", glow = color)
  span(class = "glow-badge", style = sprintf("color:#fff; background:%s; border-color:%s;", color, color), label)
card_head <- function(icon, title, ...)
  bslib::card_header(class = "with-info", bsicons::bs_icon(icon), tags$span(class = "ch-title", " ", title), ...)
fmt_int <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
