# ===========================================================================
# NEON Bird Explorer — ui.R
# ===========================================================================
# v2 flow: the sidebar is GONE. The national picker map IS the way to select a
# site (tap a dot -> "Explore this site"); the relocated select panel below the
# map is the by-name fallback. The two controls that must stay reachable
# everywhere (theme toggle + "How it works") live in a slim top bar. The loaded
# view's hero band carries a "change site" link and a "report" download.

# Compact in-app echo of the public Living Poster. The site picker remains the
# functional start; this static first frame supplies the same invitation,
# artwork, suite route, and detection-index boundary before the controls.
bird_poster <- function() {
  tags$section(
    class = "brd-poster",
    `aria-labelledby` = "brd-poster-title",
    div(class = "brd-poster-copy",
      div(class = "brd-poster-topline",
        div(class = "brd-poster-brand", "Desert Data Labs"),
        tags$nav(
          class = "brd-poster-nav", `aria-label` = "NEON Explorer Suite",
          tags$a(
            class = "brd-poster-suite-link",
            href = "https://tgilbert14.github.io/NEON-Driver-Cascade/",
            target = "_blank", rel = "noopener",
            "Whole suite: ", tags$strong("Driver Cascade"),
            tags$span(`aria-hidden` = "true", " ↗")
          )
        )
      ),
      div(class = "brd-poster-app", "NEON Breeding Bird Explorer · unofficial"),
      h1(
        id = "brd-poster-title", `aria-label` = "Who’s singing where?",
        tags$span("Who’s singing"), tags$em("where?")
      ),
      p(
        class = "brd-poster-promise",
        "Follow the birds NEON hears and sees across 47 breeding-season field sites."
      ),
      tags$a(
        class = "brd-poster-cta", href = "#site-picker-start",
        onclick = paste0(
          "window.setTimeout(function(){var target=document.getElementById('site-picker-start');",
          "if(target){target.focus({preventScroll:true});}},0)"
        ),
        "Choose a field site ", tags$span(`aria-hidden` = "true", "↓")
      ),
      p(
        class = "brd-poster-note",
        "Public NEON DP1.10003.001 · birds per count is a detection index—not population size."
      )
    ),
    tags$figure(class = "brd-poster-art",
      tags$picture(
        tags$source(
          type = "image/webp",
          srcset = paste(
            paste0(asset_url("assets/birds-living-poster-v1-840.webp"), " 840w,"),
            paste0(asset_url("assets/birds-living-poster-v1.webp"), " 1536w")
          ),
          sizes = "(max-width: 700px) 100vw, 58vw"
        ),
        tags$img(
          src = asset_url("assets/birds-living-poster-v1.png"),
          width = "1536", height = "1024", fetchpriority = "high",
          decoding = "async",
          alt = paste(
            "Screenprint illustration of a singing bird above a field observer",
            "conducting a dawn point count."
          )
        )
      ),
      tags$figcaption(
        "Generated editorial illustration · not field documentation or measured data"
      )
    )
  )
}

ui <- bslib::page_fillable(
  theme = app_theme, fillable = FALSE,
  window_title = "NEON Bird Explorer",
  tags$head(
    tags$meta(name = "ddl-app-ready", content = "breeding-birds-release-2026-v1"),
    tags$meta(name = "ddl-release-instance", content = RELEASE_STAMP_ID),
    tags$script(src = asset_url("vendor/html-to-image/html-to-image.js")),
    tags$link(rel = "stylesheet", href = asset_url("styles.css")),
    tags$link(rel = "stylesheet", href = asset_url("bird.css")),
    tags$link(rel = "stylesheet", href = asset_url("poster.css")),
    tags$script(src = asset_url("app.js")),
    tags$script(src = asset_url("pincards.js"))
  ),
  useShinyjs(),

  tags$a(class = "app-skip", href = "#appMain", "Skip to app content"),

  # ---- persistent top control bar (theme + help) -------------------------
  # Replaces the sidebar's always-on controls. Stays top-right above the hero.
  div(class = "top-bar",
    div(class = "top-bar-brand",
      tags$span(class = "tb-mark", `aria-hidden` = "true", "\U0001F426"),
      tags$span(class = "tb-title", "Bird Explorer")),
    div(class = "top-bar-actions",
      div(class = "tb-temp",
        tags$span(class = "tb-temp-lab", `aria-hidden` = "true", bs_icon("thermometer-half")),
        radioButtons(
          "tempUnit", tags$span(class = "visually-hidden", "Temperature units"),
          choices = c("°F" = "F", "°C" = "C"), selected = "F", inline = TRUE
        )),
      actionButton(
        "help", tagList(bs_icon("question-circle"), tags$span(class = "tb-help-label", "How it works")),
        class = "btn-outline-dark btn-sm tb-help", `aria-label` = "How it works"
      ),
      div(class = "tb-theme", role = "group", `aria-label` = "Color theme",
        tags$span(class = "tb-theme-lab", `aria-hidden` = "true", bs_icon("circle-half")),
        input_dark_mode(id = "colorMode", mode = "light")))
  ),

  div(
    id = "loadOverlay", class = "load-overlay", role = "dialog",
    `aria-modal` = "true", `aria-live` = "polite", `aria-busy` = "false",
    `aria-hidden` = "true", `aria-labelledby` = "loadTitle", tabindex = "-1",
    div(class = "load-card",
    div(class = "load-spin", `aria-hidden` = "true", bs_icon("binoculars")),
    div(id = "loadTitle", class = "load-title", "Loading site data"),
    div(id = "loadSite", class = "load-site"), div(class = "load-bar"),
    div(id = "loadNote", class = "load-note", "Building the species board, detection profiles, and maps."))),

  tags$main(id = "appMain", class = "app-main", tabindex = "-1",

  uiOutput("heroStats"),

  # "Open a species profile" picker — was in the sidebar, now a hidden body block
  # revealed on site load (server: shinyjs::show("spPickerWrap")). Same ids
  # (spSel, surpriseBtn) so the server logic is untouched.
  hidden(div(id = "spPickerWrap", class = "sp-picker-wrap",
    div(class = "spw-row",
      div(class = "spw-sel",
        selectizeInput("spSel", label = tagList(bs_icon("search"), " Open a species profile"), choices = NULL,
                       width = "100%", options = list(placeholder = "Pick a species…"))),
      actionButton("surpriseBtn", tagList(bs_icon("dice-5-fill"), " Surprise me"),
                   class = "btn-outline-dark spw-surprise")))),

  div(id = "splash",
    div(class = "splash splash-map",
    bird_poster(),
    div(id = "site-picker-start", class = "picker-start", tabindex = "-1",
      p(class = "picker-start-kicker", "Choose a place"),
      h2("Choose one field site."),
      p("Tap a point on the map, choose by name, or browse the complete 47-site list.")),
    div(class = "picker-map-wrap", leafletOutput("nationalPicker", height = "440px")),

    # ---- relocated select panel (was the sidebar) ----------------------
    # Same input ids the server's cascade + load path depend on (stateSel,
    # site, loadBtn). Tapping a dot is the primary path; this panel is the
    # by-name fallback. Same ids, so server.R is untouched.
    div(class = "select-panel",
      div(class = "sp-head", bs_icon("sliders"), " Or pick a site by name"),
      div(class = "sp-row",
        div(class = "sp-field",
          selectInput("stateSel", label = tagList(bs_icon("geo-alt-fill"), " State"), choices = NULL, width = "100%")),
        div(class = "sp-field",
          selectInput("site", label = tagList(bs_icon("pin-map-fill"), " Site"), choices = NULL, width = "100%"))),
      uiOutput("siteBio"),
      actionButton("loadBtn", tagList(bs_icon("globe-americas"), " Explore this site"),
                   class = "btn-primary btn-lg w-100 load-btn sp-load", onclick = "smtLoadStart()")),

    tags$details(class = "site-browse",
      tags$summary(class = "site-browse-summary",
        tags$span(class = "sbs-label", bs_icon("list-ul"), " Browse all 47 sites as a list"),
        tags$span(class = "sbs-chevron", bs_icon("chevron-down"))),
      div(class = "site-browse-body", uiOutput("siteCards"))))),
  div(id = "mainTabsWrap", class = "main-tabs-wrap",
    div(class = "hero-caveat", bs_icon("soundwave"),
      tags$span(HTML("Counts are a <b>detection index</b> (birds per 6-minute point-count), not a population. A loud species and a quiet one at equal density give unequal counts."))),
    navset_card_tab(id = "tabs",
      nav_panel(title = tagList(bs_icon("compass"), " Overview"), value = "overview",
        div(class = "home-nav",
          actionButton("goCommunity", tagList(bs_icon("diagram-3-fill"), div("Community"), tags$small("richness & how many")), class = "home-btn"),
          actionButton("goBoard", tagList(bs_icon("bullseye"), div("Bird Board"), tags$small("every species, pinnable")), class = "home-btn home-btn-star"),
          actionButton("goClimate", tagList(bs_icon("globe-americas"), div("Across the continent"), tags$small("47 sites by climate")), class = "home-btn"),
          actionButton("goSpecies", tagList(bs_icon("feather"), div("Species Profile"), tags$small("one species up close")), class = "home-btn"),
          actionButton("goMap", tagList(bs_icon("map-fill"), div("Map"), tags$small("grids across the site")), class = "home-btn")),
        card(full_screen = TRUE,
          card_head("bar-chart-steps", "Most-detected species",
            info_pop("Detection index",
              p("Each species' ", tags$b("detection index"), ", birds counted per point-count, coloured by how it was first detected (", tags$span(style="color:#1a8a5a;font-weight:700","● singing"), " / ", tags$span(style="color:#3a8fd6;font-weight:700","▲ calling"), " / ", tags$span(style="color:#D55E00;font-weight:700","■ visual"), "). Each method also carries a marker shape, so it reads without relying on colour."),
              p(class="caveat", bs_icon("exclamation-triangle"), " This is a ", tags$b("detection index, not a population"), ": a loud species and a quiet one at equal density give unequal counts. A species profile shows the recorded distance support and a descriptive relative distance signature."))),
          uiOutput("overviewInsight"),
          spin(plotlyOutput("topBar", height = "440px"))),
        card(card_head("stars", "The story so far", info_pop("Story", p("Written from this site's live data."))),
          uiOutput("siteInsights")),
        card(full_screen = TRUE,
          card_head("calendar-range", "When the counts happen, the breeding window in the site's year",
            info_pop("Seasonal context",
              p("Each shaded month belongs to the exact set of calendar months containing valid 2017–2024 bird counts; gaps are not filled into a continuous band. The curves are the site's ", tags$b("monthly green-up and temperature context"), " (co-located NEON plant-phenology + air-temperature data, ", tags$em("not"), " a bird measurement)."),
              p("It shows ", tags$b("when"), " counts happened against the seasonal cycle, not a bird-vs-environment driver model or a within-season bird trend."))),
          uiOutput("seasonInsight"),
          spin(plotlyOutput("seasonStrip", height = "260px")))),
      nav_panel(title = tagList(bs_icon("diagram-3-fill"), " Community"), value = "community",
        div(class = "tab-head", div(class = "tab-head-text",
          h4("How many species were detected here?"),
          p("Observed richness, a bias-corrected incidence Chao2 extrapolation, and the complete valid physical-count ledger behind both."),
          span(class = "scope-chip scope-site", bs_icon("geo-alt-fill"), " Showing this site only"))),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE,
            card_head("graph-up", "Species accumulation (by valid six-minute count)",
              info_pop("Accumulation", p("As valid physical counts accumulate—including supported counts with zero eligible detections—how many species are observed. Two bouts remain two protocol samples. The final-five description applies only to the sampled curve; it is not a future prediction or proof that every resident species was found."))),
            uiOutput("accumInsight"), spin(plotlyOutput("accumPlot", height = "340px"))),
          card(full_screen = TRUE,
            card_head("calculator", "Incidence richness estimate (Chao2)",
              info_pop("Chao2", p(tags$b("Bias-corrected Chao2"), " extrapolates richness from species incidence across the complete valid physical-count universe. Duplicate detections within a count collapse, but repeated valid bouts remain separate samples. Its stability flag and interval matter; the estimate is not a census or detection-corrected occupancy."))),
            uiOutput("chaoBanner")))),
      nav_panel(title = tagList(bs_icon("bullseye"), " Bird Board"), value = "board",
        div(class = "tab-head", div(class = "tab-head-text",
          h4("Every species on one board",
             info_pop("Bird Board",
               p("Each dot is a ", tags$b("species"), ", placed by ", tags$b("detection frequency"), " (% of valid physical six-minute counts with an eligible detection) and its ", tags$b("detection index"), " (birds per valid point-count). Repeated bouts are repeated protocol samples, not independent places; neither axis is detection-corrected occupancy or abundance."),
               p(tags$b("Tap a dot"), " to pin its card; tap “Open species profile” for its full detection profile. Faint dots are too few detections to place reliably."))),
          p("Each dot is a species: valid-count detection frequency × birds per count. Tap to pin a card; open any species' full profile."),
          span(class = "scope-chip scope-site", bs_icon("geo-alt-fill"), " Showing this site only")),
          div(class = "sizelab-controls",
            selectInput("boardColor", tagList(bs_icon("palette"), " Colour by"),
                        choices = c("Detection method" = "method"), selected = "method", width = "200px"))),
        card(full_screen = TRUE,
          card_head("bullseye", "Detection frequency × detection index, by species",
            info_pop("Reading this", p("The horizontal axis uses every valid physical count, including supported zero-detection counts. The vertical axis divides eligible birds by that same valid-count effort. The ", tags$span(style="color:#9a6b0f;font-weight:700","gold diamond"), " is the species you're viewing."),
              p("Dots are coloured AND shaped by first-detection method, so it reads without relying on colour: ", tags$span(style="color:#1a8a5a;font-weight:700","● singing"), " / ", tags$span(style="color:#3a8fd6;font-weight:700","▲ calling"), " / ", tags$span(style="color:#D55E00;font-weight:700","■ visual"), " / ", tags$span(style="color:#7a4a2a;font-weight:700","◆ drumming"), "."))),
          div(class = "sizelab-toolbar",
            tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveScatter()", bsicons::bs_icon("camera-fill"), " Download (with pinned cards)"),
            downloadButton("boardCsv", "Download table (CSV)", class = "smt-clear-btn"),
            tags$button(class = "smt-clear-btn", type = "button", onclick = "smtClearPins()", bsicons::bs_icon("eraser-fill"), " Clear pins"),
            tags$span(class = "sizelab-hint", bs_icon("hand-index-thumb"), " interactive · tap a dot to pin its card")),
          div(class = "smt-pinnable", id = "boardPin", spin(plotlyOutput("birdBoard", height = "540px")))),
        uiOutput("spCardSlot")),
      nav_panel(title = tagList(bs_icon("globe-americas"), " Across the continent"), value = "climate",
        div(class = "tab-head", div(class = "tab-head-text",
          h4("One protocol, 47 sites, a continent of climates"),
          p("Each dot is a NEON site, placed by its breeding-season climate against its bird community. Tap a dot to pin its card or jump to that site."),
          span(class = "scope-chip scope-all", bs_icon("globe-americas"), " All 47 NEON sites, not just this one")),
          div(class = "sizelab-controls",
            selectInput("gradMetric", tagList(bs_icon("bar-chart"), " Community metric (y)"),
              choices = c("Richness · effort-rarefied" = "rarefied", "Richness · observed (raw)" = "observed",
                          "Hill q1 · unstandardized incidence" = "hill1", "Mean point ubiquity (context)" = "ubiquity",
                          "Singing share (% of detections)" = "singing",
                          "Birds per count (detection index)" = "index"), selected = "rarefied", width = "240px"),
            radioButtons("gradX", "Climate axis (x)", inline = TRUE,
              choices = c("Temperature · 47 sites" = "temp", "Precipitation · complete years only" = "precip"), selected = "temp"))),
        card(full_screen = TRUE,
          card_head("globe-americas", "Bird community across the climate gradient",
            info_pop("Reading this",
              p("Explore how breeding-season climate aligns with 2017–2024 bird-community summaries across the release roster. Each dot is a site; ", tags$b("size = counted points"), "; colour and shape = biome; the ", tags$span(style="color:#9a6b0f;font-weight:700","gold diamond"), " is the site you're viewing. Hover cards expose valid counts, points, year bounds, rarefied richness, and estimated coverage."),
              p("By default, richness is ", tags$b("rarefied to a common number of valid six-minute counts"), ". That standardizes count-sample size only: residual completeness, detectability, repeated-place structure, biome, latitude, and spatiotemporal differences remain. This is descriptive ", tags$b("space-for-time"), ", not one site warming. Precipitation keeps only sites with at least one complete calendar year; missing values are reported as unavailable and never imputed."))),
          div(class = "sizelab-toolbar",
            tags$button(class = "smt-snap-btn", type = "button", onclick = "smtSaveClimate()", bsicons::bs_icon("camera-fill"), " Download (with pinned cards)"),
            downloadButton("gradientCsv", "Download table (CSV)", class = "smt-clear-btn"),
            tags$button(class = "smt-clear-btn", type = "button", onclick = "smtClearPins()", bsicons::bs_icon("eraser-fill"), " Clear pins"),
            tags$span(class = "sizelab-hint", bs_icon("hand-index-thumb"), " interactive · tap a site to pin its card")),
          div(class = "smt-pinnable", id = "climatePin", spin(plotlyOutput("climateGradient", height = "560px"))))),
      nav_panel(title = tagList(bs_icon("search"), " Search"), value = "search",
        div(class = "tab-head", div(class = "tab-head-text",
          h4("Search the network"),
          p("Look across all 47 NEON sites using the shared 2017–2024 valid-count window. Find every site where a species was detected, or compare effort-rarefied site richness. Pick a result to load that site's full Overview."),
          span(class = "scope-chip scope-all", bs_icon("globe-americas"), " All 47 NEON sites"))),
        radioButtons("searchMode", NULL, inline = TRUE,
          choices = c("Find a species" = "taxon", "Threshold query" = "threshold"),
          selected = "taxon"),
        # (a) FIND A SPECIES
        conditionalPanel("input.searchMode == 'taxon'",
          card(card_head("feather", "Find a species across the network"),
            div(class = "search-controls",
              selectizeInput("searchSp", "Species (common or scientific name)",
                choices = NULL, width = "420px",
                options = list(placeholder = "Start typing a bird name…",
                               maxOptions = 1200)))),
          card(card_head("geo-alt-fill", "Sites where it was detected"),
            div(class = "search-cap", uiOutput("searchSpCaption")),
            div(style = "width:100%;", DTOutput("searchSpTable")))),
        # (b) THRESHOLD QUERY
        conditionalPanel("input.searchMode == 'threshold'",
          card(card_head("sliders", "Threshold query"),
            div(class = "search-controls",
              selectInput("threshKind", "Query", width = "300px",
                choices = c("Species detected at more than N sites · 2017–2024" = "wide",
                            "Rarefied site richness above X · 2017–2024" = "rich")),
              conditionalPanel("input.threshKind == 'wide'",
                sliderInput("threshN", "More than this many sites", min = 1, max = 45, value = 20, step = 1, width = "320px")),
              conditionalPanel("input.threshKind == 'rich'",
                sliderInput("threshX", "More than this many rarefied species", min = 0, max = 200, value = 100, step = 5, width = "320px")))),
          card(card_head("list-ul", "Matches"),
            div(class = "search-cap", uiOutput("searchThreshCaption")),
            div(style = "width:100%;", DTOutput("searchThreshTable"))))),
      nav_panel(title = tagList(bs_icon("feather"), " Species Profile"), value = "species", uiOutput("speciesProfile")),
      nav_panel(title = tagList(bs_icon("map-fill"), " Map"), value = "map",
        div(class = "tab-head", div(class = "tab-head-text",
          h4("Bird grids across the site"),
          p("Each marker is a NEON point-count grid, sized by richness and coloured by your chosen metric. ", tags$b("Tap a grid"), " for the full list of birds detected there."),
          span(class = "scope-chip scope-site", bs_icon("geo-alt-fill"), " Showing this site only")),
          div(class = "map-controls",
            selectInput("mapMetric", "Colour by", width = "180px", choices = c("Species richness" = "richness", "Birds per count" = "per_visit")),
            selectInput("view", "Basemap", width = "160px", choices = c("Terrain" = "Esri.WorldTopoMap", "Light" = "CartoDB.Positron", "Satellite" = "Esri.WorldImagery")))),
        spin(leafletOutput("map", height = "560px")),
        uiOutput("gridPanel")),
      nav_panel(title = tagList(bs_icon("info-circle"), " About"), value = "about", uiOutput("aboutPanel"))
    )),
  div(class = "ddl-footer",
    div(tags$a(class = "custom-cta", href = "mailto:desertdatalabs@gmail.com?subject=NEON%20Bird%20Explorer",
      span(class = "hand", "\U0001F44B"), "Questions or feedback? Get in touch with Desert Data Labs.")),
    p(style = "margin-top:12px", HTML("Built by <strong>Desert Data Labs</strong> · Tucson, AZ · get in touch →"),
      tags$a(href = "mailto:desertdatalabs@gmail.com?subject=NEON%20Bird%20Explorer", "desertdatalabs@gmail.com")),
    p(style = "font-size:12px;opacity:.85", "Data: NEON Breeding Landbird Point Counts (DP1.10003.001). Not affiliated with NEON, Battelle, or the NSF. An educational data-exploration tool."))
  )
)
