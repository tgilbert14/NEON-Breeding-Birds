# ===========================================================================
# NEON Breeding Bird Explorer — server.R
# ===========================================================================
bird_report_export <- function(board, site, release, site_summary, visits, opportunity) {
  body <- board
  if (nrow(body)) {
    body$row_type <- rep("species", nrow(body))
  } else {
    # A fully sampled all-zero site is evidence, not an empty export. Preserve
    # its denominators and supported-zero counts in one explicit summary row.
    body <- data.frame(row_type = "site_summary", scientificName = NA_character_,
                       stringsAsFactors = FALSE)
  }
  nr <- nrow(body)
  supported_years <- opportunity$year[opportunity$supported %in% TRUE]
  yrs <- if (length(supported_years)) range(supported_years) else c(NA_integer_, NA_integer_)
  body$site <- rep(site, nr)
  body$release <- rep(release, nr)
  body$site_year_min <- rep(if (!any(is.na(yrs))) yrs[[1]] else NA_integer_, nr)
  body$site_year_max <- rep(if (!any(is.na(yrs))) yrs[[2]] else NA_integer_, nr)
  body$site_n_species <- rep(site_summary$n_species, nr)
  body$site_n_points <- rep(site_summary$n_points, nr)
  body$site_n_valid_counts <- rep(site_summary$n_visits, nr)
  body$site_n_supported_zero_counts <- rep(sum(visits$outcome == "supported_zero"), nr)
  body$site_n_supported_point_years <- rep(sum(opportunity$supported %in% TRUE), nr)
  body$site_n_supported_zero_point_years <- rep(sum(opportunity$outcome == "supported_zero"), nr)
  body$site_birds_per_count <- rep(site_summary$birds_per_count, nr)
  body$site_flyover_birds <- rep(site_summary$flyover_birds %||% 0, nr)
  for (cc in setdiff(REPORT_KEEP, names(body))) body[[cc]] <- rep(NA, nr)
  body[, REPORT_KEEP, drop = FALSE]
}

server <- function(input, output, session) {
  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark(); ink <- if (dark) "#efe7d6" else "#2b2722"
    grid <- if (dark) "rgba(239,231,214,0.09)" else "rgba(43,39,34,0.07)"; zero <- if (dark) "rgba(239,231,214,0.20)" else "rgba(43,39,34,0.14)"
    lin <- if (dark) "#3a3328" else "#e3d9c4"; legc <- if (dark) "#cabfa8" else "#4a443c"
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = if (dark) "rgba(38,33,27,0.97)" else "rgba(43,39,34,0.95)", bordercolor = "#e8a317", font = list(color = "#fff", family = "Rubik", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001F426") plotly::plot_ly(type="scatter", mode="markers") %>%
    plotly::layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)", xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
      annotations=list(list(text=paste0(icon,"<br>",msg), showarrow=FALSE, font=list(color=if(is_dark())"#b3a692" else "#7a6f5d", size=15), align="center"))) %>%
    plotly::config(displayModeBar = FALSE)

  rv <- reactiveValues(obs=NULL, held=NULL, visits=NULL, opportunity=NULL, points=NULL,
    observer_support=NULL, board=NULL,
    nvis=0, nopp=0, label=NULL, site=NULL, sp=NULL, ctx=NULL, is_demo=FALSE, grid=NULL, pendingSite=NULL)
  empty_board <- function() data.frame(
    scientificName=character(), vernacular=character(), detections=integer(), index_birds=numeric(),
    n_points=integer(), n_detected_counts=integer(), n_point_years=integer(),
    n_grids=integer(), mean_cluster=numeric(),
    method=character(), n_observers=integer(), distance_n_observed=integer(),
    distance_n_used=integer(), distance_n_outside_truncation=integer(),
    distance_n_unavailable=integer(), ubiquity=numeric(), n_visits=numeric(), index=numeric(),
    detection_frequency=numeric(), distance_usable_pct=numeric(), observer_support_complete=logical(),
    opportunity_complete=logical(), flyover_detections=integer(), flyover_birds=numeric(),
    total_birds=numeric(), stringsAsFactors=FALSE)

  observe({ ch <- bird_state_choices(); updateSelectInput(session, "stateSel", choices = ch, selected = if ("MA" %in% ch) "MA" else NULL) })
  # State cascade. When a map dot or browse-list card was tapped, rv$pendingSite
  # holds the picked site so the dropdowns snap to THAT site (not site #1 of the
  # state) and we load it, keeping the sidebar in sync with the map. A plain
  # state change (user spinning the dropdown) just lists the state's sites.
  observeEvent(input$stateSel, {
    sites <- bird_sites_in_state(input$stateSel)
    pend  <- rv$pendingSite
    sel   <- if (!is.null(pend) && pend %in% sites) pend else if (length(sites)) sites[[1]] else NULL
    rv$pendingSite <- NULL
    updateSelectInput(session, "site", choices = sites, selected = sel)
    if (!is.null(pend) && identical(sel, pend)) load_site(sel)   # map/browse pick -> load it
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  output$siteBio <- renderUI({ req(input$site); b <- site_bio(input$site); if (is.null(b)) return(NULL); div(class="site-bio", bs_icon("info-circle-fill"), span(b)) })
  output$siteCards <- renderUI({
    if (is.null(SITE_INDEX) || !nrow(site_table)) return(NULL)
    div(class="site-cards", lapply(seq_len(nrow(site_table)), function(i){ r <- site_table[i,]
      w <- if (!is.null(SEARCH_SITES)) SEARCH_SITES[SEARCH_SITES$site == r$site, , drop = FALSE] else NULL
      comparison <- if (!is.null(w) && nrow(w) == 1L)
        sprintf("%s · 2017–2024 · %s rarefied species · %s valid counts",
                r$state, w$S_rare, fmt_int(w$T_counts)) else
        sprintf("%s · 2017–2024 comparison unavailable", r$state)
      tags$a(class="site-card", href="#",
        onclick=sprintf("smtLoadStart('%s · loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;", gsub("'","",r$name), r$site),
        div(class="sc-emoji","\U0001F985"),
        div(class="sc-body", div(class="sc-name", tags$b(r$site), sprintf(" · %s", r$name)),
          div(class="sc-meta", comparison))) }))
  })
  shinyjs::hide("mainTabsWrap")

  ingest <- function(b, label, is_demo = FALSE) {
    if (is.null(b) || !all(c("obs","visits","opportunity","points","held","meta") %in% names(b)) ||
        !identical(b$meta$release, NEON_RELEASE) || !identical(as.integer(b$meta$schema_version), 4L)) {
      session$sendCustomMessage("loadDone", list())
      showNotification("This site is not in the validated RELEASE-2026 bundle.", type="error")
      return(invisible())
    }
    prepared <- tryCatch({
      nvis <- sum(b$visits$valid_count %in% TRUE)
      nopp <- sum(b$opportunity$supported %in% TRUE)
      if (nvis <= 0L || nopp <= 0L) stop("site has no supported survey effort")
      bird_validate_effort(b$opportunity, b$visits, b$obs)
      held <- bird_prepare_held(b$held)
      board <- species_board(b$obs, b$points, nvis, b$opportunity, b$visits,
                             b$meta$observer_support)
      if (is.null(board)) board <- empty_board()
      if (nrow(board)) {
        missing_name <- is.na(board$vernacular) | !nzchar(trimws(board$vernacular))
        board$vernacular[missing_name] <- board$scientificName[missing_name]
        missing_method <- is.na(board$method) | !nzchar(trimws(board$method))
        board$method[missing_method] <- "unknown"
      }
      list(nvis=nvis, nopp=nopp, board=board, held=held)
    }, error=function(e) e)
    if (inherits(prepared, "error")) {
      session$sendCustomMessage("loadDone", list())
      showNotification(paste("Site bundle failed scientific validation:", conditionMessage(prepared)), type="error")
      return(invisible())
    }
    # Commit the new site only after every validation succeeds; a failed load can
    # never leave observations from one site mixed with effort from another.
    rv$obs <- b$obs; rv$held <- prepared$held; rv$visits <- b$visits; rv$opportunity <- b$opportunity; rv$points <- b$points
    rv$observer_support <- b$meta$observer_support
    rv$nvis <- prepared$nvis; rv$nopp <- prepared$nopp; rv$board <- prepared$board
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$sp <- NULL; rv$grid <- NULL
    yrs <- range(b$opportunity$year[b$opportunity$supported %in% TRUE], na.rm=TRUE)
    rv$ctx <- paste0(b$meta$site, " · ", if (yrs[1]==yrs[2]) yrs[1] else paste0(yrs[1],"–",yrs[2]))
    shinyjs::show("mainTabsWrap"); shinyjs::hide("splash")
    if (nrow(rv$board)) shinyjs::show("spPickerWrap") else shinyjs::hide("spPickerWrap")
    ch <- setNames(rv$board$scientificName, sprintf("%s · %s", rv$board$vernacular, rv$board$scientificName))
    updateSelectizeInput(session, "spSel", choices = c("Pick a species…"="", ch), selected = "", server = TRUE)
    nav_select("tabs", "overview"); session$sendCustomMessage("countUp", list()); session$sendCustomMessage("loadDone", list())
    if (!nrow(rv$board))
      showNotification("Supported surveys are available, but no eligible non-flyover species were detected.", type="message")
    invisible(TRUE)
  }
  load_site <- function(site){ if (is.null(site)||site=="") { session$sendCustomMessage("loadDone", list()); return() }
    b <- load_site_bundle(site); if (is.null(b)) { session$sendCustomMessage("loadDone", list()); showNotification("That site isn't bundled in this demo.", type="error"); return() }
    row <- site_table[site_table$site==site,]; ingest(b, sprintf("%s · %s", site, if (nrow(row)) row$name else site)) }
  observeEvent(input$loadBtn, load_site(input$site))
  # Map dot / browse-list pick. Drive the sidebar selectors so the state + site
  # dropdowns end up reading the picked site, then load. Same state -> set the
  # site dropdown and load directly (the cascade won't re-fire on an unchanged
  # state). Different state -> stash pendingSite and switch stateSel, and the
  # cascade above lists the new state's sites, selects this one, and loads it.
  observeEvent(input$pickSite, {
    removeModal()   # if the pick came from the "About this site" card, close it
    code <- input$pickSite; if (is.null(code) || !nzchar(code)) { session$sendCustomMessage("loadDone", list()); return() }
    st <- neon_sites$state[neon_sites$site == code][1]
    if (is.na(st)) { load_site(code); return() }
    if (identical(input$stateSel, st)) {
      updateSelectInput(session, "site", choices = bird_sites_in_state(st), selected = code)
      load_site(code)
    } else {
      rv$pendingSite <- code
      updateSelectInput(session, "stateSel", selected = st)
    }
  })
  # (v2 flow: the LBJ Grassland demo path is gone — users pick a real site on the
  #  map, the Browse-all-sites list, or the by-name select panel. demoBtn/demoBtn2
  #  and their observers were removed with it.)

  # "Change site" (in the hero band) -> back to the picker-map landing.
  observeEvent(input$changeSite, {
    rv$obs <- NULL; rv$visits <- NULL; rv$opportunity <- NULL; rv$points <- NULL; rv$observer_support <- NULL
    rv$board <- NULL; rv$nvis <- 0; rv$nopp <- 0; rv$label <- NULL; rv$site <- NULL; rv$sp <- NULL; rv$grid <- NULL
    shinyjs::hide("mainTabsWrap"); shinyjs::hide("spPickerWrap"); shinyjs::show("splash")
    # the picker map was hidden while a site was loaded; nudge it to recompute its
    # size now that it's visible again so it never paints blank/half-width on return
    session$sendCustomMessage("kickMaps", list())
  })

  # ---- site report card (top-bar / hero "report" download) ----------------
  # No PDF path exists in this app, so the report is a tidy species-row CSV.
  # Site/release/support fields repeat explicitly on each species row. A sampled
  # all-zero site emits one typed site_summary row; scientificName is never
  # overloaded with sentinel metadata text.
  output$reportCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_report_%s_%s.csv",
      gsub("[^A-Za-z0-9]+", "-", rv$site %||% "site"), format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      brd <- rv$board; sb <- site_birds(rv$obs, rv$points, rv$nvis, rv$opportunity,
                                       rv$visits, rv$observer_support)
      if (is.null(brd) || is.null(sb)) { utils::write.csv(data.frame(note = "No site loaded"), file, row.names = FALSE); return() }
      out <- bird_report_export(brd, rv$site, NEON_RELEASE, sb, rv$visits, rv$opportunity)
      utils::write.csv(out, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv")

  pick_species <- function(sci, navigate=FALSE){ if (is.null(sci)||is.na(sci)||sci=="") return()
    if (is.null(rv$board) || !(sci %in% rv$board$scientificName)) return()
    rv$sp <- sci; if (!identical(input$spSel, sci)) updateSelectizeInput(session, "spSel", selected=sci); if (navigate) nav_select("tabs","species") }
  observeEvent(input$spSel, if (nzchar(input$spSel %||% "")) pick_species(input$spSel, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_species(input$qcCardRequest, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$surpriseBtn, { req(rv$board, nrow(rv$board) > 0); pick_species(sample(rv$board$scientificName, 1), navigate=TRUE) })
  observeEvent(input$goCommunity, nav_select("tabs","community")); observeEvent(input$goBoard, nav_select("tabs","board"))
  observeEvent(input$goSpecies, { if (is.null(rv$sp) && !is.null(rv$board) && nrow(rv$board)) rv$sp <- rv$board$scientificName[1]; nav_select("tabs","species") })
  observeEvent(input$goMap, nav_select("tabs","map"))
  observeEvent(input$goClimate, nav_select("tabs","climate"))

  # ---- Search the network -------------------------------------------------
  # Filters the bundled SEARCH_INDEX in memory (no fetch). The "Go to site"
  # button in each row reuses the shared pickSite path: it loads that site from
  # its bundle (instant) and lands the user on the Overview, identical to a map
  # dot or browse-list pick.
  go_btn <- function(code, name) sprintf(
    "<button class='smt-clear-btn search-go' onclick=\"smtLoadStart('%s · loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});\">Go to this site &rarr;</button>",
    gsub("'", "", name), code)
  search_dt <- function(df) DT::datatable(df, rownames = FALSE, escape = FALSE,
    selection = "none", class = "compact stripe hover",
    options = list(pageLength = 12, dom = "tip", order = list(),
                   columnDefs = list(list(orderable = FALSE, targets = ncol(df) - 1))))

  # autocomplete: populate server-side from the index (independent of loaded site)
  updateSelectizeInput(session, "searchSp",
    choices = c("Pick a species…" = "", SEARCH_SPECIES_CHOICES), selected = "", server = TRUE)

  # (a) FIND A SPECIES — every site where it was detected
  sp_hits <- reactive({
    if (is.null(SEARCH_TAXA)) return(NULL)
    sci <- input$searchSp %||% ""; if (!nzchar(sci)) return(NULL)
    h <- SEARCH_TAXA[SEARCH_TAXA$scientificName == sci, , drop = FALSE]
    if (!nrow(h)) return(h)
    h[order(-h$detection_index_window), , drop = FALSE]
  })
  output$searchSpCaption <- renderUI({
    if (is.null(SEARCH_TAXA)) return(div(class = "search-empty", "Search index not available."))
    sci <- input$searchSp %||% ""
    if (!nzchar(sci)) return(div(class = "search-empty", bs_icon("arrow-up"), " Pick a species to see where it turns up across the 47 sites."))
    h <- sp_hits()
    if (is.null(h) || !nrow(h)) return(div(class = "search-empty", bs_icon("emoji-frown"), " Not detected at any bundled site."))
    vn <- h$vernacular[1] %||% sci
    tagList(
      div(class = "search-count", sprintf("%d of %d sites", nrow(h), nrow(SEARCH_SITES %||% h))),
    div(class = "search-note", bs_icon("info-circle"),
        sprintf(" %s (%s), using only valid physical counts from 2017–2024. The index is eligible in-window non-flyover birds per valid count. It is not abundance, density, occupancy, breeding status, or a population estimate.", vn, sci)))
  })
  output$searchSpTable <- DT::renderDT({
    h <- sp_hits(); req(!is.null(h)); if (!nrow(h)) return(NULL)
    yrs <- ifelse(is.finite(h$year_min) & is.finite(h$year_max),
                  ifelse(h$year_min == h$year_max, as.character(h$year_min),
                         paste0(h$year_min, "–", h$year_max)), "—")
    df <- data.frame(Site = h$site, Name = h$name %||% h$site, State = h$state %||% "",
                     `Detection index · 2017–2024` = h$detection_index_window,
                     `Valid counts detected` = h$n_detected_counts_window,
                     `Detection rows` = h$detection_rows_window, Years = yrs,
                     ` ` = mapply(go_btn, h$site, h$name %||% h$site),
                     check.names = FALSE, stringsAsFactors = FALSE)
    search_dt(df)
  })

  # (b) THRESHOLD QUERY
  thresh_hits <- reactive({
    kind <- input$threshKind %||% "wide"
    if (kind == "wide") {
      if (is.null(SEARCH_TAXA)) return(NULL)
      n <- input$threshN %||% 20
      tab <- SEARCH_TAXA[!duplicated(SEARCH_TAXA$scientificName),
                         c("scientificName", "vernacular", "n_sites_window"), drop = FALSE]
      tab <- tab[tab$n_sites_window > n, , drop = FALSE]
      tab[order(-tab$n_sites_window, tab$vernacular), , drop = FALSE]
    } else {
      if (is.null(SEARCH_SITES)) return(NULL)
      x <- input$threshX %||% 100
      s <- SEARCH_SITES[is.finite(SEARCH_SITES$S_rare) & SEARCH_SITES$S_rare > x, , drop = FALSE]
      s[order(-s$S_rare, -s$S_obs, s$site), , drop = FALSE]
    }
  })
  output$searchThreshCaption <- renderUI({
    kind <- input$threshKind %||% "wide"; h <- thresh_hits()
    if (kind == "wide") {
      total <- if (!is.null(SEARCH_TAXA)) length(unique(SEARCH_TAXA$scientificName)) else 0L
      if (is.null(h) || !nrow(h)) return(div(class = "search-empty", bs_icon("emoji-frown"), " No species pass that threshold. Lower the site count."))
      tagList(div(class = "search-count", sprintf("%d of %d species", nrow(h), total)),
        div(class = "search-note", bs_icon("info-circle"),
          sprintf(" Species detected at more than %d of the 47 sites during valid physical counts in 2017–2024. Flyovers are excluded; a missing detection is not an absence-confirmed site.", input$threshN %||% 20)))
    } else {
      total <- if (!is.null(SEARCH_SITES)) nrow(SEARCH_SITES) else 0L
      if (is.null(h) || !nrow(h)) return(div(class = "search-empty", bs_icon("emoji-frown"), " No sites pass that threshold. Lower the richness cutoff."))
      tagList(div(class = "search-count", sprintf("%d of %d sites", nrow(h), total)),
        div(class = "search-note", bs_icon("info-circle"),
          sprintf(" Sites with 2017–2024 richness rarefied above %d species at the common support of %s valid counts. Rarefaction standardizes count-sample size only; completeness and detectability can still differ.", input$threshX %||% 100, if (nrow(h)) h$t_used[[1]] else "—")))
    }
  })
  output$searchThreshTable <- DT::renderDT({
    kind <- input$threshKind %||% "wide"; h <- thresh_hits(); req(!is.null(h)); if (!nrow(h)) return(NULL)
    if (kind == "wide") {
      df <- data.frame(Species = h$vernacular, `Scientific name` = h$scientificName,
                       `Sites · 2017–2024` = h$n_sites_window,
                       check.names = FALSE, stringsAsFactors = FALSE)
      return(DT::datatable(df, rownames = FALSE, selection = "none",
        class = "compact stripe hover", options = list(pageLength = 12, dom = "tip")))
    }
    df <- data.frame(Site = h$site, Name = h$name %||% h$site, State = h$state %||% "",
                     `Rarefied species` = h$S_rare, `Observed species` = h$S_obs,
                     `Valid counts` = h$T_counts, Points = h$n_points_window,
                     `Birds/count` = round(h$birds_per_count_window, 2),
                     Coverage = ifelse(is.finite(h$coverage), paste0(round(100 * h$coverage), "%"), "—"),
                     Top = h$top_species_window,
                     ` ` = mapply(go_btn, h$site, h$name %||% h$site),
                     check.names = FALSE, stringsAsFactors = FALSE)
    search_dt(df)
  })

  # ---- hero ----
  output$heroStats <- renderUI({
    sb <- site_birds(rv$obs, rv$points, rv$nvis, rv$opportunity, rv$visits,
                     rv$observer_support); if (is.null(sb)) return(NULL)
    hero <- function(v,l,suf="",icon,tone,info=NULL) div(class=paste0("hero-stat hero-",tone),
      div(class="hs-icon", bs_icon(icon)),
      div(div(class="hs-v count-up", `data-target`=v, `data-suffix`=suf, "0"),
          div(class="hs-l", l, if (!is.null(info)) info)))
    div(class="hero-band",
      div(class="hero-title", bs_icon("broadcast"), tags$b(rv$label),
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " change site"), class = "hero-change"),
        downloadLink("reportCsv", tagList(bs_icon("file-earmark-arrow-down"), " report"), class = "hero-report")),
      div(class="hero-grid",
        hero(sb$n_species, "species", icon="feather", tone="navy",
          info=info_pop("Species", p("The number of different eligible bird species ", tags$b("detected"), " here across supported point counts. 'Detected' matters: a species can be present and still be missed, so observed richness is not a complete inventory."))),
        hero(sb$n_points, "count points", icon="geo", tone="pine",
          info=info_pop("Count points", p("The fixed spots where an observer stands and records every bird detected by sight or sound in a ", tags$b("6-minute count"), ". NEON returns to the same points each breeding season."))),
        hero(sb$birds_per_count, "birds / count", icon="soundwave", tone="gold",
          info=info_pop("Birds per count", p("Average birds tallied in one 6-minute count, a ", tags$b("detection index, not a population"), ". Loud, conspicuous species inflate it; quiet, skulking ones are undercounted, so it can't be compared between species as abundance."),
            p(tags$b("Flyovers are excluded operationally."), " A detection method containing ‘flyover’ means a bird was recorded passing overhead; those rows do not enter this on-point community detection index",
              if (!is.null(sb$flyover_birds) && sb$flyover_birds > 0) HTML(sprintf(", %s flyover bird%s retained in the audit here", fmt_int(sb$flyover_birds), if (sb$flyover_birds == 1) "" else "s")), "."))),
        hero(sb$n_visits, "point-counts run", icon="clipboard-check", tone="terra",
          info=info_pop("Point-counts run", p("The total number of ", tags$b("6-minute counts"), " performed here. A point counted twice in one year counts as two. This is the effort behind the ", tags$b("birds / count"), " average.")))))
  })

  # ---- Overview ----
  output$topBar <- renderPlotly({
    brd <- rv$board; req(!is.null(brd))
    if (!nrow(brd)) return(note_plot("Supported counts, zero eligible in-window non-flyover detections"))
    brd <- head(brd[order(-brd$index),], 18)
    brd$lab <- factor(brd$vernacular, levels = rev(brd$vernacular))
    # Colour each bar by its canonical first-detection method, but draw one trace
    # PER method so the chart carries a LEGEND (the bar colour is meaningless without
    # one). canon_method collapses compound NEON methods to a display channel.
    brd$cmeth <- canon_method(brd$method)
    meth_lab <- c(singing="singing", calling="calling", visual="visual", drumming="drumming",
                  flyover="flyover", other="other", unknown="unknown")
    ord_m <- intersect(c("singing","calling","visual","drumming","flyover","other","unknown"), unique(brd$cmeth))
    p <- plot_ly()
    for (mm in ord_m) { sub <- brd[brd$cmeth == mm, ]
      p <- p %>% add_trace(data=sub, x=~index, y=~lab, type="bar", orientation="h",
        name = unname(meth_lab[mm]) %||% mm, marker=list(color=method_col(mm)),
        text=~paste0(method), hovertemplate="%{y}<br>%{x:.2f} birds/count · %{text}<extra></extra>") }
    p %>% plotly_theme() %>% plotly::layout(barmode="stack", showlegend=TRUE,
        legend=list(orientation="h", x=0, y=-0.16, title=list(text="first detected by")),
        xaxis=list(title="Detection index (birds / point-count)"), yaxis=list(title=""), margin=list(l=170, t=34, b=58),
        annotations=list(list(text=sprintf("at <b>%s</b> · this site only", rv$site %||% "this site"), x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=if(is_dark())"#b3a692" else "#7a6f5d", size=11))))
  })
  output$overviewInsight <- renderUI({
    brd <- rv$board; req(!is.null(brd))
    if (!nrow(brd)) return(insight_banner("clipboard-check", tone="gold",
      HTML(sprintf("NEON completed <b>%s</b> valid point-counts across <b>%s</b> supported point-years here, with <b>zero eligible non-flyover species detections</b>. That is a supported zero record, not missing survey effort and not proof that birds were absent.", fmt_int(rv$nvis), fmt_int(rv$nopp)))))
    top <- brd[which.max(brd$index),]; freq <- brd[which.max(brd$detection_frequency),]
    insight_banner("soundwave", tone="navy", HTML(sprintf("<b>%s</b> has the highest detection index here (%.2f birds per count); <b>%s</b> appears on the largest share of valid six-minute counts (%.0f%%). The site has <span class='ci-hero'>%d</span> detected species.",
      top$vernacular %||% top$scientificName, top$index, freq$vernacular %||% freq$scientificName, freq$detection_frequency, nrow(brd))))
  })
  output$siteInsights <- renderUI({
    brd <- rv$board; req(!is.null(brd)); ch <- chao2_points(rv$obs, rv$visits)
    yrs <- range(rv$opportunity$year[rv$opportunity$supported %in% TRUE], na.rm=TRUE)
    yr_lab <- if (yrs[1]==yrs[2]) as.character(yrs[1]) else sprintf("%d–%d", yrs[1], yrs[2])
    if (!nrow(brd)) return(tags$ul(class="insight-list",
      tags$li(HTML(sprintf("Over <b>%s</b>, NEON ran <b>%s</b> valid six-minute counts across <b>%s</b> supported point-years.", yr_lab, fmt_int(rv$nvis), fmt_int(rv$nopp)))),
      tags$li(HTML("Those supported opportunities contained <b>zero eligible non-flyover species detections</b>. The scientific value is zero for this sampled record; unsurveyed opportunities remain unavailable rather than zero.")),
      tags$li(HTML("No detection index, observer-distance profile, or Chao2 point estimate is inferred from an empty detection record."))))
    top <- brd[which.max(brd$index),]; freq <- brd[which.max(brd$detection_frequency),]
    nm <- function(r) r$vernacular %||% r$scientificName
    eligible <- eligible_breeding_detections(rv$obs)
    sing_share <- round(100 * mean(eligible$method_singing %in% TRUE, na.rm=TRUE))
    pts <- c(
      sprintf("Over <b>%s</b>, NEON ran <b>%s</b> valid six-minute point-counts at <b>%d</b> points here. Those counts produced <b>%s</b> eligible in-window non-flyover birds across <b>%d</b> detected species.",
        yr_lab, fmt_int(rv$nvis), sum(rv$points$n_visits > 0), fmt_int(sum(brd$index_birds)), nrow(brd)),
      sprintf("The highest detection index belongs to the <b>%s</b> (<i>%s</i>), about <b>%.2f</b> birds per count; the <b>%s</b> was detected on <b>%.0f%%</b> of valid six-minute counts.",
        nm(top), top$scientificName, top$index, nm(freq), freq$detection_frequency))
    if (is.finite(sing_share)) pts <- c(pts, sprintf("<b>%d%%</b> of eligible detections included <i>singing</i> as a recorded detection channel; other records included calling or visual channels. This method mix does not establish territory or breeding status.", sing_share))
    if (!is.null(ch)) {
      cov <- site_coverage(rv$obs, rv$visits)
      if (ch$unstable && is.finite(cov))
        pts <- c(pts, sprintf("Observers detected <b>%d</b> species, and estimated incidence <b>coverage is %.0f%%</b> (the share of incidence probability represented by detected species). A Chao2 richness extrapolation is unstable here, so read the measured support and uncertainty rather than a single projected number.", ch$S_obs, round(100 * cov)))
      else
        pts <- c(pts, sprintf("Observers detected <b>%d</b> species; bias-corrected <b>Chao2</b> across %s valid six-minute counts estimates <b>%.0f</b>, with its uncertainty shown in Community. Counts are repeated protocol samples, not independent places, and can miss secretive, nocturnal, and rare birds.", ch$S_obs, fmt_int(ch$m), ch$chao2))
    }
    pts <- c(pts, "Remember: birds per count is a <b>detection index</b>, not a census. A loud species and a quiet one at equal density give unequal counts. Open a species profile for its descriptive distance signature and full support audit.")
    tags$ul(class="insight-list", lapply(pts, function(t) tags$li(HTML(t))))
  })

  # ---- Community ----
  output$accumPlot <- renderPlotly({
    ac <- bird_accum(rv$obs, rv$visits); if (is.null(ac)) return(note_plot("Not enough valid six-minute counts for an accumulation curve"))
    plot_ly(ac, x=~counts, y=~richness, type="scatter", mode="lines", line=list(color=DDL$rust, width=3),
      fill="tozeroy", fillcolor="rgba(193,80,46,0.08)",
      hovertemplate="%{x} valid counts<br>%{y:.0f} species<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Valid six-minute counts"), yaxis=list(title="Species detected"))
  })
  output$accumInsight <- renderUI({
    ac <- bird_accum(rv$obs, rv$visits); req(!is.null(ac))
    slope <- ac$richness[nrow(ac)] - ac$richness[max(1,nrow(ac)-5)]
    shape <- if (slope > 2) "The sampled curve was still rising over its final five count positions." else
      "The sampled curve flattened over its final five count positions."
    insight_banner("graph-up", tone="pine", HTML(sprintf("By <b>%d</b> valid six-minute counts, <span class='ci-hero'>%.0f</span> species had been detected. %s This describes only the sampled curve, not what future counts would find.",
      ac$counts[nrow(ac)], ac$richness[nrow(ac)], shape)))
  })
  output$chaoBanner <- renderUI({
    ch <- chao2_points(rv$obs, rv$visits); req(!is.null(ch))
    if (ch$S_obs == 0L) return(insight_banner("calculator", tone="gold", HTML(sprintf(
      "Across <b>%d valid six-minute counts</b>, no eligible non-flyover species were detected. Chao2 is <b>not estimable</b> from an all-zero incidence record; this is supported sampling evidence, not proof of ecological absence.", ch$m))))
    cov <- site_coverage(rv$obs, rv$visits)
    ci_clause <- if (is.finite(ch$ci_lo) && is.finite(ch$ci_hi))
      sprintf("95%% CI %.0f–%.0f species; asymmetric log-normal interval", ch$ci_lo, ch$ci_hi) else
      "interval unavailable (insufficient singleton signal)"
    # Chao2 estimate and, when singleton support exists, its analytic 95% CI.
    chao_pop <- info_pop("Chao2 estimate",
      p(tags$b("Bias-corrected Chao2"), " estimates ", tags$b(sprintf("%.0f", ch$chao2)), " species from valid-count incidence (", tags$b(ci_clause), "). This is an extrapolation, not a census of species at the site."),
      if (ch$unstable) p(class="pop-caveat", bsicons::bs_icon("exclamation-triangle"),
        sprintf(" Only %d species were detected at exactly two occasions (Q2=%d), so the doubleton support is sparse. Read the interval, coverage, and support rather than the single point estimate.", ch$Q2, ch$Q2)))
    if (ch$unstable && is.finite(cov)) {
      # lead with the honest completeness story; the volatile point estimate hides behind the click
      insight_banner("calculator", tone="gold", HTML(sprintf("Detected <b>%d</b> species across %d valid six-minute counts. Estimated incidence <b>coverage is %.0f%%</b> (the share of incidence probability represented by detected species). The Chao2 extrapolation is unstable here (only %d species were detected on exactly two counts), so the estimate does not lead. ", ch$S_obs, ch$m, round(100 * cov), ch$Q2)), chao_pop)
    } else {
      insight_banner("calculator", tone="gold", HTML(sprintf("Detected <b>%d</b> species across %d valid six-minute counts. Bias-corrected <b>Chao2</b> estimates <span class='ci-hero'>%.0f</span> species from that incidence record (%s).",
        ch$S_obs, ch$m, ch$chao2, ci_clause)), chao_pop)
    }
  })

  # ---- Bird Board (flagship) ----
  output$birdBoard <- renderPlotly({
    brd <- rv$board; req(!is.null(brd))
    if (!nrow(brd)) return(note_plot("No eligible species to place on the Bird Board"))
    brd$reliable <- brd$detections >= 3
    brd$col <- method_col(brd$method); brd$col[!brd$reliable] <- "rgba(138,129,117,0.35)"   # faded ink, parchment-friendly
    brd$tip <- paste0("<span class='smt-pin-emoji'>\U0001F426</span> <b>", brd$vernacular %||% brd$scientificName, "</b><br/>",
      "<em>", brd$scientificName, "</em><br/>",
      "<span class='smt-pin-stats'>", brd$index, " birds/count · ", brd$detection_frequency, "% of valid counts<br/>",
      brd$detections, " detections · mostly ", brd$method %||% "—", "</span>",
      ifelse(brd$reliable, "", "<br/><span class='smt-pin-rar' style='color:#ffd9a7'>⚠ few detections</span>"),
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", brd$scientificName, "'>\U0001F985 Open species profile &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    qcol <- if (is_dark()) "#9a8f7c" else "#b3a892"; muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p <- plot_ly()
    # one trace per detection method so the legend reads
    for (m in unique(brd$method)) { sub <- brd[brd$method %in% m, ]
      # redundant non-colour channel: each method also gets a distinct marker SYMBOL
      # (●▲■◆) so the board is legible without relying on hue (CVD-safe). The symbol is
      # constant per method, so the plotly legend swatch shows symbol + colour paired.
      p <- p %>% add_trace(data=sub, x=~detection_frequency, y=~index, type="scatter", mode="markers",
        name=sprintf("%s %s", method_glyph(m), m %||% "—"),
        customdata=~tip, marker=list(color=sub$col, symbol=method_sym(m), size=11, opacity=0.82, line=list(color="#fff", width=0.5)),
        text=~paste0(vernacular %||% scientificName), hovertemplate="%{text}<br>%{x}% of valid counts · %{y:.2f}/count<extra></extra>") }
    mx <- stats::median(brd$detection_frequency); my <- stats::median(brd$index[brd$reliable])
    xr <- range(brd$detection_frequency); yr <- range(brd$index); px <- diff(xr)*0.02; py <- diff(yr)*0.02
    qlab <- function(x,y,t,xa,ya) list(text=t, x=x, y=y, xref="x", yref="y", showarrow=FALSE, xanchor=xa, yanchor=ya, font=list(color=qcol, size=10.5))
    ann <- list(list(text=sprintf("at <b>%s</b> · each dot is a species · valid-count detection frequency × detection index", rv$site %||% "this site"), x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11)),
      qlab(xr[2]-px, yr[2]-py, "FREQUENTLY DETECTED \U0001F3C6", "right", "top"),
      qlab(xr[1]+px, yr[2]-py, "HIGH INDEX · LOW DETECTION FREQUENCY", "left", "top"),
      qlab(xr[2]-px, yr[1]+py, "HIGH DETECTION FREQUENCY · LOW INDEX", "right", "bottom"),
      qlab(xr[1]+px, yr[1]+py, "SELDOM DETECTED", "left", "bottom"))
    if (!is.null(rv$sp)) { ir <- brd[brd$scientificName == rv$sp, ]
      if (nrow(ir)==1) p <- p %>% add_trace(x=ir$detection_frequency, y=ir$index, type="scatter", mode="markers", name="★ viewing", customdata=ir$tip, showlegend=TRUE,
        marker=list(symbol="diamond", size=18, color="#e8a317", line=list(color="#fff", width=1.6)), hovertemplate=paste0("viewing ", ir$vernacular %||% ir$scientificName, "<extra></extra>")) }
    p %>% plotly_theme() %>% plotly::layout(xaxis=list(title="Detection frequency (% of valid six-minute counts)"), yaxis=list(title="Detection index (birds / valid point-count)", rangemode="tozero"),
      shapes=list(list(type="line", xref="x", yref="paper", x0=mx, x1=mx, y0=0, y1=1, line=list(color=qcol, dash="dot", width=1)),
                  list(type="line", xref="paper", yref="y", x0=0, x1=1, y0=my, y1=my, line=list(color=qcol, dash="dot", width=1))),
      annotations=ann, hovermode="closest")
  })
  output$spCardSlot <- renderUI({
    if (!is.null(rv$board) && !nrow(rv$board)) return(div(class="qc-empty",
      div(class="qc-empty-icon", "\U0001F4CB"), h4("Supported counts, no eligible species detections"),
      p("There is no species card to open for this sampled record. This is a supported zero, not missing effort.")))
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F426"), h4("Tap a species to see its card"),
      p("Tap a dot above and choose “Open species profile”, or pick a species in the sidebar.")))
    r <- rv$board[rv$board$scientificName == rv$sp,]; if (!nrow(r)) return(NULL)
    div(class="lab-sel", span(class="ls-emoji","\U0001F985"),
      div(class="ls-body", div(class="ls-id", tags$b(r$vernacular %||% r$scientificName), sprintf(" · %.2f birds/count · %.0f%% of valid counts", r$index, r$detection_frequency)),
        div(class="ls-dom", em(r$scientificName))),
      actionButton("goSpFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full profile"), class="btn-outline-dark btn-sm"))
  })
  observeEvent(input$goSpFromCard, nav_select("tabs","species"))

  # ---- Species Profile (downloadable card) ----
  output$decayPlot <- renderPlotly({
    sci <- rv$sp; req(sci)
    dd <- distance_decay(rv$obs, sci, rv$opportunity, rv$visits, rv$nvis)
    if (is.null(dd)) return(note_plot("At least 8 usable distances and complete visit effort are required"))
    bar_col <- method_col((rv$board$method[rv$board$scientificName == sci])[1] %||% "other")  # match its Bird Board dot
    plot_ly(dd, x=~band, y=~relative_rate, type="bar", marker=list(color=bar_col),
            customdata=~n, hovertemplate="%{x} m<br>relative rate %{y:.4f} · %{customdata} detections<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(
        xaxis=list(title="Observer-estimated distance band (0–200 m)"),
        yaxis=list(title="Relative area- and effort-standardized detection rate"),
        margin=list(l=62,r=10,t=10,b=40))
  })
  # data-quality flags for the viewed species (recomputed per species; cheap)
  qc <- reactive({ req(rv$sp); bird_qc(rv$obs, rv$sp, rv$points) })
  qc_icon <- function(level) switch(level, high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill", "check-circle-fill")

  output$speciesProfile <- renderUI({
    if (!is.null(rv$board) && !nrow(rv$board)) return(div(class="qc-empty",
      div(class="qc-empty-icon", "\U0001F4CB"), h4("No eligible species profile for this sampled record"),
      p("Valid point-counts were completed, but no eligible non-flyover species were detected. Annual and community views retain that zero without inventing a species-level profile.")))
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F426"), h4("Pick a species to open its profile"),
      p("Use the Bird Board (tap a dot → “Open species profile”) or the sidebar picker.")))
    r <- rv$board[rv$board$scientificName == rv$sp,]; req(nrow(r)==1)
    my <- detection_by_year(rv$obs, rv$sp, rv$opportunity); mm <- method_mix(rv$obs, rv$sp)
    dist_rows <- eligible_breeding_detections(rv$obs)
    dist_rows <- dist_rows[dist_rows$communityScientificName == rv$sp, , drop = FALSE]
    dist_finite <- dist_rows$distance_state == "observed" & is.finite(dist_rows$observerDistance)
    dist_used <- sum(dist_finite & dist_rows$observerDistance >= 0 & dist_rows$observerDistance <= 200)
    dist_outside <- sum(dist_finite & (dist_rows$observerDistance < 0 | dist_rows$observerDistance > 200))
    dist_unavailable <- sum(!dist_finite)
    tile <- function(v,l) div(class="qc-tile", div(class="qc-tile-v", v), div(class="qc-tile-l", l))
    qf <- qc()$flags
    qc_block <- tagList(
      div(class="qc-section-h", bs_icon("clipboard-check"), " Data-quality review flags ",
        tags$span(class="qcf-sub","· verify, not errors")),
      if (length(qf)) tagList(
        div(class="qc-flags", lapply(qf, function(f) div(
          class = paste0("qc-flag qc-flag-", f$level, " qc-flag-click"), role = "button", tabindex = "0",
          onclick = sprintf("Shiny.setInputValue('birdQcInspect','%s',{priority:'event'})", f$key),
          bs_icon(qc_icon(f$level)),
          div(class="qcf-body",
            div(class="qcf-title", f$title, tags$span(class="qcf-n", f$n)),
            div(class="qcf-detail", f$detail)),
          tags$span(class="qcf-go", bs_icon("chevron-right"))))),
        div(class="qcf-hint", bs_icon("hand-index-thumb"), " tap a flag to list the exact detections behind it"))
      else div(class="qc-flag qc-flag-ok", bs_icon("check-circle-fill"),
        div(class="qcf-body", div(class="qcf-title","No data-quality flags for this species"),
          div(class="qcf-detail","Distances, flock sizes, names, and point effort all look consistent, nothing to verify."))))
    body <- div(id="qcCardNode", class="qc-card", `data-short`=gsub("[^A-Za-z]","",substr(r$vernacular %||% r$scientificName,1,20)),
      div(class="qc-head", span(class="qc-emoji","\U0001F985"),
        div(div(class="qc-id", r$vernacular %||% r$scientificName), div(class="qc-sci", em(r$scientificName))),
        div(class="qc-head-badges", glow_badge(paste0(r$detections, " detections"), DDL$sky),
            glow_badge(r$method, method_col(r$method)))),
      div(class="qc-tiles",
        tile(r$index, "birds/count"), tile(paste0(r$detection_frequency,"%"), "valid counts detected"),
        tile(r$n_points, "points"), tile(r$n_grids, "grids"),
        tile(dist_used, "finite distances used · 0–200 m"),
        tile(dist_outside, "finite distances outside 0–200 m"),
        tile(ifelse(r$observer_support_complete, r$n_observers, paste0(r$n_observers, "*")), "observers represented"),
        tile(paste0(r$distance_usable_pct, "%"), "usable distances")),
      if (!isTRUE(r$observer_support_complete))
        p(class="qc-cap-note", bs_icon("info-circle"),
          " * Observer support is incomplete for one or more valid visits; the count is a documented minimum."),
      div(class="qc-section-h", bs_icon("reception-4"), " Relative detection signature by distance"),
      plotlyOutput("decayPlot", height="150px"),
      p(class="qc-cap-note", bs_icon("info-circle"), sprintf(
        " The distance signature uses %s finite recorded distance%s within the fixed 0–200 m display window; %s finite distance%s fell outside that window and %s detection%s had unavailable distance. At least eight in-window distances are required to draw the bars.",
        fmt_int(dist_used), if (dist_used == 1) "" else "s",
        fmt_int(dist_outside), if (dist_outside == 1) "" else "s",
        fmt_int(dist_unavailable), if (dist_unavailable == 1) "" else "s")),
      div(class="qc-section-h", bs_icon("soundwave"), " Recorded detection methods"),
      if (!is.null(mm) && nrow(mm)) div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(tags$th("Raw method"), tags$th("Detections"), tags$th("Components"))),
        tags$tbody(lapply(seq_len(nrow(mm)), function(i) {
          components <- c("singing"[mm$method_singing[i]], "calling"[mm$method_calling[i]],
                          "visual"[mm$method_visual[i]], "drumming"[mm$method_drumming[i]])
          tags$tr(tags$td(ifelse(is.na(mm$detectionMethod[i]) || !nzchar(mm$detectionMethod[i]), "unknown", mm$detectionMethod[i])),
                  tags$td(mm$n[i]), tags$td(if (length(components)) paste(components, collapse=", ") else "other/unknown"))
        })))) else p(class="qc-cap-note", "No recorded method support."),
      div(class="qc-section-h", bs_icon("calendar3"), " Supported annual detections"),
      if (!is.null(my) && nrow(my)) div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(tags$th("Year"), tags$th("Birds"), tags$th("Birds/count"), tags$th("Valid counts"), tags$th("Supported point-years"))),
        tags$tbody(lapply(seq_len(nrow(my)), function(i) tags$tr(
          tags$td(my$year[i]), tags$td(my$birds[i]), tags$td(sprintf("%.2f", my$birds_per_count[i])),
          tags$td(my$n_valid_visits[i]), tags$td(my$n_supported_point_years[i])))))) else p(class="qc-cap-note","—"),
      qc_block,
      p(class="qc-cap-note", style="margin-top:8px", bs_icon("info-circle"),
        " Birds per count is a detection index, not a population. Distance bars divide detections by annulus area and all valid visits to show a relative observation signature. They are not density, a fitted detection function, or proof of a true detectability curve."))
    div(div(class="plot-profile-wrap", body), div(class="qc-toolbar",
      tags$button(class="smt-snap-btn", type="button", onclick="smtSaveQcCard()", bsicons::bs_icon("download"), " Save species card (PNG)"),
      downloadButton("spCsv", "Download detections (CSV)", class="smt-clear-btn"),
      if (length(qf)) downloadButton("qcReportCsv", "Download QC report (CSV)", class="smt-clear-btn"),
      downloadButton("codebookCsv", "Download column codebook (CSV)", class="smt-clear-btn")),
      uiOutput("birdQcInspector"))
  })

  # clickable QC inspector: lists the exact offending detections for the tapped flag
  output$birdQcInspector <- renderUI({
    key <- input$birdQcInspect; q <- qc(); req(!is.null(key), key %in% names(q$sets))
    st <- q$sets[[key]]; req(!is.null(st), nrow(st))
    f <- Filter(function(x) x$key == key, q$flags)[[1]]
    show <- intersect(c("vernacularName","plotID","pointkey","year","bout","pointCountMinute",
                        "point_count_minute_state","observerDistance","detectionMethod","clusterSize"), names(st))
    head_n <- min(nrow(st), 200L); sv <- st[seq_len(head_n), show, drop=FALSE]
    div(class="qc-inspector",
      div(class="qci-head", bs_icon(qc_icon(f$level)), tags$b(sprintf(" %s · %d detection%s", f$title, f$n, if (f$n==1) "" else "s")),
        downloadButton("qcSubsetCsv", "Download these", class="btn-outline-dark btn-sm qci-dl")),
      div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(lapply(show, tags$th))),
        tags$tbody(lapply(seq_len(nrow(sv)), function(i)
          tags$tr(lapply(show, function(cc) tags$td(format(sv[[cc]][i]))))) ))),
      if (nrow(st) > head_n) p(class="qc-cap-note", sprintf("Showing first %d of %d. Download for the full list.", head_n, nrow(st))))
  })
  output$qcSubsetCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_QC-%s_%s_%s.csv", input$birdQcInspect %||% "flag",
      gsub("[^A-Za-z]","",substr(rv$sp %||% "species",1,20)), format(Sys.Date(),"%Y%m%d")),
    content = function(file){ q <- qc(); st <- q$sets[[input$birdQcInspect]]; req(!is.null(st))
      utils::write.csv(st, file, row.names=FALSE, na="") }, contentType="text/csv")
  output$qcReportCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_QC-report_%s_%s.csv", gsub("[^A-Za-z]","",substr(rv$sp %||% "species",1,20)), format(Sys.Date(),"%Y%m%d")),
    content = function(file){ rep <- bird_qc_report(rv$obs, rv$sp, rv$points)
      if (is.null(rep)) rep <- data.frame(note="No data-quality flags for this species.")
      utils::write.csv(rep, file, row.names=FALSE, na="") }, contentType="text/csv")
  output$spCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_%s_%s.csv", gsub("[^A-Za-z]","",substr(rv$sp %||% "species",1,24)), format(Sys.Date(),"%Y%m%d")),
    content = function(file){ sci <- rv$sp; req(sci)
      d <- species_detection_export(rv$obs, rv$held, sci); req(!is.null(d), nrow(d))
      # Eligibility is computed once by bird_prepare_obs(). Never overwrite it here:
      # the predicate also requires a valid joined visit, species-level identity,
      # a formal minute 1-6, and a positive finite integer cluster in addition to the
      # flyover quarantine.
      utils::write.csv(d[, SPCSV_KEEP, drop=FALSE], file, row.names=FALSE, na="") },
    contentType="text/csv")
  # machine-readable column dictionary for every CSV export (FAIR codebook)
  output$codebookCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_codebook_%s.csv", format(Sys.Date(),"%Y%m%d")),
    content = function(file) utils::write.csv(bird_codebook(), file, row.names=FALSE, na=""),
    contentType="text/csv")

  # ---- Map (grids) ----
  output$map <- leaflet::renderLeaflet({
    obs <- rv$obs; pts <- rv$points; req(obs, pts)
    eligible <- eligible_breeding_detections(obs)
    grid <- eligible %>% dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(richness = dplyr::n_distinct(.data$communityScientificName), birds = sum(.data$clusterSize, na.rm=TRUE), .groups="drop")
    gv <- pts %>% dplyr::group_by(.data$plotID) %>% dplyr::summarise(lat=stats::median(.data$lat, na.rm=TRUE), lng=stats::median(.data$lng, na.rm=TRUE), visits=sum(.data$n_visits), .groups="drop")
    g <- dplyr::left_join(gv, grid, by="plotID")
    g$richness <- ifelse(is.na(g$richness) & g$visits > 0, 0L, g$richness)
    g$birds <- ifelse(is.na(g$birds) & g$visits > 0, 0, g$birds)
    g$per_visit <- ifelse(g$visits>0, round(g$birds/g$visits,1), NA_real_)
    g <- g[g$visits > 0, , drop=FALSE]
    metric <- input$mapMetric %||% "richness"; val <- g[[metric]]; val[is.na(val)] <- 0
    dom <- if (diff(range(val,na.rm=TRUE))>0) range(val,na.rm=TRUE) else c(val[1]-1,val[1]+1)
    # warm field-guide ramp (parchment -> goldfinch -> rust -> deep) = "more birds, warmer"
    pal <- leaflet::colorNumeric(c("#f3e9d2","#e8a317","#c1502e","#7a2e16"), domain=dom)
    rr <- range(g$richness, na.rm=TRUE); g$radius <- if (diff(rr)>0) 7 + 13*(g$richness-rr[1])/diff(rr) else 11
    leaflet::leaflet(g) %>% add_suite_basemap(input$view %||% "Esri.WorldTopoMap") %>%
      leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=~radius, fillColor=pal(val), color="#fff", weight=1, fillOpacity=0.85,
        layerId=~plotID,
        label=~lapply(sprintf("<b>%s</b><br>%d species · %s birds/count<br><span style='color:#c1502e'>\U0001F446 click for the bird list</span>", short_point(plotID), richness, ifelse(is.na(per_visit),"—",per_visit)), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal=pal, values=val, title=if (metric=="richness") "species" else "birds/count")
  })
  observeEvent(input$map_marker_click, { id <- input$map_marker_click$id; if (!is.null(id)) rv$grid <- id })
  output$gridPanel <- renderUI({
    if (is.null(rv$obs)) return(NULL)
    if (is.null(rv$grid)) return(div(class="grid-empty", bs_icon("hand-index-thumb"),
      span(" Tap a grid marker above to list every bird species detected there, then download it.")))
    gs <- grid_species(rv$obs, rv$grid)
    if (is.null(gs) || !nrow(gs)) return(div(class="grid-empty", bs_icon("info-circle"), span(sprintf(" No species records at grid %s.", short_point(rv$grid)))))
    rows <- lapply(seq_len(nrow(gs)), function(i) {
      lbl <- gs$vernacular[i]; if (is.na(lbl)) lbl <- gs$scientificName[i]
      m <- gs$method[i]; if (is.na(m)) m <- "—"
      tags$tr(
        tags$td(tags$b(lbl), tags$br(), tags$em(class="grid-sci", gs$scientificName[i])),
        tags$td(class="grid-num", gs$birds[i]), tags$td(class="grid-num", gs$detections[i]),
        tags$td(span(class="grid-method", style=sprintf("color:%s", method_col(m)), m)))
    })
    div(class="grid-card",
      div(class="grid-head",
        div(tags$b(sprintf("Grid %s", short_point(rv$grid))), span(class="grid-sub", sprintf(" · %d species detected here", nrow(gs)))),
        downloadButton("gridSpeciesCsv", "Download species list (CSV)", class="smt-clear-btn")),
      div(class="grid-scroll", tags$table(class="inspect-tbl grid-tbl",
        tags$thead(tags$tr(tags$th("Species"), tags$th(class="grid-num","Birds"), tags$th(class="grid-num","Detections"), tags$th("Mostly"))),
        tags$tbody(rows))))
  })
  output$gridSpeciesCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_%s_grid-%s_%s.csv", rv$site %||% "site", gsub("[^A-Za-z0-9]","",short_point(rv$grid %||% "grid")), format(Sys.Date(),"%Y%m%d")),
    content = function(file){ req(rv$grid); gs <- grid_species(rv$obs, rv$grid); req(!is.null(gs))
      out <- gs[, c("scientificName","vernacular","birds","detections","method")]
      names(out) <- c("scientificName","vernacularName","eligible_birds","detections","primary_method")
      utils::write.csv(out, file, row.names=FALSE, na="") },
    contentType="text/csv")

  # ---- Bird Board table export (the per-species board for THIS site) --------
  # The flagship Bird Board only saved a PNG; export the underlying frame too so a
  # reader can re-derive the dots: the detection index AND its parts (total/index/
  # flyover birds), ubiquity, effort (points/grids), and primary method. Columns
  # are BOARD_KEEP, which is also what bird_codebook() documents (no drift).
  output$boardCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_board_%s_%s.csv", rv$site %||% "site", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ brd <- rv$board; req(!is.null(brd), nrow(brd))
      out <- brd[order(-brd$index), intersect(BOARD_KEEP, names(brd)), drop=FALSE]
      utils::write.csv(out, file, row.names=FALSE, na="") },
    contentType="text/csv")

  # ---- Cross-site gradient table export (the full 47-row frame) -------------
  # The "Across the continent" scatter only saved a PNG; export every site's row so
  # the gradient is reproducible: climate (x), the community metrics (y options),
  # effort, biome, and the rarefaction target t_used. Columns = GRADIENT_KEEP, also
  # the codebook source. Temperature is exported in °C (storage unit), unit-free.
  output$gradientCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_cross-site-gradient_%s.csv", format(Sys.Date(),"%Y%m%d")),
    content = function(file){ g <- GRADIENT
      if (is.null(g) || !nrow(g)) g <- data.frame(note="Cross-site gradient unavailable (run scripts/build_cross_site.R).")
      else g <- g[order(g$breeding_temp_c, g$site, na.last = TRUE, method = "radix"),
                  intersect(GRADIENT_KEEP, names(g)), drop=FALSE]
      utils::write.csv(g, file, row.names=FALSE, na="") },
    contentType="text/csv")

  # ---- Splash: national site picker (the continental story, pre-site) -------
  output$nationalPicker <- leaflet::renderLeaflet({
    d <- SEARCH_SITES
    if (is.null(d) || !nrow(d)) return(leaflet::leaflet() %>% add_suite_basemap("CartoDB.Positron") %>% leaflet::setView(-96, 40, 3))
    meta <- neon_sites[match(d$site, neon_sites$site), , drop = FALSE]
    d$lat <- meta$lat; d$lng <- meta$lng
    d$biome <- biome_of(d$site); d$bcol <- biome_col(d$biome); d$blab <- unname(BIOME_LAB[d$biome])
    if (sum(is.finite(d$S_rare)) == nrow(d) && diff(range(d$S_rare)) > 0) {
      rr <- range(d$S_rare); d$rad <- 7 + 9 * (d$S_rare - rr[1]) / diff(rr)
    } else d$rad <- 10
    pop <- sprintf("<div style='font-family:system-ui,sans-serif;min-width:210px'><b>%s · %s</b><br><span style='color:#7a6f5d'>%s · %s · 2017–2024</span><br><b>%s</b> rarefied species · <b>%d</b> observed<br><b>%s</b> valid counts at <b>%s</b> points · <b>%.2f</b> birds/count<br><span style='color:#7a6f5d'>estimated coverage %s</span><br><a href='#' style='color:#c1502e;font-weight:700' onclick=\"smtLoadStart('%s · loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;\">\U0001F426 Explore this site &rarr;</a><br><a href='#' style='color:#2f7fb5;font-weight:600' onclick=\"Shiny.setInputValue('siteInfo','%s',{priority:'event'});return false;\">About this site</a></div>",
                   d$site, d$name, d$blab, d$state, d$S_rare, d$S_obs,
                   fmt_int(d$T_counts), fmt_int(d$n_points_window), d$birds_per_count_window,
                   ifelse(is.finite(d$coverage), paste0(round(100 * d$coverage), "%"), "unavailable"),
                   gsub("'", "", d$name), d$site, d$site)
    leaflet::leaflet(d) %>% add_suite_basemap("CartoDB.Positron") %>% leaflet::setView(-96, 41, 3) %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~rad, fillColor = ~bcol, color = "#fff", weight = 1, fillOpacity = 0.85,
        label = ~lapply(sprintf("<b>%s</b> · %s<br>%s · %s rarefied species · 2017–2024", site, name, blab, S_rare), htmltools::HTML), popup = pop) %>%
      leaflet::addLegend("bottomright", colors = unname(BIOME_COL), labels = unname(BIOME_LAB), title = "Biome", opacity = 0.9)
  })

  # ---- "About this site" instant info card (popup parity with the flagship) -
  # The picker popup offers two buttons: "Explore this site" (loads the record,
  # via input$pickSite -> the sync cascade) and "About this site" (this modal, an
  # instant info card, no bundle load). The modal's footer button reuses the same
  # pickSite channel so a load from here also keeps the sidebar in sync.
  site_info_modal <- function(code) {
    m <- neon_sites[neon_sites$site == code, ]
    row <- if (!is.null(SEARCH_SITES)) SEARCH_SITES[SEARCH_SITES$site == code, ] else NULL
    if (!nrow(m))
      return(modalDialog(title = "Site info", easyClose = TRUE, footer = modalButton("Close"),
                         p("No details are available for this site.")))
    coords <- if (!is.na(m$lat[1]) && !is.na(m$lng[1])) sprintf("%.3f, %.3f", m$lat[1], m$lng[1]) else "—"
    stat <- function(v, lab) div(class = "si-stat",
      div(class = "si-stat-n", if (is.null(v) || is.na(v)) "—" else format(v, big.mark = ",")),
      div(class = "si-stat-l", lab))
    modalDialog(
      title = HTML(sprintf("\U0001F426 %s <span class='si-code'>(%s)</span>", m$name[1], code)),
      easyClose = TRUE, size = "m",
      footer = tagList(
        modalButton("Close"),
        tags$button(type = "button", class = "btn btn-primary",
          onclick = sprintf("smtLoadStart('%s · loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});", gsub("'", "", m$name[1]), code),
          HTML("Explore this site &rarr;"))),
      div(class = "site-info",
        div(class = "si-sec",
          div(class = "si-h", "Where"),
          div(class = "si-row", m$state[1], HTML(sprintf(" · NEON %s", m$domain[1]))),
          if (!is.na(m$bio[1])) div(class = "si-row si-bio", m$bio[1]),
          div(class = "si-coords", "\U0001F4CD ", coords)),
        if (!is.null(row) && nrow(row))
          div(class = "si-sec",
            div(class = "si-h", "2017–2024 comparison record"),
            div(class = "si-stats",
              stat(row$S_rare[1], "rarefied species"),
              stat(row$n_points_window[1], "count points"),
              stat(row$T_counts[1], "valid counts")))))
  }
  observeEvent(input$siteInfo, showModal(site_info_modal(input$siteInfo)))

  # ---- Across the continent: cross-site climate gradient (flagship) ---------
  output$climateGradient <- renderPlotly({
    g <- GRADIENT; if (is.null(g) || !nrow(g)) return(note_plot("Climate gradient unavailable. Run scripts/build_cross_site.R", "\U0001F30D"))
    unit <- input$tempUnit %||% "F"
    xvar <- input$gradX %||% "temp"
    if (identical(xvar, "precip")) {
      precip_ok <- is.finite(g$precip_annual_mm)
      if ("n_complete_precip_years" %in% names(g))
        precip_ok <- precip_ok & is.finite(g$n_complete_precip_years) & g$n_complete_precip_years > 0
      g <- g[precip_ok, , drop = FALSE]
      if (!nrow(g)) return(note_plot("Precipitation unavailable / no complete year", "\U0001F327"))
      xcol <- "precip_annual_mm"; xlab <- "Mean annual precipitation (mm · complete years)"; xsuf <- " mm"
    }
    else { xcol <- "breeding_temp_c"; xlab <- sprintf("Breeding-season air temperature (%s · NEON record)", temp_unit_lab(unit)); xsuf <- temp_unit_lab(unit) }
    tcom <- if ("t_used" %in% names(g)) g$t_used[1] else NA
    metric <- input$gradMetric %||% "rarefied"
    yc <- switch(metric,
      rarefied = list(col = "S_rare",        lab = sprintf("Species richness (rarefied to %s valid counts)", ifelse(is.na(tcom), "equal count support", tcom))),
      observed = list(col = "S_obs",         lab = "Species richness (observed, count effort differs)"),
      hill1    = list(col = "hill_q1",       lab = "Unstandardized sample-incidence Hill q1"),
      ubiquity = list(col = "mean_ubiquity", lab = "Community mean ubiquity (% of points)"),
      singing  = list(col = "pct_singing",  lab = "Singing share (% of detections, habitat/detectability signature)"),
      index    = list(col = "birds_per_count_window", lab = "Birds per count (2017–2024 detection index)"),
      list(col = "S_rare", lab = "Species richness (rarefied)"))
    if (!yc$col %in% names(g)) yc <- list(col = "S_obs", lab = "Species richness (observed)")
    g$xx <- suppressWarnings(as.numeric(g[[xcol]])); g$yy <- suppressWarnings(as.numeric(g[[yc$col]]))
    if (identical(xvar, "temp")) g$xx <- temp_val(g$xx, unit)
    n_context_supported <- sum(is.finite(g$xx))
    g <- g[is.finite(g$xx) & is.finite(g$yy), ]; if (!nrow(g)) return(note_plot("No sites with this combination", "\U0001F30D"))
    g$precip_lab <- ifelse(is.finite(g$precip_annual_mm),
      paste0(round(g$precip_annual_mm), " mm/yr"), "precipitation unavailable / no complete year")
    if ("n_complete_precip_years" %in% names(g))
      g$precip_lab <- ifelse(is.finite(g$n_complete_precip_years) & g$n_complete_precip_years > 0,
        paste0(g$precip_lab, " · ", g$n_complete_precip_years, " complete year", ifelse(g$n_complete_precip_years == 1, "", "s")),
        "precipitation unavailable / no complete year")
    g$coverage_lab <- ifelse(is.finite(g$coverage), paste0(round(100 * g$coverage), "% estimated coverage"), "coverage unavailable")
    g$bird_year_lab <- ifelse(g$bird_year_min == g$bird_year_max, as.character(g$bird_year_min),
      paste0(g$bird_year_min, "–", g$bird_year_max))
    g$tip <- paste0("<span class='smt-pin-emoji'>\U0001F985</span> <b>", g$site, " · ", g$name, "</b><br/>",
      "<em>", g$biome_lab, " · ", g$state, "</em><br/>",
      "<span class='smt-pin-stats'>", temp_disp(g$breeding_temp_c, unit), " breeding · ",
      g$precip_lab, "<br/>",
      g$S_obs, " species detected · ", g$S_rare, " rarefied · ", g$coverage_lab, "<br/>",
      g$n_points_window, " points · ", g$T_counts, " valid counts · comparison window ",
      g$analysis_year_min, "–", g$analysis_year_max, " (bird support ", g$bird_year_lab, ")<br/>",
      "top: <em>", g$top_species_window, "</em></span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-action='site' data-tag='", g$site, "'>\U0001F426 Open this site &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    sref <- 2 * max(g$n_points_window, na.rm = TRUE) / (26^2)
    muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p <- plot_ly()
    for (bm in unique(g$biome)) { sub <- g[g$biome == bm, ]
      p <- p %>% add_trace(data = sub, x = ~xx, y = ~yy, type = "scatter", mode = "markers", name = unname(BIOME_LAB[bm]),
        customdata = ~tip, text = ~paste0(site, " · ", name),
        marker = list(color = sub$biome_col[1], symbol = unname(BIOME_SYM[bm]) %||% "circle",   # redundant shape channel for CVD
                      size = sub$n_points_window, sizemode = "area", sizeref = sref, sizemin = 5,
                      opacity = 0.82, line = list(color = "#fff", width = 0.6)),
        hovertemplate = paste0("%{text}<br>%{x:.1f}", xsuf, " · %{y:.1f}<extra></extra>")) }
    if (!is.null(rv$site)) { ir <- g[g$site == rv$site, ]
      if (nrow(ir) == 1) p <- p %>% add_trace(x = ir$xx, y = ir$yy, type = "scatter", mode = "markers", name = "★ viewing", customdata = ir$tip,
        marker = list(symbol = "diamond", size = 18, color = "#e8a317", line = list(color = "#fff", width = 1.6)),
        hovertemplate = paste0("viewing ", ir$site, "<extra></extra>")) }
    rho <- suppressWarnings(stats::cor(g$xx, g$yy, method = "spearman"))
    n_sites <- nrow(g)
    conf <- if (identical(metric, "observed"))
      "biome, latitude, valid-count effort, completeness, detectability, and spatiotemporal sampling" else
      "biome, latitude, residual completeness, detectability, and spatiotemporal sampling"
    # both caveats stacked at the TOP, so they never collide with the x-axis title
    # + legend at the bottom (the overlap fix).
    context_name <- if (identical(xvar, "precip"))
      "complete-year precipitation" else "complete realized-month temperature"
    nshown <- if (n_context_supported < 47)
      sprintf("<b>%d of 47 NEON sites have %s support</b>", n_context_supported, context_name) else
      sprintf("<b>47 of 47 NEON sites have %s support</b>", context_name)
    plotted <- if (nrow(g) < n_context_supported)
      sprintf(" · %d have the selected bird metric", nrow(g)) else ""
    rho_label <- if (is.finite(rho)) sprintf("%.2f, n = %d", rho, n_sites) else
      sprintf("unavailable, n = %d", n_sites)
    ann <- list(
      list(text = sprintf("%s%s · one dot per supported pair · 2017–2024 bird window · %s × %s · dot size = counted points", nshown, plotted, if (xvar == "precip") "precipitation" else "breeding-season temperature", tolower(yc$lab)),
           x = 0, y = 1.15, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", font = list(color = muted, size = 11)),
      list(text = sprintf("Descriptive Spearman ρ = %s · space-for-time, not one site warming · confounded by %s", rho_label, conf),
           x = 0, y = 1.075, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", font = list(color = muted, size = 10.5)))
    p %>% plotly_theme() %>% plotly::layout(xaxis = list(title = list(text = xlab, standoff = 10)),
      yaxis = list(title = yc$lab, rangemode = "tozero"),
      annotations = ann, hovermode = "closest", margin = list(l = 60, r = 30, t = 96, b = 52))
  })

  # ---- Within-site: breeding window against the seasonal climatology --------
  output$seasonStrip <- renderPlotly({
    req(rv$site); if (is.null(SITE_MONTH_CLIM)) return(note_plot("No environmental data bundled", "\U0001F326"))
    mc <- SITE_MONTH_CLIM[SITE_MONTH_CLIM$site == rv$site, , drop = FALSE]; if (!nrow(mc)) return(note_plot("No environmental data for this site", "\U0001F326"))
    mc <- mc[order(mc$mon), ]; cl <- if (!is.null(SITE_CLIMATE)) SITE_CLIMATE[SITE_CLIMATE$site == rv$site, , drop = FALSE] else NULL
    unit <- input$tempUnit %||% "F"; mc$temp_d <- temp_val(mc$temp_c, unit)
    thov <- if (identical(unit, "C")) "%{y:.1f} °C<extra></extra>" else "%{y:.0f} °F<extra></extra>"
    p <- plot_ly()
    if (any(!is.na(mc$greenup_pct)))
      p <- p %>% add_trace(x = ~mc$mon, y = ~mc$greenup_pct, type = "scatter", mode = "lines+markers", name = "Green-up %",
        line = list(color = "#1a7f37", width = 3), marker = list(color = "#1a7f37", size = 6), yaxis = "y",
        hovertemplate = "%{y:.0f}% leafing out<extra></extra>")
    p <- p %>% add_trace(x = ~mc$mon, y = ~mc$temp_d, type = "scatter", mode = "lines", name = paste0("Air temp ", temp_unit_lab(unit)),
        line = list(color = "#c1502e", width = 2, dash = "dot"), yaxis = "y2",
        hovertemplate = thov)
    count_months <- integer()
    if (!is.null(cl) && nrow(cl) && "count_months" %in% names(cl)) {
      month_tokens <- unlist(strsplit(as.character(cl$count_months[[1]]), ",", fixed = TRUE))
      count_months <- suppressWarnings(as.integer(trimws(month_tokens)))
      count_months <- sort(unique(count_months[is.finite(count_months) & count_months >= 1L & count_months <= 12L]))
    }
    shp <- lapply(count_months, function(mon) list(
      type = "rect", xref = "x", yref = "paper", x0 = mon - 0.5, x1 = mon + 0.5,
      y0 = 0, y1 = 1, fillcolor = "rgba(232,163,23,0.16)", line = list(width = 0), layer = "below"))
    muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p %>% plotly_theme() %>% plotly::layout(
      xaxis = list(title = "", tickvals = 1:12, ticktext = c("J","F","M","A","M","J","J","A","S","O","N","D"), range = c(0.5, 12.5)),
      yaxis = list(title = "Green-up %", rangemode = "tozero"),
      yaxis2 = list(title = paste0("Temp ", temp_unit_lab(unit)), overlaying = "y", side = "right", showgrid = FALSE),
      shapes = shp, margin = list(l = 52, r = 52, t = 44, b = 30),
      annotations = list(list(text = sprintf("at <b>%s</b> · individually shaded months = exact count-month set", rv$site), x = 0, y = 1.14, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "left", font = list(color = muted, size = 11))))
  })
  output$seasonInsight <- renderUI({
    req(rv$site); cl <- if (!is.null(SITE_CLIMATE)) SITE_CLIMATE[SITE_CLIMATE$site == rv$site, , drop = FALSE] else NULL
    if (is.null(cl) || !nrow(cl)) return(NULL)
    win <- if (is.null(cl$count_months_lab) || is.na(cl$count_months_lab)) "the breeding season" else cl$count_months_lab
    n_realized <- suppressWarnings(as.integer(cl$n_realized_months[[1]]))
    n_supported <- suppressWarnings(as.integer(cl$n_supported_realized_months[[1]]))
    temp_support <- if (is.finite(n_realized) && is.finite(n_supported) &&
                        n_supported < n_realized) sprintf(
      paste0(
        " Coverage-qualified temperature is available for <b>%d of %d</b> realized ",
        "months, so the aggregate breeding-season temperature is unavailable and ",
        "this site is omitted only from the temperature gradient; no value is imputed."
      ), n_supported, n_realized) else ""
    gp <- if (!is.na(cl$peak_greenup_pct)) sprintf(
      " The separate RELEASE-2026 plant-phenology climatology peaks near <b>%d%%</b> green-up in %s; this is neutral seasonal context only.",
      cl$peak_greenup_pct, cl$greenup_peak_lab) else ""
    insight_banner("calendar-range", tone="pine", HTML(sprintf(
      "At <b>%s</b>, the exact distinct calendar months containing valid 2017–2024 point counts are <b>%s</b>. Each realized month is shaded separately; the curves are contextual monthly climatologies, not a measured bird response or a continuous breeding-season band.%s%s",
      rv$site, win, temp_support, gp)))
  })

  output$aboutPanel <- renderUI({
    div(class="about-wrap",
      div(class="about-card", h4("\U0001F426 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Breeding landbird point counts"), " (", tags$code("DP1.10003.001"), "). At each point, an observer records every bird detected by sight or sound in a ", tags$b("6-minute count"), ", with the distance to each, once or twice each breeding season. Only records from minutes 1–6 enter the metrics; NEON minute 88 incidentals and missing or unknown minute values remain auditable but are excluded.")),
      div(class="about-card", h4(bs_icon("soundwave"), " Detection index, not population"),
        p("Raw point-count totals are ", tags$b("detection-confounded"), ": a loud, conspicuous species and a quiet, skulking one at the same true density produce different counts. So the abundance axis here is a ", tags$b("detection index"), " (birds per point-count), never a population."),
        p(tags$b("Detection frequency"), " is the % of valid physical six-minute counts where a species was detected. Supported zero counts stay in that denominator; invalid or unavailable visits do not. Repeated bouts are repeated protocol samples, not independent places, and this remains a naïve detection summary—not detection-corrected occupancy. The distance panel is truncated to 0–200 m and is a relative area- and effort-standardized observation signature, not density or a fitted detection function.")),
      div(class="about-card", h4(bs_icon("calculator"), " How many species?"),
        p(tags$b("Bias-corrected Chao2"), " extrapolates richness from incidence across the complete valid physical-count ledger. Two valid bouts at one point-year remain two samples, while duplicate detections of a species within one count collapse to one incidence. Supported zero counts remain explicit. The estimator does not make repeated counts independent places or estimate occupancy.")),
      div(class="about-card", h4(bs_icon("globe-americas"), " Across the continent (climate gradient)"),
        p("NEON runs this same protocol at ", tags$b("47 sites"), " from arctic tundra to Hawai'i and Caribbean forests. The ", tags$b("Across the continent"), " tab places sites with complete realized-month support by their ", tags$b("breeding-season temperature"), " against their bird community; unsupported site temperatures stay explicit and are never imputed."),
        p("The public comparison uses bird counts from ", tags$b("2017–2024 only"), ". Richness is ", tags$b("rarefied to a common number of valid six-minute counts"), " (incidence rarefaction; Colwell et al. 2012), which standardizes count-sample size only. Completeness, detectability, repeated-place structure, biome, latitude, and spatiotemporal sampling can still differ. It is a descriptive ", tags$b("space-for-time"), " comparison, not one site warming. Precipitation is shown only where at least one complete calendar year exists; missing values are never imputed."),
        p("The per-site ", tags$b("season"), " panel places the breeding-count window on the site's green-up and temperature year, context for ", tags$em("when"), " counts happen, not a bird-vs-environment driver model (counts run only once or twice a year). Environment data: air temperature ", tags$code("DP1.00002.001"), ", precipitation ", tags$code("DP1.00044.001"), ", plant phenology ", tags$code("DP1.10055.001"), ".")),
      div(class="about-card", h4(bs_icon("table"), " Data dictionary & downloads"),
        p("Every CSV download carries a documented column. The ", tags$b("site-wide detection audit"), " includes every privacy-safe detection row: eligible records, flyover-only and coarse-identification-only records, unsafe taxonomy, and all held minute/visit rows. Canonical biological-species units sit beside the exact reported taxonomy. The ", tags$b("codebook"), " defines every column and its missing-value meaning."),
        if (!is.null(rv$site))
          downloadButton("siteAuditCsv", "Download this site's full detection audit (CSV)",
                         class="smt-clear-btn"),
        downloadButton("codebookCsvGlobal", "Download the full column codebook (CSV)", class="smt-clear-btn")),
      div(class="about-card", h4(bs_icon("award"), " Data attribution & license"),
        p(class="caveat",
          "Built with data from the National Ecological Observatory Network (NEON), a U.S. National Science Foundation program operated by Battelle. NEON data are provided under a Creative Commons Attribution 4.0 International (CC BY 4.0) license (",
          tags$a(href="https://creativecommons.org/licenses/by/4.0/", target="_blank", "creativecommons.org/licenses/by/4.0"),
          "). This app aggregates and derives summary metrics from the raw NEON data products; the underlying measurements are unaltered. It is an independent, unofficial tool and is not endorsed by NEON, Battelle, or the NSF.")),
      div(class="about-card", h4(bs_icon("envelope"), " Desert Data Labs"),
        p(bs_icon("envelope"), " ", tags$a(href="mailto:desertdatalabs@gmail.com","desertdatalabs@gmail.com"), " · ",
          tags$a(href="https://data.neonscience.org/data-products/DP1.10003.001", target="_blank", "NEON data product"))))
  })
  # global codebook (mirror of output$codebookCsv, reachable from About without a species)
  output$codebookCsvGlobal <- downloadHandler(
    filename = function() sprintf("NEON-Birds_codebook_%s.csv", format(Sys.Date(),"%Y%m%d")),
    content = function(file) utils::write.csv(bird_codebook(), file, row.names=FALSE, na=""),
    contentType="text/csv")
  output$siteAuditCsv <- downloadHandler(
    filename = function() sprintf("NEON-Birds_detection-audit_%s_%s.csv",
      gsub("[^A-Za-z0-9]+", "-", rv$site %||% "site"), format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      req(rv$site, rv$obs)
      audit <- site_detection_audit_export(rv$obs, rv$held)
      req(!is.null(audit), nrow(audit))
      utils::write.csv(audit, file, row.names=FALSE, na="")
    },
    contentType="text/csv")
  observeEvent(input$help, showModal(modalDialog(easyClose=TRUE, title=tagList(bs_icon("question-circle"), " How it works"),
    tags$ul(
      tags$li(HTML("<b>Pick a site</b> — tap a dot on the map, or choose one by name in the panel below it.")),
      tags$li(HTML("Use <b>change site</b> in the site banner to come back here and pick a different one.")),
      tags$li(HTML("<b>Community</b> · observed richness, opportunity-complete accumulation, and a support-qualified Chao2 extrapolation.")),
      tags$li(HTML("<b>Bird Board</b> · every species by valid-count detection frequency × detection index; <b>tap one</b> to pin its card, then “Open species profile”.")),
      tags$li(HTML("<b>Species Profile</b> · relative distance signature, supported annual detections, QC, and downloads.")),
      tags$li(HTML("Counts are a <b>detection index</b>, not a population. Detectability differs by species."))),
    footer=modalButton("Got it"))))
}
