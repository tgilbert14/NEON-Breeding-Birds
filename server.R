# ===========================================================================
# NEON Breeding Bird Explorer — server.R
# ===========================================================================
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

  rv <- reactiveValues(obs=NULL, points=NULL, board=NULL, nvis=0, label=NULL, site=NULL, sp=NULL, ctx=NULL, is_demo=FALSE, grid=NULL)

  observe({ ch <- bird_state_choices(); updateSelectInput(session, "stateSel", choices = ch, selected = if ("MA" %in% ch) "MA" else NULL) })
  observeEvent(input$stateSel, updateSelectInput(session, "site", choices = bird_sites_in_state(input$stateSel)), ignoreInit = FALSE)
  output$siteBio <- renderUI({ req(input$site); b <- site_bio(input$site); if (is.null(b)) return(NULL); div(class="site-bio", bs_icon("info-circle-fill"), span(b)) })
  output$siteCards <- renderUI({
    if (is.null(SITE_INDEX) || !nrow(site_table)) return(NULL)
    div(class="site-cards", lapply(seq_len(nrow(site_table)), function(i){ r <- site_table[i,]
      tags$a(class="site-card", href="#",
        onclick=sprintf("smtLoadStart('%s — loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;", gsub("'","",r$name), r$site),
        div(class="sc-emoji","\U0001F985"),
        div(class="sc-body", div(class="sc-name", tags$b(r$site), sprintf(" · %s", r$name)),
          div(class="sc-meta", sprintf("%s · %s species · %s birds/count", r$state, r$n_species, r$birds_per_count)))) }))
  })
  shinyjs::hide("mainTabsWrap")

  ingest <- function(b, label, is_demo = FALSE) {
    if (is.null(b) || is.null(b$obs) || !nrow(b$obs)) { session$sendCustomMessage("loadDone", list()); showNotification("No bird data for that site.", type="warning"); return(invisible()) }
    rv$obs <- b$obs; rv$points <- b$points
    # effort denominator = total point-visits, from the STRUCTURAL effort table only.
    # Never fall back to n_distinct(obs$eventID): in obs, eventID is at plot×year
    # grain (~7), not point-visit grain (~646) — that fallback would inflate every
    # index ~90×. If meta and points both lack it, fail loudly rather than lie.
    rv$nvis <- b$meta$n_visits %||% (if (!is.null(b$points$n_visits)) sum(b$points$n_visits, na.rm = TRUE) else NA_integer_)
    rv$board <- species_board(b$obs, b$points, rv$nvis)
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$sp <- NULL; rv$grid <- NULL
    yrs <- range(b$obs$year, na.rm=TRUE); rv$ctx <- paste0(b$meta$site, " · ", if (yrs[1]==yrs[2]) yrs[1] else paste0(yrs[1],"–",yrs[2]))
    shinyjs::show("mainTabsWrap"); shinyjs::show("spPickerWrap"); shinyjs::hide("splash")
    ch <- setNames(rv$board$scientificName, sprintf("%s · %s", rv$board$vernacular %||% rv$board$scientificName, rv$board$scientificName))
    updateSelectizeInput(session, "spSel", choices = c("Pick a species…"="", ch), selected = "", server = TRUE)
    nav_select("tabs", "overview"); session$sendCustomMessage("countUp", list()); session$sendCustomMessage("loadDone", list())
    invisible(TRUE)
  }
  load_site <- function(site){ if (is.null(site)||site=="") { session$sendCustomMessage("loadDone", list()); return() }
    b <- load_site_bundle(site); if (is.null(b)) { session$sendCustomMessage("loadDone", list()); showNotification("That site isn't bundled in this demo.", type="error"); return() }
    row <- site_table[site_table$site==site,]; ingest(b, sprintf("%s · %s", site, if (nrow(row)) row$name else site)) }
  observeEvent(input$loadBtn, load_site(input$site)); observeEvent(input$pickSite, load_site(input$pickSite))
  observeEvent(input$demoBtn, ingest(load_demo(), DEMO_META$label, is_demo=TRUE)); observeEvent(input$demoBtn2, ingest(load_demo(), DEMO_META$label, is_demo=TRUE))

  pick_species <- function(sci, navigate=FALSE){ if (is.null(sci)||is.na(sci)||sci=="") return()
    if (is.null(rv$board) || !(sci %in% rv$board$scientificName)) return()
    rv$sp <- sci; if (!identical(input$spSel, sci)) updateSelectizeInput(session, "spSel", selected=sci); if (navigate) nav_select("tabs","species") }
  observeEvent(input$spSel, if (nzchar(input$spSel %||% "")) pick_species(input$spSel, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$qcCardRequest, if (nzchar(input$qcCardRequest %||% "")) pick_species(input$qcCardRequest, navigate=TRUE), ignoreInit=TRUE)
  observeEvent(input$surpriseBtn, { req(rv$board); pick_species(sample(rv$board$scientificName, 1), navigate=TRUE) })
  observeEvent(input$goCommunity, nav_select("tabs","community")); observeEvent(input$goBoard, nav_select("tabs","board"))
  observeEvent(input$goSpecies, { if (is.null(rv$sp) && !is.null(rv$board)) rv$sp <- rv$board$scientificName[1]; nav_select("tabs","species") })
  observeEvent(input$goMap, nav_select("tabs","map"))
  observeEvent(input$goClimate, nav_select("tabs","climate"))

  # ---- hero ----
  output$heroStats <- renderUI({
    sb <- site_birds(rv$obs, rv$points, rv$nvis); if (is.null(sb)) return(NULL)
    hero <- function(v,l,suf="",icon,tone,info=NULL) div(class=paste0("hero-stat hero-",tone),
      div(class="hs-icon", bs_icon(icon)),
      div(div(class="hs-v count-up", `data-target`=v, `data-suffix`=suf, "0"),
          div(class="hs-l", l, if (!is.null(info)) info)))
    div(class="hero-band", div(class="hero-title", bs_icon("broadcast"), tags$b(rv$label)),
      div(class="hero-grid",
        hero(sb$n_species, "species", icon="feather", tone="navy",
          info=info_pop("Species", p("The number of different bird species ", tags$b("detected"), " here across all years of counts. 'Detected' matters — shy, rare, or nocturnal birds can be present but missed, so the true total is higher (see the Chao2 estimate)."))),
        hero(sb$n_points, "count points", icon="geo", tone="pine",
          info=info_pop("Count points", p("The fixed spots where an observer stands and records every bird seen or heard in a ", tags$b("6-minute count"), ". NEON returns to the same points each breeding season."))),
        hero(sb$birds_per_count, "birds / count", icon="soundwave", tone="gold",
          info=info_pop("Birds per count", p("Average birds tallied in one 6-minute count — a ", tags$b("detection index, not a population"), ". Loud, conspicuous species inflate it; quiet, skulking ones are undercounted, so it can't be compared between species as abundance."))),
        hero(sb$n_visits, "point-counts run", icon="clipboard-check", tone="terra",
          info=info_pop("Point-counts run", p("The total number of ", tags$b("6-minute counts"), " performed here. A point counted twice in one year counts as two — this is the effort behind the ", tags$b("birds / count"), " average.")))))
  })

  # ---- Overview ----
  output$topBar <- renderPlotly({
    brd <- rv$board; req(brd); brd <- head(brd[order(-brd$index),], 18)
    brd$lab <- factor(brd$vernacular %||% brd$scientificName, levels = rev(brd$vernacular %||% brd$scientificName))
    plot_ly(brd, x=~index, y=~lab, type="bar", orientation="h", marker=list(color=method_col(brd$method)),
      text=~paste0(method), hovertemplate="%{y}<br>%{x:.2f} birds/count · %{text}<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(showlegend=FALSE, xaxis=list(title="Detection index (birds / point-count)"), yaxis=list(title=""), margin=list(l=170, t=34),
        annotations=list(list(text=sprintf("at <b>%s</b> · this site only", rv$site %||% "this site"), x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=if(is_dark())"#b3a692" else "#7a6f5d", size=11))))
  })
  output$overviewInsight <- renderUI({
    brd <- rv$board; req(brd); top <- brd[which.max(brd$index),]; ubi <- brd[which.max(brd$ubiquity),]
    insight_banner("soundwave", tone="navy", HTML(sprintf("<b>%s</b> is the most-detected bird here (%.2f per count); <b>%s</b> is the most widespread (heard at %.0f%% of points). The site holds <span class='ci-hero'>%d</span> species.",
      top$vernacular %||% top$scientificName, top$index, ubi$vernacular %||% ubi$scientificName, ubi$ubiquity, nrow(brd))))
  })
  output$siteInsights <- renderUI({
    brd <- rv$board; req(brd); ch <- chao2_points(rv$obs, rv$points)
    yrs <- range(rv$obs$year, na.rm=TRUE); yr_lab <- if (yrs[1]==yrs[2]) as.character(yrs[1]) else sprintf("%d–%d", yrs[1], yrs[2])
    top <- brd[which.max(brd$index),]; ubi <- brd[which.max(brd$ubiquity),]
    nm <- function(r) r$vernacular %||% r$scientificName
    sing_share <- round(100 * mean(species_level_only(rv$obs)$detectionMethod %in% "singing", na.rm=TRUE))
    pts <- c(
      sprintf("Over <b>%s</b>, NEON ran <b>%s</b> six-minute point-counts at <b>%d</b> points here and tallied <b>%s</b> birds of <b>%d</b> species.",
        yr_lab, fmt_int(rv$nvis), nrow(rv$points), fmt_int(sum(brd$total_birds)), nrow(brd)),
      sprintf("The most-detected bird is the <b>%s</b> (<i>%s</i>), about <b>%.2f</b> per count; the most <i>widespread</i> is the <b>%s</b>, heard at <b>%.0f%%</b> of points.",
        nm(top), top$scientificName, top$index, nm(ubi), ubi$ubiquity))
    if (is.finite(sing_share)) pts <- c(pts, sprintf("<b>%d%%</b> of detections were birds <i>singing</i> on territory — the rest were call notes or birds seen, the texture of a breeding-season morning.", sing_share))
    if (!is.null(ch)) pts <- c(pts, sprintf("Observers found <b>%d</b> species; <b>Chao2</b> (across %s survey occasions) estimates at least <b>%.0f</b> really use the site — point counts miss secretive, nocturnal, and rare birds.", ch$S_obs, fmt_int(ch$m), ch$chao2))
    pts <- c(pts, "Remember: these are a <b>detection index</b>, not a census — a loud species and a quiet one at equal density give unequal counts. Open any species' profile for its detectability-by-distance.")
    tags$ul(class="insight-list", lapply(pts, function(t) tags$li(HTML(t))))
  })

  # ---- Community ----
  output$accumPlot <- renderPlotly({
    ac <- bird_accum(rv$obs, rv$points); if (is.null(ac)) return(note_plot("Not enough survey occasions for an accumulation curve"))
    plot_ly(ac, x=~points, y=~richness, type="scatter", mode="lines", line=list(color=DDL$rust, width=3),
      fill="tozeroy", fillcolor="rgba(193,80,46,0.08)",
      hovertemplate="%{x} occasions<br>%{y:.0f} species<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Survey occasions (point × year)"), yaxis=list(title="Species found"))
  })
  output$accumInsight <- renderUI({
    ac <- bird_accum(rv$obs, rv$points); req(!is.null(ac))
    slope <- ac$richness[nrow(ac)] - ac$richness[max(1,nrow(ac)-5)]
    insight_banner("graph-up", tone="pine", HTML(sprintf("By <b>%d</b> survey occasions, <span class='ci-hero'>%.0f</span> species had turned up.%s",
      ac$points[nrow(ac)], ac$richness[nrow(ac)], if (slope > 2) " The curve is still rising — more counts would find more species." else " The curve is flattening — most detectable species have been found.")))
  })
  output$chaoBanner <- renderUI({
    ch <- chao2_points(rv$obs, rv$points); req(!is.null(ch))
    insight_banner("calculator", tone="gold", HTML(sprintf("Observed <b>%d</b> species across %d survey occasions (point × year). <b>Chao2</b> estimates <span class='ci-hero'>%.0f</span> use the site%s — roughly <b>%.0f</b> remain undetected by point counts.",
      ch$S_obs, ch$m, ch$chao2, if (ch$unstable) " (a rough floor)" else "", max(0, round(ch$chao2 - ch$S_obs)))))
  })

  # ---- Bird Board (flagship) ----
  output$birdBoard <- renderPlotly({
    brd <- rv$board; req(brd)
    brd$reliable <- brd$detections >= 3
    brd$col <- method_col(brd$method); brd$col[!brd$reliable] <- "rgba(138,129,117,0.35)"   # faded ink, parchment-friendly
    brd$tip <- paste0("<span class='smt-pin-emoji'>\U0001F426</span> <b>", brd$vernacular %||% brd$scientificName, "</b><br/>",
      "<em>", brd$scientificName, "</em><br/>",
      "<span class='smt-pin-stats'>", brd$index, " birds/count · ", brd$ubiquity, "% of points<br/>",
      brd$detections, " detections · mostly ", brd$method %||% "—", "</span>",
      ifelse(brd$reliable, "", "<br/><span class='smt-pin-rar' style='color:#ffd9a7'>⚠ few detections</span>"),
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", brd$scientificName, "'>\U0001F985 Open species profile &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    qcol <- if (is_dark()) "#9a8f7c" else "#b3a892"; muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p <- plot_ly()
    # one trace per detection method so the legend reads
    for (m in unique(brd$method)) { sub <- brd[brd$method %in% m, ]
      p <- p %>% add_trace(data=sub, x=~ubiquity, y=~index, type="scatter", mode="markers", name=m %||% "—",
        customdata=~tip, marker=list(color=sub$col, size=11, opacity=0.82, line=list(color="#fff", width=0.5)),
        text=~paste0(vernacular %||% scientificName), hovertemplate="%{text}<br>%{x}% of points · %{y:.2f}/count<extra></extra>") }
    mx <- stats::median(brd$ubiquity); my <- stats::median(brd$index[brd$reliable])
    xr <- range(brd$ubiquity); yr <- range(brd$index); px <- diff(xr)*0.02; py <- diff(yr)*0.02
    qlab <- function(x,y,t,xa,ya) list(text=t, x=x, y=y, xref="x", yref="y", showarrow=FALSE, xanchor=xa, yanchor=ya, font=list(color=qcol, size=10.5))
    ann <- list(list(text=sprintf("at <b>%s</b> (this site) · each dot is a species · ubiquity × detection index (not a population)", rv$site %||% "this site"), x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11)),
      qlab(xr[2]-px, yr[2]-py, "EVERYONE'S NEIGHBOUR \U0001F3C6", "right", "top"),
      qlab(xr[1]+px, yr[2]-py, "LOCAL SPECIALIST", "left", "top"),
      qlab(xr[2]-px, yr[1]+py, "THINLY EVERYWHERE", "right", "bottom"),
      qlab(xr[1]+px, yr[1]+py, "SELDOM SEEN", "left", "bottom"))
    if (!is.null(rv$sp)) { ir <- brd[brd$scientificName == rv$sp, ]
      if (nrow(ir)==1) p <- p %>% add_trace(x=ir$ubiquity, y=ir$index, type="scatter", mode="markers", name="★ viewing", customdata=ir$tip, showlegend=TRUE,
        marker=list(symbol="diamond", size=18, color="#e8a317", line=list(color="#fff", width=1.6)), hovertemplate=paste0("viewing ", ir$vernacular %||% ir$scientificName, "<extra></extra>")) }
    p %>% plotly_theme() %>% plotly::layout(xaxis=list(title="Ubiquity (% of points detected)"), yaxis=list(title="Detection index (birds / point-count)", rangemode="tozero"),
      shapes=list(list(type="line", xref="x", yref="paper", x0=mx, x1=mx, y0=0, y1=1, line=list(color=qcol, dash="dot", width=1)),
                  list(type="line", xref="paper", yref="y", x0=0, x1=1, y0=my, y1=my, line=list(color=qcol, dash="dot", width=1))),
      annotations=ann, hovermode="closest")
  })
  output$spCardSlot <- renderUI({
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F426"), h4("Tap a species to see its card"),
      p("Tap a dot above and choose “Open species profile”, or pick a species in the sidebar.")))
    r <- rv$board[rv$board$scientificName == rv$sp,]; if (!nrow(r)) return(NULL)
    div(class="lab-sel", span(class="ls-emoji","\U0001F985"),
      div(class="ls-body", div(class="ls-id", tags$b(r$vernacular %||% r$scientificName), sprintf(" — %.2f birds/count · %.0f%% of points", r$index, r$ubiquity)),
        div(class="ls-dom", em(r$scientificName))),
      actionButton("goSpFromCard", tagList(bs_icon("arrows-fullscreen"), " Open full profile"), class="btn-outline-dark btn-sm"))
  })
  observeEvent(input$goSpFromCard, nav_select("tabs","species"))

  # ---- Species Profile (downloadable card) ----
  output$decayPlot <- renderPlotly({
    sci <- rv$sp; req(sci); dd <- distance_decay(rv$obs, sci); if (is.null(dd)) return(note_plot("Too few distance-measured detections"))
    bar_col <- method_col((rv$board$method[rv$board$scientificName == sci])[1] %||% "other")  # match its Bird Board dot
    plot_ly(dd, x=~band, y=~density, type="bar", marker=list(color=bar_col),
            customdata=~n, hovertemplate="%{x} m<br>%{y} detections/ha · %{customdata} raw<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Distance from observer (m)"), yaxis=list(title="Detections / ha"), margin=list(l=46,r=10,t=10,b=40))
  })
  # data-quality flags for the viewed species (recomputed per species; cheap)
  qc <- reactive({ req(rv$sp); bird_qc(rv$obs, rv$sp, rv$points) })
  qc_icon <- function(level) switch(level, high = "exclamation-octagon-fill", warn = "exclamation-triangle-fill", info = "info-circle-fill", "check-circle-fill")

  output$speciesProfile <- renderUI({
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F426"), h4("Pick a species to open its profile"),
      p("Use the Bird Board (tap a dot → “Open species profile”) or the sidebar picker.")))
    r <- rv$board[rv$board$scientificName == rv$sp,]; req(nrow(r)==1)
    my <- detection_by_year(rv$obs, rv$sp); mm <- method_mix(rv$obs, rv$sp)
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
          div(class="qcf-detail","Distances, flock sizes, names, and point effort all look consistent — nothing to verify."))))
    body <- div(id="qcCardNode", class="qc-card", `data-short`=gsub("[^A-Za-z]","",substr(r$vernacular %||% r$scientificName,1,20)),
      div(class="qc-head", span(class="qc-emoji","\U0001F985"),
        div(div(class="qc-id", r$vernacular %||% r$scientificName), div(class="qc-sci", em(r$scientificName))),
        div(class="qc-head-badges", glow_badge(paste0(r$detections, " detections"), DDL$sky))),
      div(class="qc-tiles",
        tile(r$index, "birds/count"), tile(paste0(r$ubiquity,"%"), "of points"),
        tile(r$n_points, "points"), tile(r$n_grids, "grids"),
        tile(r$mean_cluster, "mean cluster"), tile(r$method %||% "—", "mostly")),
      div(class="qc-section-h", bs_icon("reception-4"), " Detectability by distance (area-corrected, detections/ha)"),
      plotlyOutput("decayPlot", height="150px"),
      div(class="qc-section-h", bs_icon("calendar3"), " Birds counted, by year"),
      if (!is.null(my) && nrow(my)) div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(tags$th("Year"), tags$th("Birds counted"))),
        tags$tbody(lapply(seq_len(nrow(my)), function(i) tags$tr(tags$td(my$year[i]), tags$td(my$birds[i])))))) else p(class="qc-cap-note","—"),
      qc_block,
      p(class="qc-cap-note", style="margin-top:8px", bs_icon("info-circle"),
        " Detections are a detection index, not a population. The bars are area-corrected (detections per hectare per distance ring) — far rings cover more ground, so a raw count would rise then fall on geometry alone; dividing by ring area recovers the true detectability decline."))
    div(div(class="plot-profile-wrap", body), div(class="qc-toolbar",
      tags$button(class="smt-snap-btn", type="button", onclick="smtSaveQcCard()", bsicons::bs_icon("download"), " Save species card (PNG)"),
      downloadButton("spCsv", "Download detections (CSV)", class="smt-clear-btn"),
      if (length(qf)) downloadButton("qcReportCsv", "Download QC report (CSV)", class="smt-clear-btn")),
      uiOutput("birdQcInspector"))
  })

  # clickable QC inspector: lists the exact offending detections for the tapped flag
  output$birdQcInspector <- renderUI({
    key <- input$birdQcInspect; q <- qc(); req(!is.null(key), key %in% names(q$sets))
    st <- q$sets[[key]]; req(!is.null(st), nrow(st))
    f <- Filter(function(x) x$key == key, q$flags)[[1]]
    show <- intersect(c("vernacularName","plotID","pointkey","year","bout","observerDistance","detectionMethod","clusterSize"), names(st))
    head_n <- min(nrow(st), 200L); sv <- st[seq_len(head_n), show, drop=FALSE]
    div(class="qc-inspector",
      div(class="qci-head", bs_icon(qc_icon(f$level)), tags$b(sprintf(" %s — %d detection%s", f$title, f$n, if (f$n==1) "" else "s")),
        downloadButton("qcSubsetCsv", "Download these", class="btn-outline-dark btn-sm qci-dl")),
      div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(lapply(show, tags$th))),
        tags$tbody(lapply(seq_len(nrow(sv)), function(i)
          tags$tr(lapply(show, function(cc) tags$td(format(sv[[cc]][i]))))) ))),
      if (nrow(st) > head_n) p(class="qc-cap-note", sprintf("Showing first %d of %d — download for the full list.", head_n, nrow(st))))
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
    content = function(file){ sci <- rv$sp; req(sci); d <- species_detail(rv$obs, sci); req(!is.null(d))
      utils::write.csv(d[, c("scientificName","vernacularName","pointkey","plotID","year","bout","observerDistance","detectionMethod","clusterSize")], file, row.names=FALSE, na="") },
    contentType="text/csv")

  # ---- Map (grids) ----
  output$map <- leaflet::renderLeaflet({
    obs <- rv$obs; pts <- rv$points; req(obs, pts)
    grid <- species_level_only(obs) %>% dplyr::group_by(.data$plotID) %>%
      dplyr::summarise(richness = dplyr::n_distinct(.data$scientificName), birds = sum(.data$clusterSize, na.rm=TRUE), .groups="drop")
    gv <- pts %>% dplyr::group_by(.data$plotID) %>% dplyr::summarise(lat=stats::median(.data$lat, na.rm=TRUE), lng=stats::median(.data$lng, na.rm=TRUE), visits=sum(.data$n_visits), .groups="drop")
    g <- dplyr::left_join(gv, grid, by="plotID"); g$richness <- ifelse(is.na(g$richness),0L,g$richness)
    g$per_visit <- ifelse(g$visits>0, round(g$birds/g$visits,1), NA_real_)
    metric <- input$mapMetric %||% "richness"; val <- g[[metric]]; val[is.na(val)] <- 0
    dom <- if (diff(range(val,na.rm=TRUE))>0) range(val,na.rm=TRUE) else c(val[1]-1,val[1]+1)
    # warm field-guide ramp (parchment -> goldfinch -> rust -> deep) = "more birds, warmer"
    pal <- leaflet::colorNumeric(c("#f3e9d2","#e8a317","#c1502e","#7a2e16"), domain=dom)
    rr <- range(g$richness, na.rm=TRUE); g$radius <- if (diff(rr)>0) 7 + 13*(g$richness-rr[1])/diff(rr) else 11
    leaflet::leaflet(g) %>% leaflet::addProviderTiles(input$view %||% "Esri.WorldTopoMap") %>%
      leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=~radius, fillColor=pal(val), color="#fff", weight=1, fillOpacity=0.85,
        layerId=~plotID,
        label=~lapply(sprintf("<b>%s</b><br>%d species · %s birds/count<br><span style='color:#c1502e'>\U0001F446 click for the bird list</span>", short_point(plotID), richness, ifelse(is.na(per_visit),"—",per_visit)), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal=pal, values=val, title=if (metric=="richness") "species" else "birds/count")
  })
  observeEvent(input$map_marker_click, { id <- input$map_marker_click$id; if (!is.null(id)) rv$grid <- id })
  output$gridPanel <- renderUI({
    if (is.null(rv$obs)) return(NULL)
    if (is.null(rv$grid)) return(div(class="grid-empty", bs_icon("hand-index-thumb"),
      span(" Tap a grid marker above to list every bird species detected there — then download it.")))
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
      names(out) <- c("scientificName","vernacularName","total_birds","detections","primary_method")
      utils::write.csv(out, file, row.names=FALSE, na="") },
    contentType="text/csv")

  # ---- Splash: national site picker (the continental story, pre-site) -------
  output$nationalPicker <- leaflet::renderLeaflet({
    d <- site_table; if (is.null(d) || !nrow(d)) return(leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>% leaflet::setView(-96, 40, 3))
    d$biome <- biome_of(d$site); d$bcol <- biome_col(d$biome); d$blab <- unname(BIOME_LAB[d$biome])
    rr <- range(d$n_species, na.rm = TRUE); d$rad <- 6 + 11 * (d$n_species - rr[1]) / max(1, diff(rr))
    pop <- sprintf("<div style='font-family:Rubik,sans-serif;min-width:170px'><b>%s · %s</b><br><span style='color:#7a6f5d'>%s · %s</span><br><b>%d</b> species · <b>%s</b> birds/count<br><a href='#' style='color:#c1502e;font-weight:700' onclick=\"smtLoadStart('%s — loading…');Shiny.setInputValue('pickSite','%s',{priority:'event'});return false;\">\U0001F426 Explore this site &rarr;</a></div>",
                   d$site, d$name, d$blab, d$state, d$n_species, d$birds_per_count, gsub("'", "", d$name), d$site)
    leaflet::leaflet(d) %>% leaflet::addProviderTiles("CartoDB.Positron") %>% leaflet::setView(-96, 41, 3) %>%
      leaflet::addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~rad, fillColor = ~bcol, color = "#fff", weight = 1, fillOpacity = 0.85,
        label = ~lapply(sprintf("<b>%s</b> · %s<br>%s · %d species", site, name, blab, n_species), htmltools::HTML), popup = pop) %>%
      leaflet::addLegend("bottomright", colors = unname(BIOME_COL), labels = unname(BIOME_LAB), title = "Biome", opacity = 0.9)
  })

  # ---- Across the continent: cross-site climate gradient (flagship) ---------
  output$climateGradient <- renderPlotly({
    g <- GRADIENT; if (is.null(g) || !nrow(g)) return(note_plot("Climate gradient unavailable — run scripts/build_cross_site.R", "\U0001F30D"))
    unit <- input$tempUnit %||% "F"
    xvar <- input$gradX %||% "temp"
    if (identical(xvar, "precip")) { g <- g[!is.na(g$precip_annual_mm), ]; xcol <- "precip_annual_mm"; xlab <- "Annual precipitation (mm · NEON record)"; xsuf <- " mm" }
    else { xcol <- "breeding_temp_c"; xlab <- sprintf("Breeding-season air temperature (%s · NEON record)", temp_unit_lab(unit)); xsuf <- temp_unit_lab(unit) }
    tcom <- if ("t_used" %in% names(g)) g$t_used[1] else NA
    metric <- input$gradMetric %||% "rarefied"
    yc <- switch(metric,
      rarefied = list(col = "S_rare",        lab = sprintf("Species richness (rarefied to %s counts)", ifelse(is.na(tcom), "equal", tcom))),
      observed = list(col = "n_species",     lab = "Species richness (observed — effort differs)"),
      hill1    = list(col = "hill_q1",       lab = "Common-species diversity (Hill q1)"),
      ubiquity = list(col = "mean_ubiquity", lab = "Community mean ubiquity (% of points)"),
      index    = list(col = "birds_per_count", lab = "Birds per count (detection index — biome-biased)"),
      list(col = "S_rare", lab = "Species richness (rarefied)"))
    if (!yc$col %in% names(g)) yc <- list(col = "n_species", lab = "Species richness (observed)")
    g$xx <- suppressWarnings(as.numeric(g[[xcol]])); g$yy <- suppressWarnings(as.numeric(g[[yc$col]]))
    if (identical(xvar, "temp")) g$xx <- temp_val(g$xx, unit)
    g <- g[is.finite(g$xx) & is.finite(g$yy), ]; if (!nrow(g)) return(note_plot("No sites with this combination", "\U0001F30D"))
    g$tip <- paste0("<span class='smt-pin-emoji'>\U0001F985</span> <b>", g$site, " · ", g$name, "</b><br/>",
      "<em>", g$biome_lab, " · ", g$state, "</em><br/>",
      "<span class='smt-pin-stats'>", temp_disp(g$breeding_temp_c, unit), " breeding · ",
      ifelse(is.na(g$precip_annual_mm), "no precip sensor", paste0(g$precip_annual_mm, " mm/yr")), "<br/>",
      g$n_species, " species seen",
      ifelse(is.na(g$S_rare), "", paste0(" · ", g$S_rare, " rarefied")), " · ", g$n_points, " points<br/>",
      "top: <em>", g$top_species, "</em></span>",
      "<br/><span class='smt-open' role='button' tabindex='0' data-action='site' data-tag='", g$site, "'>\U0001F426 Open this site &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    sref <- 2 * max(g$n_points, na.rm = TRUE) / (26^2)
    muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p <- plot_ly()
    for (bm in unique(g$biome)) { sub <- g[g$biome == bm, ]
      p <- p %>% add_trace(data = sub, x = ~xx, y = ~yy, type = "scatter", mode = "markers", name = unname(BIOME_LAB[bm]),
        customdata = ~tip, text = ~paste0(site, " · ", name),
        marker = list(color = sub$biome_col[1], size = sub$n_points, sizemode = "area", sizeref = sref, sizemin = 5,
                      opacity = 0.82, line = list(color = "#fff", width = 0.6)),
        hovertemplate = paste0("%{text}<br>%{x:.1f}", xsuf, " · %{y:.0f}<extra></extra>")) }
    if (!is.null(rv$site)) { ir <- g[g$site == rv$site, ]
      if (nrow(ir) == 1) p <- p %>% add_trace(x = ir$xx, y = ir$yy, type = "scatter", mode = "markers", name = "★ viewing", customdata = ir$tip,
        marker = list(symbol = "diamond", size = 18, color = "#e8a317", line = list(color = "#fff", width = 1.6)),
        hovertemplate = paste0("viewing ", ir$site, "<extra></extra>")) }
    rho <- suppressWarnings(stats::cor(g$xx, g$yy, method = "spearman"))
    conf <- if (identical(metric, "observed")) "biome, latitude &amp; survey effort (raw richness tracks effort — see the rarefied metric)" else "biome &amp; latitude"
    # both caveats stacked at the TOP, so they never collide with the x-axis title
    # + legend at the bottom (the overlap fix).
    nshown <- if (nrow(g) < 46) sprintf("<b>%d of 46 NEON sites</b>", nrow(g)) else "<b>each of 46 NEON sites</b>"
    ann <- list(
      list(text = sprintf("Every dot is %s · %s × %s · dot size = survey effort (points)", nshown, if (xvar == "precip") "precipitation" else "breeding-season temperature", tolower(yc$lab)),
           x = 0, y = 1.15, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left", font = list(color = muted, size = 11)),
      list(text = sprintf("Spearman ρ = %.2f · space-for-time (46 places, not one site warming) — correlational, confounded by %s", ifelse(is.na(rho), 0, rho), conf),
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
    shp <- list()
    if (!is.null(cl) && nrow(cl) && !is.na(cl$count_month_min))
      shp <- list(list(type = "rect", xref = "x", yref = "paper", x0 = cl$count_month_min - 0.5, x1 = cl$count_month_max + 0.5,
                       y0 = 0, y1 = 1, fillcolor = "rgba(232,163,23,0.16)", line = list(width = 0), layer = "below"))
    muted <- if (is_dark()) "#b3a692" else "#7a6f5d"
    p %>% plotly_theme() %>% plotly::layout(
      xaxis = list(title = "", tickvals = 1:12, ticktext = c("J","F","M","A","M","J","J","A","S","O","N","D"), range = c(0.5, 12.5)),
      yaxis = list(title = "Green-up %", rangemode = "tozero"),
      yaxis2 = list(title = paste0("Temp ", temp_unit_lab(unit)), overlaying = "y", side = "right", showgrid = FALSE),
      shapes = shp, margin = list(l = 52, r = 52, t = 44, b = 30),
      annotations = list(list(text = sprintf("at <b>%s</b> · shaded band = when the breeding counts run", rv$site), x = 0, y = 1.14, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "left", font = list(color = muted, size = 11))))
  })
  output$seasonInsight <- renderUI({
    req(rv$site); cl <- if (!is.null(SITE_CLIMATE)) SITE_CLIMATE[SITE_CLIMATE$site == rv$site, , drop = FALSE] else NULL
    if (is.null(cl) || !nrow(cl)) return(NULL)
    win <- if (is.null(cl$count_months_lab) || is.na(cl$count_months_lab)) "the breeding season" else cl$count_months_lab
    gp  <- if (!is.na(cl$peak_greenup_pct)) sprintf(" — when green-up peaks near <b>%d%%</b> (%s), the leaf-out flush that feeds nesting insectivores", cl$peak_greenup_pct, cl$greenup_peak_lab) else ""
    insight_banner("calendar-range", tone="pine", HTML(sprintf(
      "At <b>%s</b>, NEON runs its point counts in <b>%s</b>%s. The curves are the site's average green-up and temperature by month — context for <i>when</i> the counts happen, not a measured bird response (counts run once or twice a year, so there's no within-season bird trend to track).",
      rv$site, win, gp)))
  })

  output$aboutPanel <- renderUI({
    div(class="about-wrap",
      div(class="about-card", h4("\U0001F426 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Breeding landbird point counts"), " (", tags$code("DP1.10003.001"), "). At each point, an observer records every bird seen or heard in a ", tags$b("6-minute count"), ", with the distance to each — once or twice each breeding season.")),
      div(class="about-card", h4(bs_icon("soundwave"), " Detection index, not population"),
        p("Raw point-count totals are ", tags$b("detection-confounded"), ": a loud, conspicuous species and a quiet, skulking one at the same true density produce different counts. So the abundance axis here is a ", tags$b("detection index"), " (birds per point-count), never a population."),
        p("A ", tags$b("less count-biased"), " axis is ", tags$b("ubiquity"), " — the % of points where a species is ever detected. Presence is less count-biased than a raw total, but it is still effort- and detection-dependent: rarely-visited points detect fewer species, and naïve occupancy under-counts quiet, secretive birds (treat it as a ", tags$b("floor"), ", not detection-corrected occupancy). Each species' ", tags$b("area-corrected detectability-by-distance"), " is its detection signature.")),
      div(class="about-card", h4(bs_icon("calculator"), " How many species?"),
        p(tags$b("Chao2"), " (incidence-based) estimates how many species use the site beyond those observed — point counts systematically miss nocturnal, secretive, and rare birds. The sampling unit is a ", tags$b("point × year occasion"), ", so a point's yearly revisits aren't double-counted as separate places.")),
      div(class="about-card", h4(bs_icon("globe-americas"), " Across the continent (climate gradient)"),
        p("NEON runs this same protocol at ", tags$b("46 sites"), " from arctic tundra (Utqiaġvik, −2 °C) to Caribbean dry forest (Guánica, 26 °C). The ", tags$b("Across the continent"), " tab places each site by its ", tags$b("breeding-season temperature"), " against its bird community."),
        p("Because sites differ in effort (13–144 points), richness is ", tags$b("rarefied to a common number of point-counts"), " (incidence rarefaction; Colwell et al. 2012) — raw richness would just track effort. It is a ", tags$b("space-for-time"), " comparison: 46 different places observed at once, not one place warming — correlational, confounded by biome and latitude. Precipitation is shown only for the 19 sites with a NEON gauge, never imputed."),
        p("The per-site ", tags$b("season"), " panel places the breeding-count window on the site's green-up and temperature year — context for ", tags$em("when"), " counts happen, not a bird-vs-environment driver model (counts run only once or twice a year). Environment data: air temperature ", tags$code("DP1.00002.001"), ", precipitation ", tags$code("DP1.00044.001"), ", plant phenology ", tags$code("DP1.10055.001"), ".")),
      div(class="about-card", h4(bs_icon("envelope"), " Desert Data Labs"),
        p(bs_icon("envelope"), " ", tags$a(href="mailto:desertdatalabs@gmail.com","desertdatalabs@gmail.com"), " · ",
          tags$a(href="https://data.neonscience.org/data-products/DP1.10003.001", target="_blank", "NEON data product"))))
  })
  observeEvent(input$help, showModal(modalDialog(easyClose=TRUE, title=tagList(bs_icon("question-circle"), " How it works"),
    tags$ul(
      tags$li(HTML("Pick a <b>site</b> (or open the Harvard Forest demo).")),
      tags$li(HTML("<b>Community</b> — species richness + a Chao2 estimate of how many species use the site.")),
      tags$li(HTML("<b>Bird Board</b> — every species by ubiquity × detection index; <b>tap one</b> to pin its card, then “Open species profile”.")),
      tags$li(HTML("<b>Species Profile</b> — the detection-decay (how far it's detected), yearly counts, and downloads.")),
      tags$li(HTML("Counts are a <b>detection index</b>, not a population — detectability differs by species."))),
    footer=modalButton("Got it"))))
}
