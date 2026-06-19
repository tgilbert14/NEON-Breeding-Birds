# ===========================================================================
# NEON Breeding Bird Explorer — server.R
# ===========================================================================
server <- function(input, output, session) {
  is_dark <- function() identical(input$colorMode, "dark")
  plotly_theme <- function(p, legend = TRUE) {
    dark <- is_dark(); ink <- if (dark) "#e8eef2" else "#1f2a30"
    grid <- if (dark) "rgba(220,230,240,0.10)" else "rgba(31,42,48,0.08)"; zero <- if (dark) "rgba(220,230,240,0.22)" else "rgba(31,42,48,0.15)"
    lin <- if (dark) "#3a4759" else "#d6ddd4"; legc <- if (dark) "#c3cedd" else "#344049"
    p %>% plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = ink, family = "Rubik"),
      xaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      yaxis = list(gridcolor = grid, zerolinecolor = zero, linecolor = lin),
      legend = list(bgcolor = "rgba(0,0,0,0)", orientation = "h", y = -0.2, font = list(color = legc)),
      margin = list(l = 55, r = 30, t = 48, b = 44),
      hoverlabel = list(bgcolor = "rgba(12,35,75,0.96)", bordercolor = "#FFD200", font = list(color = "#fff", family = "Rubik", size = 13))) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }
  note_plot <- function(msg, icon = "\U0001F426") plotly::plot_ly(type="scatter", mode="markers") %>%
    plotly::layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)", xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
      annotations=list(list(text=paste0(icon,"<br>",msg), showarrow=FALSE, font=list(color=if(is_dark())"#9fb0c4" else "#6b7a85", size=15), align="center"))) %>%
    plotly::config(displayModeBar = FALSE)

  rv <- reactiveValues(obs=NULL, points=NULL, board=NULL, nvis=0, label=NULL, site=NULL, sp=NULL, ctx=NULL, is_demo=FALSE)

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
    rv$obs <- b$obs; rv$points <- b$points; rv$nvis <- b$meta$n_visits %||% length(unique(b$obs$eventID))
    rv$board <- species_board(b$obs, b$points, rv$nvis)
    rv$label <- label; rv$site <- b$meta$site; rv$is_demo <- is_demo; rv$sp <- NULL
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

  # ---- hero ----
  output$heroStats <- renderUI({
    sb <- site_birds(rv$obs, rv$points, rv$nvis); if (is.null(sb)) return(NULL)
    hero <- function(v,l,suf="",icon,tone,ttl=NULL) div(class=paste0("hero-stat hero-",tone), title=ttl,
      div(class="hs-icon", bs_icon(icon)), div(div(class="hs-v count-up", `data-target`=v, `data-suffix`=suf, "0"), div(class="hs-l", l)))
    div(class="hero-band", div(class="hero-title", bs_icon("broadcast"), tags$b(rv$label)),
      div(class="hero-grid",
        hero(sb$n_species, "species", icon="feather", tone="navy"),
        hero(sb$n_points, "count points", icon="geo", tone="pine"),
        hero(sb$birds_per_count, "birds / count", icon="soundwave", tone="gold", ttl="A detection index (birds per 6-min point-count), not a population estimate — detectability differs by species."),
        hero(sb$n_visits, "point-counts run", icon="clipboard-check", tone="terra")))
  })

  # ---- Overview ----
  output$topBar <- renderPlotly({
    brd <- rv$board; req(brd); brd <- head(brd[order(-brd$index),], 18)
    brd$lab <- factor(brd$vernacular %||% brd$scientificName, levels = rev(brd$vernacular %||% brd$scientificName))
    plot_ly(brd, x=~index, y=~lab, type="bar", orientation="h", marker=list(color=method_col(brd$method)),
      text=~paste0(method), hovertemplate="%{y}<br>%{x:.2f} birds/count · %{text}<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(showlegend=FALSE, xaxis=list(title="Detection index (birds / point-count)"), yaxis=list(title=""), margin=list(l=170))
  })
  output$overviewInsight <- renderUI({
    brd <- rv$board; req(brd); top <- brd[which.max(brd$index),]; ubi <- brd[which.max(brd$ubiquity),]
    insight_banner("soundwave", tone="navy", HTML(sprintf("<b>%s</b> is the most-detected bird here (%.2f per count); <b>%s</b> is the most widespread (heard at %.0f%% of points). The site holds <span class='ci-hero'>%d</span> species.",
      top$vernacular %||% top$scientificName, top$index, ubi$vernacular %||% ubi$scientificName, ubi$ubiquity, nrow(brd))))
  })
  output$siteInsights <- renderUI({
    brd <- rv$board; req(brd); ch <- chao2_points(rv$obs, rv$points)
    pts <- c(sprintf("Across <b>%d</b> point-counts at <b>%d</b> points, observers logged <b>%s</b> birds of <b>%d</b> species.",
      rv$nvis, nrow(rv$points), format(sum(brd$total_birds), big.mark=","), nrow(brd)))
    if (!is.null(ch)) pts <- c(pts, sprintf("Chao2 estimates at least <b>%.0f</b> species use the site — point counts miss secretive, nocturnal, and rare birds, so the true total is higher than the <b>%d</b> observed.", ch$chao2, ch$S_obs))
    pts <- c(pts, "Counts are a <b>detection index</b>, not a census: a loud species and a quiet one at equal density give unequal counts. Open a species' profile to see its detection-decay with distance.")
    div(class="insight-list", lapply(pts, function(t) div(class="il-item", bs_icon("dot"), HTML(t))))
  })

  # ---- Community ----
  output$accumPlot <- renderPlotly({
    ac <- bird_accum(rv$obs, rv$points); if (is.null(ac)) return(note_plot("Not enough points for an accumulation curve"))
    plot_ly(ac, x=~points, y=~richness, type="scatter", mode="lines", line=list(color=DDL$sky, width=3),
      hovertemplate="%{x} points<br>%{y:.0f} species<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Points counted"), yaxis=list(title="Species found"))
  })
  output$accumInsight <- renderUI({
    ac <- bird_accum(rv$obs, rv$points); req(!is.null(ac))
    slope <- ac$richness[nrow(ac)] - ac$richness[max(1,nrow(ac)-5)]
    insight_banner("graph-up", tone="pine", HTML(sprintf("By <b>%d</b> points, <span class='ci-hero'>%.0f</span> species had turned up.%s",
      ac$points[nrow(ac)], ac$richness[nrow(ac)], if (slope > 2) " The curve is still rising — more points would find more species." else " The curve is flattening — most detectable species have been found.")))
  })
  output$chaoBanner <- renderUI({
    ch <- chao2_points(rv$obs, rv$points); req(!is.null(ch))
    insight_banner("calculator", tone="gold", HTML(sprintf("Observed <b>%d</b> species across %d points. <b>Chao2</b> estimates <span class='ci-hero'>%.0f</span> use the site%s — roughly <b>%.0f</b> remain undetected by point counts.",
      ch$S_obs, ch$m, ch$chao2, if (ch$unstable) " (a rough floor)" else "", max(0, round(ch$chao2 - ch$S_obs)))))
  })

  # ---- Bird Board (flagship) ----
  output$birdBoard <- renderPlotly({
    brd <- rv$board; req(brd)
    brd$reliable <- brd$detections >= 3
    brd$col <- method_col(brd$method); brd$col[!brd$reliable] <- "rgba(150,160,170,0.35)"
    brd$tip <- paste0("<span class='smt-pin-emoji'>\U0001F426</span> <b>", brd$vernacular %||% brd$scientificName, "</b><br/>",
      "<em>", brd$scientificName, "</em><br/>",
      "<span class='smt-pin-stats'>", brd$index, " birds/count · ", brd$ubiquity, "% of points<br/>",
      brd$detections, " detections · mostly ", brd$method %||% "—", "</span>",
      ifelse(brd$reliable, "", "<br/><span class='smt-pin-rar' style='color:#ffd9a7'>⚠ few detections</span>"),
      "<br/><span class='smt-open' role='button' tabindex='0' data-tag='", brd$scientificName, "'>\U0001F985 Open species profile &rarr;</span>",
      "<br/><em class='smt-pin-hint'>Tap the dot to pin this card</em>")
    qcol <- if (is_dark()) "#7e8da0" else "#9aa6b2"; muted <- if (is_dark()) "#9fb0c4" else "#6b7a85"
    p <- plot_ly()
    # one trace per detection method so the legend reads
    for (m in unique(brd$method)) { sub <- brd[brd$method %in% m, ]
      p <- p %>% add_trace(data=sub, x=~ubiquity, y=~index, type="scatter", mode="markers", name=m %||% "—",
        customdata=~tip, marker=list(color=sub$col, size=11, opacity=0.82, line=list(color="#fff", width=0.5)),
        text=~paste0(vernacular %||% scientificName), hovertemplate="%{text}<br>%{x}% of points · %{y:.2f}/count<extra></extra>") }
    mx <- stats::median(brd$ubiquity); my <- stats::median(brd$index[brd$reliable])
    xr <- range(brd$ubiquity); yr <- range(brd$index); px <- diff(xr)*0.02; py <- diff(yr)*0.02
    qlab <- function(x,y,t,xa,ya) list(text=t, x=x, y=y, xref="x", yref="y", showarrow=FALSE, xanchor=xa, yanchor=ya, font=list(color=qcol, size=10.5))
    ann <- list(list(text="each dot is a species · ubiquity × detection index (not a population)", x=0, y=1.07, xref="paper", yref="paper", showarrow=FALSE, xanchor="left", font=list(color=muted, size=11)),
      qlab(xr[2]-px, yr[2]-py, "EVERYONE'S NEIGHBOUR \U0001F3C6", "right", "top"),
      qlab(xr[1]+px, yr[2]-py, "LOCAL SPECIALIST", "left", "top"),
      qlab(xr[2]-px, yr[1]+py, "THINLY EVERYWHERE", "right", "bottom"),
      qlab(xr[1]+px, yr[1]+py, "SELDOM SEEN", "left", "bottom"))
    if (!is.null(rv$sp)) { ir <- brd[brd$scientificName == rv$sp, ]
      if (nrow(ir)==1) p <- p %>% add_trace(x=ir$ubiquity, y=ir$index, type="scatter", mode="markers", name="★ viewing", customdata=ir$tip, showlegend=TRUE,
        marker=list(symbol="diamond", size=18, color="#c9a300", line=list(color="#fff", width=1.6)), hovertemplate=paste0("viewing ", ir$vernacular %||% ir$scientificName, "<extra></extra>")) }
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
    plot_ly(dd, x=~band, y=~n, type="bar", marker=list(color=DDL$sky), hovertemplate="%{x} m<br>%{y} detections<extra></extra>") %>%
      plotly_theme(legend=FALSE) %>% plotly::layout(xaxis=list(title="Distance from observer (m)"), yaxis=list(title="Detections"), margin=list(l=45,r=10,t=10,b=40))
  })
  output$speciesProfile <- renderUI({
    if (is.null(rv$sp)) return(div(class="qc-empty", div(class="qc-empty-icon","\U0001F426"), h4("Pick a species to open its profile"),
      p("Use the Bird Board (tap a dot → “Open species profile”) or the sidebar picker.")))
    r <- rv$board[rv$board$scientificName == rv$sp,]; req(nrow(r)==1)
    my <- detection_by_year(rv$obs, rv$sp); mm <- method_mix(rv$obs, rv$sp)
    tile <- function(v,l) div(class="qc-tile", div(class="qc-tile-v", v), div(class="qc-tile-l", l))
    body <- div(id="qcCardNode", class="qc-card", `data-short`=gsub("[^A-Za-z]","",substr(r$vernacular %||% r$scientificName,1,20)),
      div(class="qc-head", span(class="qc-emoji","\U0001F985"),
        div(div(class="qc-id", r$vernacular %||% r$scientificName), div(class="qc-sci", em(r$scientificName))),
        div(class="qc-head-badges", glow_badge(paste0(r$detections, " detections"), DDL$sky))),
      div(class="qc-tiles",
        tile(r$index, "birds/count"), tile(paste0(r$ubiquity,"%"), "of points"),
        tile(r$n_points, "points"), tile(r$n_grids, "grids"),
        tile(r$mean_cluster, "mean cluster"), tile(r$method %||% "—", "mostly")),
      div(class="qc-section-h", bs_icon("reception-4"), " Detection-decay — how far it's detected"),
      plotlyOutput("decayPlot", height="150px"),
      div(class="qc-section-h", bs_icon("calendar3"), " Birds counted, by year"),
      if (!is.null(my) && nrow(my)) div(class="qc-cap-scroll", tags$table(class="inspect-tbl",
        tags$thead(tags$tr(tags$th("Year"), tags$th("Birds counted"))),
        tags$tbody(lapply(seq_len(nrow(my)), function(i) tags$tr(tags$td(my$year[i]), tags$td(my$birds[i])))))) else p(class="qc-cap-note","—"),
      p(class="qc-cap-note", style="margin-top:8px", bs_icon("info-circle"),
        " Detections are a detection index, not a population: distance-decay shows how detectability falls off with distance, which is why raw counts aren't density."))
    div(div(class="plot-profile-wrap", body), div(class="qc-toolbar",
      tags$button(class="smt-snap-btn", type="button", onclick="smtSaveQcCard()", bsicons::bs_icon("download"), " Save species card (PNG)"),
      downloadButton("spCsv", "Download detections (CSV)", class="smt-clear-btn")))
  })
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
    pal <- leaflet::colorNumeric("viridis", domain=dom)
    rr <- range(g$richness, na.rm=TRUE); g$radius <- if (diff(rr)>0) 7 + 13*(g$richness-rr[1])/diff(rr) else 11
    leaflet::leaflet(g) %>% leaflet::addProviderTiles(input$view %||% "Esri.WorldImagery") %>%
      leaflet::addCircleMarkers(lng=~lng, lat=~lat, radius=~radius, fillColor=pal(val), color="#fff", weight=1, fillOpacity=0.85,
        label=~lapply(sprintf("<b>%s</b><br>%d species · %s birds/count", short_point(plotID), richness, ifelse(is.na(per_visit),"—",per_visit)), htmltools::HTML)) %>%
      leaflet::addLegend("bottomright", pal=pal, values=val, title=if (metric=="richness") "species" else "birds/count")
  })

  output$aboutPanel <- renderUI({
    div(class="about-wrap",
      div(class="about-card", h4("\U0001F426 What this is"),
        p("An (unofficial) explorer for NEON's ", tags$b("Breeding landbird point counts"), " (", tags$code("DP1.10003.001"), "). At each point, an observer records every bird seen or heard in a ", tags$b("6-minute count"), ", with the distance to each — once or twice each breeding season.")),
      div(class="about-card", h4(bs_icon("soundwave"), " Detection index, not population"),
        p("Raw point-count totals are ", tags$b("detection-confounded"), ": a loud, conspicuous species and a quiet, skulking one at the same true density produce different counts. So the abundance axis here is a ", tags$b("detection index"), " (birds per point-count), never a population."),
        p("The most honest abundance axis is ", tags$b("ubiquity"), " — the % of points where a species is ever detected (presence is far less detection-biased than count). Each species' ", tags$b("detection-decay"), " (detections by distance) is its detectability signature.")),
      div(class="about-card", h4(bs_icon("calculator"), " How many species?"),
        p(tags$b("Chao2"), " (incidence-based) estimates how many species use the site beyond those observed — point counts systematically miss nocturnal, secretive, and rare birds."),
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
