# ============================================================================
# Onglet « Carte » — réseau origine → destination sur fond de carte US
# Endpoint : /routes/map
#
# Choix technique : plotly::scattergeo plutôt que leaflet, qui impose la chaîne
# raster/terra/sf (GDAL, GEOS, PROJ) et rend l'installation lourde pour l'équipe.
# ============================================================================

mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Carte du réseau",
      "Lignes au départ de New York : l'épaisseur du trait est proportionnelle au nombre de vols."
    ),
    bslib::layout_columns(
      col_widths = c(3, 9),
      bslib::card(
        bslib::card_header("Filtres"),
        bslib::card_body(
          shiny::selectInput(ns("origin"), "Aéroport de départ",
                             choices = c("Tous" = ""), width = "100%"),
          shiny::sliderInput(ns("top"), "Nombre de lignes affichées",
                             min = 10, max = 300, value = 80, step = 10),
          shiny::checkboxInput(ns("labels"), "Afficher les codes aéroport", TRUE),
          shiny::hr(),
          bslib::value_box(
            title = "Lignes affichées", value = shiny::textOutput(ns("kpi_routes")),
            showcase = shiny::icon("route"),
        showcase_layout = "left center", theme = "primary"
          ),
          bslib::value_box(
            title = "Vols couverts", value = shiny::textOutput(ns("kpi_flights")),
            showcase = shiny::icon("plane"),
        showcase_layout = "left center", theme = "secondary"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Réseau origine → destination"),
        bslib::card_body(
          plotly::plotlyOutput(ns("map"), height = "560px")
        )
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Détail des lignes"),
      bslib::card_body(DT::DTOutput(ns("table")))
    )
  )
}

mod_map_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      shiny::updateSelectInput(
        session, "origin",
        choices = choices_from(ref_origins(), "origin", "airport_name", "Tous")
      )
    })

    # Le réseau complet fait quelques centaines de lignes : un seul appel suffit,
    # les filtres sont ensuite appliqués côté dashboard.
    routes_all <- shiny::reactive({
      page <- api_paginated("/routes/map", list(page = 1, limit = 2000))
      page$data
    })

    routes <- shiny::reactive({
      df <- routes_all()
      need_rows(df)
      if (nzchar(input$origin %||% "")) {
        df <- df[df$origin == input$origin, , drop = FALSE]
      }
      need_rows(df, "Aucune ligne pour cet aéroport.")
      df <- df[order(-df$flight_count), , drop = FALSE]
      utils::head(df, input$top)
    })

    output$kpi_routes  <- shiny::renderText(fmt_int(nrow(routes())))
    output$kpi_flights <- shiny::renderText(fmt_int(sum(routes()$flight_count)))

    output$map <- plotly::renderPlotly({
      df <- routes()
      need_rows(df)

      # Un trait par ligne serait une trace plotly par ligne (lent au-delà de
      # quelques dizaines). On regroupe : une trace par (origine × palier
      # d'épaisseur), les segments étant séparés par des NA.
      brk <- stats::quantile(df$flight_count, c(0, 1/3, 2/3, 1), na.rm = TRUE)
      df$bucket <- cut(df$flight_count, breaks = unique(brk),
                       include.lowest = TRUE, labels = FALSE)
      widths <- c(0.8, 2, 3.6)

      p <- plotly::plot_geo()

      for (o in unique(df$origin)) {
        for (b in sort(unique(df$bucket[df$origin == o]))) {
          d <- df[df$origin == o & df$bucket == b, , drop = FALSE]
          if (nrow(d) == 0) next
          lons <- as.vector(rbind(d$origin_lon, d$dest_lon, NA))
          lats <- as.vector(rbind(d$origin_lat, d$dest_lat, NA))
          p <- plotly::add_trace(
            p, type = "scattergeo", mode = "lines",
            lon = lons, lat = lats,
            line = list(width = widths[b], color = origin_color(o)),
            opacity = 0.5, showlegend = FALSE, hoverinfo = "skip"
          )
        }
      }

      # marqueurs destinations (agrégés)
      dest <- stats::aggregate(
        flight_count ~ dest + dest_name + dest_lat + dest_lon,
        data = df, FUN = sum
      )
      dsize <- dest$flight_count
      drng <- range(dsize, na.rm = TRUE)
      dscaled <- if (diff(drng) == 0) rep(9, length(dsize)) else
        5 + 16 * sqrt((dsize - drng[1]) / diff(drng))

      p <- plotly::add_trace(
        p, type = "scattergeo",
        lon = dest$dest_lon, lat = dest$dest_lat,
        mode = if (isTRUE(input$labels)) "markers+text" else "markers",
        marker = list(color = ADP$blue, opacity = 0.8, size = dscaled,
                      line = list(color = "#FFFFFF", width = 1)),
        text = if (isTRUE(input$labels)) dest$dest else NULL,
        textposition = "top center",
        textfont = list(size = 9, color = ADP$slate),
        hovertext = paste0("<b>", dest$dest, "</b> — ", dest$dest_name,
                           "<br>", fmt_int(dest$flight_count), " vols reçus"),
        hoverinfo = "text", name = "Destinations"
      )

      # marqueurs origines NYC
      org <- stats::aggregate(
        flight_count ~ origin + origin_name + origin_lat + origin_lon,
        data = df, FUN = sum
      )
      p <- plotly::add_trace(
        p, type = "scattergeo", mode = "markers+text",
        lon = org$origin_lon, lat = org$origin_lat,
        marker = list(color = ADP$red, size = 12,
                      line = list(color = "#FFFFFF", width = 1.5)),
        text = org$origin, textposition = "bottom center",
        textfont = list(size = 11, color = ADP$red),
        hovertext = paste0("<b>", org$origin, "</b> — ", org$origin_name,
                           "<br>", fmt_int(org$flight_count), " départs"),
        hoverinfo = "text", name = "Départs NYC"
      )

      plotly::layout(
        p,
        geo = list(
          scope = "north america",
          projection = list(type = "albers usa"),
          showland = TRUE, landcolor = "#F0F3F6",
          showlakes = TRUE, lakecolor = "#FFFFFF",
          subunitcolor = "#D8DFE6", countrycolor = "#C4CDD6",
          showsubunits = TRUE, showcountries = TRUE
        ),
        margin = list(l = 0, r = 0, t = 10, b = 0),
        legend = list(orientation = "h", y = 0, x = 0)
      ) |>
        plotly::config(displayModeBar = FALSE, locale = "fr")
    })

    output$table <- DT::renderDT({
      df <- routes()
      need_rows(df)
      out <- data.frame(
        Ligne = paste(df$origin, "→", df$dest),
        `Aéroport de départ` = df$origin_name,
        `Aéroport d'arrivée` = df$dest_name,
        Vols = fmt_int(df$flight_count),
        check.names = FALSE
      )
      adp_table(out, page_length = 10)
    })
  })
}
