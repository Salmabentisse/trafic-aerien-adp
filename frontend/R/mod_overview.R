# ============================================================================
# Onglet « Vue d'ensemble » — volumétrie, top aéroports, top lignes, cadrage
# Endpoints : /stats/overview, /delays/summary, /traffic/top-airports,
#             /traffic/weekday-vs-weekend, /routes/top
# ============================================================================

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Vue d'ensemble du trafic",
      "Vols au départ des trois aéroports de New York (JFK, LGA, EWR) — données BTS."
    ),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      bslib::value_box(
        title = "Vols enregistrés", value = shiny::textOutput(ns("kpi_flights")),
        showcase = shiny::icon("plane-departure"),
        showcase_layout = "left center", theme = "primary"
      ),
      bslib::value_box(
        title = "Destinations desservies", value = shiny::textOutput(ns("kpi_dest")),
        showcase = shiny::icon("map-location-dot"),
        showcase_layout = "left center", theme = "secondary"
      ),
      bslib::value_box(
        title = "Compagnies", value = shiny::textOutput(ns("kpi_airlines")),
        showcase = shiny::icon("building"),
        showcase_layout = "left center", theme = "success"
      ),
      bslib::value_box(
        title = "Vols annulés", value = shiny::textOutput(ns("kpi_cancelled")),
        showcase = shiny::icon("ban"),
        showcase_layout = "left center", theme = "danger"
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE, class = "mt-2",
      bslib::value_box(
        title = "Retard moyen au départ", value = shiny::textOutput(ns("kpi_dep")),
        showcase = shiny::icon("clock"),
        showcase_layout = "left center", theme = "warning"
      ),
      bslib::value_box(
        title = "Retard moyen à l'arrivée", value = shiny::textOutput(ns("kpi_arr")),
        showcase = shiny::icon("clock-rotate-left"),
        showcase_layout = "left center", theme = "warning"
      ),
      bslib::value_box(
        title = "Aéroports référencés", value = shiny::textOutput(ns("kpi_airports")),
        showcase = shiny::icon("tower-observation"),
        showcase_layout = "left center", theme = "secondary"
      ),
      bslib::value_box(
        title = "Avions référencés", value = shiny::textOutput(ns("kpi_planes")),
        showcase = shiny::icon("jet-fighter"),
        showcase_layout = "left center", theme = "secondary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7), class = "mt-3",
      bslib::card(
        bslib::card_header("Répartition par aéroport de départ"),
        bslib::card_body(
          hint("Part de chaque aéroport new-yorkais dans le total des départs."),
          plotly::plotlyOutput(ns("plot_origins"), height = "300px")
        )
      ),
      bslib::card(
        bslib::card_header("Top 10 des destinations"),
        bslib::card_body(
          hint("Destinations les plus desservies au départ de New York."),
          plotly::plotlyOutput(ns("plot_dests"), height = "300px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 8), class = "mt-3",
      bslib::card(
        bslib::card_header("Semaine vs week-end"),
        bslib::card_body(
          shiny::uiOutput(ns("note_semaine")),
          plotly::plotlyOutput(ns("plot_week"), height = "280px")
        )
      ),
      bslib::card(
        bslib::card_header("Lignes les plus fréquentées"),
        bslib::card_body(
          hint("Couples origine → destination classés par nombre de vols."),
          DT::DTOutput(ns("table_routes"))
        )
      )
    )
  )
}

mod_overview_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    overview <- shiny::reactive(api_obj("/stats/overview"))
    delays <- shiny::reactive(api_obj("/delays/summary"))
    top_airports <- shiny::reactive(api_get("/traffic/top-airports", list(limit = 10)))
    # /traffic/weekday-vs-weekend applique DAYOFWEEK à la date stockée, dont
    # l'année (2021) ne correspond pas au rythme réel des données : le calcul
    # attribue au week-end des jours de semaine et conclut à l'inverse de la
    # réalité. On recalcule depuis le détail journalier, calendrier corrigé, et
    # en vols par jour — comparer 5 jours à 2 sur des totaux bruts n'aurait pas
    # de sens.
    semaine <- shiny::reactive({
      page <- api_paginated("/traffic/daily", list(page = 1, limit = 366))
      df <- page$data
      need_rows(df)
      df$date <- as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day))
      weekend <- jour_semaine_reel(df$date) >= 6
      out <- data.frame(
        type = c("Semaine", "Week-end"),
        jours = c(sum(!weekend), sum(weekend)),
        vols = c(sum(df$flight_count[!weekend]), sum(df$flight_count[weekend])),
        stringsAsFactors = FALSE
      )
      out$par_jour <- out$vols / out$jours
      out$part <- 100 * out$vols / sum(out$vols)
      out
    })

    output$kpi_flights   <- shiny::renderText(fmt_int(overview()$flights))
    output$kpi_dest      <- shiny::renderText(fmt_int(overview()$destinations))
    output$kpi_airlines  <- shiny::renderText(fmt_int(overview()$airlines))
    output$kpi_airports  <- shiny::renderText(fmt_int(overview()$airports))
    output$kpi_planes    <- shiny::renderText(fmt_int(overview()$planes))
    output$kpi_cancelled <- shiny::renderText(fmt_int(overview()$cancelled_flights))
    output$kpi_dep       <- shiny::renderText(fmt_min(delays()$avg_dep_delay))
    output$kpi_arr       <- shiny::renderText(fmt_min(delays()$avg_arr_delay))

    output$plot_origins <- plotly::renderPlotly({
      res <- top_airports()
      api_guard(res)
      df <- records_to_df(res$data$origins)
      need_rows(df)
      df <- df[order(-df$flight_count), , drop = FALSE]
      plotly::plot_ly(
        labels = df$origin, values = df$flight_count,
        type = "pie", hole = 0.55, sort = FALSE,
        marker = list(colors = origin_color(df$origin),
                      line = list(color = "#FFFFFF", width = 2)),
        textinfo = "label+percent",
        insidetextorientation = "horizontal",
        hovertemplate = paste0("<b>%{label}</b><br>%{value:,} vols",
                               "<br>%{percent}<extra></extra>")
      ) |>
        adp_plotly(legend = FALSE)
    })

    output$plot_dests <- plotly::renderPlotly({
      res <- top_airports()
      api_guard(res)
      df <- records_to_df(res$data$destinations)
      need_rows(df)
      df <- utils::head(df[order(-df$flight_count), , drop = FALSE], 10)
      df$dest <- factor(df$dest, levels = rev(df$dest))
      plotly::plot_ly(
        df, y = ~dest, x = ~flight_count, type = "bar", orientation = "h",
        marker = list(color = ADP$blue),
        text = ~fmt_int(flight_count), textposition = "auto", textangle = 0,
        hovertemplate = paste0("<b>%{y}</b><br>%{customdata}",
                              "<br>%{x:,} vols<extra></extra>"),
        customdata = ~name
      ) |>
        adp_plotly(x_title = "Nombre de vols", y_title = "", legend = FALSE)
    })

    output$note_semaine <- shiny::renderUI({
      d <- semaine()
      ecart <- 100 * (d$par_jour[2] / d$par_jour[1] - 1)
      hint(sprintf(
        "En vols par jour, pour comparer %s jours de semaine à %s jours de week-end. Le week-end est %s moins chargé.",
        fmt_int(d$jours[1]), fmt_int(d$jours[2]), fmt_pct(abs(ecart))
      ))
    })

    output$plot_week <- plotly::renderPlotly({
      df <- semaine()
      need_rows(df)
      plotly::plot_ly(
        df, x = ~type, y = ~par_jour, type = "bar",
        marker = list(color = c(ADP$navy, ADP$sky)),
        text = ~paste0(fmt_num(par_jour, 0), " vols/jour"),
        textposition = "auto", textangle = 0,
        customdata = ~paste0(fmt_int(vols), " vols sur ", fmt_int(jours),
                             " jours — ", fmt_pct(part), " du total"),
        hovertemplate = "<b>%{x}</b><br>%{y:,.0f} vols par jour<br>%{customdata}<extra></extra>"
      ) |>
        adp_plotly(x_title = "", y_title = "Vols par jour", legend = FALSE)
    })

    output$table_routes <- DT::renderDT({
      df <- api_df("/routes/top", list(limit = 15))
      need_rows(df)
      out <- data.frame(
        Ligne = paste(df$origin, "→", df$dest),
        Origine = df$origin_name,
        Destination = df$dest_name,
        Vols = fmt_int(df$flight_count),
        check.names = FALSE
      )
      adp_table(out, page_length = 8, scroll_x = FALSE)
    })
  })
}
