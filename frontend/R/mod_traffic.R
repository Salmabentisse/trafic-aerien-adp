# ============================================================================
# Onglet « Trafic » — saisonnalité, croissance, périodes remarquables
# Endpoints : /traffic/monthly, /traffic/daily, /traffic/by-period,
#             /traffic/special-days-vs-average
# ============================================================================

mod_traffic_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Évolution du trafic",
      "Saisonnalité mensuelle, croissance d'un mois sur l'autre et périodes remarquables."
    ),
    bslib::card(
      bslib::card_body(
        class = "py-2",
        shiny::selectInput(
          ns("origin"), "Aéroport de départ",
          choices = c("Tous" = ""), width = "320px"
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5), class = "mt-3",
      bslib::card(
        bslib::card_header("Vols par mois"),
        bslib::card_body(
          hint("Une courbe par aéroport de départ ; le creux de février tient au nombre de jours."),
          plotly::plotlyOutput(ns("plot_monthly"), height = "320px")
        )
      ),
      bslib::card(
        bslib::card_header("Croissance mensuelle"),
        bslib::card_body(
          hint("Variation du nombre de vols par rapport au mois précédent."),
          plotly::plotlyOutput(ns("plot_growth"), height = "320px")
        )
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Trafic journalier sur l'année"),
      bslib::card_body(
        hint("Total des trois aéroports. Les chutes correspondent aux jours fériés et aux épisodes météo."),
        shiny::uiOutput(ns("coverage")),
        plotly::plotlyOutput(ns("plot_daily"), height = "300px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7), class = "mt-3",
      bslib::card(
        bslib::card_header("Zoom sur une période"),
        bslib::card_body(
          shiny::selectInput(ns("period"), "Période", choices = PERIODS),
          bslib::layout_column_wrap(
            width = 1 / 3, fill = FALSE,
            bslib::value_box(
              title = "Vols", value = shiny::textOutput(ns("kpi_period_flights")),
              showcase = shiny::icon("plane"),
        showcase_layout = "left center", theme = "primary"
            ),
            bslib::value_box(
              title = "Origines", value = shiny::textOutput(ns("kpi_period_origins")),
              showcase = shiny::icon("tower-observation"),
        showcase_layout = "left center", theme = "secondary"
            ),
            bslib::value_box(
              title = "Destinations", value = shiny::textOutput(ns("kpi_period_dest")),
              showcase = shiny::icon("map-pin"),
        showcase_layout = "left center", theme = "secondary"
            )
          ),
          plotly::plotlyOutput(ns("plot_period"), height = "220px")
        )
      ),
      bslib::card(
        bslib::card_header("Jours fériés vs moyenne journalière"),
        bslib::card_body(
          hint("Barres : vols le jour férié. Ligne : moyenne journalière annuelle."),
          shiny::uiOutput(ns("special_note")),
          plotly::plotlyOutput(ns("plot_special"), height = "340px")
        )
      )
    )
  )
}

mod_traffic_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      shiny::updateSelectInput(
        session, "origin",
        choices = choices_from(ref_origins(), "origin", "airport_name", "Tous")
      )
    })

    monthly_res <- shiny::reactive({
      api_get("/traffic/monthly", list(origin = input$origin))
    })

    output$plot_monthly <- plotly::renderPlotly({
      res <- monthly_res()
      api_guard(res)
      df <- records_to_df(res$data$monthly)
      need_rows(df)
      df$label <- month_label(df$month)
      df <- df[order(df$origin, df$month), , drop = FALSE]

      p <- plotly::plot_ly()
      for (o in unique(df$origin)) {
        d <- complete_months(df[df$origin == o, , drop = FALSE])
        p <- plotly::add_trace(
          p, x = d$month, y = d$flight_count, name = o,
          type = "scatter", mode = "lines+markers", connectgaps = FALSE,
          line = list(color = origin_color(o), width = 2.5),
          marker = list(color = origin_color(o), size = 6),
          customdata = d$avg_daily_flights,
          hovertemplate = paste0("<b>", o, "</b> — %{x}<br>%{y:,} vols",
                                 "<br>%{customdata:.1f} vols/jour<extra></extra>")
        )
      }
      p |>
        adp_plotly(x_title = "Mois", y_title = "Nombre de vols") |>
        plotly::layout(xaxis = list(
          tickmode = "array", tickvals = 1:12, ticktext = MONTHS_FR,
          gridcolor = ADP$grid
        ))
    })

    output$plot_growth <- plotly::renderPlotly({
      res <- monthly_res()
      api_guard(res)
      df <- records_to_df(res$data$with_growth)
      need_rows(df)
      df <- df[!is.na(df$growth_rate_pct), , drop = FALSE]
      need_rows(df, "Pas assez de mois pour calculer une croissance.")
      df$color <- ifelse(df$growth_rate_pct >= 0, ADP$teal, ADP$red)
      df <- df[order(df$origin, df$month), , drop = FALSE]
      df$label <- paste0(df$origin, " ", month_label(df$month))

      plotly::plot_ly(
        df, x = ~label, y = ~growth_rate_pct, type = "bar",
        marker = list(color = ~color),
        hovertemplate = "<b>%{x}</b><br>%{y:+.2f} %<extra></extra>"
      ) |>
        adp_plotly(x_title = "", y_title = "Croissance (%)", legend = FALSE) |>
        plotly::layout(xaxis = list(
          tickangle = -60, gridcolor = ADP$grid,
          # sans cet ordre explicite, plotly classe les libellés alphabétiquement
          categoryorder = "array", categoryarray = df$label
        ))
    })

    daily <- shiny::reactive({
      page <- api_paginated("/traffic/daily", list(page = 1, limit = 366))
      df <- page$data
      need_rows(df)
      df <- df[order(df$year, df$month, df$day), , drop = FALSE]
      df$date <- as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day))
      df
    })

    output$coverage <- shiny::renderUI(hint(coverage_note(daily())))

    output$plot_daily <- plotly::renderPlotly({
      df <- complete_days(daily())

      plotly::plot_ly(
        df, x = ~date, y = ~flight_count,
        type = "scatter", mode = "lines", fill = "tozeroy",
        connectgaps = FALSE,
        line = list(color = ADP$navy, width = 1.5),
        fillcolor = "rgba(29,138,193,0.18)",
        hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:,} vols<extra></extra>"
      ) |>
        adp_plotly(x_title = "", y_title = "Vols par jour", legend = FALSE)
    })

    period_res <- shiny::reactive({
      api_get("/traffic/by-period",
              list(period = input$period, origin = input$origin))
    })

    period_summary <- shiny::reactive({
      res <- period_res()
      api_guard(res)
      object_to_list(res$data$summary)
    })

    output$kpi_period_flights <- shiny::renderText(fmt_int(period_summary()$flight_count))
    output$kpi_period_origins <- shiny::renderText(fmt_int(period_summary()$origins))
    output$kpi_period_dest    <- shiny::renderText(fmt_int(period_summary()$destinations))

    output$plot_period <- plotly::renderPlotly({
      res <- period_res()
      api_guard(res)
      df <- records_to_df(res$data$by_origin)
      need_rows(df, "Aucun vol sur cette période.")
      df <- df[order(-df$flight_count), , drop = FALSE]

      plotly::plot_ly(
        df, x = ~origin, y = ~flight_count, type = "bar",
        marker = list(color = origin_color(df$origin)),
        text = ~fmt_int(flight_count), textposition = "auto",
        hovertemplate = "<b>%{x}</b><br>%{y:,} vols<extra></extra>"
      ) |>
        adp_plotly(x_title = "", y_title = "Vols", legend = FALSE)
    })

    special <- shiny::reactive({
      df <- api_df("/traffic/special-days-vs-average")
      need_rows(df)
      df$nom <- unname(SPECIAL_DAY_LABELS[df$label])
      df$nom[is.na(df$nom)] <- df$label[is.na(df$nom)]
      df
    })

    # Un jour férié absent du jeu de données remonte de l'API avec des champs
    # nuls (agrégat sur zéro ligne) : on l'écarte du graphique et on le signale.
    output$special_note <- shiny::renderUI({
      df <- special()
      absents <- df$nom[is.na(df$avg_flights)]
      if (length(absents) == 0) return(NULL)
      hint(sprintf(
        "Absent%s du jeu de données, donc non représenté%s : %s.",
        if (length(absents) > 1) "s" else "",
        if (length(absents) > 1) "s" else "",
        paste(absents, collapse = ", ")
      ))
    })

    output$plot_special <- plotly::renderPlotly({
      df <- special()
      df <- df[!is.na(df$avg_flights), , drop = FALSE]
      need_rows(df, "Aucun jour férié présent dans le jeu de données.")
      df$nom <- paste0(df$nom, "\n", sprintf("%02d/%02d", df$day, df$month))
      annual <- suppressWarnings(as.numeric(stats::na.omit(df$annual_avg)[1]))

      plotly::plot_ly(
        df, x = ~nom, y = ~avg_flights, type = "bar",
        name = "Vols ce jour-là",
        marker = list(color = ifelse(df$diff_vs_avg >= 0, ADP$teal, ADP$red)),
        text = ~fmt_int(avg_flights), textposition = "auto",
        customdata = ~diff_vs_avg,
        hovertemplate = paste0("<b>%{x}</b><br>%{y:,.0f} vols",
                               "<br>écart : %{customdata:+.0f}<extra></extra>")
      ) |>
        plotly::add_trace(
          x = df$nom, y = rep(annual, nrow(df)),
          type = "scatter", mode = "lines",
          name = sprintf("Moyenne annuelle (%s)", fmt_num(annual, 0)),
          line = list(color = ADP$navy, dash = "dash", width = 2),
          hovertemplate = "Moyenne annuelle : %{y:,.0f}<extra></extra>"
        ) |>
        adp_plotly(x_title = "", y_title = "Vols par jour")
    })
  })
}
