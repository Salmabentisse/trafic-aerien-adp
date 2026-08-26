# ============================================================================
# Onglet « Météo » — conditions horaires aux trois aéroports de départ
# Endpoint : /weather?origin=&month=&day=&page=&limit=
#
# Le backend plafonne limit à 500 : on enchaîne les pages pour couvrir un mois.
# ============================================================================

F_to_C <- function(f) (suppressWarnings(as.numeric(f)) - 32) * 5 / 9

# Aucune rafale relevée au sol ne dépasse ~120 mph : au-delà, c'est une valeur
# aberrante du relevé. Une seule suffit à écraser l'échelle du graphique.
VENT_MAX_PLAUSIBLE <- 150

mod_weather_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Conditions météo au départ",
      "Relevés horaires (source : weather.pdf). Températures converties en degrés Celsius."
    ),
    bslib::card(
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(ns("origin"), "Aéroport", choices = c("JFK" = "JFK")),
          shiny::selectInput(
            ns("month"), "Mois",
            choices = c("Tous" = "", stats::setNames(as.character(1:12), MONTHS_FR))
          ),
          shiny::selectInput(
            ns("day"), "Jour",
            choices = c("Tous" = "", stats::setNames(as.character(1:31), 1:31))
          )
        )
      )
    ),
    shiny::uiOutput(ns("coverage")),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE, class = "mt-2",
      bslib::value_box(
        title = "Température moyenne", value = shiny::textOutput(ns("kpi_temp")),
        showcase = shiny::icon("temperature-half"),
        showcase_layout = "left center", theme = "primary"
      ),
      bslib::value_box(
        title = "Vent moyen", value = shiny::textOutput(ns("kpi_wind")),
        showcase = shiny::icon("wind"),
        showcase_layout = "left center", theme = "secondary"
      ),
      bslib::value_box(
        title = "Précipitations cumulées", value = shiny::textOutput(ns("kpi_precip")),
        showcase = shiny::icon("cloud-rain"),
        showcase_layout = "left center", theme = "success"
      ),
      bslib::value_box(
        title = "Visibilité moyenne", value = shiny::textOutput(ns("kpi_visib")),
        showcase = shiny::icon("eye"),
        showcase_layout = "left center", theme = "warning"
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Température et point de rosée"),
      bslib::card_body(plotly::plotlyOutput(ns("plot_temp"), height = "300px"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), class = "mt-3",
      bslib::card(
        bslib::card_header("Vent et rafales"),
        bslib::card_body(
          shiny::uiOutput(ns("wind_note")),
          plotly::plotlyOutput(ns("plot_wind"), height = "300px")
        )
      ),
      bslib::card(
        bslib::card_header("Précipitations et visibilité"),
        bslib::card_body(
          shiny::uiOutput(ns("precip_note")),
          plotly::plotlyOutput(ns("plot_precip"), height = "300px")
        )
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Relevés horaires"),
      bslib::card_body(DT::DTOutput(ns("table")))
    )
  )
}

mod_weather_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      df <- ref_weather_origins()
      choices <- if (nrow(df) == 0) c("EWR" = "EWR") else {
        stats::setNames(as.character(df$origin),
                        paste0(df$origin, " — ", df$airport_name))
      }
      shiny::updateSelectInput(session, "origin", choices = choices)
    })

    weather <- shiny::reactive({
      shiny::req(nzchar(input$origin %||% ""))
      query <- list(origin = input$origin, month = input$month, day = input$day,
                    page = 1, limit = 500)
      first <- api_paginated("/weather", query)
      out <- first$data
      # l'API plafonne limit à 500 : on enchaîne les pages, avec un garde-fou
      max_pages <- 10
      if (isTRUE(first$pages > 1)) {
        for (p in 2:min(max_pages, first$pages)) {
          query$page <- p
          more <- api_paginated("/weather", query)
          if (nrow(more$data) > 0) out <- rbind(out, more$data)
        }
      }
      attr(out, "total") <- first$total
      attr(out, "tronque") <- isTRUE(first$pages > max_pages)
      out
    })

    stamped <- shiny::reactive({
      df <- weather()
      need_rows(df, "Aucun relevé pour cette sélection.")
      df$ts <- as.POSIXct(
        sprintf("%04d-%02d-%02d %02d:00:00", df$year, df$month, df$day, df$hour),
        tz = "UTC"
      )
      df$temp_c <- F_to_C(df$temp)
      df$dewp_c <- F_to_C(df$dewp)

      aberrants <- which((!is.na(df$wind_speed) & df$wind_speed > VENT_MAX_PLAUSIBLE) |
                         (!is.na(df$wind_gust) & df$wind_gust > VENT_MAX_PLAUSIBLE))
      if (length(aberrants)) {
        attr(df, "vent_aberrant") <- data.frame(
          quand = format(df$ts[aberrants], "%d/%m/%Y %Hh"),
          vent = df$wind_speed[aberrants],
          rafale = df$wind_gust[aberrants],
          stringsAsFactors = FALSE
        )
        df$wind_speed[aberrants] <- NA
        df$wind_gust[aberrants] <- NA
      }
      df[order(df$ts), , drop = FALSE]
    })

    output$coverage <- shiny::renderUI({
      df <- stamped()
      total <- attr(weather(), "total") %||% nrow(df)
      mois <- sort(unique(df$month))
      txt <- sprintf(
        "%s relevés chargés — %s %d, mois : %s.",
        fmt_int(nrow(df)), input$origin, df$year[1],
        paste(month_label(mois), collapse = ", ")
      )
      if (isTRUE(attr(weather(), "tronque"))) {
        txt <- paste(txt, sprintf("Extrait limité aux 5 000 premiers relevés sur %s.",
                                  fmt_int(total)))
      }
      hint(txt)
    })

    output$kpi_temp <- shiny::renderText({
      paste0(fmt_num(mean(stamped()$temp_c, na.rm = TRUE), 1), " °C")
    })
    output$kpi_wind <- shiny::renderText({
      paste0(fmt_num(mean(stamped()$wind_speed, na.rm = TRUE), 1), " mph")
    })
    output$kpi_precip <- shiny::renderText({
      paste0(fmt_num(sum(stamped()$precip, na.rm = TRUE), 2), " in")
    })
    output$kpi_visib <- shiny::renderText({
      paste0(fmt_num(mean(stamped()$visib, na.rm = TRUE), 1), " mi")
    })

    output$plot_temp <- plotly::renderPlotly({
      df <- stamped()
      plotly::plot_ly(
        df, x = ~ts, y = ~temp_c, type = "scatter", mode = "lines",
        name = "Température",
        line = list(color = ADP$red, width = 2),
        hovertemplate = "<b>%{x|%d/%m %Hh}</b><br>%{y:.1f} °C<extra></extra>"
      ) |>
        plotly::add_trace(
          y = ~dewp_c, name = "Point de rosée",
          line = list(color = ADP$blue, width = 1.5, dash = "dot"),
          hovertemplate = "<b>%{x|%d/%m %Hh}</b><br>%{y:.1f} °C<extra></extra>"
        ) |>
        adp_plotly(x_title = "", y_title = "Degrés Celsius") |>
        plotly::layout(xaxis = axe_temps_fr(df$ts))
    })

    output$wind_note <- shiny::renderUI({
      ab <- attr(stamped(), "vent_aberrant")
      if (is.null(ab)) return(NULL)
      hint(sprintf(
        "%s relevé%s écarté%s du graphique : valeur physiquement impossible (%s — vent %s mph, rafale %s mph, contre un second maximum sous 50 mph). La moyenne affichée plus haut l'exclut également.",
        nrow(ab), if (nrow(ab) > 1) "s" else "", if (nrow(ab) > 1) "s" else "",
        paste(ab$quand, collapse = ", "),
        paste(fmt_num(ab$vent, 0), collapse = ", "),
        paste(fmt_num(ab$rafale, 0), collapse = ", ")
      ))
    })

    output$plot_wind <- plotly::renderPlotly({
      df <- stamped()
      plotly::plot_ly(
        df, x = ~ts, y = ~wind_speed, type = "scatter", mode = "lines",
        name = "Vent moyen",
        line = list(color = ADP$navy, width = 2),
        hovertemplate = "<b>%{x|%d/%m %Hh}</b><br>%{y:.1f} mph<extra></extra>"
      ) |>
        plotly::add_trace(
          y = ~wind_gust, name = "Rafales", mode = "lines",
          line = list(color = ADP$amber, width = 1.5),
          hovertemplate = "<b>%{x|%d/%m %Hh}</b><br>%{y:.1f} mph<extra></extra>"
        ) |>
        adp_plotly(x_title = "", y_title = "Vitesse (mph)") |>
        plotly::layout(xaxis = axe_temps_fr(df$ts))
    })

    # Au-delà d'une semaine, le pas horaire donne des milliers de barres
    # illisibles : on agrège par jour (précipitations cumulées, visibilité la
    # plus basse de la journée — celle qui compte pour l'exploitation).
    precip_data <- shiny::reactive({
      df <- stamped()
      jours <- length(unique(as.Date(df$ts)))
      if (jours <= 7) {
        return(list(pas = "heure", data = data.frame(
          x = df$ts, precip = df$precip, visib = df$visib)))
      }
      jour <- as.Date(df$ts)
      agg <- data.frame(
        x = as.POSIXct(paste(unique(sort(jour)), "12:00:00"), tz = "UTC"),
        precip = as.numeric(tapply(df$precip, jour, sum, na.rm = TRUE)),
        visib = as.numeric(tapply(df$visib, jour, min, na.rm = TRUE))
      )
      agg$visib[!is.finite(agg$visib)] <- NA
      list(pas = "jour", data = agg)
    })

    output$precip_note <- shiny::renderUI({
      hint(if (identical(precip_data()$pas, "jour"))
        "Agrégé par jour : barres = précipitations cumulées, courbe = visibilité la plus basse de la journée. Choisir un jour précis pour le détail horaire."
      else
        "Pas horaire : barres = précipitations (pouces), courbe = visibilité (miles).")
    })

    output$plot_precip <- plotly::renderPlotly({
      res <- precip_data()
      df <- res$data
      need_rows(df)
      par_jour <- identical(res$pas, "jour")
      fmt_x <- if (par_jour) "%d/%m/%Y" else "%d/%m %Hh"

      plotly::plot_ly(
        df, x = ~x, y = ~precip, type = "bar", name = "Précipitations",
        marker = list(color = ADP$blue),
        hovertemplate = paste0("<b>%{x|", fmt_x, "}</b><br>%{y:.2f} in<extra></extra>")
      ) |>
        plotly::add_trace(
          x = df$x, y = df$visib, inherit = FALSE,
          type = "scatter", mode = "lines",
          name = if (par_jour) "Visibilité minimale" else "Visibilité",
          yaxis = "y2", line = list(color = ADP$slate, width = 1.4),
          hovertemplate = paste0("<b>%{x|", fmt_x, "}</b><br>%{y:.1f} mi<extra></extra>")
        ) |>
        adp_plotly(x_title = "",
                   y_title = if (par_jour) "Précipitations cumulées / jour (in)"
                             else "Précipitations (in)") |>
        plotly::layout(
          xaxis = axe_temps_fr(df$x),
          yaxis2 = list(overlaying = "y", side = "right",
                        title = "Visibilité (mi)", showgrid = FALSE,
                        rangemode = "tozero")
        )
    })

    output$table <- DT::renderDT({
      df <- stamped()
      out <- data.frame(
        `Date / heure` = format(df$ts, "%d/%m/%Y %Hh"),
        `Temp. (°C)` = fmt_num(df$temp_c, 1),
        `Rosée (°C)` = fmt_num(df$dewp_c, 1),
        `Humidité (%)` = fmt_num(df$humid, 0),
        `Vent (mph)` = fmt_num(df$wind_speed, 1),
        `Rafales (mph)` = fmt_num(df$wind_gust, 1),
        `Précip. (in)` = fmt_num(df$precip, 2),
        `Pression (mbar)` = fmt_num(df$pressure, 0),
        `Visibilité (mi)` = fmt_num(df$visib, 1),
        check.names = FALSE
      )
      adp_table(out, page_length = 12)
    })
  })
}
