# ============================================================================
# Onglet « Retards » — synthèse, fiabilité des compagnies, aéroports, extrêmes
# Endpoints : /delays/summary, /delays/daily, /delays/by-hour,
#             /delays/by-carrier, /delays/by-origin-airport,
#             /delays/by-destination, /delays/distance, /delays/gain,
#             /flights/most-delayed
# ============================================================================

mod_delays_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Analyse des retards",
      "Un vol est « en retard » dès que le retard est strictement positif (convention du backend)."
    ),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      bslib::value_box(
        title = "Retard moyen au départ", value = shiny::textOutput(ns("kpi_dep")),
        showcase = shiny::icon("plane-departure"),
        showcase_layout = "left center", theme = "warning"
      ),
      bslib::value_box(
        title = "Retard moyen à l'arrivée", value = shiny::textOutput(ns("kpi_arr")),
        showcase = shiny::icon("plane-arrival"),
        showcase_layout = "left center", theme = "warning"
      ),
      bslib::value_box(
        title = "Départs en retard", value = shiny::textOutput(ns("kpi_rate")),
        showcase = shiny::icon("percent"),
        showcase_layout = "left center", theme = "danger"
      ),
      bslib::value_box(
        title = "Retard maximum", value = shiny::textOutput(ns("kpi_max")),
        showcase = shiny::icon("triangle-exclamation"),
        showcase_layout = "left center", theme = "danger"
      )
    ),
    bslib::navset_card_tab(
      id = ns("tabs"),
      title = NULL,

      bslib::nav_panel(
        "Synthèse",
        bslib::layout_columns(
          col_widths = c(12),
          bslib::card(
            bslib::card_header("Retard moyen au départ, jour par jour"),
            bslib::card_body(
              hint("Les pics correspondent aux épisodes météo ; la ligne pointillée est la moyenne annuelle."),
              plotly::plotlyOutput(ns("plot_daily"), height = "300px")
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5), class = "mt-3",
          bslib::card(
            bslib::card_header("Retard moyen selon l'heure de départ"),
            bslib::card_body(
              hint("Les retards s'accumulent au fil de la journée : décoller tôt est le meilleur moyen de partir à l'heure."),
              plotly::plotlyOutput(ns("plot_hour"), height = "320px")
            )
          ),
          bslib::card(
            bslib::card_header("Avance / retard : répartition"),
            bslib::card_body(
              hint("Nombre de vols partis (ou arrivés) en avance, à l'heure ou en retard."),
              plotly::plotlyOutput(ns("plot_split"), height = "320px")
            )
          )
        )
      ),

      bslib::nav_panel(
        "Compagnies",
        bslib::card(
          bslib::card_header("Taux de vols arrivés en retard, par compagnie"),
          bslib::card_body(
            hint("Classement décroissant : en haut les compagnies les moins ponctuelles."),
            plotly::plotlyOutput(ns("plot_carrier"), height = "440px")
          )
        ),
        bslib::card(
          class = "mt-3",
          bslib::card_header("Détail par compagnie"),
          bslib::card_body(DT::DTOutput(ns("table_carrier")))
        )
      ),

      bslib::nav_panel(
        "Aéroports & destinations",
        bslib::layout_columns(
          col_widths = c(5, 7),
          bslib::card(
            bslib::card_header("Retard moyen par aéroport de départ"),
            bslib::card_body(
              plotly::plotlyOutput(ns("plot_origin"), height = "300px")
            )
          ),
          bslib::card(
            bslib::card_header("Destinations les plus pénalisées"),
            bslib::card_body(
              hint("Retard moyen à l'arrivée (HNL exclu par le backend : vol atypique)."),
              plotly::plotlyOutput(ns("plot_dest"), height = "300px")
            )
          )
        ),
        bslib::card(
          class = "mt-3",
          bslib::card_header("Distance et retard à l'arrivée"),
          bslib::card_body(
            hint("Une destination = un point. Les vols longs rattrapent souvent leur retard en vol."),
            shiny::checkboxInput(ns("exclude_hnl"), "Exclure Honolulu (HNL)", TRUE),
            plotly::plotlyOutput(ns("plot_distance"), height = "380px")
          )
        )
      ),

      bslib::nav_panel(
        "Vols extrêmes",
        bslib::layout_columns(
          col_widths = c(12),
          bslib::card(
            bslib::card_header("Vols les plus retardés"),
            bslib::card_body(
              shiny::radioButtons(
                ns("most_sort"), "Trier sur",
                choices = c("Retard à l'arrivée" = "arr",
                            "Retard au départ" = "dep",
                            "Le plus grand des deux" = "both"),
                selected = "arr", inline = TRUE
              ),
              DT::DTOutput(ns("table_most"))
            )
          )
        ),
        bslib::card(
          class = "mt-3",
          bslib::card_header("Retard rattrapé en vol"),
          bslib::card_body(
            hint(paste(
              "Vols partis avec au moins 1 h de retard et arrivés avec moins de retard",
              "qu'au départ, classés par minutes regagnées par heure de vol."
            )),
            hint(shiny::HTML(
              "Reconstitué à partir de <code>/flights</code> : l'endpoint
               <code>/delays/gain</code> filtre sur <code>arr_delay - dep_delay</code>,
               ce qui sélectionne les vols dont le retard <em>s'aggrave</em> en vol
               et non ceux qui le rattrapent."
            )),
            DT::DTOutput(ns("table_gain"))
          )
        )
      )
    )
  )
}

mod_delays_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    summary_d <- shiny::reactive(api_obj("/delays/summary"))

    output$kpi_dep <- shiny::renderText(fmt_min(summary_d()$avg_dep_delay))
    output$kpi_arr <- shiny::renderText(fmt_min(summary_d()$avg_arr_delay))
    output$kpi_max <- shiny::renderText(fmt_min(summary_d()$max_dep_delay, 0))
    output$kpi_rate <- shiny::renderText({
      s <- summary_d()
      total <- suppressWarnings(as.numeric(s$total_flights))
      late <- suppressWarnings(as.numeric(s$delayed_departures))
      if (is.na(total) || total == 0) return("—")
      fmt_pct(100 * late / total)
    })

    output$plot_daily <- plotly::renderPlotly({
      page <- api_paginated("/delays/daily", list(page = 1, limit = 366))
      df <- page$data
      need_rows(df)
      df <- df[order(df$year, df$month, df$day), , drop = FALSE]
      df$date <- as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day))
      moy <- mean(df$avg_dep_delay, na.rm = TRUE)
      df <- complete_days(df)

      plotly::plot_ly(
        df, x = ~date, y = ~avg_dep_delay,
        type = "scatter", mode = "lines", connectgaps = FALSE,
        line = list(color = ADP$blue, width = 1.6),
        name = "Retard moyen",
        hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:.1f} min<extra></extra>"
      ) |>
        plotly::add_trace(
          x = df$date, y = rep(moy, nrow(df)),
          type = "scatter", mode = "lines", inherit = FALSE,
          name = sprintf("Moyenne : %s", fmt_min(moy)),
          line = list(color = ADP$slate, dash = "dot", width = 1.5),
          hoverinfo = "skip"
        ) |>
        adp_plotly(x_title = "", y_title = "Retard moyen au départ (min)") |>
        plotly::layout(xaxis = axe_mois_fr(df$date))
    })

    output$plot_hour <- plotly::renderPlotly({
      df <- api_df("/delays/by-hour")
      need_rows(df)
      df <- df[order(df$hour), , drop = FALSE]

      plotly::plot_ly(
        df, x = ~hour, y = ~avg_dep_delay, type = "bar",
        marker = list(color = ~avg_dep_delay, colorscale = SCALE_YELLOW_RED,
                      showscale = FALSE),
        customdata = ~total_flights,
        hovertemplate = paste0("<b>%{x} h</b><br>%{y:.1f} min de retard moyen",
                               "<br>%{customdata:,} vols<extra></extra>")
      ) |>
        adp_plotly(x_title = "Heure de départ programmée",
                   y_title = "Retard moyen (min)", legend = FALSE) |>
        plotly::layout(xaxis = list(dtick = 2, gridcolor = ADP$grid))
    })

    output$plot_split <- plotly::renderPlotly({
      s <- summary_d()
      shiny::validate(shiny::need(length(s) > 0, "Données indisponibles."))
      total <- as.numeric(s$total_flights)
      df <- data.frame(
        phase = rep(c("Départ", "Arrivée"), each = 3),
        etat = rep(c("En avance", "À l'heure", "En retard"), 2),
        n = c(
          as.numeric(s$early_departures),
          total - as.numeric(s$early_departures) - as.numeric(s$delayed_departures),
          as.numeric(s$delayed_departures),
          as.numeric(s$early_arrivals),
          total - as.numeric(s$early_arrivals) - as.numeric(s$delayed_arrivals),
          as.numeric(s$delayed_arrivals)
        )
      )
      cols <- c("En avance" = ADP$teal, "À l'heure" = ADP$sky, "En retard" = ADP$red)
      p <- plotly::plot_ly()
      for (e in names(cols)) {
        d <- df[df$etat == e, , drop = FALSE]
        p <- plotly::add_trace(
          p, x = d$phase, y = d$n, type = "bar", name = e,
          marker = list(color = cols[[e]]),
          hovertemplate = paste0("<b>", e, " — %{x}</b><br>%{y:,} vols<extra></extra>")
        )
      }
      p |>
        adp_plotly(x_title = "", y_title = "Nombre de vols") |>
        plotly::layout(barmode = "stack")
    })

    carriers <- shiny::reactive({
      page <- api_paginated("/delays/by-carrier", list(page = 1, limit = 100))
      page$data
    })

    output$plot_carrier <- plotly::renderPlotly({
      df <- carriers()
      need_rows(df)
      df <- df[order(df$delay_rate_pct), , drop = FALSE]
      df$label <- paste0(df$carrier, " — ", df$airline_name)
      df$label <- factor(df$label, levels = df$label)

      plotly::plot_ly(
        df, y = ~label, x = ~delay_rate_pct, type = "bar", orientation = "h",
        marker = list(color = ~delay_rate_pct, colorscale = SCALE_YELLOW_RED,
                      showscale = FALSE),
        text = ~fmt_pct(delay_rate_pct), textposition = "auto", textangle = 0,
        customdata = ~total_flights,
        hovertemplate = paste0("<b>%{y}</b><br>%{x:.2f} % de vols en retard",
                               "<br>%{customdata:,} vols<extra></extra>")
      ) |>
        adp_plotly(x_title = "Part des vols arrivés en retard (%)",
                   y_title = "", legend = FALSE) |>
        plotly::layout(margin = list(l = 240))
    })

    output$table_carrier <- DT::renderDT({
      df <- carriers()
      need_rows(df)
      df <- df[order(-df$delay_rate_pct), , drop = FALSE]
      out <- data.frame(
        Code = df$carrier,
        Compagnie = df$airline_name,
        `Vols` = fmt_int(df$total_flights),
        `Vols en retard` = fmt_int(df$delayed_flights),
        `Taux de retard` = fmt_pct(df$delay_rate_pct),
        check.names = FALSE
      )
      adp_table(out, page_length = 10, scroll_x = FALSE)
    })

    output$plot_origin <- plotly::renderPlotly({
      df <- api_df("/delays/by-origin-airport")
      need_rows(df)
      df <- df[order(df$avg_dep_delay), , drop = FALSE]

      plotly::plot_ly(
        df, x = ~origin, y = ~avg_dep_delay, type = "bar", name = "Départ",
        marker = list(color = ADP$navy),
        text = ~fmt_min(avg_dep_delay), textposition = "auto", textangle = 0,
        customdata = ~total_flights,
        hovertemplate = paste0("<b>%{x}</b><br>%{y:.2f} min au départ",
                               "<br>%{customdata:,} vols<extra></extra>")
      ) |>
        plotly::add_trace(
          y = ~avg_arr_delay, name = "Arrivée",
          marker = list(color = ADP$sky),
          text = ~fmt_min(avg_arr_delay), textangle = 0,
          hovertemplate = "<b>%{x}</b><br>%{y:.2f} min à l'arrivée<extra></extra>"
        ) |>
        adp_plotly(x_title = "", y_title = "Retard moyen (min)") |>
        plotly::layout(barmode = "group")
    })

    output$plot_dest <- plotly::renderPlotly({
      df <- api_df("/delays/by-destination", list(limit = 12))
      need_rows(df)
      df <- df[order(df$avg_arr_delay), , drop = FALSE]
      df$label <- factor(df$dest, levels = df$dest)

      plotly::plot_ly(
        df, y = ~label, x = ~avg_arr_delay, type = "bar", orientation = "h",
        marker = list(color = ADP$red),
        text = ~fmt_min(avg_arr_delay), textposition = "auto", textangle = 0,
        customdata = ~paste0(destination_name, " — ", fmt_int(total_flights), " vols"),
        hovertemplate = "<b>%{y}</b><br>%{customdata}<br>%{x:.2f} min<extra></extra>"
      ) |>
        adp_plotly(x_title = "Retard moyen à l'arrivée (min)",
                   y_title = "", legend = FALSE)
    })

    output$plot_distance <- plotly::renderPlotly({
      df <- api_df("/delays/distance",
                   list(exclude_hnl = tolower(as.character(isTRUE(input$exclude_hnl)))))
      need_rows(df)

      sizes <- sqrt(df$flight_count)
      rng <- range(sizes, na.rm = TRUE)
      sizes <- if (diff(rng) == 0) rep(10, length(sizes)) else
        6 + 22 * (sizes - rng[1]) / diff(rng)

      plotly::plot_ly(
        df, x = ~avg_distance, y = ~avg_arr_delay,
        type = "scatter", mode = "markers",
        marker = list(size = sizes, color = ADP$blue, opacity = 0.65,
                      line = list(color = "#FFFFFF", width = 1)),
        customdata = ~paste0(dest, " — ", destination_name,
                             "<br>", fmt_int(flight_count), " vols"),
        hovertemplate = paste0("<b>%{customdata}</b><br>%{x:,.0f} miles",
                               "<br>%{y:.2f} min de retard<extra></extra>")
      ) |>
        plotly::add_lines(
          x = ~avg_distance,
          y = ~stats::fitted(stats::lm(avg_arr_delay ~ avg_distance, data = df)),
          inherit = FALSE, name = "Tendance",
          line = list(color = ADP$red, width = 2, dash = "dash"),
          hoverinfo = "skip"
        ) |>
        adp_plotly(x_title = "Distance moyenne (miles)",
                   y_title = "Retard moyen à l'arrivée (min)", legend = TRUE)
    })

    output$table_most <- DT::renderDT({
      df <- api_df("/flights/most-delayed",
                   list(limit = 25, sort_by = input$most_sort %||% "arr"))
      need_rows(df)
      out <- data.frame(
        Date = date_label(df),
        Compagnie = paste0(df$carrier, " — ", df$airline_name),
        `N° vol` = fmt_int(df$flight),
        Trajet = paste(df$origin, "→", df$dest),
        `Retard départ` = fmt_signed_min(df$dep_delay, 0),
        `Retard arrivée` = fmt_signed_min(df$arr_delay, 0),
        check.names = FALSE
      )
      adp_table(out, page_length = 10)
    })

    # /delays/gain calcule (arr_delay - dep_delay) : il remonte les vols dont le
    # retard s'aggrave, pas ceux qui le rattrapent. On reconstruit la vraie
    # population : parmi les vols partis avec >= 1 h de retard, ceux dont le
    # retard à l'arrivée est le plus faible.
    output$table_gain <- DT::renderDT({
      page <- api_paginated("/flights", list(
        page = 1, limit = 300, min_dep_delay = 60,
        sort_by = "arr_delay", sort_dir = "asc"
      ))
      df <- page$data
      need_rows(df)
      df$rattrapage <- df$dep_delay - df$arr_delay
      df <- df[!is.na(df$rattrapage) & df$rattrapage > 0 &
                 !is.na(df$air_time) & df$air_time > 0, , drop = FALSE]
      need_rows(df, "Aucun vol n'a rattrapé de retard sur cette sélection.")
      df$par_heure <- df$rattrapage / (df$air_time / 60)
      df <- utils::head(df[order(-df$par_heure), , drop = FALSE], 30)

      out <- data.frame(
        Date = date_label(df),
        Compagnie = df$carrier,
        `N° vol` = fmt_int(df$flight),
        Trajet = paste(df$origin, "→", df$dest),
        `Retard départ` = fmt_signed_min(df$dep_delay, 0),
        `Retard arrivée` = fmt_signed_min(df$arr_delay, 0),
        `Temps de vol` = fmt_min(df$air_time, 0),
        `Minutes rattrapées` = fmt_num(df$rattrapage, 0),
        `Rattrapage / h de vol` = fmt_num(df$par_heure, 1),
        check.names = FALSE
      )
      adp_table(out, page_length = 10)
    })
  })
}
