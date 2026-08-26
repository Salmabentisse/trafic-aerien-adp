# ============================================================================
# Onglet « Annulations » — volumétrie, compagnies, destinations, qualité des données
# Endpoints : /cancellations, /cancellations/sorted
#
# Convention du backend : un vol est annulé quand dep_time ET arr_time sont nuls.
# ============================================================================

mod_cancellations_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Vols annulés",
      "Un vol est considéré annulé lorsque l'heure de départ et l'heure d'arrivée sont toutes deux absentes."
    ),
    bslib::layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      bslib::value_box(
        title = "Vols annulés", value = shiny::textOutput(ns("kpi_total")),
        showcase = shiny::icon("ban"),
        showcase_layout = "left center", theme = "danger"
      ),
      bslib::value_box(
        title = "Taux d'annulation", value = shiny::textOutput(ns("kpi_rate")),
        showcase = shiny::icon("percent"),
        showcase_layout = "left center", theme = "warning"
      ),
      bslib::value_box(
        title = "Vols au total", value = shiny::textOutput(ns("kpi_flights")),
        showcase = shiny::icon("plane"),
        showcase_layout = "left center", theme = "secondary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5), class = "mt-3",
      bslib::card(
        bslib::card_header("Annulations mois par mois"),
        bslib::card_body(
          hint("Février et les mois d'hiver concentrent les annulations (météo)."),
          plotly::plotlyOutput(ns("plot_month"), height = "300px")
        )
      ),
      bslib::card(
        bslib::card_header("Qualité des données"),
        bslib::card_body(
          hint("Part de valeurs manquantes par colonne de la table flights."),
          plotly::plotlyOutput(ns("plot_missing"), height = "300px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6), class = "mt-3",
      bslib::card(
        bslib::card_header("Annulations par compagnie"),
        bslib::card_body(plotly::plotlyOutput(ns("plot_carrier"), height = "340px"))
      ),
      bslib::card(
        bslib::card_header("Destinations les plus touchées"),
        bslib::card_body(plotly::plotlyOutput(ns("plot_dest"), height = "340px"))
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Vols aux données incomplètes (valeurs manquantes en tête)"),
      bslib::card_body(
        hint("Reprend le tri du backend : NA d'abord, puis retards décroissants."),
        DT::DTOutput(ns("table_sorted"))
      )
    )
  )
}

mod_cancellations_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    cancel <- shiny::reactive({
      res <- api_get("/cancellations")
      api_guard(res)
      res$data
    })

    output$kpi_total   <- shiny::renderText(fmt_int(as_scalar(cancel()$total_cancelled)))
    output$kpi_flights <- shiny::renderText(fmt_int(as_scalar(cancel()$total_flights)))
    output$kpi_rate    <- shiny::renderText(fmt_pct(as_scalar(cancel()$cancellation_rate_pct), 2))

    output$plot_month <- plotly::renderPlotly({
      df <- records_to_df(cancel()$by_month)
      need_rows(df)
      df <- df[order(df$month), , drop = FALSE]
      df$label <- month_label(df$month)
      df$label <- factor(df$label, levels = df$label)

      plotly::plot_ly(
        df, x = ~label, y = ~cancelled_count, type = "bar",
        marker = list(color = ~cancelled_count, colorscale = "Reds",
                      showscale = FALSE),
        text = ~fmt_int(cancelled_count), textposition = "auto", textangle = 0,
        hovertemplate = "<b>%{x}</b><br>%{y:,} annulations<extra></extra>"
      ) |>
        adp_plotly(x_title = "", y_title = "Vols annulés", legend = FALSE)
    })

    output$plot_missing <- plotly::renderPlotly({
      mv <- object_to_list(cancel()$missing_values)
      shiny::validate(shiny::need(length(mv) > 0, "Données indisponibles."))
      labels <- c(
        dep_time_na_pct  = "Heure de départ",
        arr_time_na_pct  = "Heure d'arrivée",
        dep_delay_na_pct = "Retard au départ",
        arr_delay_na_pct = "Retard à l'arrivée"
      )
      df <- data.frame(
        champ = unname(labels[names(mv)]),
        pct = suppressWarnings(as.numeric(unlist(mv))),
        stringsAsFactors = FALSE
      )
      df <- df[!is.na(df$champ) & !is.na(df$pct), , drop = FALSE]
      need_rows(df)
      df <- df[order(df$pct), , drop = FALSE]
      df$champ <- factor(df$champ, levels = df$champ)

      plotly::plot_ly(
        df, y = ~champ, x = ~pct, type = "bar", orientation = "h",
        marker = list(color = ADP$amber),
        text = ~fmt_pct(pct, 2), textposition = "auto", textangle = 0,
        hovertemplate = "<b>%{y}</b><br>%{x:.2f} % de valeurs manquantes<extra></extra>"
      ) |>
        adp_plotly(x_title = "Valeurs manquantes (%)", y_title = "", legend = FALSE) |>
        plotly::layout(margin = list(l = 140))
    })

    output$plot_carrier <- plotly::renderPlotly({
      df <- records_to_df(cancel()$by_carrier)
      need_rows(df)
      df <- utils::head(df[order(-df$cancelled_count), , drop = FALSE], 12)
      df$label <- factor(df$carrier, levels = rev(df$carrier))

      plotly::plot_ly(
        df, y = ~label, x = ~cancelled_count, type = "bar", orientation = "h",
        marker = list(color = ADP$red),
        text = ~fmt_int(cancelled_count), textposition = "auto", textangle = 0,
        customdata = ~airline_name,
        hovertemplate = "<b>%{y}</b> — %{customdata}<br>%{x:,} annulations<extra></extra>"
      ) |>
        adp_plotly(x_title = "Vols annulés", y_title = "", legend = FALSE)
    })

    output$plot_dest <- plotly::renderPlotly({
      df <- records_to_df(cancel()$by_destination)
      need_rows(df)
      df <- utils::head(df[order(-df$cancelled_count), , drop = FALSE], 12)
      df$label <- factor(df$dest, levels = rev(df$dest))

      plotly::plot_ly(
        df, y = ~label, x = ~cancelled_count, type = "bar", orientation = "h",
        marker = list(color = ADP$navy),
        text = ~fmt_int(cancelled_count), textposition = "auto", textangle = 0,
        customdata = ~destination_name,
        hovertemplate = "<b>%{y}</b> — %{customdata}<br>%{x:,} annulations<extra></extra>"
      ) |>
        adp_plotly(x_title = "Vols annulés", y_title = "", legend = FALSE)
    })

    output$table_sorted <- DT::renderDT({
      df <- api_df("/cancellations/sorted", list(limit = 100))
      need_rows(df)
      out <- data.frame(
        Compagnie = df$carrier,
        `N° vol` = fmt_int(df$flight),
        Trajet = paste(df$origin, "→", df$dest),
        `Départ` = hhmm(df$dep_time),
        `Retard départ` = fmt_signed_min(df$dep_delay, 0),
        `Arrivée` = hhmm(df$arr_time),
        `Retard arrivée` = fmt_signed_min(df$arr_delay, 0),
        check.names = FALSE
      )
      adp_table(out, page_length = 10)
    })
  })
}
