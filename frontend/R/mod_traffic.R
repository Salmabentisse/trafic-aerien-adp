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
          shiny::radioButtons(
            ns("mode"), NULL,
            choices = c("Total du mois" = "total",
                        "Moyenne par jour observé" = "par_jour"),
            selected = "total", inline = TRUE
          ),
          shiny::uiOutput(ns("monthly_note")),
          plotly::plotlyOutput(ns("plot_monthly"), height = "340px")
        )
      ),
      bslib::card(
        bslib::card_header("Croissance mensuelle"),
        bslib::card_body(
          hint("Variation par rapport au mois précédent, entre mois consécutifs et complets uniquement."),
          shiny::uiOutput(ns("growth_note")),
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
          shiny::uiOutput(ns("period_note")),
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

    # Nombre de jours réellement présents dans chaque mois. /traffic/monthly ne
    # donne que le total mensuel : sans ce décompte, juillet (3 jours de données)
    # se lit comme un effondrement du trafic.
    couverture <- shiny::reactive({
      d <- daily()
      cle <- paste(d$year, d$month)
      obs <- tapply(d$day, cle, function(x) length(unique(x)))
      parts <- do.call(rbind, strsplit(names(obs), " ", fixed = TRUE))
      out <- data.frame(
        year = as.integer(parts[, 1]),
        month = as.integer(parts[, 2]),
        jours = as.integer(obs),
        stringsAsFactors = FALSE
      )
      out$attendus <- jours_du_mois(out$year, out$month)
      out$complet <- out$jours >= out$attendus
      out[order(out$year, out$month), , drop = FALSE]
    })

    output$monthly_note <- shiny::renderUI({
      cov <- couverture()
      partiels <- cov[!cov$complet, , drop = FALSE]
      if (nrow(partiels) == 0) return(NULL)
      hint(sprintf(
        "Mois incomplets, marqués par un point creux : %s. En « moyenne par jour observé », ils redeviennent comparables aux autres.",
        paste(sprintf("%s (%d j sur %d)", month_label(partiels$month),
                      partiels$jours, partiels$attendus), collapse = ", ")
      ))
    })

    output$plot_monthly <- plotly::renderPlotly({
      res <- monthly_res()
      api_guard(res)
      df <- records_to_df(res$data$monthly)
      need_rows(df)
      cov <- couverture()
      df <- merge(df, cov[, c("year", "month", "jours", "complet")],
                  by = c("year", "month"), all.x = TRUE)
      df <- df[order(df$origin, df$month), , drop = FALSE]

      par_jour <- identical(input$mode, "par_jour")
      df$valeur <- if (par_jour) df$flight_count / df$jours else df$flight_count

      # Une facette par aéroport, chacune avec sa propre moyenne mensuelle :
      # c'est la comparaison demandée par l'énoncé (Mission 2, §3.1 Q2).
      origines <- sort(unique(df$origin))
      axe_x <- list(tickmode = "array", tickvals = seq(1, 12, 2),
                    ticktext = MONTHS_FR[seq(1, 12, 2)], gridcolor = ADP$grid,
                    tickangle = -45)

      facettes <- lapply(seq_along(origines), function(i) {
        o <- origines[i]
        d <- complete_months(df[df$origin == o, , drop = FALSE])
        # La moyenne ne porte que sur les mois complets : inclure juillet et ses
        # 3 jours la tirerait vers le bas d'environ 9 %.
        ref <- d$valeur[!is.na(d$valeur) & !is.na(d$complet) & d$complet]
        moy <- if (length(ref)) mean(ref) else mean(d$valeur, na.rm = TRUE)
        # point creux = mois incomplet, pour qu'un total partiel ne se lise pas
        # comme une chute du trafic
        symboles <- ifelse(is.na(d$complet) | d$complet, "circle", "circle-open")

        plotly::plot_ly() |>
          plotly::add_trace(
            x = d$month, y = d$valeur, name = o,
            type = "scatter", mode = "lines+markers", connectgaps = FALSE,
            line = list(color = origin_color(o), width = 2.5),
            marker = list(color = origin_color(o), size = 7, symbol = symboles,
                          line = list(color = origin_color(o), width = 2)),
            customdata = d$jours, legendgroup = o, showlegend = FALSE,
            hovertemplate = paste0(
              "<b>", o, "</b> — %{x}<br>",
              if (par_jour) "%{y:,.0f} vols/jour" else "%{y:,} vols",
              "<br>%{customdata} jours de données<extra></extra>")
          ) |>
          plotly::add_trace(
            x = 1:12, y = rep(moy, 12), type = "scatter", mode = "lines",
            name = "Moyenne de l'aéroport", legendgroup = "moyenne",
            showlegend = i == 1,
            line = list(color = ADP$slate, dash = "dash", width = 1.6),
            hovertemplate = paste0("Moyenne ", o, " : %{y:,.0f}<extra></extra>")
          ) |>
          plotly::layout(
            xaxis = axe_x,
            annotations = list(list(
              text = sprintf("<b>%s</b>  —  moyenne %s", o,
                             if (par_jour) fmt_num(moy, 0) else fmt_int(round(moy))),
              x = 0.5, y = 1.06, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 12, color = ADP$ink)
            ))
          )
      })

      plotly::subplot(facettes, nrows = 1, shareY = TRUE, titleX = FALSE,
                      margin = 0.025) |>
        adp_plotly(y_title = if (par_jour) "Vols par jour observé" else "Nombre de vols") |>
        plotly::layout(
          margin = list(l = 70, r = 10, t = 34, b = 70),
          legend = list(orientation = "h", y = -0.28, x = 0)
        )
    })

    # L'API calcule la croissance avec un LAG sur les lignes présentes : octobre
    # se retrouve comparé aux 3 jours de juillet, d'où des variations à +1 000 %.
    # On recalcule en n'acceptant que deux mois consécutifs et tous deux complets.
    croissance <- shiny::reactive({
      res <- monthly_res()
      api_guard(res)
      df <- records_to_df(res$data$monthly)
      need_rows(df)
      cov <- couverture()
      df <- merge(df, cov[, c("year", "month", "complet")],
                  by = c("year", "month"), all.x = TRUE)
      df <- df[order(df$origin, df$year, df$month), , drop = FALSE]

      out <- do.call(rbind, lapply(split(df, df$origin), function(d) {
        if (nrow(d) < 2) return(NULL)
        prec <- d[-nrow(d), , drop = FALSE]
        suiv <- d[-1, , drop = FALSE]
        ok <- (suiv$month - prec$month == 1) & prec$complet & suiv$complet
        ok[is.na(ok)] <- FALSE
        if (!any(ok)) return(NULL)
        data.frame(
          origin = suiv$origin[ok], month = suiv$month[ok],
          taux = 100 * (suiv$flight_count[ok] - prec$flight_count[ok]) /
            prec$flight_count[ok],
          depuis = prec$month[ok],
          stringsAsFactors = FALSE
        )
      }))
      out
    })

    output$growth_note <- shiny::renderUI({
      cov <- couverture()
      partiels <- cov[!cov$complet, , drop = FALSE]
      manquants <- setdiff(1:12, cov$month)
      if (nrow(partiels) == 0 && length(manquants) == 0) return(NULL)
      hint(sprintf(
        "Transitions écartées : celles qui touchent un mois absent (%s) ou incomplet (%s). Les comparer donnerait des variations à plusieurs centaines de pour cent.",
        if (length(manquants)) paste(month_label(manquants), collapse = ", ") else "aucun",
        if (nrow(partiels)) paste(month_label(partiels$month), collapse = ", ") else "aucun"
      ))
    })

    output$plot_growth <- plotly::renderPlotly({
      df <- croissance()
      need_rows(df, "Pas assez de mois consécutifs complets pour calculer une croissance.")
      df$growth_rate_pct <- df$taux
      df$color <- ifelse(df$growth_rate_pct >= 0, ADP$teal, ADP$red)
      df <- df[order(df$origin, df$month), , drop = FALSE]
      df$label <- paste0(df$origin, " ", month_label(df$month))

      plotly::plot_ly(
        df, x = ~label, y = ~growth_rate_pct, type = "bar",
        marker = list(color = ~color),
        customdata = ~month_label(depuis),
        hovertemplate = "<b>%{x}</b><br>%{y:+.2f} % vs %{customdata}<extra></extra>"
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
      df <- plages_contigues(complete_days(daily()), "flight_count")
      need_rows(df)

      p <- plotly::plot_ly()
      for (pl in unique(df$plage)) {
        d <- df[df$plage == pl, , drop = FALSE]
        p <- plotly::add_trace(
          p, x = d$date, y = d$flight_count,
          type = "scatter", mode = "lines", fill = "tozeroy",
          line = list(color = ADP$navy, width = 1.5),
          fillcolor = "rgba(29,138,193,0.18)", showlegend = FALSE,
          hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:,} vols<extra></extra>"
        )
      }
      p |>
        adp_plotly(x_title = "", y_title = "Vols par jour", legend = FALSE) |>
        plotly::layout(xaxis = axe_mois_fr(df$date))
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

    # Une période qui recouvre les mois absents (l'été, notamment) donne un total
    # qui n'a pas de sens sans cet avertissement.
    output$period_note <- shiny::renderUI({
      mois <- PERIOD_MONTHS[[input$period %||% ""]]
      if (is.null(mois)) return(NULL)
      cov <- couverture()
      absents <- setdiff(mois, cov$month)
      partiels <- intersect(mois, cov$month[!cov$complet])
      if (length(absents) == 0 && length(partiels) == 0) return(NULL)
      shiny::div(
        class = "alert alert-warning py-2 px-3 small mb-2",
        sprintf(
          "Attention : cette période recouvre %s%s%s. Le total ci-dessous ne couvre donc qu'une fraction de la période.",
          if (length(absents)) sprintf("des mois absents du jeu de données (%s)",
                                       paste(month_label(absents), collapse = ", ")) else "",
          if (length(absents) && length(partiels)) " et " else "",
          if (length(partiels)) sprintf("un mois incomplet (%s)",
                                        paste(month_label(partiels), collapse = ", ")) else ""
        )
      )
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
        text = ~fmt_int(flight_count), textposition = "auto", textangle = 0,
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
        text = ~fmt_int(avg_flights), textposition = "auto", textangle = 0,
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
