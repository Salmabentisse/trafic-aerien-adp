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
        "Vitesse & courriers",
        bslib::layout_column_wrap(
          width = 1 / 4, fill = FALSE,
          bslib::value_box(
            title = "Vol le plus long", value = shiny::textOutput(ns("kpi_plus_long")),
            showcase = shiny::icon("earth-americas"),
            showcase_layout = "left center", theme = "primary"
          ),
          bslib::value_box(
            title = "Vol le plus court", value = shiny::textOutput(ns("kpi_plus_court")),
            showcase = shiny::icon("arrows-left-right-to-line"),
            showcase_layout = "left center", theme = "secondary"
          ),
          bslib::value_box(
            title = "Vitesse la plus élevée relevée",
            value = shiny::textOutput(ns("kpi_vitesse_max")),
            showcase = shiny::icon("gauge-high"),
            showcase_layout = "left center", theme = "warning"
          ),
          bslib::value_box(
            title = "Écart de vitesse moyen",
            value = shiny::textOutput(ns("kpi_ecart_vitesse")),
            showcase = shiny::icon("right-left"),
            showcase_layout = "left center", theme = "success"
          )
        ),
        bslib::layout_columns(
          col_widths = c(5, 7), class = "mt-3",
          bslib::card(
            bslib::card_header("Vitesse selon le type de courrier"),
            bslib::card_body(
              hint("Vitesse au sol = distance / temps de vol × 60. Un long courrier vole nettement plus vite qu'une navette : la montée et la descente pèsent peu sur un vol long."),
              plotly::plotlyOutput(ns("plot_vitesse"), height = "300px")
            )
          ),
          bslib::card(
            bslib::card_header("Distance et vitesse"),
            bslib::card_body(
              hint("Un point = un vol. Les deux nuages correspondent aux deux échantillons chargés."),
              plotly::plotlyOutput(ns("plot_dist_vitesse"), height = "300px")
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6), class = "mt-3",
          bslib::card(
            bslib::card_header("Long courrier — les vols les plus longs"),
            bslib::card_body(DT::DTOutput(ns("table_long")))
          ),
          bslib::card(
            bslib::card_header("Court courrier — les vols les plus courts"),
            bslib::card_body(DT::DTOutput(ns("table_court")))
          )
        ),
        bslib::card(
          class = "mt-3",
          bslib::card_body(
            class = "py-2",
            shiny::uiOutput(ns("note_methode"))
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

    # --- Vitesse et types de courrier (Mission 2, §3.2 Q7) -----------------
    # Le tri par distance est fait par l'API (exact). La vitesse, elle, n'est pas
    # un champ de la base : elle est calculée ici sur les vols chargés.
    TAILLE_ECH <- 1000

    charger_par_distance <- function(sens) {
      pages <- ceiling(TAILLE_ECH / 500)
      out <- NULL
      for (pg in seq_len(pages)) {
        res <- api_paginated("/flights", list(
          page = pg, limit = 500, sort_by = "distance", sort_dir = sens
        ))
        if (nrow(res$data) == 0) break
        out <- if (is.null(out)) res$data else rbind(out, res$data)
        if (pg >= res$pages) break
      }
      out
    }

    courriers <- shiny::reactive({
      longs <- charger_par_distance("desc")
      courts <- charger_par_distance("asc")
      need_rows(longs)
      need_rows(courts)
      prep <- function(d, type) {
        d <- d[!is.na(d$air_time) & d$air_time > 0 & !is.na(d$distance), , drop = FALSE]
        d$vitesse <- d$distance / d$air_time * 60
        d$type <- type
        d
      }
      list(
        longs = prep(longs, "Long courrier"),
        courts = prep(courts, "Court courrier")
      )
    })

    output$kpi_plus_long <- shiny::renderText({
      d <- courriers()$longs
      d <- d[order(-d$distance), , drop = FALSE][1, ]
      sprintf("%s mi — %s → %s", fmt_int(d$distance), d$origin, d$dest)
    })

    output$kpi_plus_court <- shiny::renderText({
      d <- courriers()$courts
      d <- d[order(d$distance), , drop = FALSE][1, ]
      sprintf("%s mi — %s → %s", fmt_int(d$distance), d$origin, d$dest)
    })

    output$kpi_vitesse_max <- shiny::renderText({
      d <- rbind(courriers()$longs, courriers()$courts)
      i <- which.max(d$vitesse)
      sprintf("%s mph — %s → %s", fmt_num(d$vitesse[i], 0), d$origin[i], d$dest[i])
    })

    output$kpi_ecart_vitesse <- shiny::renderText({
      c1 <- courriers()
      sprintf("%s mph", fmt_num(mean(c1$longs$vitesse, na.rm = TRUE) -
                                 mean(c1$courts$vitesse, na.rm = TRUE), 0))
    })

    output$plot_vitesse <- plotly::renderPlotly({
      c1 <- courriers()
      plotly::plot_ly(type = "box") |>
        plotly::add_trace(
          y = c1$courts$vitesse, name = "Court courrier",
          marker = list(color = ADP$sky), line = list(color = ADP$sky),
          boxmean = TRUE
        ) |>
        plotly::add_trace(
          y = c1$longs$vitesse, name = "Long courrier",
          marker = list(color = ADP$navy), line = list(color = ADP$navy),
          boxmean = TRUE
        ) |>
        adp_plotly(x_title = "", y_title = "Vitesse au sol (mph)", legend = FALSE)
    })

    output$plot_dist_vitesse <- plotly::renderPlotly({
      c1 <- courriers()
      d <- rbind(c1$longs, c1$courts)
      plotly::plot_ly(
        d, x = ~distance, y = ~vitesse, color = ~type,
        colors = c("Court courrier" = ADP$sky, "Long courrier" = ADP$navy),
        type = "scatter", mode = "markers",
        marker = list(size = 6, opacity = 0.55,
                      line = list(color = "#FFFFFF", width = 0.5)),
        customdata = ~paste0(origin, " → ", dest, " · ", carrier,
                             " · ", fmt_min(air_time, 0), " de vol"),
        hovertemplate = paste0("<b>%{customdata}</b><br>%{x:,} miles",
                               "<br>%{y:.0f} mph<extra></extra>")
      ) |>
        adp_plotly(x_title = "Distance (miles)", y_title = "Vitesse (mph)")
    })

    table_courrier <- function(d, decroissant) {
      d <- d[order(if (decroissant) -d$distance else d$distance), , drop = FALSE]
      d <- utils::head(d, 15)
      data.frame(
        Date = date_label(d),
        Compagnie = d$carrier,
        Trajet = paste(d$origin, "→", d$dest),
        `Distance (mi)` = fmt_int(d$distance),
        `Temps de vol` = fmt_min(d$air_time, 0),
        `Vitesse (mph)` = fmt_num(d$vitesse, 0),
        check.names = FALSE
      )
    }

    output$table_long <- DT::renderDT(
      adp_table(table_courrier(courriers()$longs, TRUE), page_length = 8))
    output$table_court <- DT::renderDT(
      adp_table(table_courrier(courriers()$courts, FALSE), page_length = 8))

    output$note_methode <- shiny::renderUI({
      c1 <- courriers()
      hint(shiny::HTML(sprintf(
        "<b>Méthode.</b> Le classement par distance est calculé par l'API
         (<code>sort_by=distance</code>) : les extrêmes affichés sont donc exacts.
         La vitesse n'existe pas en base, elle est calculée ici sur les
         %s vols les plus longs et les %s les plus courts effectivement chargés.
         Le vol le plus rapide de toute la table demanderait un tri SQL sur
         <code>distance / air_time</code> — c'est ce que fait le script d'analyse,
         l'API n'exposant pas ce tri.",
        fmt_int(nrow(c1$longs)), fmt_int(nrow(c1$courts))
      )))
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
