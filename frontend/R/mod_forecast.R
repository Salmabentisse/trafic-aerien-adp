# ============================================================================
# Onglet « Prévisions » — Mission 3, volet analyse prédictive
#
# Modèle volontairement simple et lisible, conformément à l'énoncé : « un modèle
# plus complexe [...] est hors du cadre de ce projet : traiter par ex la
# saisonnalité ». On décompose donc la série journalière en
#     tendance + saisonnalité mensuelle + effet du jour de la semaine
# ajustée par régression linéaire, et on la valide sur les derniers jours connus
# face à une référence naïve (le même jour de la semaine précédente).
#
# Endpoints : /traffic/daily (trafic), /delays/daily (retard moyen)
# ============================================================================

HORIZONS <- c("7 jours" = "7", "14 jours" = "14", "30 jours" = "30",
              "60 jours" = "60", "90 jours" = "90")

# ANNEE_CALENDRIER et JOURS_FR sont définis dans utils_ui.R.

#' Jour de la semaine (1 = lundi). Les dates de cet onglet étant déjà ramenées
#' au calendrier de référence, la lecture directe suffit.
jour_semaine <- function(dates) as.integer(format(dates, "%u"))

# Jours fériés cités par l'énoncé, plus la période des fêtes : sans eux, le
# modèle surestime systématiquement la fin décembre.
FERIES <- c("01-01", "07-04", "11-28", "11-29", "12-25")

est_ferie <- function(dates) format(dates, "%m-%d") %in% FERIES
est_fetes <- function(dates) {
  md <- format(dates, "%m-%d")
  md >= "12-20" & md <= "12-31"
}

# --- outils de modélisation -------------------------------------------------

#' Prépare une série journalière pour la régression
preparer_serie <- function(df, valeur) {
  df <- df[order(df$year, df$month, df$day), , drop = FALSE]
  # Dates ramenées au calendrier de référence : sans cela, le jour de la semaine
  # est décalé de trois jours et l'effet du samedi se retrouve attribué au mardi.
  df$date <- as.Date(sprintf("%04d-%02d-%02d", ANNEE_CALENDRIER, df$month, df$day))
  df$y <- df[[valeur]]
  df <- df[!is.na(df$y), , drop = FALSE]
  df$t <- as.numeric(df$date - min(df$date))
  df$mois <- factor(df$month, levels = 1:12)
  df$jsem <- factor(jour_semaine(df$date), levels = 1:7)
  df$ferie <- as.integer(est_ferie(df$date))
  df$fetes <- as.integer(est_fetes(df$date))
  df
}

#' Ajuste le modèle. `log_scale` pour une grandeur strictement positive
#' (le trafic) : les effets deviennent multiplicatifs, ce qui convient mieux
#' qu'une addition à une série de comptages.
ajuster_modele <- function(d, log_scale = TRUE) {
  d$mois <- droplevels(d$mois)
  d$jsem <- droplevels(d$jsem)
  termes <- c("t", if (nlevels(d$mois) > 1) "mois", "jsem")
  if (length(unique(d$ferie)) > 1) termes <- c(termes, "ferie")
  if (length(unique(d$fetes)) > 1) termes <- c(termes, "fetes")
  formule <- stats::as.formula(paste("y ~", paste(termes, collapse = " + ")))
  if (log_scale) {
    d$y <- log(d$y)
    fit <- stats::lm(formule, data = d)
  } else {
    fit <- stats::lm(formule, data = d)
  }
  attr(fit, "log_scale") <- log_scale
  attr(fit, "mois_connus") <- as.integer(levels(d$mois))
  attr(fit, "origine") <- min(d$date)
  attr(fit, "termes") <- termes
  fit
}

#' Mois de substitution quand un mois n'a aucune donnée d'apprentissage
#' (août et septembre ici) : on prend le mois connu le plus proche dans l'année,
#' et la prévision est signalée comme extrapolée.
mois_substitut <- function(m, connus) {
  vapply(m, function(mi) {
    if (mi %in% connus) return(as.integer(mi))
    ecart <- pmin(abs(connus - mi), 12 - abs(connus - mi))
    as.integer(connus[which.min(ecart)])
  }, integer(1))
}

#' Prédit sur un vecteur de dates, avec intervalle de prévision
predire <- function(fit, dates, niveau = 0.9) {
  connus <- attr(fit, "mois_connus")
  origine <- attr(fit, "origine")
  mois_demande <- as.integer(format(dates, "%m"))
  mois_utilise <- mois_substitut(mois_demande, connus)

  nd <- data.frame(
    t = as.numeric(dates - origine),
    mois = factor(mois_utilise, levels = connus),
    jsem = factor(jour_semaine(dates), levels = 1:7),
    ferie = as.integer(est_ferie(dates)),
    fetes = as.integer(est_fetes(dates))
  )
  nd <- nd[, intersect(names(nd), attr(fit, "termes")), drop = FALSE]

  pr <- suppressWarnings(
    stats::predict(fit, newdata = nd, interval = "prediction", level = niveau)
  )
  transf <- if (isTRUE(attr(fit, "log_scale"))) exp else identity
  data.frame(
    date = dates,
    prevision = transf(pr[, "fit"]),
    bas = transf(pr[, "lwr"]),
    haut = transf(pr[, "upr"]),
    extrapole = mois_demande != mois_utilise
  )
}

#' Compare le modèle à une référence naïve sur les derniers jours observés
valider <- function(d, n_test = 28, log_scale = TRUE) {
  if (nrow(d) < n_test + 60) return(NULL)
  train <- utils::head(d, nrow(d) - n_test)
  test <- utils::tail(d, n_test)

  fit <- ajuster_modele(train, log_scale = log_scale)
  pred <- predire(fit, test$date)$prevision

  # Référence naïve honnête : on répète la dernière semaine CONNUE. Reprendre
  # « la même valeur sept jours plus tôt » ferait piocher la référence dans la
  # période de test elle-même dès le huitième jour — une fuite qui la rendrait
  # imbattable pour de mauvaises raisons.
  derniere <- utils::tail(train, 7)
  naif <- derniere$y[match(jour_semaine(test$date), jour_semaine(derniere$date))]

  err <- function(p, obs) {
    ok <- !is.na(p) & !is.na(obs)
    list(
      mae = mean(abs(p[ok] - obs[ok])),
      mape = mean(abs(p[ok] - obs[ok]) / abs(obs[ok])) * 100,
      n = sum(ok)
    )
  }
  list(
    n_test = n_test,
    debut = min(test$date), fin = max(test$date),
    modele = err(pred, test$y),
    naif = err(naif, test$y),
    detail = data.frame(date = test$date, observe = test$y,
                        modele = pred, naif = naif)
  )
}

# --- interface --------------------------------------------------------------

mod_forecast_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Prévisions de trafic",
      "Extrapolation de la série journalière à partir de sa tendance et de ses deux saisonnalités : le mois et le jour de la semaine."
    ),
    shiny::div(
      class = "alert alert-secondary py-2 px-3 small",
      shiny::HTML(sprintf(
        "<b>Calendrier.</b> La colonne <code>year</code> de la base indique 2021, mais le rythme hebdomadaire des données ne correspond qu'à <b>%d</b> : sous ce calendrier le jour creux tombe exactement sur le samedi (741 vols/jour contre plus de 940 en semaine), alors qu'en 2021 il tomberait un mardi. Cet onglet raisonne donc en calendrier %d — d'où un historique daté %d et des prévisions qui démarrent en %d.",
        ANNEE_CALENDRIER, ANNEE_CALENDRIER, ANNEE_CALENDRIER, ANNEE_CALENDRIER + 1L))
    ),
    bslib::card(
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(ns("horizon"), "Horizon de prévision",
                             choices = HORIZONS, selected = "30"),
          shiny::selectInput(
            ns("cible"), "Grandeur prédite",
            choices = c("Vols par jour" = "trafic",
                        "Retard moyen au départ" = "retard")
          ),
          shiny::checkboxInput(ns("montrer_ajuste"),
                               "Superposer le modèle sur l'historique", TRUE)
        )
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE, class = "mt-2",
      bslib::value_box(
        title = "Erreur moyenne du modèle", value = shiny::textOutput(ns("kpi_mae")),
        showcase = shiny::icon("bullseye"),
        showcase_layout = "left center", theme = "primary"
      ),
      bslib::value_box(
        title = "Erreur relative", value = shiny::textOutput(ns("kpi_mape")),
        showcase = shiny::icon("percent"),
        showcase_layout = "left center", theme = "primary"
      ),
      bslib::value_box(
        title = "Référence naïve", value = shiny::textOutput(ns("kpi_naif")),
        showcase = shiny::icon("scale-balanced"),
        showcase_layout = "left center", theme = "secondary"
      ),
      bslib::value_box(
        title = "Modèle vs référence", value = shiny::textOutput(ns("kpi_gain")),
        showcase = shiny::icon("arrow-trend-down"),
        showcase_layout = "left center", theme = "success"
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Historique et prévision"),
      bslib::card_body(
        shiny::uiOutput(ns("note_extrapolation")),
        plotly::plotlyOutput(ns("plot_prevision"), height = "360px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7), class = "mt-3",
      bslib::card(
        bslib::card_header("Validation sur les derniers jours connus"),
        bslib::card_body(
          shiny::uiOutput(ns("note_validation")),
          plotly::plotlyOutput(ns("plot_validation"), height = "280px")
        )
      ),
      bslib::card(
        bslib::card_header("Effet estimé de chaque jour de la semaine"),
        bslib::card_body(
          shiny::uiOutput(ns("note_calendrier")),
          plotly::plotlyOutput(ns("plot_jsem"), height = "280px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5), class = "mt-3",
      bslib::card(
        bslib::card_header("Prévisions jour par jour"),
        bslib::card_body(DT::DTOutput(ns("table_prevision")))
      ),
      bslib::card(
        bslib::card_header("Le modèle, et ce qu'il ne sait pas faire"),
        bslib::card_body(shiny::uiOutput(ns("note_modele")))
      )
    )
  )
}

# --- serveur ----------------------------------------------------------------

mod_forecast_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    est_trafic <- shiny::reactive(identical(input$cible %||% "trafic", "trafic"))

    serie <- shiny::reactive({
      if (est_trafic()) {
        page <- api_paginated("/traffic/daily", list(page = 1, limit = 366))
        need_rows(page$data)
        preparer_serie(page$data, "flight_count")
      } else {
        page <- api_paginated("/delays/daily", list(page = 1, limit = 366))
        need_rows(page$data)
        preparer_serie(page$data, "avg_dep_delay")
      }
    })

    # le retard moyen peut être négatif : pas de passage au log
    log_scale <- shiny::reactive(est_trafic())

    modele <- shiny::reactive(ajuster_modele(serie(), log_scale = log_scale()))

    validation <- shiny::reactive(
      valider(serie(), n_test = 28, log_scale = log_scale()))

    prevision <- shiny::reactive({
      d <- serie()
      n <- as.integer(input$horizon %||% "30")
      dates <- seq(max(d$date) + 1, by = "day", length.out = n)
      predire(modele(), dates)
    })

    unite <- shiny::reactive(if (est_trafic()) "vols" else "min")
    fmt_val <- function(x, dec = 0) fmt_num(x, dec)

    # --- KPI de validation ---
    output$kpi_mae <- shiny::renderText({
      v <- validation()
      if (is.null(v)) return("—")
      sprintf("%s %s", fmt_val(v$modele$mae, if (est_trafic()) 0 else 1), unite())
    })
    output$kpi_mape <- shiny::renderText({
      v <- validation()
      if (is.null(v)) return("—")
      fmt_pct(v$modele$mape)
    })
    output$kpi_naif <- shiny::renderText({
      v <- validation()
      if (is.null(v)) return("—")
      sprintf("%s %s", fmt_val(v$naif$mae, if (est_trafic()) 0 else 1), unite())
    })
    output$kpi_gain <- shiny::renderText({
      v <- validation()
      if (is.null(v) || !is.finite(v$naif$mae) || v$naif$mae == 0) return("—")
      gain <- 100 * (v$naif$mae - v$modele$mae) / v$naif$mae
      sprintf("%s d'erreur en %s", fmt_pct(abs(gain)),
              if (gain > 0) "moins" else "plus")
    })

    output$note_validation <- shiny::renderUI({
      v <- validation()
      if (is.null(v)) return(hint("Série trop courte pour une validation séparée."))
      hint(sprintf(
        "Modèle ajusté sans les %d derniers jours (%s au %s), puis confronté à eux. La référence naïve répète la dernière semaine connue — la seule comparaison honnête, puisqu'elle n'a pas plus d'information que le modèle.",
        v$n_test, format(v$debut, "%d/%m"), format(v$fin, "%d/%m/%Y")
      ))
    })

    output$note_extrapolation <- shiny::renderUI({
      p <- prevision()
      if (!any(p$extrapole)) return(NULL)
      mois <- unique(as.integer(format(p$date[p$extrapole], "%m")))
      shiny::div(
        class = "alert alert-warning py-2 px-3 small mb-2",
        sprintf(
          "L'horizon choisi atteint %s, mois sans aucune donnée d'apprentissage. Le modèle y réutilise le mois connu le plus proche : ces jours sont tracés en pointillé et restent des extrapolations.",
          paste(month_label(mois), collapse = " et ")
        )
      )
    })

    # --- graphique principal ---
    output$plot_prevision <- plotly::renderPlotly({
      d <- serie()
      p <- prevision()
      hist <- plages_contigues(complete_days(d, "date"), "y")
      titre_y <- if (est_trafic()) "Vols par jour" else "Retard moyen au départ (min)"

      g <- plotly::plot_ly()
      for (pl in unique(hist$plage)) {
        h <- hist[hist$plage == pl, , drop = FALSE]
        g <- plotly::add_trace(
          g, x = h$date, y = h$y, type = "scatter", mode = "lines",
          line = list(color = ADP$navy, width = 1.2),
          name = "Observé", legendgroup = "obs",
          showlegend = pl == min(hist$plage),
          hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:,.0f}<extra></extra>"
        )
      }

      if (isTRUE(input$montrer_ajuste)) {
        aj <- predire(modele(), d$date)
        g <- plotly::add_trace(
          g, x = aj$date, y = aj$prevision, type = "scatter", mode = "lines",
          line = list(color = ADP$amber, width = 1.2, dash = "dot"),
          name = "Modèle sur l'historique",
          hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:,.0f}<extra></extra>"
        )
      }

      g <- g |>
        plotly::add_ribbons(
          x = p$date, ymin = p$bas, ymax = p$haut,
          line = list(color = "transparent"),
          fillcolor = "rgba(228,87,46,0.16)",
          name = "Intervalle de prévision (90 %)",
          hoverinfo = "skip"
        ) |>
        plotly::add_trace(
          x = p$date, y = p$prevision, type = "scatter", mode = "lines",
          line = list(color = ADP$red, width = 2.2,
                      dash = if (any(p$extrapole)) "dash" else "solid"),
          name = "Prévision",
          hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>%{y:,.0f}<extra></extra>"
        )

      g |>
        adp_plotly(x_title = "", y_title = titre_y) |>
        plotly::layout(
          xaxis = axe_mois_fr(c(d$date, p$date)),
          shapes = list(list(
            type = "line", x0 = max(d$date), x1 = max(d$date),
            y0 = 0, y1 = 1, yref = "paper",
            line = list(color = ADP$slate, width = 1, dash = "dot")
          ))
        )
    })

    # --- validation ---
    output$plot_validation <- plotly::renderPlotly({
      v <- validation()
      shiny::validate(shiny::need(!is.null(v), "Série trop courte."))
      d <- v$detail
      plotly::plot_ly() |>
        plotly::add_trace(
          x = d$date, y = d$observe, type = "scatter", mode = "lines+markers",
          line = list(color = ADP$navy, width = 2), name = "Observé",
          marker = list(size = 5),
          hovertemplate = "<b>%{x|%d/%m}</b><br>%{y:,.0f}<extra></extra>"
        ) |>
        plotly::add_trace(
          x = d$date, y = d$modele, type = "scatter", mode = "lines",
          line = list(color = ADP$red, width = 2), name = "Modèle",
          hovertemplate = "<b>%{x|%d/%m}</b><br>%{y:,.0f}<extra></extra>"
        ) |>
        plotly::add_trace(
          x = d$date, y = d$naif, type = "scatter", mode = "lines",
          line = list(color = ADP$slate, width = 1.4, dash = "dot"),
          name = "Référence naïve",
          hovertemplate = "<b>%{x|%d/%m}</b><br>%{y:,.0f}<extra></extra>"
        ) |>
        adp_plotly(x_title = "", y_title = if (est_trafic()) "Vols par jour" else "Minutes") |>
        plotly::layout(
          xaxis = list(tickformat = "%d/%m", dtick = 7 * 86400000,
                       gridcolor = ADP$grid),
          margin = list(b = 80),
          legend = list(orientation = "h", y = -0.3, x = 0)
        )
    })

    output$note_calendrier <- shiny::renderUI({
      hint("Écart au lundi, toutes choses égales par ailleurs. C'est le rythme hebdomadaire que le modèle a appris : le samedi est de très loin le jour le plus creux.")
    })

    # --- effet du jour de la semaine ---
    output$plot_jsem <- plotly::renderPlotly({
      fit <- modele()
      co <- stats::coef(fit)
      eff <- co[grepl("^jsem", names(co))]
      niveaux <- as.integer(sub("^jsem", "", names(eff)))
      # le premier jour présent sert de référence, son effet vaut 0
      ref <- setdiff(1:7, niveaux)[1]
      df <- data.frame(
        jour = c(ref, niveaux),
        effet = c(0, unname(eff))
      )
      df <- df[order(df$jour), , drop = FALSE]
      multiplicatif <- isTRUE(attr(fit, "log_scale"))
      df$valeur <- if (multiplicatif) 100 * (exp(df$effet) - 1) else df$effet
      df$label <- factor(JOURS_FR[df$jour], levels = JOURS_FR)

      plotly::plot_ly(
        df, x = ~label, y = ~valeur, type = "bar",
        marker = list(color = ifelse(df$valeur >= 0, ADP$teal, ADP$red)),
        text = ~if (multiplicatif) fmt_pct(valeur) else fmt_min(valeur),
        textposition = "auto", textangle = 0,
        hovertemplate = "<b>%{x}</b><br>%{y:+.1f}<extra></extra>"
      ) |>
        adp_plotly(
          x_title = "",
          y_title = if (multiplicatif) "Écart au jour de référence (%)"
                    else "Écart au jour de référence (min)",
          legend = FALSE
        ) |>
        plotly::layout(xaxis = list(tickangle = -35, gridcolor = ADP$grid))
    })

    # --- table ---
    output$table_prevision <- DT::renderDT({
      p <- prevision()
      dec <- if (est_trafic()) 0 else 1
      out <- data.frame(
        Date = format(p$date, "%d/%m/%Y"),
        Jour = JOURS_FR[as.integer(format(p$date, "%u"))],
        Prévision = fmt_num(p$prevision, dec),
        `Fourchette basse` = fmt_num(p$bas, dec),
        `Fourchette haute` = fmt_num(p$haut, dec),
        Statut = ifelse(p$extrapole, "Extrapolé", "Estimé"),
        check.names = FALSE
      )
      adp_table(out, page_length = 10)
    })

    output$note_modele <- shiny::renderUI({
      d <- serie()
      fit <- modele()
      co <- stats::coef(fit)
      r2 <- summary(fit)$adj.r.squared
      pc <- function(nom) {
        if (!nom %in% names(co)) return(NULL)
        v <- if (isTRUE(attr(fit, "log_scale"))) 100 * (exp(co[[nom]]) - 1) else co[[nom]]
        sprintf("%+.1f %%", v)
      }
      eff_ferie <- pc("ferie")
      eff_samedi <- pc("jsem6")

      shiny::HTML(sprintf(
        "<p class='mb-2'><b>Ce que fait le modèle.</b> Une régression linéaire
         %s décomposant la série en cinq termes : une tendance journalière,
         un effet par mois, un effet par jour de la semaine, une indicatrice de
         jour férié et une indicatrice de période des fêtes. Ajustée sur %s jours,
         elle explique %s de la variance (R² ajusté).</p>
         <p class='mb-2'><b>Ce qu'elle a appris de plus net.</b> Un samedi vaut
         %s de trafic par rapport à un lundi, et un jour férié %s. Ce sont les
         deux coefficients qui pèsent le plus dans la prévision.</p>
         <p class='mb-2'><b>Pourquoi si simple.</b> L'énoncé écarte explicitement
         les modèles plus lourds et demande de traiter la saisonnalité : ici
         chaque coefficient est interprétable et la prévision reste vérifiable
         à la main.</p>
         <p class='mb-0'><b>Ses limites.</b> Elle ignore la météo et les
         mouvements sociaux, et ne connaît que les cinq jours fériés déclarés —
         pas les ponts ni les vacances scolaires. Les mois d'août et septembre
         étant absents des données, tout horizon qui les atteint est une
         extrapolation. Enfin la tendance est estimée sur une seule année :
         la prolonger loin dans le futur n'aurait pas de sens.</p>",
        if (isTRUE(attr(fit, "log_scale")))
          "sur le logarithme du nombre de vols, ce qui rend les effets multiplicatifs,"
        else "sur le retard moyen au départ,",
        fmt_int(nrow(d)),
        fmt_pct(100 * r2),
        eff_samedi %||% "—",
        eff_ferie %||% "—"
      ))
    })
  })
}
