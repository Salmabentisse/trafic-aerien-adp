# ============================================================================
# Thème, palette, formatage et helpers de présentation
# ============================================================================

ADP <- list(
  navy  = "#0B3C5D",
  blue  = "#1D8AC1",
  sky   = "#7FB2D4",
  teal  = "#2E9E6B",
  amber = "#E8A33D",
  red   = "#E4572E",
  slate = "#5A6B7B",
  grid  = "#E3E9EF",
  light = "#F4F7FA",
  ink   = "#12202B"
)

# Couleurs fixes par aéroport d'origine : même code couleur partout
ORIGIN_COLORS <- c(JFK = ADP$navy, LGA = ADP$blue, EWR = ADP$teal)

# L'échelle nommée "YlOrRd" de plotly.js est inversée par rapport à ColorBrewer
# (valeurs hautes en jaune pâle) : on la définit explicitement.
SCALE_YELLOW_RED <- list(
  list(0,    "#FFF0A8"),
  list(0.35, "#F6C263"),
  list(0.7,  "#E8823D"),
  list(1,    "#B3261E")
)

MONTHS_FR <- c("janv.", "févr.", "mars", "avr.", "mai", "juin",
               "juil.", "août", "sept.", "oct.", "nov.", "déc.")

PERIODS <- c(
  "Janvier"                     = "january",
  "Novembre–décembre"           = "november_december",
  "Été (juil.–sept.)"           = "summer",
  "Noël (25/12)"                = "christmas",
  "Nouvel An (01/01)"           = "new_year",
  "Independence Day (04/07)"    = "independence",
  "Thanksgiving (29/11)"        = "thanksgiving",
  "Vols de nuit (0h–6h)"        = "overnight",
  "Tous les jours fériés"       = "special_days"
)

# Mois couverts par chaque période, pour avertir quand la sélection tombe dans
# une zone sans données
PERIOD_MONTHS <- list(
  january = 1L, november_december = c(11L, 12L), summer = c(7L, 8L, 9L),
  christmas = 12L, new_year = 1L, independence = 7L, thanksgiving = 11L,
  overnight = 1:12, special_days = c(1L, 7L, 11L, 12L)
)

SPECIAL_DAY_LABELS <- c(
  new_year     = "Nouvel An",
  independence = "Independence Day",
  thanksgiving = "Thanksgiving",
  christmas    = "Noël"
)

adp_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = ADP$ink,
    primary = ADP$navy,
    secondary = ADP$slate,
    success = ADP$teal,
    warning = ADP$amber,
    danger = ADP$red,
    base_font = bslib::font_collection(
      "Helvetica Neue", "Segoe UI", "Roboto", "Arial", "sans-serif"
    ),
    "navbar-brand-font-weight" = "600"
  )
}

# --- formatage --------------------------------------------------------------

fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", formatC(x, format = "d", big.mark = " "))
}

fmt_num <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—",
         formatC(round(x, digits), format = "f", digits = digits, big.mark = " "))
}

fmt_pct <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(fmt_num(x, digits), " %"))
}

fmt_min <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(fmt_num(x, digits), " min"))
}

fmt_signed_min <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—",
         paste0(ifelse(x > 0, "+", ""), fmt_num(x, digits), " min"))
}

month_label <- function(m) {
  m <- suppressWarnings(as.integer(m))
  ifelse(is.na(m) | m < 1 | m > 12, "—", MONTHS_FR[m])
}

#' Étiquette de date à partir des colonnes year/month/day
date_label <- function(df) {
  sprintf("%02d/%02d/%s",
          as.integer(df$day), as.integer(df$month), as.integer(df$year))
}

#' Heure HHMM (entier) -> "HH:MM"
hhmm <- function(x) {
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), "—", sprintf("%02d:%02d", x %/% 100, x %% 100))
}

# --- composants UI ----------------------------------------------------------

#' Carte KPI (bslib value_box) avec icône Font Awesome fournie par Shiny
kpi_box <- function(title, value, icon = "plane", theme = "primary",
                    subtitle = NULL) {
  bslib::value_box(
    title = title,
    value = value,
    if (!is.null(subtitle)) shiny::p(subtitle, class = "mb-0 small") else NULL,
    showcase = shiny::icon(icon),
    theme = theme
  )
}

#' Bandeau d'explication au-dessus d'un graphique
hint <- function(...) {
  shiny::p(..., class = "text-body-secondary small mb-2")
}

#' Titre de section
section_title <- function(text, sub = NULL) {
  shiny::div(
    class = "mb-3",
    shiny::h4(text, class = "mb-0"),
    if (!is.null(sub)) shiny::p(sub, class = "text-body-secondary mb-0") else NULL
  )
}

# --- graphiques -------------------------------------------------------------

#' Mise en forme commune des graphiques plotly
adp_plotly <- function(p, x_title = "", y_title = "", legend = TRUE,
                       hovermode = "closest") {
  p <- plotly::layout(
    p,
    font = list(family = "Helvetica Neue, Segoe UI, Arial, sans-serif",
                size = 13, color = ADP$ink),
    margin = list(l = 60, r = 20, t = 30, b = 60),
    xaxis = list(title = x_title, gridcolor = ADP$grid, zeroline = FALSE),
    yaxis = list(title = y_title, gridcolor = ADP$grid, zeroline = FALSE),
    hovermode = hovermode,
    showlegend = legend,
    legend = list(orientation = "h", y = -0.2, x = 0),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    # masque les étiquettes qui ne tiennent pas plutôt que de les rétrécir
    uniformtext = list(minsize = 9, mode = "hide")
  )
  plotly::config(p, displayModeBar = FALSE, locale = "fr")
}

#' Message affiché quand une requête ne renvoie rien
need_rows <- function(df, msg = "Aucune donnée pour ces filtres.") {
  shiny::validate(shiny::need(is.data.frame(df) && nrow(df) > 0, msg))
  invisible(TRUE)
}

#' Couleur par aéroport d'origine (fallback bleu)
origin_color <- function(codes) {
  out <- unname(ORIGIN_COLORS[as.character(codes)])
  out[is.na(out)] <- ADP$slate
  out
}

# --- tables -----------------------------------------------------------------

#' DataTable configurée en français, sobre et exportable visuellement
adp_table <- function(df, page_length = 10, scroll_x = TRUE) {
  DT::datatable(
    df,
    rownames = FALSE,
    selection = "none",
    class = "stripe hover row-border",
    options = list(
      pageLength = page_length,
      lengthChange = FALSE,
      scrollX = scroll_x,
      dom = "tip",
      language = list(
        search = "Rechercher :",
        info = "_START_ à _END_ sur _TOTAL_ lignes",
        infoEmpty = "Aucune ligne",
        emptyTable = "Aucune donnée",
        zeroRecords = "Aucun résultat",
        paginate = list(previous = "Précédent", `next` = "Suivant")
      )
    )
  )
}

# --- complétude temporelle --------------------------------------------------
# Le jeu de données ne couvre pas toute l'année (août et septembre sont absents
# de flights.xlsx). Sans réindexation, plotly relie les points de part et d'autre
# du trou et laisse croire à un trafic continu.

#' Réindexe un data.frame journalier sur toutes les dates de la plage observée.
#' Les jours absents deviennent NA (donc des trous dans les courbes).
complete_days <- function(df, date_col = "date") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  full <- data.frame(seq(min(df[[date_col]]), max(df[[date_col]]), by = "day"))
  names(full) <- date_col
  merge(full, df, by = date_col, all.x = TRUE)
}

#' Réindexe une série mensuelle sur les 12 mois
complete_months <- function(df, month_col = "month") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  full <- data.frame(seq_len(12L))
  names(full) <- month_col
  merge(full, df, by = month_col, all.x = TRUE)
}

#' Phrase décrivant la couverture réelle du jeu de données
coverage_note <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  jours <- nrow(unique(df[, c("year", "month", "day")]))
  par_mois <- tapply(df$day, df$month, function(x) length(unique(x)))
  attendus <- vapply(as.integer(names(par_mois)), function(m) {
    as.integer(format(
      seq(as.Date(sprintf("%d-%02d-01", df$year[1], m)), by = "month",
          length.out = 2)[2] - 1, "%d"))
  }, integer(1))
  absents <- setdiff(1:12, as.integer(names(par_mois)))
  partiels <- as.integer(names(par_mois))[par_mois < attendus]

  txt <- sprintf("Couverture du jeu de données : %s jours sur l'année %d.",
                 fmt_int(jours), df$year[1])
  if (length(absents)) {
    txt <- paste(txt, sprintf("Mois totalement absents : %s.",
                              paste(month_label(absents), collapse = ", ")))
  }
  if (length(partiels)) {
    txt <- paste(txt, sprintf("Mois incomplets : %s.",
                              paste(sprintf("%s (%d j)", month_label(partiels),
                                            par_mois[as.character(partiels)]),
                                    collapse = ", ")))
  }
  paste(txt, "Les courbes laissent ces périodes vides plutôt que de les relier.")
}

#' Nombre de jours du mois (y, m)
jours_du_mois <- function(y, m) {
  vapply(seq_along(m), function(i) {
    debut <- as.Date(sprintf("%04d-%02d-01", as.integer(y[i]), as.integer(m[i])))
    as.integer(format(seq(debut, by = "month", length.out = 2)[2] - 1, "%d"))
  }, integer(1))
}

#' Découpe une série journalière en plages contiguës de jours renseignés.
#' `fill = "tozeroy"` de plotly relie les deux bords d'un trou : il faut une
#' trace par plage pour que le remplissage s'interrompe vraiment.
plages_contigues <- function(df, value_col) {
  present <- !is.na(df[[value_col]])
  if (!any(present)) return(df[0, , drop = FALSE])
  df$plage <- cumsum(c(TRUE, present[-1] & !present[-length(present)]))
  df[present, , drop = FALSE]
}

#' Graduations mensuelles en français pour un axe de dates
#' (le locale plotly ne francise pas les abréviations de mois de façon fiable)
axe_mois_fr <- function(dates) {
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return(NULL)
  debuts <- seq(as.Date(format(min(dates), "%Y-%m-01")), max(dates), by = "month")
  list(
    tickmode = "array",
    tickvals = as.character(debuts),
    ticktext = paste(MONTHS_FR[as.integer(format(debuts, "%m"))],
                     format(debuts, "%Y")),
    gridcolor = ADP$grid
  )
}

#' Graduations mensuelles françaises, seulement si la période le justifie
#' (sur quelques jours, des graduations mensuelles n'apporteraient rien)
axe_temps_fr <- function(ts, seuil_jours = 45) {
  ts <- ts[!is.na(ts)]
  if (!length(ts)) return(NULL)
  etendue <- as.numeric(difftime(max(ts), min(ts), units = "days"))
  if (etendue < seuil_jours) return(list(gridcolor = ADP$grid))
  axe_mois_fr(as.Date(ts))
}
