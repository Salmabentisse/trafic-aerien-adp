# ============================================================================
# Onglet « Explorateur de vols » — recherche filtrée sur la table flights
# Endpoint : /flights (pagination et tri assurés par l'API, pas par le client)
# ============================================================================

mod_flights_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    section_title(
      "Explorateur de vols",
      "Les filtres, le tri et la pagination sont exécutés côté API : le dashboard ne charge qu'une page à la fois."
    ),
    bslib::layout_columns(
      col_widths = c(3, 9),
      bslib::card(
        bslib::card_header("Filtres"),
        bslib::card_body(
          shiny::selectInput(ns("carrier"), "Compagnie", choices = c("Toutes" = "")),
          shiny::selectInput(ns("origin"), "Départ", choices = c("Tous" = "")),
          shiny::selectizeInput(ns("dest"), "Destination", choices = c("Toutes" = ""),
                                options = list(placeholder = "Rechercher…")),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectInput(
              ns("month"), "Mois",
              choices = c("Tous" = "", stats::setNames(as.character(1:12), MONTHS_FR))
            ),
            shiny::selectInput(
              ns("day"), "Jour",
              choices = c("Tous" = "", stats::setNames(as.character(1:31), 1:31))
            )
          ),
          shiny::numericInput(ns("min_delay"), "Retard au départ ≥ (min)",
                              value = NA, min = -100, max = 2000, step = 15),
          shiny::checkboxInput(ns("cancelled"), "Vols annulés uniquement", FALSE),
          shiny::hr(),
          shiny::selectInput(
            ns("sort_by"), "Trier par",
            choices = c("Identifiant" = "id", "Date" = "date",
                        "Retard au départ" = "dep_delay",
                        "Retard à l'arrivée" = "arr_delay",
                        "Distance" = "distance")
          ),
          shiny::radioButtons(ns("sort_dir"), "Sens",
                              choices = c("Croissant" = "asc", "Décroissant" = "desc"),
                              selected = "asc", inline = TRUE),
          shiny::selectInput(ns("limit"), "Vols par page",
                             choices = c(25, 50, 100, 200), selected = 50),
          shiny::actionButton(ns("reset"), "Réinitialiser",
                              icon = shiny::icon("rotate-left"),
                              class = "btn-outline-secondary btn-sm w-100")
        )
      ),
      bslib::card(
        bslib::card_header(shiny::textOutput(ns("summary"), inline = TRUE)),
        bslib::card_body(
          shiny::div(
            class = "d-flex align-items-center gap-2 mb-3",
            shiny::actionButton(ns("prev"), "Précédent",
                                icon = shiny::icon("chevron-left"),
                                class = "btn-outline-primary btn-sm"),
            shiny::actionButton(ns("next"), "Suivant",
                                icon = shiny::icon("chevron-right"),
                                class = "btn-outline-primary btn-sm"),
            shiny::div(class = "ms-2 text-body-secondary small",
                       shiny::textOutput(ns("page_info"), inline = TRUE))
          ),
          DT::DTOutput(ns("table"))
        )
      )
    )
  )
}

mod_flights_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    page <- shiny::reactiveVal(1)

    shiny::observe({
      shiny::updateSelectInput(
        session, "carrier",
        choices = choices_from(ref_airlines(), "carrier", "name", "Toutes")
      )
      shiny::updateSelectInput(
        session, "origin",
        choices = choices_from(ref_origins(), "origin", "airport_name", "Tous")
      )
      shiny::updateSelectizeInput(
        session, "dest",
        choices = choices_from(ref_airports(), "faa", "name", "Toutes"),
        server = TRUE
      )
    })

    # Tout changement de filtre ramène à la première page
    shiny::observeEvent(
      list(input$carrier, input$origin, input$dest, input$month, input$day,
           input$min_delay, input$cancelled, input$sort_by, input$sort_dir,
           input$limit),
      page(1),
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$reset, {
      shiny::updateSelectInput(session, "carrier", selected = "")
      shiny::updateSelectInput(session, "origin", selected = "")
      shiny::updateSelectizeInput(session, "dest", selected = "")
      shiny::updateSelectInput(session, "month", selected = "")
      shiny::updateSelectInput(session, "day", selected = "")
      shiny::updateNumericInput(session, "min_delay", value = NA)
      shiny::updateCheckboxInput(session, "cancelled", value = FALSE)
      shiny::updateSelectInput(session, "sort_by", selected = "id")
      shiny::updateRadioButtons(session, "sort_dir", selected = "asc")
      page(1)
    })

    result <- shiny::reactive({
      api_paginated("/flights", list(
        page = page(),
        limit = input$limit,
        carrier = input$carrier,
        origin = input$origin,
        dest = input$dest,
        month = input$month,
        day = input$day,
        min_dep_delay = if (isTRUE(is.na(input$min_delay))) "" else input$min_delay,
        cancelled_only = if (isTRUE(input$cancelled)) "true" else "",
        sort_by = input$sort_by,
        sort_dir = input$sort_dir
      ))
    })

    shiny::observeEvent(input$prev, {
      if (page() > 1) page(page() - 1)
    })
    shiny::observeEvent(input[["next"]], {
      if (page() < result()$pages) page(page() + 1)
    })

    output$summary <- shiny::renderText({
      sprintf("%s vols correspondent aux filtres", fmt_int(result()$total))
    })

    output$page_info <- shiny::renderText({
      r <- result()
      if (r$pages == 0) return("Aucune page")
      sprintf("Page %s sur %s", fmt_int(r$page), fmt_int(r$pages))
    })

    output$table <- DT::renderDT({
      df <- result()$data
      need_rows(df, "Aucun vol ne correspond à ces filtres.")
      out <- data.frame(
        Date = date_label(df),
        Compagnie = paste0(df$carrier, " — ", df$airline_name),
        `N° vol` = fmt_int(df$flight),
        Départ = paste0(df$origin, " ", hhmm(df$dep_time)),
        Arrivée = paste0(df$dest, " ", hhmm(df$arr_time)),
        `Retard départ` = fmt_signed_min(df$dep_delay, 0),
        `Retard arrivée` = fmt_signed_min(df$arr_delay, 0),
        `Distance (mi)` = fmt_int(df$distance),
        `Temps de vol` = fmt_min(df$air_time, 0),
        Statut = ifelse(df$is_cancelled == 1, "Annulé", "Réalisé"),
        check.names = FALSE
      )
      adp_table(out, page_length = 15)
    })
  })
}
