# ============================================================================
# Dashboard Trafic Aérien ADP — partie « Dashboard / Frontend & présentation »
#
# Consomme exclusivement l'API REST Plumber du backend (aucun accès direct à
# MySQL). Lancement :
#   Rscript frontend/run_app.R          (ou shiny::runApp("frontend"))
# Variable d'environnement facultative :
#   API_BASE_URL (défaut http://127.0.0.1:8000)
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
  library(httr2)
})

# Shiny source automatiquement les fichiers de R/. Filet de sécurité au cas où
# l'app est lancée autrement (Rscript app.R, source() manuel...).
if (!exists("api_get")) {
  app_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
  r_dir <- file.path(app_dir, "R")
  if (!dir.exists(r_dir)) r_dir <- "R"
  for (f in sort(list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    source(f)
  }
}

# ---------------------------------------------------------------- interface ---

ui <- bslib::page_navbar(
  title = shiny::tags$span(
    shiny::icon("plane-departure"), " Trafic Aérien ADP"
  ),
  window_title = "Trafic Aérien ADP — Dashboard",
  theme = adp_theme(),
  fillable = FALSE,
  id = "nav",

  bslib::nav_panel("Vue d'ensemble", mod_overview_ui("overview")),
  bslib::nav_panel("Trafic", mod_traffic_ui("traffic")),
  bslib::nav_panel("Retards", mod_delays_ui("delays")),
  bslib::nav_panel("Annulations", mod_cancellations_ui("cancel")),
  bslib::nav_panel("Carte", mod_map_ui("map")),
  bslib::nav_panel("Météo", mod_weather_ui("weather")),
  bslib::nav_panel("Vols", mod_flights_ui("flights")),

  bslib::nav_panel(
    "Données & méthode",
    section_title("Sources, définitions et architecture"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("D'où viennent les chiffres"),
        bslib::card_body(shiny::HTML(
          "<p>Vols au départ des trois aéroports de New York (JFK, LGA, EWR),
           données <em>Bureau of Transportation Statistics</em>.</p>
           <ul>
             <li><code>airlines.json</code> — compagnies</li>
             <li><code>airports.xlsx</code> — aéroports (coordonnées, fuseau)</li>
             <li><code>flights.xlsx</code> — vols</li>
             <li><code>planes.html</code> — flotte (page web parsée)</li>
             <li><code>weather.pdf</code> — relevés météo horaires (PDF extrait)</li>
           </ul>
           <p class='mb-0'>Nettoyage et injection : <code>data/05_injection_mysql.R</code>.
           Schéma et contraintes : <code>schema.sql</code>.</p>"
        ))
      ),
      bslib::card(
        bslib::card_header("Conventions de lecture"),
        bslib::card_body(shiny::HTML(
          "<ul>
             <li><strong>Vol en retard</strong> : retard strictement positif.</li>
             <li><strong>Vol annulé</strong> : heure de départ <em>et</em>
                 heure d'arrivée absentes.</li>
             <li><strong>Retard négatif</strong> : vol en avance.</li>
             <li><strong>Gain en vol</strong> : différence entre retard au départ
                 et retard à l'arrivée.</li>
             <li>Honolulu (HNL) est écarté des classements de retard par
                 destination : distance atypique qui écrase les échelles.</li>
             <li>Températures converties de °F en °C pour l'affichage.</li>
           </ul>"
        ))
      )
    ),
    bslib::card(
      class = "mt-3",
      bslib::card_header("Chaîne de traitement"),
      bslib::card_body(shiny::HTML(
        "<pre class='mb-2' style='background:#F4F7FA;padding:1rem;border-radius:.5rem'>
Fichiers (xlsx, json, html, pdf)
        │  nettoyage + injection (R)
        ▼
   MySQL 8  ──  schema.sql : airlines, airports, planes, weather, flights
        │  requêtes SQL
        ▼
 API REST Plumber (R) — :8000
        │  JSON
        ▼
 Dashboard Shiny (ce site)</pre>
         <p class='mb-0 text-body-secondary'>Le dashboard n'ouvre aucune
         connexion à la base : il n'interroge que l'API, ce qui permet de
         déployer les deux séparément.</p>"
      ))
    )
  ),

  bslib::nav_spacer(),
  bslib::nav_item(shiny::uiOutput("api_badge")),

  footer = shiny::div(
    class = "container-fluid text-body-secondary small py-3 border-top mt-4",
    shiny::span("Projet Trafic Aérien ADP — dashboard Shiny alimenté par l'API Plumber."),
    shiny::span(class = "ms-2", shiny::textOutput("api_url", inline = TRUE))
  )
)

# ------------------------------------------------------------------ serveur ---

server <- function(input, output, session) {

  mod_overview_server("overview")
  mod_traffic_server("traffic")
  mod_delays_server("delays")
  mod_cancellations_server("cancel")
  mod_map_server("map")
  mod_weather_server("weather")
  mod_flights_server("flights")

  # État de l'API, rafraîchi toutes les 30 s
  health <- shiny::reactivePoll(
    30000, session,
    checkFunc = function() Sys.time(),
    valueFunc = function() api_get("/health")
  )

  output$api_badge <- shiny::renderUI({
    h <- health()
    if (isTRUE(h$ok)) {
      tables <- as_scalar(h$data$database$tables_count)
      shiny::span(
        class = "navbar-text",
        shiny::span(class = "badge bg-success", shiny::icon("circle-check"), " API connectée"),
        shiny::span(class = "ms-2 small text-white-50",
                    sprintf("%s tables", fmt_int(tables)))
      )
    } else {
      shiny::span(
        class = "navbar-text",
        shiny::span(class = "badge bg-danger", shiny::icon("circle-xmark"), " API injoignable")
      )
    }
  })

  output$api_url <- shiny::renderText(paste("API :", api_base_url()))
}

shiny::shinyApp(ui, server)
