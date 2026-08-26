# Lance le dashboard Shiny : http://127.0.0.1:3838
#
#   Rscript frontend/run_app.R
#
# Packages : install.packages(c("shiny", "bslib", "plotly", "DT", "httr2"))
# L'API backend doit tourner (Rscript backend/run.R) ; sinon le dashboard
# s'ouvre quand même et affiche « API injoignable ».

user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

this_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
app_dir <- if (length(this_file)) {
  dirname(normalizePath(sub("^--file=", "", this_file)))
} else {
  getwd()
}

# .Renviron de la racine : permet de surcharger API_BASE_URL avec le reste
renviron <- file.path(dirname(app_dir), ".Renviron")
if (file.exists(renviron)) readRenviron(renviron)

port <- as.integer(Sys.getenv("SHINY_PORT", unset = "3838"))
message("Dashboard Trafic Aérien ADP — http://127.0.0.1:", port)
message("API interrogée              — ",
        Sys.getenv("API_BASE_URL", unset = "http://127.0.0.1:8000"))

shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = FALSE)
