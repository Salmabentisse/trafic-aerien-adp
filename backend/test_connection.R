# Test Point 1 — connexion MySQL (R)

user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

this_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(this_file)) {
  backend_dir <- dirname(normalizePath(sub("^--file=", "", this_file)))
} else if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) {
  backend_dir <- dirname(normalizePath(sys.frame(1)$ofile))
} else {
  backend_dir <- getwd()
}

project_root <- dirname(backend_dir)
renviron <- file.path(project_root, ".Renviron")
if (file.exists(renviron)) {
  readRenviron(renviron)
}

setwd(backend_dir)

suppressPackageStartupMessages({
  library(DBI)
  library(RMariaDB)
})

source("R/database.R")

cfg <- tryCatch(db_settings(), error = function(e) {
  message("Config : ", conditionMessage(e))
  quit(status = 1)
})

message(sprintf(
  "Tentative de connexion à %s:%s/%s ...",
  cfg$host, cfg$port, cfg$dbname
))

tryCatch({
  status <- check_connection()
  message("Connexion MySQL OK")
  message("  Base   : ", status$database)
  message("  Tables : ", status$tables_count)
}, error = function(e) {
  message("Échec de connexion (MySQL doit être démarré par l'équipe BD) :")
  message("  ", conditionMessage(e))
})

close_db()
