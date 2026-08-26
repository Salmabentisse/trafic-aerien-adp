# Lance l'API Plumber : http://127.0.0.1:8000
# Docs Swagger : http://127.0.0.1:8000/__docs__/
#
# Packages : install.packages(c("plumber", "DBI", "RMariaDB"))

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
  library(plumber)
  library(DBI)
  library(RMariaDB)
})

source(file.path(backend_dir, "R", "database.R"))
on.exit(close_db(), add = TRUE)

pr <- plumber::plumb(file.path(backend_dir, "plumber.R"))
message("API Trafic Aérien ADP — http://127.0.0.1:8000")
message("Documentation      — http://127.0.0.1:8000/__docs__/")
pr$run(host = "127.0.0.1", port = 8000)
