# Connexion MySQL — Point 1
# Variables : .Renviron à la racine (DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASS)

.adp_con <- NULL

user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

require_env <- function(name) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(sprintf(
      "Variable d'environnement manquante : %s. Vérifiez le fichier .Renviron à la racine.",
      name
    ), call. = FALSE)
  }
  value
}

db_settings <- function() {
  list(
    host = require_env("DB_HOST"),
    port = as.integer(require_env("DB_PORT")),
    dbname = require_env("DB_NAME"),
    user = require_env("DB_USER"),
    password = require_env("DB_PASS")
  )
}

cors_origins <- function() {
  raw <- Sys.getenv(
    "CORS_ORIGINS",
    unset = "http://localhost:3000,http://localhost:5173,http://127.0.0.1:3000"
  )
  trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
}

close_db <- function() {
  if (!is.null(.adp_con)) {
    try(DBI::dbDisconnect(.adp_con), silent = TRUE)
    .adp_con <<- NULL
  }
  invisible(NULL)
}

db_driver <- function() {
  RMariaDB::MariaDB()
}

connection_is_alive <- function(con) {
  tryCatch({
    DBI::dbGetQuery(con, "SELECT 1 AS ok")
    TRUE
  }, error = function(e) FALSE)
}

get_con <- function() {
  if (!is.null(.adp_con) && connection_is_alive(.adp_con)) {
    return(.adp_con)
  }
  close_db()

  cfg <- db_settings()
  .adp_con <<- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = cfg$host,
    port = cfg$port,
    dbname = cfg$dbname,
    user = cfg$user,
    password = cfg$password
  )
  .adp_con
}

check_connection <- function() {
  con <- get_con()
  row <- DBI::dbGetQuery(con, "SELECT DATABASE() AS db_name")
  tables <- DBI::dbListTables(con)
  list(
    connected = TRUE,
    database = row$db_name[[1]],
    tables_count = length(tables)
  )
}

query_df <- function(sql) {
  con <- get_con()
  DBI::dbGetQuery(con, sql)
}

query_scalar <- function(sql) {
  df <- query_df(sql)
  as.integer(df[[1]][[1]])
}
