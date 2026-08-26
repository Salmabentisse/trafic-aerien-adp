# Helpers API — pagination, validation, erreurs HTTP, SQL sûr

stop_http <- function(status, detail, error = "http_error") {
  err <- structure(
    list(
      message = detail,
      status = as.integer(status),
      detail = detail,
      error = error
    ),
    class = c("http_error", "error", "condition")
  )
  stop(err)
}

with_db <- function(expr) {
  tryCatch(
    force(expr),
    error = function(e) {
      if (inherits(e, "http_error")) {
        stop(e)
      }
      stop_http(
        503,
        paste("Erreur SQL :", conditionMessage(e)),
        "database_unavailable"
      )
    }
  )
}

as_int <- function(value, default, min_value, max_value, field) {
  if (is.null(value) || (is.character(value) && !nzchar(value))) {
    n <- default
  } else {
    n <- suppressWarnings(as.integer(value))
  }
  if (length(n) != 1 || is.na(n) || n < min_value || n > max_value) {
    stop_http(
      422,
      sprintf("Paramètre '%s' invalide.", field)
    )
  }
  n
}

as_int_opt <- function(value, min_value, max_value, field) {
  if (is.null(value) || (is.character(value) && !nzchar(value))) {
    return(NULL)
  }
  n <- suppressWarnings(as.integer(value))
  if (length(n) != 1 || is.na(n) || n < min_value || n > max_value) {
    stop_http(422, sprintf("Paramètre '%s' invalide.", field))
  }
  n
}

as_bool <- function(value, default = FALSE) {
  if (is.null(value) || (is.character(value) && !nzchar(value))) {
    return(default)
  }
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(value)
  }
  tolower(as.character(value)) %in% c("true", "1", "yes", "oui")
}

normalize_iata <- function(value, field, min_len = 2, max_len = 3) {
  if (is.null(value) || !nzchar(value)) {
    return(NULL)
  }
  code <- toupper(trimws(value))
  if (!grepl(sprintf("^[A-Z]{%d,%d}$", min_len, max_len), code)) {
    stop_http(
      400,
      sprintf("Paramètre '%s' invalide : '%s' (attendu : %d–%d lettres).",
              field, value, min_len, max_len)
    )
  }
  code
}

sql_quote <- function(value) {
  as.character(DBI::dbQuoteString(get_con(), as.character(value)))
}

sql_like <- function(value) {
  sql_quote(value)
}

df_to_records <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    names(row) <- names(df)
    lapply(row, function(x) {
      if (length(x) == 0 || is.na(x)) {
        return(NULL)
      }
      if (inherits(x, "POSIXt") || inherits(x, "Date")) {
        return(format(x, "%Y-%m-%dT%H:%M:%S"))
      }
      if (is.factor(x)) {
        return(as.character(x))
      }
      x
    })
  })
}

row_to_object <- function(df) {
  recs <- df_to_records(df)
  if (length(recs) == 0) {
    return(NULL)
  }
  recs[[1]]
}

paginated <- function(page, limit, total, df) {
  pages <- if (total == 0) 0 else as.integer(ceiling(total / limit))
  list(
    page = page,
    limit = limit,
    total = as.integer(total),
    pages = pages,
    data = df_to_records(df)
  )
}

traffic_period_sql <- function(period) {
  if (is.null(period) || !nzchar(period)) {
    stop_http(422, "Paramètre 'period' obligatoire.")
  }
  switch(
    period,
    january = "month = 1",
    november_december = "month IN (11, 12)",
    summer = "month IN (7, 8, 9)",
    christmas = "month = 12 AND day = 25",
    new_year = "month = 1 AND day = 1",
    independence = "month = 7 AND day = 4",
    thanksgiving = "month = 11 AND day = 29",
    overnight = "hour IS NOT NULL AND hour BETWEEN 0 AND 6",
    special_days = paste(
      "(month = 1 AND day = 1) OR",
      "(month = 7 AND day = 4) OR",
      "(month = 11 AND day = 29) OR",
      "(month = 12 AND day = 25)"
    ),
    stop_http(422, "Période inconnue.")
  )
}

flight_order_sql <- function(sort_by, sort_dir) {
  if (is.null(sort_by) || !nzchar(sort_by)) sort_by <- "id"
  if (is.null(sort_dir) || !nzchar(sort_dir)) sort_dir <- "asc"
  sort_dir <- tolower(sort_dir)
  if (!sort_dir %in% c("asc", "desc")) {
    stop_http(422, "Paramètre 'sort_dir' invalide (asc|desc).")
  }
  col <- switch(
    sort_by,
    id = "f.id",
    dep_delay = "f.dep_delay IS NULL, f.dep_delay",
    arr_delay = "f.arr_delay IS NULL, f.arr_delay",
    distance = "f.distance IS NULL, f.distance",
    date = "f.year, f.month, f.day, f.hour",
    stop_http(422, "Paramètre 'sort_by' invalide.")
  )
  sprintf("%s %s", col, toupper(sort_dir))
}
