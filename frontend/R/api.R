# ============================================================================
# Client de l'API REST Trafic Aérien ADP (Plumber)
#
# Le dashboard ne parle JAMAIS directement à MySQL : l'API du backend est la
# seule source de données. Conventions du backend (backend/R/utils.R) :
#   - endpoints paginés  -> {page, limit, total, pages, data:[...]}
#   - valeurs manquantes -> objet vide {} dans le JSON  (converti en NA ici)
# ============================================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

api_base_url <- function() {
  sub("/+$", "", Sys.getenv("API_BASE_URL", unset = "http://127.0.0.1:8000"))
}

api_timeout <- function() {
  suppressWarnings(as.numeric(Sys.getenv("API_TIMEOUT", unset = "90"))) %||% 90
}

# --- petit cache mémoire (TTL) pour les référentiels : compagnies, aéroports --
.api_cache <- new.env(parent = emptyenv())

api_cached <- function(key, compute, ttl = 300) {
  now <- as.numeric(Sys.time())
  hit <- .api_cache[[key]]
  if (!is.null(hit) && (now - hit$at) < ttl) return(hit$value)
  value <- compute()
  assign(key, list(at = now, value = value), envir = .api_cache)
  value
}

api_clear_cache <- function() {
  rm(list = ls(envir = .api_cache), envir = .api_cache)
  invisible(NULL)
}

# --- appel HTTP --------------------------------------------------------------

# Retire les paramètres vides pour laisser le backend appliquer ses défauts.
drop_empty_query <- function(query) {
  if (length(query) == 0) return(list())
  keep <- vapply(query, function(v) {
    !is.null(v) && length(v) == 1 && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  query[keep]
}

#' Appelle l'API et renvoie toujours une liste (ok, status, data, message)
api_get <- function(path, query = list()) {
  url <- paste0(api_base_url(), path)
  tryCatch({
    resp <- httr2::request(url)
    q <- drop_empty_query(query)
    if (length(q)) resp <- httr2::req_url_query(resp, !!!q)
    resp <- httr2::req_timeout(resp, api_timeout())
    resp <- httr2::req_user_agent(resp, "adp-dashboard/1.0 (shiny)")
    resp <- httr2::req_error(resp, is_error = function(r) FALSE)
    resp <- httr2::req_perform(resp)

    status <- httr2::resp_status(resp)
    body <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (status >= 400) {
      detail <- body$detail %||% paste("Erreur HTTP", status)
      list(ok = FALSE, status = status, data = NULL,
           message = sprintf("%s (%s)", detail, path))
    } else {
      list(ok = TRUE, status = status, data = body, message = NULL)
    }
  }, error = function(e) {
    list(
      ok = FALSE, status = NA_integer_, data = NULL,
      message = sprintf(
        "API injoignable sur %s — lancer le backend (Rscript backend/run.R).\n%s",
        api_base_url(), conditionMessage(e)
      )
    )
  })
}

#' Interrompt proprement un render Shiny quand l'API ne répond pas
api_guard <- function(res) {
  shiny::validate(shiny::need(
    isTRUE(res$ok),
    res$message %||% "Données indisponibles."
  ))
  invisible(TRUE)
}

# --- normalisation JSON -> data.frame ---------------------------------------

# Le backend sérialise les NA en {} : on les ramène à NA.
as_scalar <- function(v) {
  if (is.null(v)) return(NA)
  if (is.list(v)) {
    if (length(v) == 0) return(NA)
    v <- unlist(v, use.names = FALSE)
  }
  if (length(v) == 0) return(NA)
  v[[1]]
}

#' Liste d'enregistrements JSON -> data.frame typé (numérique si possible)
records_to_df <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(data.frame())
  }
  if (is.data.frame(records)) return(records)
  keys <- unique(unlist(lapply(records, names)))
  if (length(keys) == 0) return(data.frame())

  cols <- lapply(keys, function(k) {
    chr <- vapply(records, function(r) {
      v <- as_scalar(r[[k]])
      if (length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v)
    }, character(1))
    num <- suppressWarnings(as.numeric(chr))
    # conversion numérique acceptée seulement si elle ne perd aucune valeur
    if (all(is.na(num) == is.na(chr))) num else chr
  })
  names(cols) <- keys
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Objet JSON unique -> liste de scalaires
object_to_list <- function(obj) {
  if (is.null(obj) || length(obj) == 0) return(list())
  out <- lapply(obj, as_scalar)
  names(out) <- names(obj)
  out
}

# --- raccourcis d'accès -----------------------------------------------------

#' GET renvoyant un data.frame ; `key` = champ contenant le tableau (NULL = racine)
api_df <- function(path, query = list(), key = "data") {
  res <- api_get(path, query)
  api_guard(res)
  recs <- if (is.null(key)) res$data else res$data[[key]]
  records_to_df(recs)
}

#' GET renvoyant un objet unique (ex. /stats/overview)
api_obj <- function(path, query = list()) {
  res <- api_get(path, query)
  api_guard(res)
  object_to_list(res$data)
}

#' GET sur un endpoint paginé -> list(page, limit, total, pages, data = df)
api_paginated <- function(path, query = list()) {
  res <- api_get(path, query)
  api_guard(res)
  d <- res$data
  list(
    page  = as_scalar(d$page)  %||% 1,
    limit = as_scalar(d$limit) %||% 0,
    total = as_scalar(d$total) %||% 0,
    pages = as_scalar(d$pages) %||% 0,
    data  = records_to_df(d$data)
  )
}

# --- référentiels (mis en cache) -------------------------------------------

ref_airlines <- function() {
  api_cached("airlines", function() {
    res <- api_get("/airlines", list(limit = 500))
    if (!isTRUE(res$ok)) return(data.frame())
    records_to_df(res$data$data)
  })
}

ref_origins <- function() {
  api_cached("origins", function() {
    res <- api_get("/delays/by-origin-airport")
    if (!isTRUE(res$ok)) return(data.frame())
    records_to_df(res$data$data)
  })
}

#' Choix "CODE — Nom" pour les selectInput, avec option "Tous"
choices_from <- function(df, code_col, label_col, all_label = "Tous") {
  out <- stats::setNames("", all_label)
  if (is.null(df) || nrow(df) == 0 || !code_col %in% names(df)) return(out)
  codes <- as.character(df[[code_col]])
  labels <- if (label_col %in% names(df)) {
    paste0(codes, " — ", as.character(df[[label_col]]))
  } else {
    codes
  }
  c(out, stats::setNames(codes, labels))
}

#' Référentiel complet des aéroports (paginé côté API : 500 max par appel)
ref_airports <- function() {
  api_cached("airports", function() {
    out <- data.frame()
    page <- 1
    repeat {
      res <- api_get("/airports", list(page = page, limit = 500))
      if (!isTRUE(res$ok)) break
      df <- records_to_df(res$data$data)
      if (nrow(df) == 0) break
      out <- if (nrow(out) == 0) df else rbind(out, df)
      pages <- as_scalar(res$data$pages) %||% 1
      if (page >= pages || page >= 6) break
      page <- page + 1
    }
    out
  }, ttl = 3600)
}

#' Aéroports pour lesquels la table weather contient réellement des relevés.
#' L'extrait weather.pdf ne couvre pas forcément les trois aéroports : on sonde
#' l'API (un appel léger par aéroport) plutôt que d'afficher un onglet vide.
ref_weather_origins <- function() {
  api_cached("weather_origins", function() {
    origins <- ref_origins()
    if (nrow(origins) == 0) return(origins)
    keep <- vapply(as.character(origins$origin), function(code) {
      res <- api_get("/weather", list(origin = code, limit = 1))
      isTRUE(res$ok) && (as_scalar(res$data$total) %||% 0) > 0
    }, logical(1))
    if (!any(keep)) return(origins)
    origins[keep, , drop = FALSE]
  }, ttl = 3600)
}
