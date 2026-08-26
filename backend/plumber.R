# API REST Trafic Aérien ADP — Plumber (R)
# Lancer : source("run.R") depuis le dossier backend/

source("R/database.R", local = TRUE)
source("R/utils.R", local = TRUE)

#* @apiTitle Trafic Aérien ADP — API
#* @apiDescription API REST pour le reporting du trafic aérien NYC (BTS)
#* @apiVersion 1.0.0

#* CORS pour le dashboard frontend
#* @filter cors
function(req, res) {
  origin <- req$HTTP_ORIGIN
  allowed <- cors_origins()
  if (!is.null(origin) && origin %in% allowed) {
    res$setHeader("Access-Control-Allow-Origin", origin)
    res$setHeader("Access-Control-Allow-Credentials", "true")
  }
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Page d'accueil
#* @get /
#* @serializer json
function() {
  list(
    message = "API Trafic Aérien ADP",
    docs = "/__docs__/",
    health = "/health",
    cors_origins = cors_origins()
  )
}

#* Statut de l'API et de MySQL
#* @get /health
#* @serializer json
function(res) {
  tryCatch({
    db_status <- check_connection()
    list(status = "ok", database = db_status)
  }, error = function(e) {
    res$status <- 503
    list(
      status = "degraded",
      database = list(connected = FALSE, error = conditionMessage(e))
    )
  })
}

#* Comptages globaux — Mission 1
#* @get /stats/overview
#* @serializer json
function() {
  with_db({
    row_to_object(query_df("
      SELECT
        (SELECT COUNT(*) FROM airports)  AS airports,
        (SELECT COUNT(*) FROM airlines)  AS airlines,
        (SELECT COUNT(*) FROM planes)    AS planes,
        (SELECT COUNT(*) FROM flights)   AS flights,
        (SELECT COUNT(*) FROM weather)   AS weather_records,
        (SELECT COUNT(*) FROM flights
         WHERE dep_time IS NULL AND arr_time IS NULL) AS cancelled_flights,
        (SELECT COUNT(DISTINCT dest) FROM flights)     AS destinations,
        (SELECT COUNT(DISTINCT origin) FROM flights)   AS origins
    "))
  })
}

#* Liste paginée des vols
#* @get /flights
#* @serializer json
function(page = 1, limit = 50, carrier = "", origin = "", dest = "",
         year = "", month = "", day = "", cancelled_only = FALSE,
         min_dep_delay = "", sort_by = "id", sort_dir = "asc") {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 50, 1, 500, "limit")
    offset <- (page - 1) * limit
    conditions <- c("1=1")

    carrier <- normalize_iata(carrier, "carrier", 2, 2)
    origin <- normalize_iata(origin, "origin", 3, 3)
    dest <- normalize_iata(dest, "dest", 3, 3)
    year_n <- as_int_opt(year, 2000, 2100, "year")
    month_n <- as_int_opt(month, 1, 12, "month")
    day_n <- as_int_opt(day, 1, 31, "day")
    min_delay <- as_int_opt(min_dep_delay, -1e5, 1e5, "min_dep_delay")

    if (!is.null(carrier)) conditions <- c(conditions, sprintf("f.carrier = %s", sql_quote(carrier)))
    if (!is.null(origin)) conditions <- c(conditions, sprintf("f.origin = %s", sql_quote(origin)))
    if (!is.null(dest)) conditions <- c(conditions, sprintf("f.dest = %s", sql_quote(dest)))
    if (!is.null(year_n)) conditions <- c(conditions, sprintf("f.year = %d", year_n))
    if (!is.null(month_n)) conditions <- c(conditions, sprintf("f.month = %d", month_n))
    if (!is.null(day_n)) conditions <- c(conditions, sprintf("f.day = %d", day_n))
    if (as_bool(cancelled_only, FALSE)) {
      conditions <- c(conditions, "f.dep_time IS NULL AND f.arr_time IS NULL")
    }
    if (!is.null(min_delay)) {
      conditions <- c(conditions, sprintf("f.dep_delay >= %d", min_delay))
    }

    where <- paste(conditions, collapse = " AND ")
    order <- flight_order_sql(sort_by, sort_dir)
    total <- query_scalar(sprintf("SELECT COUNT(*) AS n FROM flights f WHERE %s", where))
    rows <- query_df(sprintf("
      SELECT
        f.id, f.year, f.month, f.day,
        f.carrier, al.name AS airline_name,
        f.flight, f.origin, ao.name AS origin_name,
        f.dest, ad.name AS dest_name,
        f.dep_delay, f.arr_delay, f.distance, f.air_time,
        f.dep_time, f.arr_time, f.time_hour,
        CASE WHEN f.dep_time IS NULL AND f.arr_time IS NULL THEN 1 ELSE 0 END AS is_cancelled
      FROM flights f
      JOIN airlines al ON f.carrier = al.carrier
      JOIN airports ao ON f.origin = ao.faa
      JOIN airports ad ON f.dest = ad.faa
      WHERE %s
      ORDER BY %s
      LIMIT %d OFFSET %d
    ", where, order, limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Top vols les plus retardés
#* @get /flights/most-delayed
#* @serializer json
function(limit = 10, sort_by = "arr") {
  with_db({
    limit <- as_int(limit, 10, 1, 100, "limit")
    if (!sort_by %in% c("dep", "arr", "both")) {
      stop_http(422, "Paramètre 'sort_by' invalide (dep|arr|both).")
    }
    order <- switch(
      sort_by,
      dep = "f.dep_delay IS NULL, f.dep_delay DESC",
      arr = "f.arr_delay IS NULL, f.arr_delay DESC",
      "GREATEST(COALESCE(f.dep_delay, 0), COALESCE(f.arr_delay, 0)) DESC"
    )
    rows <- query_df(sprintf("
      SELECT
        f.id, f.carrier, al.name AS airline_name,
        f.flight, f.origin, f.dest,
        f.dep_delay, f.arr_delay, f.year, f.month, f.day
      FROM flights f
      JOIN airlines al ON f.carrier = al.carrier
      ORDER BY %s
      LIMIT %d
    ", order, limit))
    list(limit = limit, sort_by = sort_by, data = df_to_records(rows))
  })
}

#* Détail d'un vol
#* @get /flights/<flight_id:int>
#* @serializer json
function(flight_id) {
  with_db({
    if (flight_id < 1) stop_http(400, "Identifiant de vol invalide.")
    row <- query_df(sprintf("
      SELECT
        f.*,
        al.name AS airline_name,
        ao.name AS origin_name,
        ad.name AS dest_name,
        CASE WHEN f.dep_time IS NULL AND f.arr_time IS NULL THEN 1 ELSE 0 END AS is_cancelled
      FROM flights f
      JOIN airlines al ON f.carrier = al.carrier
      JOIN airports ao ON f.origin = ao.faa
      JOIN airports ad ON f.dest = ad.faa
      WHERE f.id = %d
    ", as.integer(flight_id)))
    obj <- row_to_object(row)
    if (is.null(obj)) stop_http(404, "Vol introuvable.")
    obj
  })
}

#* Liste des aéroports
#* @get /airports
#* @serializer json
function(page = 1, limit = 50, search = "") {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 50, 1, 500, "limit")
    offset <- (page - 1) * limit
    conditions <- c("1=1")
    if (!is.null(search) && nchar(search) >= 2) {
      conditions <- c(conditions, sprintf(
        "(faa LIKE %s OR name LIKE %s)",
        sql_like(paste0(toupper(search), "%")),
        sql_like(paste0("%", search, "%"))
      ))
    }
    where <- paste(conditions, collapse = " AND ")
    total <- query_scalar(sprintf("SELECT COUNT(*) FROM airports WHERE %s", where))
    rows <- query_df(sprintf("
      SELECT faa, name, lat, lon, alt, tz, dst, tzone
      FROM airports WHERE %s
      ORDER BY name LIMIT %d OFFSET %d
    ", where, limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Détail d'un aéroport
#* @get /airports/<faa>
#* @serializer json
function(faa) {
  with_db({
    code <- normalize_iata(faa, "faa", 3, 3)
    row <- query_df(sprintf(
      "SELECT faa, name, lat, lon, alt, tz, dst, tzone FROM airports WHERE faa = %s",
      sql_quote(code)
    ))
    obj <- row_to_object(row)
    if (is.null(obj)) stop_http(404, "Aéroport introuvable.")
    obj
  })
}

#* Liste des compagnies
#* @get /airlines
#* @serializer json
function(page = 1, limit = 50, search = "") {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 50, 1, 500, "limit")
    offset <- (page - 1) * limit
    conditions <- c("1=1")
    if (!is.null(search) && nchar(search) >= 2) {
      conditions <- c(conditions, sprintf(
        "(carrier LIKE %s OR name LIKE %s)",
        sql_like(paste0(toupper(search), "%")),
        sql_like(paste0("%", search, "%"))
      ))
    }
    where <- paste(conditions, collapse = " AND ")
    total <- query_scalar(sprintf("SELECT COUNT(*) FROM airlines WHERE %s", where))
    rows <- query_df(sprintf(
      "SELECT carrier, name FROM airlines WHERE %s ORDER BY name LIMIT %d OFFSET %d",
      where, limit, offset
    ))
    paginated(page, limit, total, rows)
  })
}

#* Origines et destinations d'une compagnie
#* @get /airlines/<carrier>/routes
#* @serializer json
function(carrier) {
  with_db({
    code <- normalize_iata(carrier, "carrier", 2, 2)
    exists <- query_df(sprintf("SELECT 1 AS ok FROM airlines WHERE carrier = %s", sql_quote(code)))
    if (nrow(exists) == 0) stop_http(404, "Compagnie introuvable.")
    origins <- query_df(sprintf("
      SELECT DISTINCT f.origin, a.name
      FROM flights f JOIN airports a ON f.origin = a.faa
      WHERE f.carrier = %s ORDER BY f.origin
    ", sql_quote(code)))
    destinations <- query_df(sprintf("
      SELECT f.dest, a.name, COUNT(*) AS flight_count
      FROM flights f JOIN airports a ON f.dest = a.faa
      WHERE f.carrier = %s
      GROUP BY f.dest, a.name
      ORDER BY flight_count DESC
    ", sql_quote(code)))
    list(
      carrier = code,
      origins = df_to_records(origins),
      destinations = df_to_records(destinations)
    )
  })
}

#* Trafic mensuel par aéroport d'origine
#* @get /traffic/monthly
#* @serializer json
function(origin = "") {
  with_db({
    origin <- normalize_iata(origin, "origin", 3, 3)
    where <- if (is.null(origin)) "1=1" else sprintf("origin = %s", sql_quote(origin))
    monthly <- query_df(sprintf("
      SELECT origin, year, month, COUNT(*) AS flight_count,
        ROUND(COUNT(*) / DAY(LAST_DAY(CONCAT(year, '-', month, '-01'))), 2) AS avg_daily_flights
      FROM flights WHERE %s
      GROUP BY origin, year, month
      ORDER BY origin, year, month
    ", where))
    growth <- query_df(sprintf("
      SELECT origin, month, COUNT(*) AS flight_count,
        ROUND(
          (COUNT(*) - LAG(COUNT(*)) OVER (PARTITION BY origin ORDER BY year, month))
          / NULLIF(LAG(COUNT(*)) OVER (PARTITION BY origin ORDER BY year, month), 0) * 100, 2
        ) AS growth_rate_pct
      FROM flights WHERE %s
      GROUP BY origin, year, month
      ORDER BY origin, year, month
    ", where))
    list(monthly = df_to_records(monthly), with_growth = df_to_records(growth))
  })
}

#* Vols par jour (paginé)
#* @get /traffic/daily
#* @serializer json
function(page = 1, limit = 31) {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 31, 1, 366, "limit")
    offset <- (page - 1) * limit
    total <- query_scalar("SELECT COUNT(DISTINCT year, month, day) FROM flights")
    rows <- query_df(sprintf("
      SELECT year, month, day, COUNT(*) AS flight_count
      FROM flights GROUP BY year, month, day
      ORDER BY year, month, day LIMIT %d OFFSET %d
    ", limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Filtres temporels Mission 2
#* @get /traffic/by-period
#* @serializer json
function(period, origin = "") {
  with_db({
    period_sql <- traffic_period_sql(period)
    origin <- normalize_iata(origin, "origin", 3, 3)
    conditions <- c(sprintf("(%s)", period_sql))
    if (!is.null(origin)) {
      conditions <- c(conditions, sprintf("origin = %s", sql_quote(origin)))
    }
    where <- paste(conditions, collapse = " AND ")
    summary <- row_to_object(query_df(sprintf("
      SELECT COUNT(*) AS flight_count,
             COUNT(DISTINCT origin) AS origins,
             COUNT(DISTINCT dest) AS destinations
      FROM flights WHERE %s
    ", where)))
    by_origin <- query_df(sprintf("
      SELECT origin, COUNT(*) AS flight_count
      FROM flights WHERE %s
      GROUP BY origin ORDER BY flight_count DESC
    ", where))
    list(period = period, summary = summary, by_origin = df_to_records(by_origin))
  })
}

#* Top origines et destinations
#* @get /traffic/top-airports
#* @serializer json
function(limit = 3) {
  with_db({
    limit <- as_int(limit, 3, 1, 20, "limit")
    origins <- query_df(sprintf("
      SELECT f.origin, a.name, COUNT(*) AS flight_count
      FROM flights f JOIN airports a ON f.origin = a.faa
      GROUP BY f.origin, a.name ORDER BY flight_count DESC LIMIT %d
    ", limit))
    destinations <- query_df(sprintf("
      SELECT f.dest, a.name, COUNT(*) AS flight_count
      FROM flights f JOIN airports a ON f.dest = a.faa
      GROUP BY f.dest, a.name ORDER BY flight_count DESC LIMIT %d
    ", limit))
    list(origins = df_to_records(origins), destinations = df_to_records(destinations))
  })
}

#* Jours ouvrés vs week-end
#* @get /traffic/weekday-vs-weekend
#* @serializer json
function() {
  with_db({
    data <- row_to_object(query_df("
      SELECT
        SUM(CASE WHEN DAYOFWEEK(CONCAT(year,'-',month,'-',day)) IN (1, 7) THEN 1 ELSE 0 END) AS weekend_flights,
        SUM(CASE WHEN DAYOFWEEK(CONCAT(year,'-',month,'-',day)) NOT IN (1, 7) THEN 1 ELSE 0 END) AS weekday_flights,
        COUNT(*) AS total_flights
      FROM flights
    "))
    total <- data$total_flights
    if (is.null(total) || is.na(total) || total == 0) total <- 1
    data$weekend_pct <- round(100 * as.numeric(data$weekend_flights) / as.numeric(total), 2)
    data$weekday_pct <- round(100 * as.numeric(data$weekday_flights) / as.numeric(total), 2)
    data
  })
}

#* Jours spéciaux vs moyenne annuelle
#* @get /traffic/special-days-vs-average
#* @serializer json
function() {
  with_db({
    rows <- query_df("
      WITH daily AS (
        SELECT year, month, day, COUNT(*) AS flight_count
        FROM flights GROUP BY year, month, day
      ),
      avg_daily AS (
        SELECT ROUND(AVG(flight_count), 2) AS avg_daily_flights FROM daily
      )
      SELECT 'new_year' AS label, month, day,
             ROUND(AVG(flight_count), 2) AS avg_flights,
             (SELECT avg_daily_flights FROM avg_daily) AS annual_avg,
             ROUND(AVG(flight_count) - (SELECT avg_daily_flights FROM avg_daily), 2) AS diff_vs_avg
      FROM daily WHERE month = 1 AND day = 1
      UNION ALL
      SELECT 'independence', month, day,
             ROUND(AVG(flight_count), 2),
             (SELECT avg_daily_flights FROM avg_daily),
             ROUND(AVG(flight_count) - (SELECT avg_daily_flights FROM avg_daily), 2)
      FROM daily WHERE month = 7 AND day = 4
      UNION ALL
      SELECT 'thanksgiving', month, day,
             ROUND(AVG(flight_count), 2),
             (SELECT avg_daily_flights FROM avg_daily),
             ROUND(AVG(flight_count) - (SELECT avg_daily_flights FROM avg_daily), 2)
      FROM daily WHERE month = 11 AND day = 29
      UNION ALL
      SELECT 'christmas', month, day,
             ROUND(AVG(flight_count), 2),
             (SELECT avg_daily_flights FROM avg_daily),
             ROUND(AVG(flight_count) - (SELECT avg_daily_flights FROM avg_daily), 2)
      FROM daily WHERE month = 12 AND day = 25
    ")
    list(data = df_to_records(rows))
  })
}

#* Stats globales retards
#* @get /delays/summary
#* @serializer json
function() {
  with_db({
    row_to_object(query_df("
      SELECT
        ROUND(AVG(dep_delay), 2) AS avg_dep_delay,
        ROUND(AVG(arr_delay), 2) AS avg_arr_delay,
        MAX(dep_delay) AS max_dep_delay,
        MAX(arr_delay) AS max_arr_delay,
        MIN(dep_delay) AS min_dep_delay,
        MIN(arr_delay) AS min_arr_delay,
        SUM(CASE WHEN dep_delay > 0 THEN 1 ELSE 0 END) AS delayed_departures,
        SUM(CASE WHEN arr_delay > 0 THEN 1 ELSE 0 END) AS delayed_arrivals,
        SUM(CASE WHEN dep_delay < 0 THEN 1 ELSE 0 END) AS early_departures,
        SUM(CASE WHEN arr_delay < 0 THEN 1 ELSE 0 END) AS early_arrivals,
        COUNT(*) AS total_flights
      FROM flights
      WHERE dep_delay IS NOT NULL OR arr_delay IS NOT NULL
    "))
  })
}

#* Retard moyen journalier
#* @get /delays/daily
#* @serializer json
function(page = 1, limit = 31) {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 31, 1, 366, "limit")
    offset <- (page - 1) * limit
    total <- query_scalar("
      SELECT COUNT(*) FROM (
        SELECT year, month, day FROM flights
        WHERE dep_delay IS NOT NULL GROUP BY year, month, day
      ) d
    ")
    rows <- query_df(sprintf("
      SELECT year, month, day, ROUND(AVG(dep_delay), 2) AS avg_dep_delay
      FROM flights WHERE dep_delay IS NOT NULL
      GROUP BY year, month, day ORDER BY year, month, day
      LIMIT %d OFFSET %d
    ", limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Classement compagnies par retards
#* @get /delays/by-carrier
#* @serializer json
function(page = 1, limit = 20) {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 20, 1, 100, "limit")
    offset <- (page - 1) * limit
    total <- query_scalar("SELECT COUNT(DISTINCT carrier) FROM flights WHERE arr_delay IS NOT NULL")
    rows <- query_df(sprintf("
      SELECT f.carrier, al.name AS airline_name, COUNT(*) AS total_flights,
        SUM(CASE WHEN f.arr_delay > 0 THEN 1 ELSE 0 END) AS delayed_flights,
        ROUND(100.0 * SUM(CASE WHEN f.arr_delay > 0 THEN 1 ELSE 0 END) / COUNT(*), 2) AS delay_rate_pct
      FROM flights f JOIN airlines al ON f.carrier = al.carrier
      WHERE f.arr_delay IS NOT NULL
      GROUP BY f.carrier, al.name ORDER BY delay_rate_pct DESC
      LIMIT %d OFFSET %d
    ", limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Destinations les plus retardées (HNL exclu)
#* @get /delays/by-destination
#* @serializer json
function(limit = 10) {
  with_db({
    limit <- as_int(limit, 10, 1, 50, "limit")
    rows <- query_df(sprintf("
      SELECT f.dest, a.name AS destination_name, COUNT(*) AS total_flights,
             ROUND(AVG(f.arr_delay), 2) AS avg_arr_delay
      FROM flights f JOIN airports a ON f.dest = a.faa
      WHERE f.arr_delay IS NOT NULL AND f.dest != 'HNL'
      GROUP BY f.dest, a.name ORDER BY avg_arr_delay DESC LIMIT %d
    ", limit))
    list(data = df_to_records(rows))
  })
}

#* Retard par heure de décollage
#* @get /delays/by-hour
#* @serializer json
function() {
  with_db({
    rows <- query_df("
      SELECT hour, COUNT(*) AS total_flights, ROUND(AVG(dep_delay), 2) AS avg_dep_delay
      FROM flights WHERE dep_delay IS NOT NULL AND hour IS NOT NULL
      GROUP BY hour ORDER BY hour
    ")
    list(data = df_to_records(rows))
  })
}

#* Retard moyen par aéroport d'origine
#* @get /delays/by-origin-airport
#* @serializer json
function() {
  with_db({
    rows <- query_df("
      SELECT f.origin, a.name AS airport_name, COUNT(*) AS total_flights,
             ROUND(AVG(f.dep_delay), 2) AS avg_dep_delay,
             ROUND(AVG(f.arr_delay), 2) AS avg_arr_delay
      FROM flights f JOIN airports a ON f.origin = a.faa
      WHERE f.dep_delay IS NOT NULL
      GROUP BY f.origin, a.name ORDER BY avg_dep_delay ASC
    ")
    list(data = df_to_records(rows))
  })
}

#* Distance vs retard moyen
#* @get /delays/distance
#* @serializer json
function(exclude_hnl = TRUE) {
  with_db({
    exclude <- as_bool(exclude_hnl, TRUE)
    hnl <- if (exclude) "AND f.dest != 'HNL'" else ""
    rows <- query_df(sprintf("
      SELECT f.dest, a.name AS destination_name, COUNT(*) AS flight_count,
             ROUND(AVG(f.distance), 2) AS avg_distance,
             ROUND(AVG(f.arr_delay), 2) AS avg_arr_delay
      FROM flights f JOIN airports a ON f.dest = a.faa
      WHERE f.arr_delay IS NOT NULL AND f.distance IS NOT NULL %s
      GROUP BY f.dest, a.name HAVING COUNT(*) >= 10
      ORDER BY avg_distance
    ", hnl))
    list(exclude_hnl = exclude, data = df_to_records(rows))
  })
}

#* Gain en vol (rattrapage de retard)
#* @get /delays/gain
#* @serializer json
function(limit = 20) {
  with_db({
    limit <- as_int(limit, 20, 1, 100, "limit")
    rows <- query_df(sprintf("
      SELECT f.id, f.carrier, f.flight, f.origin, f.dest,
             f.dep_delay, f.arr_delay, f.air_time,
             (f.arr_delay - f.dep_delay) AS gain,
             ROUND((f.arr_delay - f.dep_delay) / NULLIF(f.air_time / 60.0, 0), 2) AS gain_per_hour
      FROM flights f
      WHERE f.dep_delay >= 60
        AND (f.arr_delay - f.dep_delay) >= 30
        AND f.air_time IS NOT NULL AND f.air_time > 0
      ORDER BY gain_per_hour DESC LIMIT %d
    ", limit))
    list(data = df_to_records(rows))
  })
}

#* Vols annulés (dep_time ET arr_time manquants)
#* @get /cancellations
#* @serializer json
function() {
  with_db({
    total <- query_scalar("SELECT COUNT(*) FROM flights WHERE dep_time IS NULL AND arr_time IS NULL")
    total_flights <- query_scalar("SELECT COUNT(*) FROM flights")
    missing_stats <- row_to_object(query_df("
      SELECT
        ROUND(100.0 * SUM(dep_time IS NULL) / COUNT(*), 2) AS dep_time_na_pct,
        ROUND(100.0 * SUM(arr_time IS NULL) / COUNT(*), 2) AS arr_time_na_pct,
        ROUND(100.0 * SUM(dep_delay IS NULL) / COUNT(*), 2) AS dep_delay_na_pct,
        ROUND(100.0 * SUM(arr_delay IS NULL) / COUNT(*), 2) AS arr_delay_na_pct
      FROM flights
    "))
    by_carrier <- query_df("
      SELECT f.carrier, al.name AS airline_name, COUNT(*) AS cancelled_count
      FROM flights f JOIN airlines al ON f.carrier = al.carrier
      WHERE f.dep_time IS NULL AND f.arr_time IS NULL
      GROUP BY f.carrier, al.name ORDER BY cancelled_count DESC
    ")
    by_destination <- query_df("
      SELECT f.dest, a.name AS destination_name, COUNT(*) AS cancelled_count
      FROM flights f JOIN airports a ON f.dest = a.faa
      WHERE f.dep_time IS NULL AND f.arr_time IS NULL
      GROUP BY f.dest, a.name ORDER BY cancelled_count DESC LIMIT 20
    ")
    by_month <- query_df("
      SELECT month, COUNT(*) AS cancelled_count
      FROM flights WHERE dep_time IS NULL AND arr_time IS NULL
      GROUP BY month ORDER BY month
    ")
    rate <- if (total_flights == 0) 0 else round(100 * total / total_flights, 2)
    list(
      total_cancelled = total,
      total_flights = total_flights,
      cancellation_rate_pct = rate,
      missing_values = missing_stats,
      by_carrier = df_to_records(by_carrier),
      by_destination = df_to_records(by_destination),
      by_month = df_to_records(by_month)
    )
  })
}

#* Tri NA en premier (Mission 2)
#* @get /cancellations/sorted
#* @serializer json
function(limit = 50) {
  with_db({
    limit <- as_int(limit, 50, 1, 500, "limit")
    rows <- query_df(sprintf("
      SELECT id, carrier, flight, origin, dest, dep_time, dep_delay, arr_time, arr_delay
      FROM flights
      ORDER BY dep_delay IS NULL DESC, dep_delay DESC,
               dep_time IS NULL DESC, dep_time ASC
      LIMIT %d
    ", limit))
    list(data = df_to_records(rows))
  })
}

#* Carte US origine/destination
#* @get /routes/map
#* @serializer json
function(page = 1, limit = 100) {
  with_db({
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 100, 1, 5000, "limit")
    offset <- (page - 1) * limit
    total <- query_scalar("
      SELECT COUNT(*) FROM (
        SELECT 1 FROM flights f
        JOIN airports ao ON f.origin = ao.faa
        JOIN airports ad ON f.dest = ad.faa
        WHERE ao.lat IS NOT NULL AND ad.lat IS NOT NULL
        GROUP BY f.origin, ao.name, ao.lat, ao.lon, f.dest, ad.name, ad.lat, ad.lon
      ) t
    ")
    rows <- query_df(sprintf("
      SELECT f.origin, ao.name AS origin_name, ao.lat AS origin_lat, ao.lon AS origin_lon,
             f.dest, ad.name AS dest_name, ad.lat AS dest_lat, ad.lon AS dest_lon,
             COUNT(*) AS flight_count
      FROM flights f
      JOIN airports ao ON f.origin = ao.faa
      JOIN airports ad ON f.dest = ad.faa
      WHERE ao.lat IS NOT NULL AND ad.lat IS NOT NULL
      GROUP BY f.origin, ao.name, ao.lat, ao.lon, f.dest, ad.name, ad.lat, ad.lon
      ORDER BY flight_count DESC LIMIT %d OFFSET %d
    ", limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Top lignes aériennes
#* @get /routes/top
#* @serializer json
function(limit = 5) {
  with_db({
    limit <- as_int(limit, 5, 1, 20, "limit")
    rows <- query_df(sprintf("
      SELECT f.origin, ao.name AS origin_name, f.dest, ad.name AS dest_name, COUNT(*) AS flight_count
      FROM flights f
      JOIN airports ao ON f.origin = ao.faa
      JOIN airports ad ON f.dest = ad.faa
      GROUP BY f.origin, ao.name, f.dest, ad.name
      ORDER BY flight_count DESC LIMIT %d
    ", limit))
    list(data = df_to_records(rows))
  })
}

#* Météo horaire par aéroport
#* @get /weather
#* @serializer json
function(origin, month = "", day = "", page = 1, limit = 24) {
  with_db({
    if (missing(origin) || is.null(origin) || !nzchar(origin)) {
      stop_http(422, "Paramètre 'origin' obligatoire.")
    }
    origin_code <- normalize_iata(origin, "origin", 3, 3)
    page <- as_int(page, 1, 1, 1e9, "page")
    limit <- as_int(limit, 24, 1, 500, "limit")
    offset <- (page - 1) * limit
    month_n <- as_int_opt(month, 1, 12, "month")
    day_n <- as_int_opt(day, 1, 31, "day")
    conditions <- c(sprintf("w.origin = %s", sql_quote(origin_code)))
    if (!is.null(month_n)) conditions <- c(conditions, sprintf("w.month = %d", month_n))
    if (!is.null(day_n)) conditions <- c(conditions, sprintf("w.day = %d", day_n))
    where <- paste(conditions, collapse = " AND ")
    total <- query_scalar(sprintf("SELECT COUNT(*) FROM weather w WHERE %s", where))
    rows <- query_df(sprintf("
      SELECT w.origin, w.year, w.month, w.day, w.hour,
             w.temp, w.dewp, w.humid, w.wind_dir, w.wind_speed,
             w.wind_gust, w.precip, w.pressure, w.visib, w.time_hour
      FROM weather w WHERE %s
      ORDER BY w.year, w.month, w.day, w.hour
      LIMIT %d OFFSET %d
    ", where, limit, offset))
    paginated(page, limit, total, rows)
  })
}

#* Météo au moment d'un vol
#* @get /weather/flight/<flight_id:int>
#* @serializer json
function(flight_id) {
  with_db({
    row <- query_df(sprintf("
      SELECT f.id AS flight_id, f.origin, f.dest, f.time_hour AS flight_time,
             w.temp, w.dewp, w.humid, w.wind_dir, w.wind_speed,
             w.wind_gust, w.precip, w.pressure, w.visib
      FROM flights f
      LEFT JOIN weather w ON f.origin = w.origin AND f.time_hour = w.time_hour
      WHERE f.id = %d
    ", as.integer(flight_id)))
    obj <- row_to_object(row)
    if (is.null(obj)) stop_http(404, "Vol introuvable.")
    obj
  })
}

#* Gestionnaire d'erreurs HTTP / SQL / internes
#* @plumber
function(pr) {
  plumber::pr_set_error(pr, function(req, res, err) {
    if (inherits(err, "http_error")) {
      res$status <- err$status
      return(list(detail = err$detail, error = err$error))
    }
    wrapped <- err
    if (!is.null(err$error) && inherits(err$error, "http_error")) {
      res$status <- err$error$status
      return(list(detail = err$error$detail, error = err$error$error))
    }
    msg <- conditionMessage(err)
    if (grepl("MySQL|connexion|Can't connect|database", msg, ignore.case = TRUE)) {
      res$status <- 503
      return(list(detail = msg, error = "database_unavailable"))
    }
    res$status <- 500
    list(detail = "Erreur interne du serveur.", error = "internal_error")
  })
}
