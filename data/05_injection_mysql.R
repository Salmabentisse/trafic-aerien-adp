# ============================================================
# Injection MySQL — 5 tables (airlines, airports, planes, weather, flights)
# Usage (depuis la racine du repo) :
#   Rscript data/05_injection_mysql.R
# ============================================================

user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

pkgs <- c("jsonlite", "readxl", "rvest", "pdftools", "dplyr", "tidyr",
          "stringr", "DBI", "RMariaDB")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(readxl)
  library(rvest)
  library(pdftools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(DBI)
  library(RMariaDB)
})

root <- if (sys.nframe() > 0 && !is.null(sys.frame(1)$ofile)) {
  dirname(dirname(normalizePath(sys.frame(1)$ofile)))
} else {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
  else getwd()
}
data_dir <- file.path(root, "data")
readRenviron(file.path(root, ".Renviron"))

empty_to_na <- function(x) {
  x[is.character(x) & !is.na(x) & trimws(x) == ""] <- NA
  x
}

to_int <- function(x) {
  x <- empty_to_na(as.character(x))
  suppressWarnings(as.integer(x))
}

to_num <- function(x) {
  x <- empty_to_na(as.character(x))
  suppressWarnings(as.numeric(x))
}

fix_hour24 <- function(hour) {
  hour <- to_int(hour)
  hour[is.na(hour)] <- NA_integer_
  hour[!is.na(hour) & hour == 24] <- 0L
  hour[!is.na(hour) & (hour < 0 | hour > 23)] <- NA_integer_
  hour
}

parse_time_hour <- function(x) {
  x <- empty_to_na(as.character(x))
  parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  parsed
}

cat("=== 1. Chargement des fichiers ===\n")

airlines <- fromJSON(file.path(data_dir, "airlines.json"))
airlines <- as.data.frame(airlines, stringsAsFactors = FALSE)
names(airlines) <- tolower(names(airlines))
airlines <- airlines %>%
  transmute(
    carrier = toupper(trimws(as.character(carrier))),
    name = as.character(name)
  ) %>%
  filter(!is.na(carrier), nchar(carrier) == 2) %>%
  distinct(carrier, .keep_all = TRUE)

airports <- read_excel(file.path(data_dir, "airports.xlsx"))
names(airports) <- tolower(names(airports))
airports <- as.data.frame(airports, stringsAsFactors = FALSE)
airports <- airports %>%
  transmute(
    faa = toupper(trimws(as.character(faa))),
    name = empty_to_na(as.character(name)),
    lat = to_num(lat),
    lon = to_num(lon),
    alt = to_int(alt),
    tz = to_int(tz),
    dst = {
      d <- toupper(trimws(empty_to_na(as.character(dst))))
      d[!d %in% c("A", "U", "N")] <- NA
      d
    },
    tzone = empty_to_na(as.character(tzone))
  ) %>%
  filter(!is.na(faa), nchar(faa) == 3) %>%
  distinct(faa, .keep_all = TRUE)

cat("  Lecture flights.xlsx ...\n")
flights_raw <- read_excel(file.path(data_dir, "flights.xlsx"), col_names = FALSE)
col_names <- str_split(flights_raw[[1]][1], ",")[[1]]
flights <- flights_raw[-1, ] %>%
  separate(col = 1, into = col_names, sep = ",", fill = "right", extra = "merge")
flights <- as.data.frame(
  lapply(flights, function(col) type.convert(col, as.is = TRUE)),
  stringsAsFactors = FALSE
)
names(flights) <- tolower(names(flights))
rm(flights_raw)
invisible(gc())

page <- read_html(file.path(data_dir, "planes.html"))
planes <- as.data.frame(html_table(page, fill = TRUE)[[1]], stringsAsFactors = FALSE)
colnames(planes)[1] <- "row_id"
names(planes) <- make.names(names(planes), unique = TRUE)
names(planes) <- tolower(gsub("[^a-z0-9]+", "_", names(planes), perl = TRUE))
names(planes) <- gsub("^_|_$", "", names(planes))
if (!"tailnum" %in% names(planes) && "tail_num" %in% names(planes)) {
  planes$tailnum <- planes$tail_num
}
planes <- as.data.frame(planes, stringsAsFactors = FALSE)
planes <- planes %>%
  transmute(
    tailnum = toupper(trimws(empty_to_na(as.character(tailnum)))),
    year = to_int(year),
    type = empty_to_na(as.character(type)),
    manufacturer = empty_to_na(as.character(manufacturer)),
    model = empty_to_na(as.character(model)),
    engines = to_int(engines),
    seats = to_int(seats),
    speed = to_int(speed),
    engine = empty_to_na(as.character(engine))
  ) %>%
  filter(!is.na(tailnum), tailnum != "") %>%
  distinct(tailnum, .keep_all = TRUE)

cat("  Lecture weather.pdf ...\n")
txt <- pdf_text(file.path(data_dir, "weather.pdf"))
full_text <- paste(txt, collapse = "\n")
lines <- str_split(full_text, "\n")[[1]]
lines <- lines[str_trim(lines) != ""]
data_lines <- lines[-1]
data_lines <- data_lines[data_lines != lines[1]]
weather <- read.csv(
  text = paste(c(lines[1], data_lines), collapse = "\n"),
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", " ")
)
names(weather) <- tolower(names(weather))

cat("  airlines :", nrow(airlines), "\n")
cat("  airports :", nrow(airports), "\n")
cat("  planes   :", nrow(planes), "\n")
cat("  weather  :", nrow(weather), "\n")
cat("  flights  :", nrow(flights), "\n")

cat("\n=== 2. Nettoyage / complétion FK ===\n")

missing_airports <- data.frame(
  faa = c("BQN", "PSE", "SJU", "STT"),
  name = c(
    "Rafael Hernandez Airport",
    "Mercedita Airport",
    "San Juan Airport",
    "Cyril E. King Airport"
  ),
  lat = NA_real_, lon = NA_real_, alt = NA_integer_, tz = NA_integer_,
  dst = NA_character_, tzone = NA_character_,
  stringsAsFactors = FALSE
)
airports <- bind_rows(airports, missing_airports %>% filter(!faa %in% airports$faa))

weather <- weather %>%
  transmute(
    origin = toupper(trimws(as.character(origin))),
    year = to_int(year),
    month = to_int(month),
    day = to_int(day),
    hour = fix_hour24(hour),
    temp = to_num(temp),
    dewp = to_num(dewp),
    humid = to_num(humid),
    wind_dir = to_int(wind_dir),
    wind_speed = to_num(wind_speed),
    wind_gust = to_num(wind_gust),
    precip = to_num(precip),
    pressure = to_num(pressure),
    visib = to_num(visib),
    time_hour = parse_time_hour(time_hour)
  ) %>%
  filter(!is.na(origin), !is.na(year), !is.na(month), !is.na(day), !is.na(hour), !is.na(time_hour)) %>%
  mutate(humid = ifelse(!is.na(humid) & (humid < 0 | humid > 100), NA_real_, humid)) %>%
  distinct(origin, year, month, day, hour, .keep_all = TRUE)

flights <- flights %>%
  transmute(
    year = to_int(year),
    month = to_int(month),
    day = to_int(day),
    dep_time = to_int(dep_time),
    sched_dep_time = to_int(sched_dep_time),
    dep_delay = to_int(dep_delay),
    arr_time = to_int(arr_time),
    sched_arr_time = to_int(sched_arr_time),
    arr_delay = to_int(arr_delay),
    carrier = toupper(trimws(as.character(carrier))),
    flight = to_int(flight),
    tailnum = {
      t <- toupper(trimws(empty_to_na(as.character(tailnum))))
      t
    },
    origin = toupper(trimws(as.character(origin))),
    dest = toupper(trimws(as.character(dest))),
    air_time = to_int(air_time),
    distance = to_int(distance),
    hour = fix_hour24(hour),
    minute = {
      m <- to_int(minute)
      m[!is.na(m) & (m < 0 | m > 59)] <- NA_integer_
      m
    },
    time_hour = parse_time_hour(time_hour)
  ) %>%
  filter(
    !is.na(year), !is.na(month), !is.na(day),
    !is.na(sched_dep_time), !is.na(sched_arr_time),
    !is.na(carrier), !is.na(flight),
    !is.na(origin), !is.na(dest), !is.na(time_hour)
  )

extra_carriers <- setdiff(unique(flights$carrier), airlines$carrier)
if (length(extra_carriers)) {
  airlines <- bind_rows(
    airlines,
    data.frame(carrier = extra_carriers, name = extra_carriers, stringsAsFactors = FALSE)
  )
}

needed_faa <- unique(c(flights$origin, flights$dest, weather$origin))
missing_faa <- setdiff(needed_faa, airports$faa)
if (length(missing_faa)) {
  cat("  Aéroports stub ajoutés :", paste(missing_faa, collapse = ", "), "\n")
  airports <- bind_rows(
    airports,
    data.frame(
      faa = missing_faa, name = missing_faa,
      lat = NA_real_, lon = NA_real_, alt = NA_integer_, tz = NA_integer_,
      dst = NA_character_, tzone = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

weather <- weather %>% filter(origin %in% airports$faa)

missing_tail <- setdiff(unique(na.omit(flights$tailnum)), planes$tailnum)
missing_tail <- missing_tail[missing_tail != ""]
if (length(missing_tail)) {
  cat("  Avions stub ajoutés :", length(missing_tail), "\n")
  planes <- bind_rows(
    planes,
    data.frame(
      tailnum = missing_tail, year = NA_integer_, type = NA_character_,
      manufacturer = NA_character_, model = NA_character_,
      engines = NA_integer_, seats = NA_integer_, speed = NA_integer_,
      engine = NA_character_, stringsAsFactors = FALSE
    )
  )
}

cat("  airports :", nrow(airports), "| planes :", nrow(planes),
    "| weather :", nrow(weather), "| flights :", nrow(flights), "\n")

cat("\n=== 3. Injection MySQL ===\n")
con <- dbConnect(
  MariaDB(),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)
on.exit(dbDisconnect(con), add = TRUE)

dbExecute(con, "SET FOREIGN_KEY_CHECKS = 0")
dbExecute(con, "SET UNIQUE_CHECKS = 0")
dbExecute(con, "SET autocommit = 0")

for (tbl in c("flights", "weather", "planes", "airports", "airlines")) {
  dbExecute(con, paste0("DELETE FROM ", tbl))
  dbExecute(con, paste0("ALTER TABLE ", tbl, " AUTO_INCREMENT = 1"))
}

write_tbl <- function(name, df, chunk = 50000L) {
  n <- nrow(df)
  if (n == 0) {
    cat("  ", name, ": 0 lignes\n", sep = "")
    return(invisible(NULL))
  }
  start <- 1L
  while (start <= n) {
    end <- min(start + chunk - 1L, n)
    dbWriteTable(con, name, df[start:end, , drop = FALSE], append = TRUE, row.names = FALSE)
    cat("  ", name, ": ", end, "/", n, "\n", sep = "")
    start <- end + 1L
  }
}

write_tbl("airlines", airlines, 1000L)
write_tbl("airports", airports, 2000L)
write_tbl("planes", planes, 5000L)
write_tbl("weather", weather, 10000L)
write_tbl("flights", flights, 40000L)

dbExecute(con, "COMMIT")
dbExecute(con, "SET FOREIGN_KEY_CHECKS = 1")
dbExecute(con, "SET UNIQUE_CHECKS = 1")
dbExecute(con, "SET autocommit = 1")

counts <- dbGetQuery(con, "
  SELECT
    (SELECT COUNT(*) FROM airlines) AS airlines,
    (SELECT COUNT(*) FROM airports) AS airports,
    (SELECT COUNT(*) FROM planes)   AS planes,
    (SELECT COUNT(*) FROM weather)  AS weather,
    (SELECT COUNT(*) FROM flights)  AS flights
")
cat("\n=== Injection terminée ===\n")
print(counts)
dbDisconnect(con)
on.exit(NULL)
