library(dplyr)

# --- 1. Ajouter les 4 aéroports manquants ---
missing_airports <- data.frame(
  faa   = c("BQN", "PSE", "SJU", "STT"),
  name  = c(
    "Rafael Hernandez Airport",
    "Mercedita Airport",
    "San Juan Airport",
    "Cyril E. King Airport"
  ),
  lat = NA,
  lon = NA,
  alt = NA,
  tz = NA,
  dst = NA,
  tzone = NA
)

missing_airports <- missing_airports %>%
  filter(!faa %in% airports$faa)

airports_full <- bind_rows(airports, missing_airports)

cat("Aéroports ajoutés :", nrow(missing_airports), "\n")
cat("Total airports_full :", nrow(airports_full), "\n")


# --- 2. Compléter planes avec les tailnums manquants ---
missing_planes_tailnums <- setdiff(
  unique(flights$tailnum),
  planes$tailnum
)

missing_planes_tailnums <- missing_planes_tailnums[
  !is.na(missing_planes_tailnums) &
    missing_planes_tailnums != ""
]

missing_planes <- data.frame(
  tailnum = missing_planes_tailnums,
  year = NA,
  type = NA,
  manufacturer = NA,
  model = NA,
  engines = NA,
  seats = NA,
  speed = NA,
  engine = NA
)

planes_full <- bind_rows(planes, missing_planes)

cat("Avions manquants ajoutés :", nrow(missing_planes), "\n")
cat("Total planes_full :", nrow(planes_full), "\n")