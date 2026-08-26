# ============================================================================
# Figures de la présentation — données tirées de l'API Plumber, tracé ggplot2
#
# Usage (API et base démarrées) :
#   Rscript presentation/figures.R
# Les PNG sont écrits dans presentation/figures/.
# ============================================================================
suppressPackageStartupMessages({library(ggplot2); library(httr2); library(jsonlite)})

this_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(this_file)) {
  dirname(dirname(normalizePath(sub("^--file=", "", this_file))))
} else getwd()
OUT <- file.path(root, "presentation", "figures")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
if (file.exists(file.path(root, ".Renviron"))) readRenviron(file.path(root, ".Renviron"))
API <- Sys.getenv("API_BASE_URL", unset = "http://127.0.0.1:8000")

get_json <- function(path) {
  httr2::request(paste0(API, path)) |> httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}
sc <- function(v) if (is.null(v) || (is.list(v) && !length(v))) NA else unlist(v)[[1]]
to_df <- function(recs) {
  keys <- unique(unlist(lapply(recs, names)))
  cols <- lapply(keys, function(k) {
    chr <- vapply(recs, function(r) { v <- sc(r[[k]]); if (all(is.na(v))) NA_character_ else as.character(v) }, character(1))
    num <- suppressWarnings(as.numeric(chr))
    if (all(is.na(num) == is.na(chr))) num else chr
  })
  names(cols) <- keys
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

NAVY <- "#0B3C5D"; BLUE <- "#1D8AC1"; RED <- "#E4572E"; TEAL <- "#2E9E6B"; SLATE <- "#5A6B7B"
theme_adp <- function(base = 15) {
  theme_minimal(base_size = base, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", size = base + 4, colour = "#12202B",
                                margin = margin(b = 4)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = element_text(colour = SLATE, size = base - 1, margin = margin(b = 12)),
      plot.caption = element_text(colour = SLATE, size = base - 4, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E3E9EF"),
      axis.title = element_text(colour = SLATE, size = base - 2),
      legend.position = "top", legend.title = element_blank(),
      plot.margin = margin(16, 20, 12, 16)
    )
}
# pourcentage au format français (virgule décimale)
pct_fr <- function(x) paste0(sub("\\.", ",", sprintf("%.1f", x)), " %")

save_fig <- function(p, file, w = 10, h = 5.6) {
  ggsave(file.path(OUT, file), p, width = w, height = h, dpi = 200, bg = "white")
  cat("écrit :", file, "\n")
}

# --- 1. retard moyen selon l'heure de départ --------------------------------
h <- to_df(get_json("/delays/by-hour")$data)
p1 <- ggplot(h, aes(hour, avg_dep_delay, fill = avg_dep_delay)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.0f", avg_dep_delay)), vjust = -0.5,
            size = 3.4, colour = SLATE) +
  scale_fill_gradient(low = "#FFE08A", high = "#B3261E", guide = "none") +
  scale_x_continuous(breaks = seq(5, 23, 2), labels = function(x) paste0(x, "h")) +
  labs(title = "Le retard s'accumule au fil de la journée",
       subtitle = "Retard moyen au départ selon l'heure programmée — 0,8 min à 5 h contre 24 min à 19 h",
       x = "Heure de départ programmée", y = "Retard moyen (min)",
       caption = "Source : API /delays/by-hour — 252 704 vols au départ de JFK, LGA et EWR") +
  theme_adp()
save_fig(p1, "01_retard_par_heure.png")

# --- 2. trafic journalier et trou de données --------------------------------
d <- to_df(get_json("/traffic/daily?limit=366")$data)
d$date <- as.Date(sprintf("%04d-%02d-%02d", d$year, d$month, d$day))
full <- merge(data.frame(date = seq(min(d$date), max(d$date), by = "day")), d, all.x = TRUE)
# geom_area relierait les deux bords du trou : on découpe la série en plages
# contiguës de jours renseignés et on trace une aire par plage.
present <- !is.na(full$flight_count)
full$plage <- cumsum(c(TRUE, present[-1] & !present[-length(present)]))
full$plage[!present] <- NA
p2 <- ggplot(subset(full, present), aes(date, flight_count, group = plage)) +
  geom_area(fill = "#CFE3F1") +
  geom_line(colour = NAVY, linewidth = 0.4) +
  annotate("rect", xmin = as.Date("2021-07-04"), xmax = as.Date("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = RED, alpha = 0.10) +
  annotate("text", x = as.Date("2021-08-17"), y = max(d$flight_count) * 0.55,
           label = "aucune donnée\n(juil. tronqué,\naoût et sept. absents)",
           colour = RED, size = 3.6, lineheight = 1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Le jeu de données ne couvre que 276 jours sur 365",
       subtitle = "Vols par jour, trois aéroports confondus — le creux hebdomadaire correspond aux dimanches",
       x = NULL, y = "Vols par jour",
       caption = "Source : API /traffic/daily — flights.xlsx contient 252 704 vols, l'énoncé en cite 326 776") +
  theme_adp()
save_fig(p2, "02_trafic_journalier.png")

# --- 3. fiabilité des compagnies -------------------------------------------
c1 <- to_df(get_json("/delays/by-carrier?limit=100")$data)
# L'effectif est affiché dans le libellé : plusieurs compagnies ne totalisent que
# quelques centaines de vols, leur taux n'a pas la même robustesse.
# on retire les mentions juridiques pour éviter un axe qui écrase le graphique
nom_court <- function(x) trimws(gsub("\\s+(Inc\\.|Co\\.|Corporation)$", "", x))
c1$label <- sprintf("%s — %s  (%s vols)", c1$carrier, nom_court(c1$airline_name),
                    format(c1$total_flights, big.mark = " ", trim = TRUE))
c1 <- c1[order(c1$delay_rate_pct), ]
c1$label <- factor(c1$label, levels = c1$label)
c1$gros <- c1$total_flights >= 10000
p3 <- ggplot(c1, aes(delay_rate_pct, label, fill = delay_rate_pct)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = pct_fr(delay_rate_pct)), hjust = -0.12,
            size = 3.3, colour = SLATE) +
  scale_fill_gradient(low = "#FFE08A", high = "#B3261E", guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "De 24 % à 59 % de vols en retard selon la compagnie",
       subtitle = "Part des vols arrivés en retard — effectif de chaque compagnie entre parenthèses",
       x = "Vols arrivés en retard (%)", y = NULL,
       caption = "Source : API /delays/by-carrier — retard à l'arrivée strictement positif") +
  theme_adp()
save_fig(p3, "03_fiabilite_compagnies.png", h = 6.4)

# --- 4. distance et retard --------------------------------------------------
dd <- to_df(get_json("/delays/distance?exclude_hnl=true")$data)
r <- cor(dd$avg_distance, dd$avg_arr_delay)
p4 <- ggplot(dd, aes(avg_distance, avg_arr_delay)) +
  geom_point(aes(size = flight_count), colour = BLUE, alpha = 0.55) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = RED, linewidth = 0.9, linetype = "dashed") +
  scale_size_continuous(range = c(1.6, 9), guide = "none") +
  labs(title = sprintf("Plus la destination est lointaine, moins le retard s'accumule (r = %s)",
                       sub("-", "\u2212", sub("\\.", ",", sprintf("%.2f", r)))),
       subtitle = "Une destination = un point ; la taille indique le nombre de vols",
       x = "Distance moyenne (miles)", y = "Retard moyen à l'arrivée (min)",
       caption = "Source : API /delays/distance — 100 destinations, Honolulu exclu") +
  theme_adp()
save_fig(p4, "04_distance_retard.png")

# --- 5. répartition et annulations mensuelles ------------------------------
cn <- get_json("/cancellations")
bm <- to_df(cn$by_month)
# month.abb suit la locale et sort en anglais : libellés français explicites
MOIS_FR <- c("janv.", "févr.", "mars", "avr.", "mai", "juin",
             "juil.", "août", "sept.", "oct.", "nov.", "déc.")
bm$mois <- factor(MOIS_FR[bm$month], levels = MOIS_FR)
pic <- bm[which.max(bm$cancelled_count), ]
p5 <- ggplot(bm, aes(mois, cancelled_count, fill = cancelled_count)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = format(cancelled_count, big.mark = " ")), vjust = -0.5,
            size = 3.4, colour = SLATE) +
  scale_fill_gradient(low = "#F6C7BC", high = "#B3261E", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = sprintf("%s %% des vols annulés, très concentrés sur l'hiver",
                       sub("\\.", ",", as.character(sc(cn$cancellation_rate_pct)))),
       subtitle = sprintf("%s annulations sur %s vols — %s culmine à %s",
                          format(sc(cn$total_cancelled), big.mark = " ", trim = TRUE),
                          format(sc(cn$total_flights), big.mark = " ", trim = TRUE),
                          as.character(pic$mois),
                          format(pic$cancelled_count, big.mark = " ", trim = TRUE)),
       x = NULL, y = "Vols annulés",
       caption = "Source : API /cancellations — vol annulé = heure de départ et d'arrivée absentes") +
  theme_adp()
save_fig(p5, "05_annulations_mensuelles.png")

cat("\nFigures générées dans", OUT, "\n")
