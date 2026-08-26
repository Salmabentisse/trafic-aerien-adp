# ============================================
# Projet Trafic Aérien - Mission 1A : Exploration
# ============================================

# --- Installation des packages (à faire une seule fois) ---
# Décommente la ligne suivante et exécute-la une seule fois si pas encore fait :
# install.packages(c("jsonlite", "readxl", "rvest", "pdftools", "dplyr", "tidyr", "stringr"))

# --- Chargement des packages (à faire à chaque session) ---
library(jsonlite)
library(readxl)
library(rvest)
library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# --- Chargement des 5 fichiers ---
airlines <- fromJSON("data/airlines.json")
airports <- read_excel("data/airports.xlsx")
# flights.xlsx contient en réalité une seule colonne texte façon CSV
# -> on sépare manuellement les valeurs par virgule
flights_raw <- read_excel("data/flights.xlsx", col_names = FALSE)
col_names <- str_split(flights_raw[[1]][1], ",")[[1]]

flights <- flights_raw[-1, ] %>%
  separate(col = 1, into = col_names, sep = ",")

# Conversion automatique des types (numérique/texte) sans package externe
flights <- as.data.frame(
  lapply(flights, function(col) type.convert(col, as.is = TRUE)),
  stringsAsFactors = FALSE
)                                      # reconvertit les types (num, chr...) automatiquement

str(flights)

page   <- read_html("data/planes.html")
tables <- html_table(page, fill = TRUE)
planes <- tables[[1]]

txt <- pdf_text("data/weather.pdf")

# Combiner toutes les pages du PDF en un seul bloc de texte
full_text <- paste(txt, collapse = "\n")

# Découper en lignes individuelles
lines <- str_split(full_text, "\n")[[1]]

# Retirer les lignes vides
lines <- lines[str_trim(lines) != ""]

# La 1ère ligne = en-tête (noms de colonnes)
header <- str_split(lines[1], ",")[[1]]

# Les lignes suivantes = données (attention : certaines lignes d'en-tête
# peuvent se répéter sur chaque page du PDF, on les enlève)
data_lines <- lines[-1]
data_lines <- data_lines[data_lines != lines[1]]   # enlève les répétitions de l'en-tête

# --- Vérification rapide que tout s'est bien chargé ---
str(airlines)
str(airports)
str(flights)
str(planes)
cat(txt[1])

weather <- read.csv(text = paste(c(lines[1], data_lines), collapse = "\n"),
                    stringsAsFactors = FALSE,
                    na.strings = c("", "NA", " "))

str(weather)

# ============================================
# Mission 1A - Q1 : Nombre d'aéroports
# ============================================

# Nombre total d'aéroports référencés dans la table airports
n_airports_total <- nrow(airports)

# Nombre d'aéroports de départ distincts dans flights
n_origin <- n_distinct(flights$origin)

# Nombre d'aéroports de destination distincts dans flights
n_dest <- n_distinct(flights$dest)

cat("Aéroports référencés (table airports) :", n_airports_total, "\n")
cat("Aéroports de départ (dans flights)     :", n_origin, "\n")
cat("Aéroports de destination (dans flights):", n_dest, "\n")


# ============================================
# Mission 1A - Q2 : DST et fuseaux horaires
# ============================================

table(airports$dst)                        # répartition A / U / N
n_no_dst <- sum(airports$dst == "N")        # aéroports sans heure d'été
n_tzones <- n_distinct(airports$tzone)      # nb de fuseaux horaires (attention aux "N"/NA)

cat("Aéroports sans DST (dst = 'N') :", n_no_dst, "\n")
cat("Nombre de fuseaux horaires (tzone) :", n_tzones, "\n")
table(airports$tzone)                       # détail

# ============================================
# Mission 1A - Q3 : Compagnies, avions, vols annulés
# ============================================

n_airlines <- n_distinct(airlines$carrier)
n_planes   <- n_distinct(planes$tailnum)

# Un vol est considéré annulé quand dep_time ET arr_time sont NA
cancelled <- flights %>% filter(is.na(dep_time) & is.na(arr_time))
n_cancelled <- nrow(cancelled)

cat("Nombre de compagnies :", n_airlines, "\n")
cat("Nombre d'avions       :", n_planes, "\n")
cat("Nombre de vols annulés:", n_cancelled, "\n")

# ============================================
# Mission 1A - Q4 : Aéroport le + emprunté, top/flop destinations, top/flop avions
# ============================================

# Aéroport de départ le plus emprunté
flights %>%
  count(origin, sort = TRUE)

# Top 10 destinations (avec nom complet + %)
dest_counts <- flights %>%
  count(dest, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

top10_dest <- dest_counts %>%
  slice_head(n = 10) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa"))

flop10_dest <- dest_counts %>%
  slice_tail(n = 10) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa"))

print(top10_dest)
print(flop10_dest)

# Top/flop 10 avions (par nb de décollages)
tail_counts <- flights %>%
  filter(!is.na(tailnum) & str_trim(tailnum) != "") %>%
  count(tailnum, sort = TRUE)

top10_planes  <- slice_head(tail_counts, n = 10)
flop10_planes <- slice_tail(tail_counts, n = 10)

print(top10_planes)
print(flop10_planes)


# ============================================
# Mission 1A - Q5 : Destinations par compagnie + viz
# ============================================

# Nb de destinations desservies par chaque compagnie (toutes origines confondues)
dest_par_cie <- flights %>%
  group_by(carrier) %>%
  summarise(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))

print(dest_par_cie)

# Nb de destinations par compagnie ET par aéroport d'origine
dest_par_cie_origin <- flights %>%
  group_by(carrier, origin) %>%
  summarise(n_dest = n_distinct(dest), .groups = "drop")

print(dest_par_cie_origin)

# --- Visualisation : heatmap compagnie x origine ---
library(ggplot2)

ggplot(dest_par_cie_origin, aes(x = origin, y = carrier, fill = n_dest)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n_dest), color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Nombre de destinations desservies par compagnie et aéroport d'origine",
       x = "Aéroport d'origine", y = "Compagnie", fill = "Nb destinations") +
  theme_minimal()

# Sauvegarde du graphique
ggsave("figures/heatmap_cie_origin.png", width = 8, height = 6)

# ============================================
# Mission 1A - Q6 : Vols vers Houston / NYC → Seattle
# ============================================

houston <- flights %>% filter(dest %in% c("IAH", "HOU"))
cat("Nombre de vols vers Houston (IAH/HOU) :", nrow(houston), "\n")

seattle <- flights %>% filter(dest == "SEA")
n_vols_seattle    <- nrow(seattle)
n_cies_seattle    <- n_distinct(seattle$carrier)
n_avions_seattle  <- n_distinct(seattle$tailnum)

cat("Vols NYC -> Seattle           :", n_vols_seattle, "\n")
cat("Compagnies desservant Seattle :", n_cies_seattle, "\n")
cat("Avions uniques vers Seattle   :", n_avions_seattle, "\n")

# ============================================
# Mission 1A - Q7 : Nb vols par destination + tri alphabétique avec jointures
# ============================================

vols_par_dest <- flights %>%
  count(dest, name = "nb_vols") %>%
  arrange(dest)

print(vols_par_dest)

# Jointures pour noms explicites + tri alphabétique (dest, origin, carrier)
f_sorted <- flights %>%
  left_join(airports %>% select(faa, dest_name = name), by = c("dest" = "faa")) %>%
  left_join(airports %>% select(faa, origin_name = name), by = c("origin" = "faa")) %>%
  left_join(airlines %>% rename(carrier_name = name), by = "carrier") %>%
  arrange(dest_name, origin_name, carrier_name)

head(f_sorted %>% select(dest_name, origin_name, carrier_name), 20)

# ============================================
# Mission 1A - Q8 : Compagnies pas sur toutes les origines / desservant toutes les destinations
# ============================================

n_origins_total <- n_distinct(flights$origin)
n_dests_total   <- n_distinct(flights$dest)

origins_par_cie <- flights %>%
  group_by(carrier) %>%
  summarise(n_origins = n_distinct(origin))

cies_pas_toutes_origines <- origins_par_cie %>% filter(n_origins < n_origins_total)
print(cies_pas_toutes_origines)

dests_par_cie <- flights %>%
  group_by(carrier) %>%
  summarise(n_dests = n_distinct(dest))

cies_toutes_dests <- dests_par_cie %>% filter(n_dests == n_dests_total)
print(cies_toutes_dests)


# ============================================
# Mission 1A - Q9 : Tableau croisé origines/destinations par compagnie
# ============================================

tableau_cie <- flights %>%
  group_by(carrier) %>%
  summarise(
    origines     = paste(sort(unique(origin)), collapse = ", "),
    destinations = paste(sort(unique(dest)), collapse = ", ")
  )

print(tableau_cie, width = Inf)

# ============================================
# Mission 1A - Q10 : Destinations exclusives à certaines compagnies
# ============================================

dest_to_cies <- flights %>%
  group_by(dest) %>%
  summarise(n_cies = n_distinct(carrier))

dest_exclusives <- dest_to_cies %>% filter(n_cies == 1)

# quelle compagnie dessert chacune de ces destinations exclusives
exclusives_detail <- flights %>%
  filter(dest %in% dest_exclusives$dest) %>%
  distinct(dest, carrier) %>%
  arrange(dest)

print(dest_exclusives)
print(exclusives_detail)

# ============================================
# Mission 1A - Q11 : Filtrer United, American, Delta
# ============================================

# UA = United, AA = American, DL = Delta
subset_3cies <- flights %>% filter(carrier %in% c("UA", "AA", "DL"))

cat("Nombre de vols United/American/Delta :", nrow(subset_3cies), "\n")
table(subset_3cies$carrier)

