# =================================================================
# Projet Trafic Aerien ADP
# Partie : Analyse & Statistiques
# Auteur : Hassan HOUSSEIN HOUMED
# =================================================================

# -----------------------------------------------------------------
# 1. CHARGEMENT DES LIBRAIRIES
# -----------------------------------------------------------------
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

# -----------------------------------------------------------------
# 2. CONNEXION A LA BASE DE DONNEES
# -----------------------------------------------------------------
con <- dbConnect(
  RMySQL::MySQL(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

cat("Connexion a la base de donnees reussie.\n")

# -----------------------------------------------------------------
# 3. CHARGEMENT DES DONNEES
# -----------------------------------------------------------------
flights  <- dbReadTable(con, "flights")
airports <- dbReadTable(con, "airports")
airlines <- dbReadTable(con, "airlines")
planes   <- dbReadTable(con, "planes")
weather  <- dbReadTable(con, "weather")

cat("Donnees chargees :\n")
cat("  - Vols      :", nrow(flights),  "\n")
cat("  - Aeroports :", nrow(airports), "\n")
cat("  - Compagnies:", nrow(airlines), "\n")
cat("  - Avions    :", nrow(planes),   "\n")
cat("  - Meteo     :", nrow(weather),  "\n")

# =================================================================
# MISSION 1 - EXPLORATION DES DONNEES
# =================================================================

# Comptages generaux
nb_airports_total <- nrow(airports)
airports_depart   <- flights %>% distinct(origin) %>% nrow()
airports_dest     <- flights %>% distinct(dest)   %>% nrow()
airports_no_dst   <- airports %>% filter(dst == "N") %>% nrow()
nb_tz             <- airports %>% distinct(tz) %>% nrow()
nb_airlines       <- nrow(airlines)
nb_planes         <- nrow(planes)

annules <- flights %>% filter(is.na(dep_time) & is.na(arr_time))
cat("\nNombre total d'aeroports :", nb_airports_total, "\n")
cat("Aeroports de depart      :", airports_depart, "\n")
cat("Aeroports de destination :", airports_dest, "\n")
cat("Sans heure d'ete (dst=N) :", airports_no_dst, "\n")
cat("Fuseaux horaires         :", nb_tz, "\n")
cat("Compagnies               :", nb_airlines, "\n")
cat("Avions                   :", nb_planes, "\n")
cat("Vols annules             :", nrow(annules), "\n")

# Top 10 destinations
top10_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  mutate(pct = round(n / sum(n) * 100, 2)) %>%
  slice_head(n = 10)
cat("\nTop 10 destinations :\n")
print(top10_dest)

# Destinations par compagnie
dest_par_compagnie <- flights %>%
  group_by(carrier) %>%
  summarise(nb_destinations = n_distinct(dest)) %>%
  left_join(airlines, by = "carrier") %>%
  arrange(desc(nb_destinations))

ggplot(dest_par_compagnie,
       aes(x = reorder(name, nb_destinations), y = nb_destinations, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Nombre de destinations par compagnie",
       x = "Compagnie", y = "Nombre de destinations") +
  theme_minimal()
ggsave("figures/destinations_par_compagnie.png", width = 10, height = 6)

# Vols vers Houston et Seattle
vols_houston <- flights %>% filter(dest %in% c("IAH", "HOU"))
vols_seattle  <- flights %>% filter(dest == "SEA")
cat("\nVols vers Houston :", nrow(vols_houston), "\n")
cat("Vols vers Seattle  :", nrow(vols_seattle),  "\n")

# Destinations exclusives par compagnie
dest_exclusives <- flights %>%
  group_by(dest) %>%
  summarise(nb_compagnies = n_distinct(carrier)) %>%
  filter(nb_compagnies == 1)
cat("Destinations exclusives :", nrow(dest_exclusives), "\n")

# =================================================================
# MISSION 2 - REPORTING
# =================================================================

# Reconstitution des datetimes
flights <- flights %>%
  mutate(
    sched_dep_time_dt = make_datetime(year, month, day, hour, minute),
    dep_time_dt = make_datetime(
      year, month, day,
      dep_time %/% 100, dep_time %% 100),
    arr_time_dt = make_datetime(
      year, month, day,
      arr_time %/% 100, arr_time %% 100),
    sched_arr_time_dt = make_datetime(
      year, month, day,
      sched_arr_time %/% 100, sched_arr_time %% 100)
  )

# -----------------------------------------------------------------
# GRAPHIQUE 2 : Trafic mensuel par aeroport (mois complets uniquement)
# -----------------------------------------------------------------
trafic_mensuel <- flights %>%
  mutate(mois = month(sched_dep_time_dt)) %>%
  group_by(origin, mois) %>%
  summarise(
    nb_vols  = n(),
    nb_jours = n_distinct(as_date(sched_dep_time_dt)),
    .groups  = "drop"
  ) %>%
  filter(nb_jours >= 25)

moyenne_mensuelle <- trafic_mensuel %>%
  group_by(origin) %>%
  summarise(moy = mean(nb_vols))

ggplot(trafic_mensuel, aes(x = mois, y = nb_vols)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  geom_hline(data = moyenne_mensuelle, aes(yintercept = moy),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~origin) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Fev","Mar","Avr","Mai","Jun",
                                "Jul","Aou","Sep","Oct","Nov","Dec")) +
  labs(title = "Evolution mensuelle du trafic par aeroport d'origine",
       subtitle = "Ligne rouge = moyenne mensuelle (mois complets uniquement)",
       x = "Mois", y = "Nombre de vols") +
  theme_minimal()
ggsave("figures/trafic_mensuel_facette.png", width = 12, height = 5)

# -----------------------------------------------------------------
# GRAPHIQUE 3 : Trafic mensuel total + taux de croissance (corrige)
# -----------------------------------------------------------------
trafic_mensuel_total <- flights %>%
  mutate(mois = month(sched_dep_time_dt)) %>%
  group_by(mois) %>%
  summarise(
    nb_jours = n_distinct(as_date(sched_dep_time_dt)),
    nb_vols  = n(),
    .groups  = "drop"
  ) %>%
  filter(nb_jours >= 25) %>%
  mutate(taux = (nb_vols - lag(nb_vols)) / lag(nb_vols) * 100)

facteur <- 300

ggplot(trafic_mensuel_total, aes(x = mois)) +
  geom_col(aes(y = nb_vols), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = taux * facteur), color = "orange",
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = taux * facteur), color = "orange", size = 2) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Fev","Mar","Avr","Mai","Jun",
                                "Jul","Aou","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(
    name = "Nombre de vols",
    sec.axis = sec_axis(~ . / facteur, name = "Taux de croissance (%)")
  ) +
  labs(title = "Trafic mensuel total + taux d'accroissement",
       x = "Mois") +
  theme_minimal()
ggsave("figures/trafic_mensuel_croissance.png", width = 10, height = 5)

# -----------------------------------------------------------------
# GRAPHIQUE 4 : Weekend vs Jours ouvres (moyenne par jour, corrige)
# -----------------------------------------------------------------
trafic_semaine <- flights %>%
  mutate(
    date       = as_date(sched_dep_time_dt),
    is_weekend = wday(sched_dep_time_dt) %in% c(1, 7),
    type       = ifelse(is_weekend, "Weekend", "Jours ouvres")
  ) %>%
  group_by(type, date) %>%
  summarise(nb_vols_jour = n(), .groups = "drop") %>%
  group_by(type) %>%
  summarise(moyenne_par_jour = round(mean(nb_vols_jour), 0))

ggplot(trafic_semaine, aes(x = type, y = moyenne_par_jour, fill = type)) +
  geom_col(show.legend = FALSE, width = 0.5) +
  geom_text(aes(label = paste0(moyenne_par_jour, " vols/jour")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Jours ouvres" = "#E74C3C", "Weekend" = "#1ABC9C")) +
  labs(title = "Trafic moyen par jour : Weekend vs Jours ouvres",
       subtitle = "Moyenne du nombre de vols par jour",
       x = "", y = "Vols moyens par jour") +
  theme_minimal() +
  ylim(0, max(trafic_semaine$moyenne_par_jour) * 1.15)
ggsave("figures/trafic_weekend_vs_ouvres.png", width = 7, height = 5)

# -----------------------------------------------------------------
# GRAPHIQUE 5 : Retard moyen selon l'heure de depart
# -----------------------------------------------------------------
retard_par_heure <- flights %>%
  mutate(heure = hour(sched_dep_time_dt)) %>%
  group_by(heure) %>%
  summarise(retard_moy = mean(dep_delay, na.rm = TRUE))

ggplot(retard_par_heure, aes(x = heure, y = retard_moy)) +
  geom_line(color = "tomato", linewidth = 1) +
  geom_point(color = "tomato") +
  labs(title = "Retard moyen au depart selon l'heure de decollage",
       x = "Heure de depart", y = "Retard moyen (min)") +
  theme_minimal()
ggsave("figures/retard_par_heure.png", width = 9, height = 5)

# -----------------------------------------------------------------
# GRAPHIQUE 6 : Relation distance et retard
# -----------------------------------------------------------------
retard_distance <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),
    dist_moy  = mean(distance,  na.rm = TRUE),
    nb_vols   = n()
  ) %>%
  filter(dest != "HNL")

ggplot(retard_distance, aes(x = dist_moy, y = delay_moy)) +
  geom_point(aes(size = nb_vols), alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Relation entre la distance et le retard moyen a l'arrivee",
       x = "Distance moyenne (miles)", y = "Retard moyen (min)", size = "Nb vols") +
  theme_minimal()
ggsave("figures/retard_vs_distance.png", width = 10, height = 6)

# -----------------------------------------------------------------
# GRAPHIQUE 7 : Distribution des retards a l'arrivee
# -----------------------------------------------------------------
ggplot(flights %>% filter(!is.na(arr_delay) & arr_delay > -60 & arr_delay < 300),
       aes(x = arr_delay)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution des retards a l'arrivee",
       x = "Retard (min)", y = "Nombre de vols") +
  theme_minimal()
ggsave("figures/distribution_retards.png", width = 10, height = 5)

# -----------------------------------------------------------------
# GRAPHIQUE 8 : Annulations par compagnie
# -----------------------------------------------------------------
annules_compagnie <- annules %>%
  count(carrier, sort = TRUE) %>%
  left_join(airlines, by = "carrier")

ggplot(annules_compagnie,
       aes(x = reorder(name, n), y = n, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Nombre de vols annules par compagnie",
       x = "Compagnie", y = "Nombre d'annulations") +
  theme_minimal()
ggsave("figures/annulations_par_compagnie.png", width = 10, height = 6)

# Statistiques retards
cat("\nRetard moyen au depart  :", round(mean(flights$dep_delay, na.rm=TRUE), 2), "min\n")
cat("Retard moyen a l'arrivee:", round(mean(flights$arr_delay, na.rm=TRUE), 2), "min\n")
cat("Vols annules total      :", nrow(annules), "\n")

# =================================================================
# DECONNEXION
# =================================================================
dbDisconnect(con)
cat("\nScript termine avec succes.\n")
