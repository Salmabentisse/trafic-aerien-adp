# =================================================================
# Projet Trafic Aérien ADP
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
# 2. CONNEXION À LA BASE DE DONNÉES
# -----------------------------------------------------------------
con <- dbConnect(
  RMySQL::MySQL(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

# Vérification de la connexion
cat("Connexion à la base de données réussie !\n")

# -----------------------------------------------------------------
# 3. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------
flights  <- dbReadTable(con, "flights")
airports <- dbReadTable(con, "airports")
airlines <- dbReadTable(con, "airlines")
planes   <- dbReadTable(con, "planes")
weather  <- dbReadTable(con, "weather")

cat("Données chargées avec succès !\n")
cat("  - Vols     :", nrow(flights),  "lignes\n")
cat("  - Aéroports:", nrow(airports), "lignes\n")
cat("  - Compagnies:", nrow(airlines),"lignes\n")
cat("  - Avions   :", nrow(planes),   "lignes\n")
cat("  - Météo    :", nrow(weather),  "lignes\n")

# =================================================================
# MISSION 1  EXPLORATION DES DONNÉES
# =================================================================

# -----------------------------------------------------------------
# Q1. Comptages généraux
# -----------------------------------------------------------------

# Nombre d'aéroports au total
nb_airports_total <- nrow(airports)
cat("\nNombre total d'aéroports :", nb_airports_total, "\n")

# Aéroports de départ
airports_depart <- flights %>% distinct(origin) %>% nrow()
cat("Aéroports de départ :", airports_depart, "\n")

# Aéroports de destination
airports_dest <- flights %>% distinct(dest) %>% nrow()
cat("Aéroports de destination :", airports_dest, "\n")

# Aéroports sans heure d'été (dst = 'N')
airports_no_dst <- airports %>% filter(dst == "N") %>% nrow()
cat("Aéroports sans heure d'été (dst=N) :", airports_no_dst, "\n")

# Nombre de fuseaux horaires
nb_tz <- airports %>% distinct(tz) %>% nrow()
cat("Nombre de fuseaux horaires :", nb_tz, "\n")

# Nombre de compagnies
nb_airlines <- nrow(airlines)
cat("Nombre de compagnies :", nb_airlines, "\n")

# Nombre d'avions
nb_planes <- nrow(planes)
cat("Nombre d'avions :", nb_planes, "\n")

# Vols annulés (dep_time ET arr_time manquants)
nb_annules <- flights %>%
  filter(is.na(dep_time) & is.na(arr_time)) %>%
  nrow()
cat("Vols annulés :", nb_annules, "\n")

# -----------------------------------------------------------------
# Q2. Aéroport le plus emprunté + top 10 destinations
# -----------------------------------------------------------------

# Aéroport de départ le plus emprunté
top_origin <- flights %>%
  count(origin, sort = TRUE) %>%
  left_join(airports %>% select(faa, name), by = c("origin" = "faa")) %>%
  slice(1)
cat("\nAéroport de départ le plus emprunté :", top_origin$name, "(", top_origin$n, "vols)\n")

# Top 10 destinations les plus prisées (avec nom complet et %)
top10_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  mutate(pct = round(n / sum(n) * 100, 2)) %>%
  slice_head(n = 10)

cat("\nTop 10 destinations les plus prisées :\n")
print(top10_dest)

# Top 10 destinations les moins prisées
bottom10_dest <- flights %>%
  count(dest, sort = FALSE) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  mutate(pct = round(n / sum(n) * 100, 2)) %>%
  slice_head(n = 10)

cat("\nTop 10 destinations les moins prisées :\n")
print(bottom10_dest)

# Top 10 avions qui ont le plus décollé
top10_planes <- flights %>%
  filter(!is.na(tailnum)) %>%
  count(tailnum, sort = TRUE) %>%
  slice_head(n = 10)
cat("\nTop 10 avions (plus de décollages) :\n")
print(top10_planes)

# -----------------------------------------------------------------
# Q3. Destinations par compagnie + graphique
# -----------------------------------------------------------------

dest_par_compagnie <- flights %>%
  group_by(carrier) %>%
  summarise(nb_destinations = n_distinct(dest)) %>%
  left_join(airlines, by = "carrier") %>%
  arrange(desc(nb_destinations))

cat("\nDestinations par compagnie :\n")
print(dest_par_compagnie)

# Graphique destinations par compagnie
ggplot(dest_par_compagnie, aes(x = reorder(name, nb_destinations), y = nb_destinations, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Nombre de destinations par compagnie",
    x = "Compagnie",
    y = "Nombre de destinations"
  ) +
  theme_minimal()
ggsave("figures/destinations_par_compagnie.png", width = 10, height = 6)

# -----------------------------------------------------------------
# Q4. Vols vers Houston (IAH ou HOU) + NYC  Seattle
# -----------------------------------------------------------------

vols_houston <- flights %>%
  filter(dest %in% c("IAH", "HOU"))
cat("\nVols vers Houston :", nrow(vols_houston), "\n")

vols_seattle <- flights %>%
  filter(dest == "SEA")
cat("Vols NYC  Seattle :", nrow(vols_seattle), "\n")

compagnies_seattle <- vols_seattle %>% distinct(carrier) %>% nrow()
cat("Compagnies vers Seattle :", compagnies_seattle, "\n")

avions_seattle <- vols_seattle %>% distinct(tailnum) %>% nrow()
cat("Avions uniques vers Seattle :", avions_seattle, "\n")

# -----------------------------------------------------------------
# Q5. Nombre de vols par destination + tri alphabétique
# -----------------------------------------------------------------

vols_par_dest <- flights %>%
  count(dest, origin, carrier) %>%
  left_join(airports %>% select(faa, name) %>% rename(nom_dest = name),
            by = c("dest" = "faa")) %>%
  left_join(airports %>% select(faa, name) %>% rename(nom_origin = name),
            by = c("origin" = "faa")) %>%
  left_join(airlines %>% rename(nom_carrier = name), by = "carrier") %>%
  arrange(nom_dest, nom_origin, nom_carrier)

cat("\nVols par destination (extrait) :\n")
print(head(vols_par_dest, 10))

# -----------------------------------------------------------------
# Q6. Compagnies n'opérant pas sur tous les aéroports d'origine
# -----------------------------------------------------------------

nb_origins <- flights %>% distinct(origin) %>% nrow()  # = 3

compagnies_toutes_origines <- flights %>%
  group_by(carrier) %>%
  summarise(nb_origins_couverts = n_distinct(origin)) %>%
  left_join(airlines, by = "carrier")

cat("\nCouverture des origines par compagnie :\n")
print(compagnies_toutes_origines)

compagnies_pas_toutes <- compagnies_toutes_origines %>%
  filter(nb_origins_couverts < nb_origins)
cat("Compagnies n'opérant pas sur tous les aéroports d'origine :\n")
print(compagnies_pas_toutes)

# Tableau complet origines x destinations x compagnies
tableau_complet <- flights %>%
  group_by(carrier, origin, dest) %>%
  summarise(nb_vols = n(), .groups = "drop") %>%
  left_join(airlines, by = "carrier")

# -----------------------------------------------------------------
# Q7. Destinations exclusives à certaines compagnies
# -----------------------------------------------------------------

dest_exclusives <- flights %>%
  group_by(dest) %>%
  summarise(nb_compagnies = n_distinct(carrier)) %>%
  filter(nb_compagnies == 1) %>%
  left_join(
    flights %>% group_by(dest) %>% summarise(carrier = first(carrier)),
    by = "dest"
  ) %>%
  left_join(airlines, by = "carrier") %>%
  left_join(airports %>% select(faa, name) %>% rename(nom_dest = name),
            by = c("dest" = "faa"))

cat("\nDestinations exclusives par compagnie :\n")
print(dest_exclusives)

# -----------------------------------------------------------------
# Q8. Vols United, American ou Delta
# -----------------------------------------------------------------

vols_UAD <- flights %>%
  filter(carrier %in% c("UA", "AA", "DL"))
cat("\nVols United + American + Delta :", nrow(vols_UAD), "\n")

# =================================================================
# MISSION 2  REPORTING
# =================================================================

# -----------------------------------------------------------------
# 2.1 TRANSFORMATION DES DATES
# -----------------------------------------------------------------

# Q1 : Reconstituer sched_dep_time en datetime
flights <- flights %>%
  mutate(
    sched_dep_time_dt = make_datetime(year, month, day, hour, minute),
    dep_time_dt       = make_datetime(
      year, month, day,
      dep_time %/% 100,
      dep_time  %% 100
    ),
    arr_time_dt       = make_datetime(
      year, month, day,
      arr_time %/% 100,
      arr_time  %% 100
    ),
    sched_arr_time_dt = make_datetime(
      year, month, day,
      sched_arr_time %/% 100,
      sched_arr_time  %% 100
    )
  ) %>%
  select(-year, -month, -day, -hour, -minute)

cat("\nColonnes datetime créées et colonnes redondantes supprimées.\n")

# -----------------------------------------------------------------
# 2.2 PIC DE TRAFIC AÉROPORTUAIRE
# -----------------------------------------------------------------

# Q2 : Trafic mensuel par aéroport d'origine (viz par facette)
trafic_mensuel <- flights %>%
  mutate(
    mois = month(sched_dep_time_dt),
    annee = year(sched_dep_time_dt)
  ) %>%
  group_by(origin, mois) %>%
  summarise(nb_vols = n(), .groups = "drop")

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
                     labels = c("Jan","Fév","Mar","Avr","Mai","Jun",
                                "Jul","Aoû","Sep","Oct","Nov","Déc")) +
  labs(
    title = "Évolution mensuelle du trafic par aéroport d'origine",
    subtitle = "Ligne rouge = moyenne mensuelle de l'aéroport",
    x = "Mois", y = "Nombre de vols"
  ) +
  theme_minimal()
ggsave("figures/trafic_mensuel_facette.png", width = 12, height = 5)

# Q3 : Taux d'accroissement mensuel
trafic_mensuel_total <- trafic_mensuel %>%
  group_by(mois) %>%
  summarise(total = sum(nb_vols)) %>%
  mutate(taux_croissance = (total - lag(total)) / lag(total) * 100)

ggplot(trafic_mensuel_total, aes(x = mois)) +
  geom_line(aes(y = total), color = "steelblue", linewidth = 1) +
  geom_line(aes(y = taux_croissance * 100), color = "orange",
            linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Fév","Mar","Avr","Mai","Jun",
                                "Jul","Aoû","Sep","Oct","Nov","Déc")) +
  labs(
    title = "Trafic mensuel total + taux d'accroissement",
    x = "Mois", y = "Nombre de vols"
  ) +
  theme_minimal()
ggsave("figures/trafic_mensuel_croissance.png", width = 10, height = 5)

# Q4 : Top 3 aéroports d'origine et de destination
top3_origin <- flights %>%
  count(origin, sort = TRUE) %>%
  slice_head(n = 3)
cat("\nTop 3 aéroports d'origine :\n")
print(top3_origin)

top3_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  slice_head(n = 3)
cat("Top 3 aéroports de destination :\n")
print(top3_dest)

# Q5 : Filtres temporels
# Vols le 1er janvier
vols_1jan <- flights %>% filter(month(sched_dep_time_dt) == 1, day(sched_dep_time_dt) == 1)
cat("\nVols le 1er janvier :", nrow(vols_1jan), "\n")

# Vols en novembre ou décembre
vols_nov_dec <- flights %>% filter(month(sched_dep_time_dt) %in% c(11, 12))
cat("Vols nov/déc :", nrow(vols_nov_dec), "\n")

# Vols jours spéciaux
vols_speciaux <- flights %>%
  filter(
    (month(sched_dep_time_dt) == 12 & day(sched_dep_time_dt) == 25) |  # Noël
    (month(sched_dep_time_dt) ==  1 & day(sched_dep_time_dt) ==  1) |  # Jour de l'an
    (month(sched_dep_time_dt) ==  7 & day(sched_dep_time_dt) ==  4) |  # Independance Day
    (month(sched_dep_time_dt) == 11 & day(sched_dep_time_dt) == 29)    # Thanksgiving
  )
cat("Vols jours spéciaux :", nrow(vols_speciaux), "\n")

# Vols en été (juillet, août, septembre)
vols_ete <- flights %>% filter(month(sched_dep_time_dt) %in% c(7, 8, 9))
cat("Vols en été :", nrow(vols_ete), "\n")

# Vols entre minuit et 6h
vols_nuit <- flights %>% filter(hour(sched_dep_time_dt) <= 6)
cat("Vols entre minuit et 6h :", nrow(vols_nuit), "\n")

# Q6 : Trafic weekend vs jours ouvrés
flights <- flights %>%
  mutate(
    jour_semaine = wday(sched_dep_time_dt, label = TRUE),
    is_weekend   = wday(sched_dep_time_dt) %in% c(1, 7)
  )

trafic_semaine <- flights %>%
  group_by(is_weekend) %>%
  summarise(nb_vols = n()) %>%
  mutate(type = ifelse(is_weekend, "Weekend", "Jours ouvrés"))

cat("\nTrafic Weekend vs Jours ouvrés :\n")
print(trafic_semaine)

ggplot(trafic_semaine, aes(x = type, y = nb_vols, fill = type)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Trafic : Weekend vs Jours ouvrés",
       x = "", y = "Nombre de vols") +
  theme_minimal()
ggsave("figures/trafic_weekend_vs_ouvres.png", width = 7, height = 5)

# -----------------------------------------------------------------
# 2.3 RETARDS
# -----------------------------------------------------------------

# Q1 : Vols les plus retardés
top_retard_arr <- flights %>%
  arrange(desc(arr_delay)) %>%
  select(flight, carrier, origin, dest, arr_delay) %>%
  slice_head(n = 10)
cat("\nTop 10 vols retardés à l'arrivée :\n")
print(top_retard_arr)

top_retard_dep <- flights %>%
  arrange(desc(dep_delay)) %>%
  select(flight, carrier, origin, dest, dep_delay) %>%
  slice_head(n = 10)
cat("Top 10 vols retardés au départ :\n")
print(top_retard_dep)

# Retardés à la fois au départ ET à l'arrivée
retard_les_deux <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay) & dep_delay > 0 & arr_delay > 0) %>%
  arrange(desc(arr_delay)) %>%
  slice_head(n = 10)

# Q2 : Retard moyen au départ
retard_moy_global <- mean(flights$dep_delay, na.rm = TRUE)
cat("\nRetard moyen au départ (global) :", round(retard_moy_global, 2), "min\n")

retard_moy_jour <- flights %>%
  mutate(date = as_date(sched_dep_time_dt)) %>%
  group_by(date) %>%
  summarise(retard_moy_jour = mean(dep_delay, na.rm = TRUE))

# Q3 : Vols arrivés avec +2h de retard mais partis à l'heure
retard_arr_ok_dep <- flights %>%
  filter(arr_delay > 120 & (is.na(dep_delay) | dep_delay <= 0))
cat("Arrivés > 2h retard mais partis à l'heure :", nrow(retard_arr_ok_dep), "\n")

# Vols sans retard >2h ni départ ni arrivée
sans_retard <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay) & dep_delay <= 120 & arr_delay <= 120)
cat("Vols sans retard > 2h :", nrow(sans_retard), "\n")

# Q4 : Décollage ou atterrissage plus tôt que prévu
partis_tot <- flights %>% filter(!is.na(dep_delay) & dep_delay < 0)
cat("Partis plus tôt que prévu :", nrow(partis_tot), "\n")

atterri_tot <- flights %>% filter(!is.na(arr_delay) & arr_delay < 0)
cat("Atterris plus tôt que prévu :", nrow(atterri_tot), "\n")

# Q5 & Q6 : Gain de temps en vol
flights <- flights %>%
  mutate(
    gain          = arr_delay - dep_delay,
    hours         = air_time / 60,
    gain_per_hour = gain / hours,
    speed         = distance / air_time * 60
  )

vols_rattrapé <- flights %>%
  filter(!is.na(gain) & dep_delay >= 60 & gain > 30) %>%
  arrange(desc(gain_per_hour))
cat("\nVols partis avec 1h retard ayant rattrapé >30min :", nrow(vols_rattrapé), "\n")

# Q7 : Vols triés par vitesse + long/court courrier
vols_vitesse <- flights %>%
  filter(!is.na(speed)) %>%
  arrange(desc(speed))

vols_long_courrier  <- flights %>% filter(!is.na(distance)) %>% arrange(desc(distance)) %>% slice_head(n = 10)
vols_court_courrier <- flights %>% filter(!is.na(distance)) %>% arrange(distance)       %>% slice_head(n = 10)

cat("\nVol le plus rapide :", vols_vitesse$flight[1], "| Vitesse :", round(vols_vitesse$speed[1], 1), "mph\n")

# Q8 : Retard moyen par aéroport
retard_par_aeroport <- flights %>%
  group_by(origin) %>%
  summarise(retard_moy = mean(arr_delay, na.rm = TRUE)) %>%
  left_join(airports %>% select(faa, name), by = c("origin" = "faa")) %>%
  arrange(retard_moy)

cat("\nAéroport avec le retard moyen le plus faible :", retard_par_aeroport$name[1], "\n")

# Q9 : Relation heure de décollage et retard
retard_par_heure <- flights %>%
  mutate(heure = hour(sched_dep_time_dt)) %>%
  group_by(heure) %>%
  summarise(retard_moy = mean(dep_delay, na.rm = TRUE))

ggplot(retard_par_heure, aes(x = heure, y = retard_moy)) +
  geom_line(color = "tomato", linewidth = 1) +
  geom_point(color = "tomato") +
  labs(
    title = "Retard moyen au départ selon l'heure de décollage",
    x = "Heure de départ", y = "Retard moyen (min)"
  ) +
  theme_minimal()
ggsave("figures/retard_par_heure.png", width = 9, height = 5)

# -----------------------------------------------------------------
# 2.4 RELATION DISTANCE ET RETARD
# -----------------------------------------------------------------

retard_distance <- flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),
    dist_moy  = mean(distance,  na.rm = TRUE),
    nb_vols   = n()
  ) %>%
  filter(dest != "HNL")  # exclure l'outlier HNL

ggplot(retard_distance, aes(x = dist_moy, y = delay_moy)) +
  geom_point(aes(size = nb_vols), alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Relation entre la distance et le retard moyen à l'arrivée",
    x = "Distance moyenne (miles)", y = "Retard moyen (min)", size = "Nb vols"
  ) +
  theme_minimal()
ggsave("figures/retard_vs_distance.png", width = 10, height = 6)

# Destinations les plus touchées par les retards
top_dest_retard <- retard_distance %>%
  arrange(desc(delay_moy)) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  slice_head(n = 10)
cat("\nTop 10 destinations les plus touchées par les retards :\n")
print(top_dest_retard)

# Résumé statistique arr_delay et dep_delay
cat("\nRésumé statistique arr_delay :\n")
print(summary(flights$arr_delay))
cat("Résumé statistique dep_delay :\n")
print(summary(flights$dep_delay))

# Visualisation distribution des retards
ggplot(flights %>% filter(!is.na(arr_delay) & arr_delay > -60 & arr_delay < 300),
       aes(x = arr_delay)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Distribution des retards à l'arrivée",
    x = "Retard (min)", y = "Nombre de vols"
  ) +
  theme_minimal()
ggsave("figures/distribution_retards.png", width = 10, height = 5)

# -----------------------------------------------------------------
# 2.5 VOLS ANNULÉS
# -----------------------------------------------------------------

# Q1 : Proportion de NA
na_dep_time  <- mean(is.na(flights$dep_time))  * 100
na_dep_delay <- mean(is.na(flights$dep_delay)) * 100
na_arr_time  <- mean(is.na(flights$arr_time))  * 100
na_arr_delay <- mean(is.na(flights$arr_delay)) * 100

cat("\nProportion de valeurs manquantes :\n")
cat("dep_time  :", round(na_dep_time, 2),  "%\n")
cat("dep_delay :", round(na_dep_delay, 2), "%\n")
cat("arr_time  :", round(na_arr_time, 2),  "%\n")
cat("arr_delay :", round(na_arr_delay, 2), "%\n")

# Q2 : Vols annulés (dep_time ET arr_time manquants)
vols_annules <- flights %>%
  filter(is.na(dep_time) & is.na(arr_time))

cat("\nNombre total de vols annulés :", nrow(vols_annules), "\n")

# Par destination
annules_dest <- vols_annules %>%
  count(dest, sort = TRUE) %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  slice_head(n = 10)
cat("Annulations par destination (top 10) :\n")
print(annules_dest)

# Par compagnie
annules_compagnie <- vols_annules %>%
  count(carrier, sort = TRUE) %>%
  left_join(airlines, by = "carrier")
cat("Annulations par compagnie :\n")
print(annules_compagnie)

# Visualisation annulations par compagnie
ggplot(annules_compagnie, aes(x = reorder(name, n), y = n, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Nombre de vols annulés par compagnie",
    x = "Compagnie", y = "Nombre d'annulations"
  ) +
  theme_minimal()
ggsave("figures/annulations_par_compagnie.png", width = 10, height = 6)

# Q3 : Trier par dep_delay, NA en premier
flights_na_first <- flights %>%
  arrange(is.na(dep_delay) == FALSE, desc(dep_delay))

# Q4 : Trier par dep_delay décroissant
flights_sorted <- flights %>%
  arrange(desc(dep_delay))

# =================================================================
# FIN DU SCRIPT  DÉCONNEXION
# =================================================================
dbDisconnect(con)
cat("\nDéconnexion de la base de données. Script terminé avec succès !\n")
