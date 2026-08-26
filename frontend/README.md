# Dashboard Shiny — Trafic Aérien ADP

Partie **Dashboard / Frontend & présentation** du projet. Application Shiny qui
consomme l'API REST Plumber du backend : **aucune connexion directe à MySQL**,
ce qui permet de lancer (ou déployer) le dashboard et la base indépendamment.

```
MySQL 8  ──►  API Plumber (:8000)  ──►  Dashboard Shiny (:3838)
```

## Prérequis

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "httr2"))
```

> `leaflet` n'est volontairement pas utilisé : il tire `raster` / `terra` / `sf`,
> donc GDAL, GEOS et PROJ à installer sur chaque poste. La carte est faite avec
> `plotly::scattergeo`, déjà disponible via `plotly`.

## Lancement

L'API doit tourner (`Rscript backend/run.R`). Ensuite, depuis la racine du dépôt :

```bash
Rscript frontend/run_app.R
```

Dashboard : <http://127.0.0.1:3838>

Équivalent depuis une session R :

```r
shiny::runApp("frontend")
```

### Variables d'environnement

| Variable       | Défaut                  | Rôle                                  |
|----------------|-------------------------|---------------------------------------|
| `API_BASE_URL` | `http://127.0.0.1:8000` | Adresse de l'API Plumber              |
| `API_TIMEOUT`  | `90`                    | Délai d'attente des appels (secondes) |
| `SHINY_PORT`   | `3838`                  | Port d'écoute du dashboard            |

Exemple (API sur une autre machine) :

```bash
API_BASE_URL=http://192.168.1.20:8000 Rscript frontend/run_app.R
```

## Les onglets

| Onglet | Contenu | Endpoints |
|--------|---------|-----------|
| **Vue d'ensemble** | KPI de volumétrie et de retard, répartition par aéroport, top destinations, semaine vs week-end, lignes les plus fréquentées | `/stats/overview`, `/delays/summary`, `/traffic/top-airports`, `/traffic/weekday-vs-weekend`, `/routes/top` |
| **Trafic** | Saisonnalité mensuelle par aéroport, croissance d'un mois sur l'autre, courbe journalière, zoom par période, jours fériés vs moyenne | `/traffic/monthly`, `/traffic/daily`, `/traffic/by-period`, `/traffic/special-days-vs-average` |
| **Retards** | Synthèse (journalier, par heure, avance/retard), fiabilité des compagnies, aéroports et destinations, distance vs retard, vitesse et types de courrier, vols extrêmes et rattrapage en vol | `/delays/*`, `/flights`, `/flights/most-delayed` |
| **Annulations** | Taux d'annulation, saisonnalité, compagnies et destinations touchées, valeurs manquantes | `/cancellations`, `/cancellations/sorted` |
| **Carte** | Réseau origine → destination sur fond de carte US, épaisseur proportionnelle au trafic | `/routes/map` |
| **Météo** | Température, vent, précipitations et visibilité horaires par aéroport | `/weather` |
| **Vols** | Explorateur filtré (compagnie, aéroports, date, retard, annulations) avec tri et pagination côté API | `/flights` |
| **Prévisions** | Modèle saisonnier (tendance + mois + jour de la semaine + jours fériés), validation sur les 28 derniers jours face à une référence naïve, prévision à 7–90 jours | `/traffic/daily`, `/delays/daily` |
| **Données & méthode** | Sources, conventions de lecture, schéma de la chaîne de traitement | — |

## Organisation du code

```
frontend/
├── app.R                    # assemblage navbar + appel des modules
├── run_app.R                # lanceur (port, API_BASE_URL, .Renviron)
└── R/
    ├── api.R                # client HTTP de l'API + normalisation JSON -> data.frame
    ├── utils_ui.R           # thème bslib, palette, formatage FR, helpers plotly/DT
    ├── mod_overview.R       # un module Shiny par onglet
    ├── mod_traffic.R
    ├── mod_delays.R
    ├── mod_cancellations.R
    ├── mod_map.R
    ├── mod_weather.R
    ├── mod_flights.R
    └── mod_forecast.R       # modèle de prévision + validation
```

### Le modèle de prévision

`R/mod_forecast.R` ajuste une régression linéaire sur le logarithme du trafic
journalier :

```
log(vols) ~ tendance + mois + jour de la semaine + jour férié + période des fêtes
```

Validation : le modèle est réajusté sans les 28 derniers jours connus, puis
confronté à eux. La référence de comparaison répète la dernière semaine **connue**
— reprendre « la valeur d'il y a sept jours » ferait piocher la référence dans la
période de test dès le huitième jour.

Résultat sur ce jeu : 63 vols d'erreur moyenne par jour (7,3 % en relatif),
contre 159 pour la référence naïve.

> **Attention au calendrier.** La colonne `year` de la base vaut 2021, mais le
> rythme hebdomadaire ne correspond qu'à 2013 : sous ce calendrier le creux tombe
> sur le samedi (741 vols/jour contre plus de 940 en semaine), alors qu'en 2021 il
> tomberait un mardi. Cet onglet ramène donc les dates au calendrier 2013 et
> l'affiche à l'écran. Sans cette correction, l'effet du samedi est attribué au
> mardi et le modèle apprend un rythme faux.

Deux points d'attention repris dans `R/api.R` :

- le backend sérialise les valeurs manquantes en objet vide `{}` — elles sont
  ramenées à `NA` puis les colonnes sont retypées en numérique quand c'est
  possible (`records_to_df()`) ;
- les référentiels (compagnies, aéroports, aéroports de départ) sont mis en
  cache mémoire avec un TTL, pour ne pas rappeler l'API à chaque interaction.

Si l'API est arrêtée, l'application reste utilisable : le badge de la barre de
navigation passe au rouge et chaque graphique affiche le message d'erreur renvoyé
par l'appel plutôt que de faire tomber la session.

## Dépannage

| Symptôme | Cause probable |
|----------|----------------|
| Badge « API injoignable » | Backend non lancé, ou `API_BASE_URL` incorrect |
| Graphiques vides, message « Erreur SQL » | MySQL arrêté (`docker compose up -d`) ou base non injectée |
| Carte sans fond de carte | Pas d'accès réseau : `plotly` télécharge les contours depuis son CDN |
| Port 3838 occupé | `SHINY_PORT=3839 Rscript frontend/run_app.R` |
