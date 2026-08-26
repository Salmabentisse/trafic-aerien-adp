# Trafic Aérien ADP

Projet de reporting du trafic aérien (données BTS, vols au départ de NYC : JFK, LGA, EWR).  
Stack : **MySQL 8** + **API REST Plumber (R)** + dashboard **Shiny** (équipe frontend).

## Architecture

```
┌──────────────┐     ┌─────────────┐     ┌──────────────────┐     ┌─────────┐
│ Fichiers     │ --> │ MySQL 8     │ <-- │ API Plumber (R)  │ <-- │ Shiny   │
│ data/*       │     │ 5 tables    │     │ :8000            │     │ (front) │
└──────────────┘     └─────────────┘     └──────────────────┘     └─────────┘
     équipe data          équipe BD            backend                 frontend
```

**Tables :** `airlines` ← `flights` → `airports` (origin/dest)  
`flights.tailnum` → `planes`  
`weather.origin` → `airports`

## Prérequis

- [R 4.4+](https://cran.r-project.org/) (testé avec 4.6.1)
- [Docker](https://www.docker.com/) (MySQL)
- Packages R :

```r
install.packages(c(
  "plumber", "DBI", "RMariaDB", "jsonlite",
  "readxl", "rvest", "pdftools", "dplyr", "tidyr", "stringr"
))
```

Variables d’environnement : fichier `.Renviron` à la racine (déjà utilisé par les scripts R).

```
DB_HOST=localhost
DB_PORT=3306
DB_NAME=trafic_aerien_adp
DB_USER=adp_user
DB_PASS=...
```

## Lancement

### 1. Base MySQL

```bash
docker compose up -d
```

Créer le schéma (une fois) :

```bash
docker exec -i trafic-mysql mysql -uadp_user -p"$DB_PASS" trafic_aerien_adp < schema.sql
```

Sous Windows (PowerShell), avec le mot de passe du `.Renviron` :

```powershell
Get-Content schema.sql -Raw | docker exec -i trafic-mysql mysql -uroot "-pTestSimple123"
```

### 2. Injection des données

Depuis la **racine** du dépôt :

```powershell
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" data/05_injection_mysql.R
```

Le script charge `data/airlines.json`, `airports.xlsx`, `flights.xlsx`, `planes.html`, `weather.pdf`, complète les FK (BQN/PSE/SJU/STT, tailnums manquants) et injecte les 5 tables.

Volumes observés sur ce jeu :

| Table     | Lignes  |
|-----------|---------|
| airlines  | 16      |
| airports  | ~1 462  |
| planes    | ~4 038  |
| weather   | ~3 299  |
| flights   | ~252 704 |

> L’énoncé cite 326 776 vols : le fichier `flights.xlsx` du dépôt en contient **252 704**. Idem, `weather.pdf` est un extrait (~3 299 lignes).

Vérifier la connexion :

```powershell
cd backend
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" test_connection.R
```

### 3. API backend

```powershell
cd backend
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" run.R
```

- API : http://127.0.0.1:8000  
- Swagger : http://127.0.0.1:8000/__docs__/  
- Santé : http://127.0.0.1:8000/health  

## API — endpoints principaux

| Méthode | Route | Rôle |
|---------|--------|------|
| GET | `/health` | Statut API + MySQL |
| GET | `/stats/overview` | Comptages globaux |
| GET | `/flights` | Liste paginée (filtres `carrier`, `origin`, `dest`, `month`, …) |
| GET | `/flights/most-delayed` | Top retards |
| GET | `/flights/{id}` | Détail d’un vol |
| GET | `/traffic/monthly` | Trafic mensuel par aéroport |
| GET | `/traffic/by-period` | janv., été, jours fériés US, 0h–6h, … |
| GET | `/delays/summary` | Stats retards |
| GET | `/delays/by-carrier` | Fiabilité compagnies |
| GET | `/cancellations` | Vols annulés (`dep_time` ET `arr_time` nuls) |
| GET | `/routes/map` | Coordonnées pour carte US |
| GET | `/weather?origin=JFK` | Météo horaire |

CORS : origines `http://localhost:3000` et `http://localhost:5173` (surcharge via `CORS_ORIGINS`).

## Structure du dépôt

```
trafic-aerien-adp/
├── schema.sql                 # Schéma MySQL (PK/FK/index)
├── docker-compose.yml         # MySQL 8 local
├── .Renviron                  # Identifiants BD (ne pas publier en prod)
├── data/
│   ├── 01_exploration.R
│   ├── 02_connexion_db.R
│   ├── 03_creation_schema.R
│   ├── 04_nettoyage_avant_injection.R
│   ├── 05_injection_mysql.R   # Chargement + injection
│   ├── airlines.json
│   ├── airports.xlsx
│   ├── flights.xlsx
│   ├── planes.html
│   └── weather.pdf
└── backend/
    ├── run.R                  # Démarre Plumber
    ├── plumber.R              # Routes REST
    ├── test_connection.R
    └── R/
        ├── database.R         # Connexion RMariaDB
        └── utils.R            # Pagination, validation, erreurs
```

## Répartition des rôles

| Rôle | Livrable |
|------|----------|
| Data | Nettoyage + `05_injection_mysql.R` |
| BD | `schema.sql`, MySQL |
| **Backend** | API Plumber, connexion sécurisée, endpoints JSON |
| Analyse | Indicateurs, requêtes métier |
| Frontend | **Shiny** consommant l’API |

## Dépannage

- **Port 3306 occupé** : arrêter un MySQL local, ou changer le mapping dans `docker-compose.yml`.
- **Packages R en lecture seule** : installer dans la lib utilisateur (`AppData/Local/R/win-library/...`).
- **RMySQL / MySQL 8** : le backend utilise **RMariaDB** (plus fiable que `RMySQL` avec l’auth `caching_sha2`).
