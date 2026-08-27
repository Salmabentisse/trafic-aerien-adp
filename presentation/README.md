# Présentation finale

Support de soutenance du projet et figures qui l'alimentent.

| Fichier | Contenu |
|---|---|
| `Trafic_Aerien_ADP_presentation.pptx` | 13 slides, notes du présentateur incluses |
| `figures.R` | Génère les cinq graphiques depuis l'API |
| `figures/01` → `07` | Graphiques ggplot2 (retards par heure, trafic journalier, compagnies, distance, annulations, facettes mensuelles, prévision) |
| `figures/10` → `14` | Captures du dashboard (vue d'ensemble, carte, retards, prévisions, facettes) |

## Regénérer les figures

Base et API démarrées, depuis la racine du dépôt :

```bash
Rscript presentation/figures.R
```

Les sept PNG sont réécrits dans `presentation/figures/`. Aucune valeur n'est
codée en dur : titres, sous-titres et corrélation sont calculés depuis les
réponses de l'API, ce qui évite qu'un chiffre du support cesse de correspondre
aux données après une réinjection.

Packages nécessaires : `ggplot2`, `httr2`, `jsonlite`.

## Regénérer les captures du dashboard

Nécessite `webshot2`, `chromote` et Google Chrome installé.

```r
Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
webshot2::webshot("http://127.0.0.1:3838",
                  "presentation/figures/10_dashboard_vue_ensemble.png",
                  vwidth = 1600, vheight = 1000, delay = 14)
```

Pour les onglets autres que le premier, il faut piloter la navigation : ouvrir une
session `chromote`, cliquer le lien de l'onglet puis capturer (voir l'historique
Git de ce dossier pour le script utilisé).

## Plan du support et répartition des temps de parole

Trois blocs de 10 minutes imposés par l'énoncé, puis 10 minutes de questions.
Deux ou trois slides par personne, pour qu'aucun bloc ne traîne.

### Bloc 1 — Traitement des données, schéma, mise en production (10 min)

| # | Slide | Qui | Question de l'énoncé |
|---|---|---|---|
| 1 | 252 704 vols au départ de New York, décortiqués | **Salma** — elle ouvre | — |
| 2 | Cinq formats en entrée, une chaîne en quatre étages | **Salma** | — |
| 3 | Les anomalies trouvées, et ce qu'on en a fait | **Salma** | i) anomalies, valeurs aberrantes et manquantes |
| 4 | Un schéma qui refuse les données incohérentes | **Meissa** | — |
| 5 | Le temps et les fuseaux horaires, sans faire semblant | **Meissa** | ii) le temps et les fuseaux horaires |
| 6 | De la base au service interrogeable | **Hedi** | — |
| 7 | Les privilèges : le point faible qu'on assume | **Hedi** | iii) la gestion des privilèges |

### Bloc 2 — L'analyse demandée (10 min)

| # | Slide | Qui |
|---|---|---|
| 8 | Trois aéroports à parts égales, un rythme net | **Hassan** |
| 9 | Le retard n'est pas subi, il s'accumule | **Hassan** |
| 10 | Annulations, et les limites du jeu de données | **Hassan** |

### Bloc 3 — Reporting interactif et prédictif, Mission 3 (10 min)

| # | Slide | Qui |
|---|---|---|
| 11 | Un dashboard en neuf onglets, branché sur l'API | **Marcus** — démonstration live ici |
| 12 | Prévoir le trafic à 30 jours, avec 7,3 % d'erreur | **Marcus** |
| 13 | Construire les écrans a révélé deux défauts, et le lancement | **Marcus** |

### Récapitulatif

| Qui | Slides | Nombre |
|---|---|---|
| Salma | 1, 2, 3 | 3 |
| Meissa | 4, 5 | 2 |
| Hedi | 6, 7 | 2 |
| Hassan | 8, 9, 10 | 3 |
| Marcus | 11, 12, 13 | 3 |

C'est **Salma qui ouvre** la soutenance, la slide 2 servant de passage de relais :
elle annonce le pipeline et qui parle de quoi.

### Deux points à valider avec leurs auteurs

Les slides 5 et 7 assument deux limites du projet, sur la seule base de ce que
contient le dépôt. À confirmer avec Meissa et Hedi, qui ont peut-être fait des
choses non versionnées :

- **fuseaux horaires** : `tz`, `tzone` et `time_hour` sont stockés mais aucune
  conversion n'est appliquée. L'argument qui tient : les retards sont des durées
  en minutes, donc insensibles aux fuseaux, et c'est sur eux que porte l'analyse ;
- **privilèges** : un seul compte `adp_user` avec tous les droits, mot de passe
  identique à celui de root, en clair dans `docker-compose.yml` et `.Renviron`,
  tous deux versionnés dans un dépôt public.

