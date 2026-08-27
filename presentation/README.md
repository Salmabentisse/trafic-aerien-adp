# Présentation finale

Support de soutenance du projet et figures qui l'alimentent.

| Fichier | Contenu |
|---|---|
| `Trafic_Aerien_ADP_presentation.pptx` | 19 slides, notes du présentateur incluses |
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

L'énoncé impose trois blocs de 10 minutes suivis de 10 minutes de questions. Les
19 slides sont rangées dans cet ordre, pour que chacun enchaîne sans chercher.

### Ouverture — commune (environ 2 min)

| # | Slide | Qui |
|---|---|---|
| 1 | Couverture | Marcus |
| 2 | Le sujet et les cinq sources | Marcus |
| 3 | Architecture en quatre étages et rôles | Marcus |

### Bloc 1 — Traitement des données, schéma, mise en production (10 min)

| # | Slide | Qui | Question de l'énoncé |
|---|---|---|---|
| 4 | Les anomalies trouvées, et ce qu'on en a fait | **Salma** | i) anomalies, valeurs aberrantes et manquantes |
| 5 | Un schéma qui refuse les données incohérentes | **Meissa** | ii) le temps et les fuseaux horaires |
| 6 | De la base au service interrogeable | **Hedi** | iii) la gestion des privilèges |

Environ 3 min 20 chacun.

### Bloc 2 — L'analyse demandée (10 min)

| # | Slide | Qui |
|---|---|---|
| 7 | Trois aéroports à parts presque égales | **Hassan** |
| 8 | Un trafic très régulier, sauf le samedi | **Hassan** |
| 9 | Trois aéroports, trois régimes (viz par facette, §3.1 Q2) | **Hassan** |
| 10 | Le retard n'est pas subi, il s'accumule | **Hassan** |
| 11 | De 24 % à 59 % de vols en retard selon la compagnie | **Hassan** |
| 12 | Plus c'est loin, moins le retard pèse (§3.2 Q7) | **Hassan** |
| 13 | 2,56 % des vols annulés, concentrés sur l'hiver | **Hassan** |
| 14 | Ce que ces données ne permettent pas de dire | **Hassan** |

### Bloc 3 — Reporting interactif et prédictif, Mission 3 (10 min)

| # | Slide | Qui |
|---|---|---|
| 15 | Un dashboard en neuf onglets | **Marcus** |
| 16 | L'année inscrite dans la base n'est pas celle des données | **Marcus** |
| 17 | Prévoir le trafic à 30 jours, avec 7,3 % d'erreur | **Marcus** |
| 18 | Un endpoint de l'API donnait l'inverse de son intitulé | **Marcus** |
| 19 | Trois commandes pour tout lancer, et la suite | **Marcus** |

La démonstration live du dashboard se place sur la slide 15 ou 19.

### Deux points à compléter par leurs auteurs

Les slides 5 et 6 assument deux limites du projet, sur la base de ce que contient
le dépôt. À vérifier avec Meissa et Hedi avant la soutenance, ils peuvent avoir
fait des choses qui ne sont pas versionnées :

- **fuseaux horaires** : `tz`, `tzone` et `time_hour` sont stockés mais aucune
  conversion n'est appliquée, tout reste en heure locale de l'aéroport de départ ;
- **privilèges** : un seul compte `adp_user` avec tous les droits, mot de passe
  identique à celui de root, en clair dans `docker-compose.yml` et `.Renviron`,
  tous deux versionnés dans un dépôt public.

