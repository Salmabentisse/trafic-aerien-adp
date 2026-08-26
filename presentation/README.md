# Présentation finale

Support de soutenance du projet et figures qui l'alimentent.

| Fichier | Contenu |
|---|---|
| `Trafic_Aerien_ADP_presentation.pptx` | 16 slides, notes du présentateur incluses |
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

## Plan du support

1. Couverture
2. Le sujet et les cinq sources
3. Architecture en quatre étages et répartition des rôles
4. Le dashboard en neuf onglets
5. Géographie : trois aéroports, top destinations et lignes
6. Calendrier : régularité hebdomadaire et couverture réelle des données
7. Trois aéroports, trois régimes — la viz par facette demandée (§3.1 Q2)
8. **Retards : l'effet de l'heure de départ** — le résultat principal
9. Compagnies : ponctualité et pièges d'effectifs
10. Distance, vitesse et types de courrier (§3.2 Q7)
11. Annulations
12. **L'année de la base n'est pas celle des données** — découverte
13. **Analyse prédictive** — modèle et validation (Mission 3)
14. Limites du jeu de données
15. Défaut relevé sur `/delays/gain`
16. Démonstration et prochaines étapes
