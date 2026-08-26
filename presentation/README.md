# Présentation finale

Support de soutenance du projet et figures qui l'alimentent.

| Fichier | Contenu |
|---|---|
| `Trafic_Aerien_ADP_presentation.pptx` | 13 slides, notes du présentateur incluses |
| `figures.R` | Génère les cinq graphiques depuis l'API |
| `figures/01` → `05` | Graphiques ggplot2 (retards par heure, trafic journalier, compagnies, distance, annulations) |
| `figures/10` → `12` | Captures du dashboard (vue d'ensemble, carte, retards) |

## Regénérer les figures

Base et API démarrées, depuis la racine du dépôt :

```bash
Rscript presentation/figures.R
```

Les cinq PNG sont réécrits dans `presentation/figures/`. Aucune valeur n'est
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
4. Le dashboard en huit onglets
5. Géographie : trois aéroports, top destinations et lignes
6. Calendrier : régularité hebdomadaire et couverture réelle des données
7. **Retards : l'effet de l'heure de départ** — le résultat principal
8. Compagnies : ponctualité et pièges d'effectifs
9. Distance et rattrapage en vol
10. Annulations
11. Limites du jeu de données
12. Défaut relevé sur `/delays/gain`
13. Démonstration et prochaines étapes
