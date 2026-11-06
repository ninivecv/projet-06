# Projet d’analyse de données : profils métaboliques et diabète

## Présentation du projet

Ce projet utilise le **Diabetes Prediction Dataset** de [Kaggle](https://www.kaggle.com/datasets/mobeenfatimah/diabetes-risk-prediction-dataset-50k-patients) pour étudier les profils métaboliques associés au diabète et classifier le statut diabétique (`Diabetes_Risk`, `Low`, `Moderate` et `High`) à partir de variables démographiques et cliniques. Le jeu de données comprend notamment l’âge, l’IMC (`BMI`), le taux d’HbA1c (`HbA1c`), le glucose sanguin (`Blood_Glucose`), ainsi que des indicateurs comme l’hypertension (`Hypertension`) et les maladies cardiaques (`Heart_Disease`).

Le jeu de données est placé dans `data/raw/diabetes_risk_raw.csv`. Les analyses et scripts du projet sont organisés ainsi :

``` text
projet-stt2200/
├── data/
│   ├── raw/
│   │   └── diabetes_risk_raw.csv
│   └── processed/
├── scripts/
│   ├── EDA.R
│   ├── ACP.R
│   ├── k_means.R
│   └── k_NN.R
└── description/
    └── ai-log.md
```

## Problématique

> Dans quelle mesure la combinaison des biomarqueurs glycémiques (HbA1c et glucose), des indicateurs morphologiques (IMC) et des antécédents cliniques permet-elle de structurer les profils métaboliques de la population et de classifier le statut diabétique ?

## Objectifs et hypothèse

1.  Explorer les distributions et les liens entre les marqueurs métaboliques et le statut diabétique.
2.  Réduire la dimension des variables quantitatives à l’aide d’une analyse en composantes principales (ACP) afin de dégager les principaux axes de variation des profils.
3.  Segmenter les individus en groupes aux caractéristiques métaboliques similaires avec les k-moyennes.
4.  Entraîner et évaluer un modèle des k plus proches voisins (k-NN) pour classifier le statut diabétique.

**Hypothèses :**

1.  L’IMC est plus fortement associé au statut diabétique que l’HbA1c et le glucose sanguin.
2.  Les niveaux de glucose et d’HbA1c ainsi que l’IMC sont couramment utilisés pour évaluer le risque de diabète. Les habitudes de vie (le tabagisme, l’activité physique et l’alimentation) nous permettent également de distinguer les personnes diabétiques des personnes non diabétiques.

## Répartition des tâches

| Membre                                             | Responsabilité                                                          | Démarche                                                                                                                                                                                                                                                                                                                                                       | Visuels et résultats à présenter                                                                                                            |
|------------------|------------------|------------------|------------------|
| **Ninve : Analyse exploratoire (EDA)**             | Décrire les données et comparer les groupes selon le statut diabétique. | Calculer moyenne, médiane et écart-type de l’âge, de l’IMC, de l’HbA1c et du glucose par statut (0 ou 1). Étudier les corrélations entre variables continues. Utiliser des tests appropriés (Student ou Mann–Whitney, selon les conditions) pour comparer notamment le glucose et l’HbA1c.                                                                     | Matrice de corrélation (carte thermique) et boîtes à moustaches du glucose et de l’HbA1c selon le statut. Résumer les principaux constats.  |
| **Ève : Analyse en composantes principales (ACP)** | Étudier les principaux axes de variation des profils métaboliques.      | Centrer et réduire les variables quantitatives (`Age`, `BMI`, `HbA1c`, `Blood_Glucose`). Examiner l’éboulis des valeurs propres et interpréter les axes à partir des contributions et corrélations des variables.                                                                                                                                              | Éboulis des valeurs propres, cercle des corrélations et projection des individus sur le plan factoriel, colorée selon le statut diabétique. |
| **Albert : k-moyennes (k-means)**                  | Segmenter et décrire des groupes de profils métaboliques.               | Standardiser les variables continues retenues. Choisir le nombre de groupes en examinant la méthode du coude et le coefficient de silhouette. Exécuter l’algorithme avec plusieurs initialisations (par exemple `nstart = 25`), puis caractériser chaque groupe.                                                                                               | Graphique de choix du nombre de groupes et projection des groupes sur le plan de l’ACP. Présenter le profil de chaque groupe.               |
| **Aymeric : k plus proches voisins (k-NN)**        | Construire et évaluer un modèle de classification du statut diabétique. | Séparer les données en ensembles d’entraînement et de test (par exemple 80/20), en tenant compte du déséquilibre possible des classes. Normaliser les variables sans fuite d’information depuis l’ensemble test. Choisir le paramètre `k` par validation croisée et évaluer les performances, en portant attention à la sensibilité pour la classe diabétique. | Erreur selon `k`, matrice de confusion et courbe ROC avec aire sous la courbe (AUC), accompagnées des métriques de performance pertinentes. |

## Affiche et présentation orale

### Organisation proposée de l’affiche

1.  **En-tête :** titre du projet, noms des quatre membres et sigle du cours.
2.  **Introduction et données :** problématique, objectifs, hypothèses et présentation des variables étudiées.
3.  **Exploration et ACP :** constats descriptifs et carte thermique (Ninive), puis cercle des corrélations et plan factoriel (Ève).
4.  **Segmentation et classification :** profils des groupes issus des k-moyennes (Albert), puis matrice de confusion, métriques et courbe ROC du k-NN (Aymeric).
5.  **Discussion et conclusion :** réponse à la problématique et examen des deux hypothèses : comparer l’association de l’IMC à celle de l’HbA1c et du glucose, puis déterminer si les habitudes de vie aident à distinguer les personnes diabétiques des personnes non diabétiques. Présenter les limites des données et la conclusion générale.

### Fil conducteur de l’oral

-   **Ninive** : présente les données et les principaux écarts observés entre les groupes.
-   **Ève** : explique les axes dégagés par l’ACP et ce qu’ils révèlent sur les profils.
-   **Albert** : décrit les groupes obtenus par les k-moyennes et leurs caractéristiques.
-   **Aymeric** : présente l’évaluation du k-NN et ses limites pour classifier le statut diabétique.
-   **Conclusion collective :** répondre à la problématique, discuter les deux hypothèses et situer les limites de l’étude, par exemple la disponibilité des variables d’habitudes de vie, l’absence de certains facteurs génétiques ou cliniques et la représentativité du jeu de données.
