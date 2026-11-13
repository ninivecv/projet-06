# Profils liés au risque de diabète

## Présentation du projet

Ce projet utilise le jeu de données [Diabetes Risk Prediction](https://www.kaggle.com/datasets/mobeenfatimah/diabetes-risk-prediction-dataset-50k-patients) pour comparer les profils qui se dégagent de deux groupes de variables : des mesures cliniques couramment utilisées pour évaluer le risque de diabète et des habitudes de vie.

Les trois variables cliniques étudiées seront l’IMC (`BMI`), l’HbA1c (`HbA1c`) et le glucose sanguin (`Blood_Glucose`). Les trois habitudes de vie seront le tabagisme (`Smoking_Status`), le niveau d’activité physique (`Physical_Activity_Level`) et la qualité de l’alimentation (`Diet_Quality`). Les variables d’habitudes de vie étant catégorielles ou ordinales, elles devront être codées de façon cohérente avant leur utilisation dans les k-moyennes.

Le fichier de données se trouve dans `data/raw/diabetes_risk.csv`. La variable `Diabetes_Risk` contient les catégories `Low`, `Moderate` et `High`. Elle pourra servir à décrire les groupes obtenus, mais l’étude porte principalement sur la comparaison des deux segmentations.

## Problématique

> Les groupes obtenus avec l’IMC, l’HbA1c et le glucose ressemblent-ils aux groupes obtenus avec le tabagisme, l’activité physique et la qualité de l’alimentation ?

## Objectif

Comparer les groupes formés par les k-moyennes à partir des trois variables cliniques avec ceux formés à partir des trois variables d’habitudes de vie, puis décrire leurs ressemblances et leurs différences à l’aide de l’ACP et de l’analyse exploratoire.

## Hypothèse

Les groupes formés à partir des habitudes de vie ne seront pas entièrement les mêmes que ceux formés à partir des variables cliniques, car ces deux ensembles de variables décrivent des aspects différents du profil des personnes. On s’attend toutefois à observer certains recoupements entre les groupes.

## Préparation et comparaison des groupes

Avant l’analyse, nous vérifierons les valeurs manquantes et expliquerons comment nous les traiterons. Nous standardiserons les variables cliniques et préciserons comment les habitudes de vie sont codées.

Nous créerons les deux segmentations à partir des mêmes personnes, puis nous comparerons les groupes avec un tableau croisé et l’indice de Rand ajusté (ARI). Un ARI proche de 1 signifie que les regroupements se ressemblent beaucoup et proche de 0 signifie qu’ils ne se ressemblent pas plus que prévu par hasard. Les numéros des groupes servent seulement à les distinguer.

## Répartition des tâches

| Membre                                                 | Responsabilité                                             | Tâches                                                                                                                                                                                |
|------------------------|------------------------|------------------------|
| **Ninive : Analyse exploratoire (EDA)**                | Décrire les données et les six variables retenues.         | Vérifier les valeurs manquantes et les catégories, résumer les variables cliniques et les habitudes de vie avec des tableaux et des graphiques simples.                               |
| **Albert : k-moyennes (k-means)**                      | Réaliser et comparer les segmentations.                    | Documenter la préparation des variables, appliquer les k-moyennes aux deux groupes de variables, puis comparer les résultats avec un tableau croisé et l’indice de Rand ajusté (ARI). |
| **Ève : Affiche**                                      | Préparer la présentation visuelle du projet.               | Organiser la problématique, l’objectif, l’hypothèse, les méthodes, les principaux résultats et la conclusion sur l’affiche.                                                           |
| **Aymeric : Analyse en composantes principales (ACP)** | Visualiser les profils et aider à interpréter les groupes. | Réaliser une ACP pour les variables cliniques et une pour les variables d’habitudes de vie après leur préparation, produire les graphiques et résumer les axes principaux.            |

## Démarche générale

1.  Décrire les six variables retenues, repérer les valeurs manquantes et consigner leur traitement.
2.  Préparer et documenter les deux ensembles de variables : cliniques et habitudes de vie.
3.  Appliquer les k-moyennes aux mêmes personnes pour chacun des ensembles.
4.  Comparer les groupes avec un tableau croisé et l’ARI. Utiliser les ACP pour visualiser les profils.
5.  Présenter les résultats et les limites, notamment celles du codage et le fait que les groupes sont des profils statistiques, pas des diagnostics médicaux.
