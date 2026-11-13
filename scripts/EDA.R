# ============================================================
# Analyse exploratoire des données (EDA)
# Projet : comparaison des variables cliniques et des habitudes de vie
# ============================================================

# Charger les bibliothèques utilisées pour manipuler les données et créer les graphiques.
library(dplyr)   # Sélectionner, compter et résumer les colonnes
library(tidyr)   # Réorganiser les variables pour les graphiques
library(ggplot2) # Créer les graphiques

# ------------------------------------------------------------
# 1. Importer le jeu de données
# ------------------------------------------------------------

# Ce chemin suppose que le répertoire de travail est la racine du projet,
# c'est-à-dire le dossier qui contient README.md et le dossier data.
diabetes_risk <- read.csv(
  "data/raw/diabetes_risk.csv",
  stringsAsFactors = FALSE
)

# Vérifier rapidement la structure, les noms et les types des variables.
glimpse(diabetes_risk)

# Afficher les premières observations afin de vérifier l'importation.
head(diabetes_risk)

# Afficher le nombre de lignes (observations) et de colonnes (variables).
dim(diabetes_risk)

# ------------------------------------------------------------
# 2. Définir les six variables retenues pour le projet
# ------------------------------------------------------------

# Les variables cliniques sont des mesures quantitatives.
variables_cliniques <- c("BMI", "HbA1c", "Blood_Glucose")

# Les variables d'habitudes de vie décrivent le tabagisme,
# l'activité physique et la qualité de l'alimentation.
variables_habitudes <- c(
  "Smoking_Status",
  "Physical_Activity_Level",
  "Diet_Quality"
)

# Diabetes_Risk sert à décrire les catégories de risque dans l'EDA.
# Cette colonne ne fait pas partie des six variables à comparer.
variable_groupe <- "Diabetes_Risk"
variables_attendues <- c(variables_cliniques, variables_habitudes, variable_groupe)

# Arrêter le script avec un message explicite si un nom de colonne est incorrect.
colonnes_absentes <- setdiff(variables_attendues, names(diabetes_risk))
if (length(colonnes_absentes) > 0) {
  stop("Colonnes absentes du fichier : ", paste(colonnes_absentes, collapse = ", "))
}

# Garder les six variables du projet et la catégorie de risque pour les résumés.
donnees_eda <- diabetes_risk %>%
  select(all_of(variables_attendues))

# ------------------------------------------------------------
# 3. Vérifier les valeurs manquantes et les catégories
# ------------------------------------------------------------

# Compter les valeurs manquantes par variable et calculer leur pourcentage.
# Ces résultats aideront l'équipe à décider comment traiter les valeurs manquantes.
resume_NA <- data.frame(
  Variable = names(donnees_eda),
  Nombre_NA = colSums(is.na(donnees_eda)),
  Pourcentage_NA = round(colMeans(is.na(donnees_eda)) * 100, 2)
)
resume_NA

# Afficher les effectifs des catégories de risque, y compris les valeurs manquantes.
# Cela permet de voir si les groupes Low, Moderate et High sont équilibrés.
table(donnees_eda$Diabetes_Risk, useNA = "ifany")

# Pour chacune des habitudes, afficher les réponses distinctes observées.
# Cela permet de vérifier les catégories avant de décider de leur codage.
lapply(variables_habitudes, function(variable) {
  cat("\nCatégories pour", variable, ":\n")
  print(table(donnees_eda[[variable]], useNA = "ifany"))
})

# ------------------------------------------------------------
# 4. Résumer les trois variables cliniques
# ------------------------------------------------------------

# Calculer le nombre de valeurs disponibles, la moyenne, la médiane
# et l'écart-type de chaque mesure clinique par catégorie de risque.
# na.rm = TRUE demande à R d'ignorer les valeurs manquantes pour le calcul.
resume_clinique <- donnees_eda %>%
  group_by(Diabetes_Risk) %>%
  summarise(
    across(
      all_of(variables_cliniques),
      list(
        nombre = ~ sum(!is.na(.x)),
        moyenne = ~ mean(.x, na.rm = TRUE),
        mediane = ~ median(.x, na.rm = TRUE),
        ecart_type = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
resume_clinique

# ------------------------------------------------------------
# 5. Résumer les habitudes de vie dans un tableau
# ------------------------------------------------------------

# Mettre les trois habitudes dans une même colonne pour compter facilement
# chaque réponse selon l'habitude et la catégorie de risque.
resume_habitudes <- donnees_eda %>%
  pivot_longer(
    cols = all_of(variables_habitudes),
    names_to = "Habitude",
    values_to = "Reponse"
  ) %>%
  count(Habitude, Diabetes_Risk, Reponse, name = "Effectif") %>%
  arrange(Habitude, Diabetes_Risk, Reponse)
resume_habitudes

# ------------------------------------------------------------
# 6. Graphique des variables cliniques
# ------------------------------------------------------------

# Réorganiser l'IMC, l'HbA1c et le glucose pour afficher un panneau par mesure.
clinique_long <- donnees_eda %>%
  select(Diabetes_Risk, all_of(variables_cliniques)) %>%
  pivot_longer(
    cols = all_of(variables_cliniques),
    names_to = "Mesure",
    values_to = "Valeur"
  )

# Les boîtes à moustaches comparent les distributions selon la catégorie de risque.
# Chaque panneau a sa propre échelle verticale, car les mesures ont des unités différentes.
graphique_clinique <- ggplot(
  clinique_long,
  aes(x = Diabetes_Risk, y = Valeur, fill = Diabetes_Risk)
) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Mesure, scales = "free_y") +
  labs(
    title = "Variables cliniques selon la catégorie de risque",
    x = "Catégorie de risque",
    y = "Valeur mesurée",
    fill = "Catégorie de risque"
  ) +
  theme_minimal()
graphique_clinique

# ------------------------------------------------------------
# 7. Graphique des habitudes de vie
# ------------------------------------------------------------

# Calculer la proportion de chaque réponse dans chaque catégorie de risque.
# Les proportions facilitent la comparaison lorsque les groupes ont des effectifs différents.
habitudes_proportions <- donnees_eda %>%
  pivot_longer(
    cols = all_of(variables_habitudes),
    names_to = "Habitude",
    values_to = "Reponse"
  ) %>%
  filter(!is.na(Diabetes_Risk), !is.na(Reponse)) %>%
  count(Habitude, Diabetes_Risk, Reponse, name = "Effectif") %>%
  group_by(Habitude, Diabetes_Risk) %>%
  mutate(Proportion = Effectif / sum(Effectif)) %>%
  ungroup()

# Les barres à 100 % montrent la composition des réponses pour chaque habitude
# dans les catégories Low, Moderate et High.
graphique_habitudes <- ggplot(
  habitudes_proportions,
  aes(x = Diabetes_Risk, y = Proportion, fill = Reponse)
) +
  geom_col() +
  facet_wrap(~ Habitude) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Habitudes de vie selon la catégorie de risque",
    x = "Catégorie de risque",
    y = "Pourcentage",
    fill = "Réponse"
  ) +
  theme_minimal()
graphique_habitudes

# Fin de l'EDA : les tableaux et graphiques décrivent les six variables retenues,
# les valeurs manquantes et leur répartition selon la catégorie de risque.
