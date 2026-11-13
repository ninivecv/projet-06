
# Analyse exploratoire des données (EDA)

# Charger les bibliothèques utilisées pour manipuler les données et créer les graphiques.
library(dplyr)   
library(tidyr)   
library(ggplot2) 
library(gridExtra)


# On veut enregistrer un tableau comme image PNG dans le dossier resultats (IA)

enregistrer_tableau_png <- function(tableau, nom_fichier) {
  tableau <- as.data.frame(tableau)
  hauteur <- max(800, 45 * (nrow(tableau) + 2)) # s'adapte au nombre de lignes pour garder le texte lisible

  png(
    filename = file.path("resultats", "EDA", nom_fichier),
    width = 2400,
    height = hauteur,
    res = 180
  )
  on.exit(dev.off())
  gridExtra::grid.table(tableau, rows = NULL)
}


# Importer le jeu de données


# Ce chemin suppose que le répertoire de travail est la racine du projet 

diabetes_risk <- read.csv(
  "data/raw/diabetes_risk.csv",
  stringsAsFactors = FALSE
)

# Vérifier rapidement la structure, les noms et les types des variables
glimpse(diabetes_risk)

# Afficher les premières observations afin de vérifier l'importation
head(diabetes_risk)

# Afficher le nombre de lignes (observations) et de colonnes (variables)
dim(diabetes_risk)


# Définir les six variables retenues pour le projet

# Les variables cliniques sont des mesures quantitatives
variables_cliniques <- c("BMI", "HbA1c", "Blood_Glucose")

# Les variables d'habitudes de vie décrivent le tabagisme,
# l'activité physique et la qualité de l'alimentation

variables_habitudes <- c(
  "Smoking_Status",
  "Physical_Activity_Level",
  "Diet_Quality"
)

# Diabetes_Risk sert à décrire les catégories de risque dans l'EDA 
# NB : Cette colonne ne fait pas partie des six variables à comparer

variable_groupe <- "Diabetes_Risk"
variables_attendues <- c(variables_cliniques, variables_habitudes, variable_groupe)

# Arrêter le script avec un message explicite si un nom de colonne est incorrect (IA)
colonnes_absentes <- setdiff(variables_attendues, names(diabetes_risk))
if (length(colonnes_absentes) > 0) {
  stop("Colonnes absentes du fichier : ", paste(colonnes_absentes, collapse = ", "))
}

# Garder les six variables du projet et la catégorie de risque pour les résumés
donnees_eda <- diabetes_risk %>%
  select(all_of(variables_attendues)) %>%
  
  # Fixer l'ordre des catégories de risque pour les tableaux et graphiques 
  mutate(Diabetes_Risk = factor(Diabetes_Risk, levels = c("Low", "Moderate", "High")))


#  Vérifier les valeurs manquantes et les catégories

# Certaines réponses de Physical_Activity_Level sont des chaînes vides
# (parfois composées d'espaces) plutôt que des NA. Les convertir en NA avant
# le décompte pour que le tableau des valeurs manquantes les inclue.
donnees_eda <- donnees_eda %>%
  mutate(
    Physical_Activity_Level = trimws(Physical_Activity_Level),
    Physical_Activity_Level = na_if(Physical_Activity_Level, "")
  )

# Compter les valeurs manquantes par variable et calculer leur pourcentage
# Ce décompte inclut les cellules vides de Physical_Activity_Level 

resume_NA <- data.frame(
  Variable = names(donnees_eda),
  Nombre_NA = colSums(is.na(donnees_eda)),
  Pourcentage_NA = round(colMeans(is.na(donnees_eda)) * 100, 2)
)
resume_NA

# On va remplacer les valeurs manquantes (NA) de Physical_Activity_Level par une catégorie
# visible afin qu'elles apparaissent comme « Non renseigné » dans les tableaux
# de catégories et dans le graphique des habitudes 

donnees_eda <- donnees_eda %>%
  mutate(
    Physical_Activity_Level = replace_na(
      Physical_Activity_Level,
      "Non renseigné"
    )
  )

# Afficher les effectifs des catégories de risque avec les valeurs manquantes (NA)
# Cela permet de voir si les groupes Low, Moderate et High sont équilibrés 
effectifs_risque <- as.data.frame(
  table(donnees_eda$Diabetes_Risk, useNA = "ifany")
)
names(effectifs_risque) <- c("Catégorie de risque", "Effectif")
effectifs_risque
enregistrer_tableau_png(effectifs_risque, "effectifs_par_risque.png")

# Pour chacune des habitudes, on affiche les réponses distinctes observées
# Cela permet de vérifier les catégories avant de décider de leur codage
lapply(variables_habitudes, function(variable) {
  cat("\nCatégories pour", variable, ":\n")
  print(table(donnees_eda[[variable]], useNA = "ifany"))
})


#  Résumer les trois variables cliniques (IA)

# Calculer le nombre de valeurs disponibles, la moyenne, la médiane
# et l'écart-type de chaque mesure clinique par catégorie de risque 

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


# Résumer les habitudes de vie dans un tableau (IA)

# Mettre les trois habitudes dans une même colonne pour compter facilement
# chaque réponse selon l'habitude et la catégorie de risque 
resume_habitudes <- donnees_eda %>%
  pivot_longer(
    cols = all_of(variables_habitudes),
    names_to = "Habitude",
    values_to = "Reponse"
  ) %>%
  count(Habitude, Diabetes_Risk, Reponse, name = "Effectif") %>%
  arrange(Habitude, Diabetes_Risk, Reponse)
resume_habitudes


# Graphique des variables cliniques


# Réorganiser l'IMC, l'HbA1c et le glucose pour afficher un panneau par mesure 
clinique_long <- donnees_eda %>%
  select(Diabetes_Risk, all_of(variables_cliniques)) %>%
  pivot_longer(
    cols = all_of(variables_cliniques),
    names_to = "Mesure",
    values_to = "Valeur"
  )

# Les boîtes à moustaches nous servent à comparer les distributions selon la catégorie de risque 
# Chaque panneau a sa propre échelle verticale, car les mesures ont des unités différentes 
graphique_clinique <- ggplot(
  clinique_long,
  aes(x = Diabetes_Risk, y = Valeur, fill = Diabetes_Risk)
) +
  geom_boxplot(na.rm = TRUE) +
  # Traduire les catégories de risque affichées sur l'axe et dans la légende
  scale_x_discrete(labels = c(Low = "Faible", Moderate = "Modéré", High = "Élevé")) +
  scale_fill_discrete(labels = c(Low = "Faible", Moderate = "Modéré", High = "Élevé")) +
  facet_wrap(
    ~ Mesure,
    scales = "free_y",
    labeller = as_labeller(c(
      BMI = "IMC",
      Blood_Glucose = "Glucose sanguin",
      HbA1c = "HbA1c"
    ))
  ) +


  labs(
    title = "Variables cliniques selon la catégorie de risque",
    x = "Catégorie de risque",
    y = "Valeur mesurée",
    fill = "Catégorie de risque"
  ) +
  theme_minimal()
graphique_clinique


# Graphique des habitudes de vie (IA)

# Calculer la proportion de chaque réponse dans chaque catégorie de risque 
# Les proportions facilitent la comparaison lorsque les groupes ont des effectifs différents
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
  scale_fill_discrete(
    breaks = c(
      "Poor", "Average", "Healthy", # Qualité alimentaire
      "Low", "Moderate", "High",  # Activité physique
      "Never", "Former", "Current",  # Tabagisme
      "Non renseigné"
    ),
    labels = c(
      "Poor" = "Faible (alimentation)",
      "Average" = "Moyenne (alimentation)",
      "Healthy" = "Saine (alimentation)",
      "Low" = "Faible (activité)",
      "Moderate" = "Modérée (activité)",
      "High" = "Élevée (activité)",
      "Never" = "Jamais (tabac)",
      "Former" = "Ancien (tabac)",
      "Current" = "Actuel (tabac)",
      "Non renseigné" = "Non renseigné"
    )
  ) + 
  # Ordre Faible, Modéré, Élevé pour les catégories de risque
  scale_x_discrete(labels = c(Low = "Faible", Moderate = "Modéré", High = "Élevé")) +
  facet_wrap(
    ~ Habitude,
    labeller = as_labeller(c(
      Diet_Quality = "Qualité de l’alimentation",
      Physical_Activity_Level = "Niveau d’activité physique",
      Smoking_Status = "Tabagisme"
    ))
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Habitudes de vie selon la catégorie de risque",
    x = "Catégorie de risque",
    y = "Pourcentage",
    fill = "Réponse"
  ) +
  theme_minimal()
graphique_habitudes



# Interprétation des résultats 

# Les six variables retenues sont BMI (IMC), HbA1c et Blood_Glucose (glucose
# sanguin), qui sont des mesures cliniques quantitatives, ainsi que
# Smoking_Status (tabagisme), Physical_Activity_Level (activité physique)
# et Diet_Quality (qualité de l'alimentation), qui sont des variables
# catégorielles ou ordinales décrivant les habitudes de vie.

# Dans le jeu de données, BMI, Smoking_Status et Diet_Quality ne présentent pas de
# valeurs manquantes. HbA1c compte 1 974 valeurs manquantes et Blood_Glucose
# en compte 959. Les résumés statistiques et les graphiques cliniques ignorent ces
# valeurs pour les calculs concernés (na.rm = TRUE). Aucune valeur n'a été imputée.
# Physical_Activity_Level contient 500 réponses vides. Elles sont d'abord
# converties en NA et comptées dans resume_NA, puis nommées « Non renseigné »
# dans les tableaux de catégories et le graphique. Aucune valeur n'a été supprimée.

# Dans le graphique clinique, les médianes de BMI et d'HbA1c augmentent des
# catégories Low à Moderate puis High. Les distributions de Blood_Glucose,
# au contraire, se chevauchent largement entre les trois catégories.

# Dans le graphique des habitudes de vie, les répartitions des réponses
# diffèrent selon la catégorie de risque. La catégorie Low comporte
# proportionnellement davantage de réponses Never pour le tabagisme et moins
# de réponses Poor pour la qualité de l'alimentation que la catégorie High.
# Les graphiques décrivent des associations dans ces données, mais ils ne démontrent
# pas forcément que les habitudes de vie causent le niveau de risque.



