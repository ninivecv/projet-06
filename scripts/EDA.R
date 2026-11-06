# Chargement des librairies
library(dplyr)
library(tidyr)
library(ggplot2)

# Importation des données
diabetes_risk <- read.csv("../data/raw/diabetes_risk.csv")


# Affichage des premières valeurs
apercu <- glimpse(diabetes_risk)
head(apercu)

# Nombre de lignes et de colonnes du jeu de données
dim(diabetes_risk)

# Nombre de valeurs manquantes
NA_par_variable <- colSums(is.na(diabetes_risk))
NA_par_variable

somme_NA <- sum(NA_par_variable)
somme_NA

# Catégories de risque
table(diabetes_risk$Diabetes_Risk)


# Calculer moyenne, médiane et écart-type de l’âge, de l’IMC, de l’HbA1c et du glucose par statut (0 ou 1).
diabetes_risk %>%
  group_by(Diabetes_Risk) %>%
  summarise(
    across(
      c(Age, BMI, HbA1c, Blood_Glucose),
      list(
        moyenne = ~ mean(.x, na.rm = TRUE),
        mediane = ~ median(.x, na.rm = TRUE),
        ecart_type = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" 
    ),
    .groups = "drop"
  )


# Sélectionner les variables continues à comparer
variables_continues <- diabetes_risk %>%
  dplyr::select(Age, BMI, HbA1c, Blood_Glucose)

# Calculer la matrice de corrélations 
matrice_correlations <- cor(
  variables_continues,
  use = "pairwise.complete.obs", # Les valeurs manquantes sont ignorées paire par paire
  method = "pearson" # Méthode de Pearson
)

round(matrice_correlations, 3)


# Les mesures à comparer entre les catégories de risque
mesures <- c("Blood_Glucose", "HbA1c")

# Pour chaque mesure, on va comparer les catégories de risque
for (mesure in mesures) {
  
  # Garder les lignes où la mesure et la catégorie sont renseignées (une limite du projet)
  donnees_valides <- diabetes_risk[
    !is.na(diabetes_risk[[mesure]]) &
      !is.na(diabetes_risk$Diabetes_Risk),
  ]
  
  # Comparer les trois catégories avec le test de Kruskal–Wallis
  test_global <- kruskal.test(
    donnees_valides[[mesure]] ~ donnees_valides$Diabetes_Risk
  )
  
  cat("\nRésultat pour", mesure, ":\n")
  print(test_global)
  
  # Si une différence globale est détectée, on va comparer les catégories deux à deux (test de Wilcoxon)
  
  if (test_global$p.value < 0.05) {
    comparaisons <- pairwise.wilcox.test(
      donnees_valides[[mesure]],
      donnees_valides$Diabetes_Risk,
      p.adjust.method = "holm"  # Sans correction, multiplier les tests augmente le risque de trouver une différence significative par hasard.
    )
    
    cat("Comparaisons deux à deux (valeurs p corrigées) :\n")
    print(comparaisons)
  }
}

# Arrondir les corrélations à cinq décimales
correlations_arrondies <- round(matrice_correlations, 3)

# Ignorer les valeurs de la diagonale, qui valent toujours 1
diag(correlations_arrondies) <- NA

# Trouver la paire de variables la plus corrélée
position <- which(
  abs(correlations_arrondies) == max(abs(correlations_arrondies), na.rm = TRUE),
  arr.ind = TRUE # renvoyer les positions sous forme de ligne et de colonne dans la matrice
)[1, ] # garde la première position trouvée et ses deux coordonnées

variable_1 <- rownames(correlations_arrondies)[position[1]]
variable_2 <- colnames(correlations_arrondies)[position[2]]
correlation <- correlations_arrondies[position[1], position[2]]

# Afficher le résultat sous forme de phrase
cat(
  "La corrélation la plus élevée en valeur absolue est entre",
  variable_1, "et", variable_2,
  ": r =", format(correlation, nsmall = 3), "\n"
)


# Préparer la matrice 
donnees_carte <- as.data.frame(as.table(matrice_correlations))
names(donnees_carte) <- c("variable_1", "variable_2", "correlation")


# Carte thermique
ggplot(donnees_carte, aes(x = variable_1, y = variable_2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 3))) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Corrélations entre les variables",
    x = NULL,
    y = NULL,
    fill = "Corrélation"
  ) +
  theme_minimal()

# Mettre le glucose et l'HbA1c dans une seule colonne pour les tracer ensemble
diabetes_HbA1c_glucose <- diabetes_risk %>%
  select(Diabetes_Risk, Blood_Glucose, HbA1c) %>%
  pivot_longer(
    cols = c(Blood_Glucose, HbA1c),
    names_to = "Mesure",
    values_to = "Valeur"
  )

# Boîte à moustaches pour chaque mesure et catégorie de risque
ggplot(diabetes_HbA1c_glucose, aes(x = Diabetes_Risk, y = Valeur, fill = Diabetes_Risk)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Mesure, scales = "free_y") +
  labs(
    title = "Glucose et HbA1c selon la catégorie de risque",
    x = "Catégorie de risque de diabète",
    y = "Valeur mesurée",
    fill = "Catégorie de risque"
  ) +
  theme_minimal()

# Mettre l'âge et l'IMC dans une seule colonne pour les tracer ensemble
diabetes_Age_BMI <- diabetes_risk %>%
  select(Diabetes_Risk, Age, BMI) %>%
  pivot_longer(
    cols = c(Age, BMI),
    names_to = "Mesure",
    values_to = "Valeur"
  )

# Boîte à moustaches pour chaque mesure et catégorie de risque
ggplot(diabetes_Age_BMI, aes(x = Diabetes_Risk, y = Valeur, fill = Diabetes_Risk)) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Mesure, scales = "free_y") +
  labs(
    title = "Âge et IMC selon la catégorie de risque",
    x = "Catégorie de risque de diabète",
    y = "Valeur mesurée",
    fill = "Catégorie de risque"
  ) +
  theme_minimal()
