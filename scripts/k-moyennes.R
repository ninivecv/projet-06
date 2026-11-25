# Comparaison des profils par k-moyennes

# Chargement des libraires 
library(cluster)
library(ggplot2)

# Enregistrer un graphique en créant son dossier de destination au besoin.
enregistrer_graphique <- function(graphique, fichier, largeur, hauteur, resolution = 300) {
  dossier <- dirname(fichier)
  if (!dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  ggsave(
    filename = fichier,
    plot = graphique,
    width = largeur,
    height = hauteur,
    dpi = resolution
  )
}

# Fixer une graine aléatoire pour reproduire les résultats.
set.seed(123)

# Importer le jeu de données
# Ce chemin suppose que le répertoire de travail est la racine du projet 

diabetes_risk <- read.csv("data/raw/diabetes_risk.csv", stringsAsFactors = FALSE)

# Définir les groupes de variables à comparer.
variables_cliniques <- c("BMI", "HbA1c", "Blood_Glucose")
variables_habitudes <- c("Smoking_Status", "Physical_Activity_Level", "Diet_Quality")
variables_analyse <- c(variables_cliniques, variables_habitudes)
variable_reference <- "Diabetes_Risk"

# Vérification que toutes les variables sont présentes dans le fichier (pour la robustesse)
colonnes_absentes <- setdiff(c(variables_analyse, variable_reference), names(diabetes_risk))
if (length(colonnes_absentes) > 0) {
  stop("Colonnes absentes du fichier : ", paste(colonnes_absentes, collapse = ", "))
}

# Vérifions les valeurs manquantes et les cellules vides.
variables_a_verifier <- c(variables_cliniques, variables_habitudes, variable_reference)

# Tableau 1 : nb de valeurs manquantes reconnues par R (NA).
tableau_NA <- data.frame(
  Variable = variables_a_verifier,
  Nombre_NA = vapply(
    diabetes_risk[variables_a_verifier],
    function(x) sum(is.na(x)),
    numeric(1)
  )
)
cat("Valeurs manquantes (NA) :\n")
print(tableau_NA, row.names = FALSE)

# Tableau 2 : compter les chaînes vides ou composées d'espaces.
# Précision : dans les colonnes numériques, les cellules vides peuvent avoir été converties
# en NA lors de l'importation du CSV. On les retrouve alors dans le tableau 1
tableau_cellules_vides <- data.frame(
  Variable = variables_a_verifier,
  Nombre_cellules_vides = vapply(
    diabetes_risk[variables_a_verifier],
    function(x) sum(!is.na(x) & trimws(as.character(x)) == ""),
    numeric(1)
  )
)
cat("\nCellules vides (texte vide ou espaces) :\n")
print(tableau_cellules_vides, row.names = FALSE)

# Dans le fichier, on que les réponses vides se retrouvent dans Physical_Activity_Level.
# Ce nettoyage porte uniquement sur cette colonne. 

# Créer une copie du jeu de données brut pour le nettoyage
diabetes_risk_processed <- diabetes_risk

# Nettoyer les réponses vides dans la copie
diabetes_risk_processed$Physical_Activity_Level <- trimws(
  diabetes_risk_processed$Physical_Activity_Level
)
diabetes_risk_processed$Physical_Activity_Level[
  diabetes_risk_processed$Physical_Activity_Level == ""
] <- NA


# Enregistrer la copie nettoyée sans modifier le fichier brut
write.csv(diabetes_risk_processed,"data/processed/diabetes_risk_processed.csv",row.names = FALSE)

# À partir d'ici, on effectue tous les traitements et analyses sur la copie nettoyée.

# Afficher les valeurs manquantes, puis garder les personnes complètes sur les six variables
# et la variable de risque. N.B qu'aucune imputation n'est effectuée.
valeurs_manquantes <- sapply(diabetes_risk_processed[variables_analyse], function(x) sum(is.na(x)))
cat("Valeurs manquantes par variable :\n")
print(valeurs_manquantes)
lignes_completes <- complete.cases(
  diabetes_risk_processed[c(variables_analyse, variable_reference)]
)
donnees_analyse <- diabetes_risk_processed[lignes_completes, variables_analyse, drop = FALSE]
risque_observe <- factor(
  diabetes_risk_processed[[variable_reference]][lignes_completes],
  levels = c("Low", "Moderate", "High")
)
cat("\nPersonnes conservées :", nrow(donnees_analyse), "sur", nrow(diabetes_risk_processed), "\n")
cat("Personnes exclues pour données manquantes :", sum(!lignes_completes), "\n")

# Préparation des variables cliniques. On va les standardiser pour qu'elles soient 
# sur une échelle comparable malgré leurs unités différentes.
matrice_clinique <- scale(as.matrix(donnees_analyse[variables_cliniques]))
if (any(!is.finite(matrice_clinique))) {
  stop("La matrice clinique contient des valeurs infinies après standardisation.")
}

# Préparation des habitudes de vie comme catégories et utiliser un codage 
# 0/1 (pour ne pas imposer un ordre numérique arbitraire au tabagisme)

donnees_analyse$Smoking_Status <- factor(donnees_analyse$Smoking_Status)
donnees_analyse$Physical_Activity_Level <- factor(
  donnees_analyse$Physical_Activity_Level,
  levels = c("Low", "Moderate", "High")
)
donnees_analyse$Diet_Quality <- factor(
  donnees_analyse$Diet_Quality,
  levels = c("Poor", "Average", "Healthy")
)
indicateurs_tabagisme <- model.matrix(~ Smoking_Status - 1, data = donnees_analyse)
indicateurs_activite <- model.matrix(~ Physical_Activity_Level - 1, data = donnees_analyse)
indicateurs_alimentation <- model.matrix(~ Diet_Quality - 1, data = donnees_analyse)
matrice_habitudes <- cbind(
  indicateurs_tabagisme,
  indicateurs_activite,
  indicateurs_alimentation
)

# Afficher les dimensions des matrices (pour vérifier que tout est correct)
cat("\nDimensions de la matrice clinique :", dim(matrice_clinique), "\n")
cat("Dimensions de la matrice habitudes :", dim(matrice_habitudes), "\n")
cat("Colonnes indicatrices des habitudes :\n")
print(colnames(matrice_habitudes))

# Comparons les solutions k = 2 à 8 avec la silhouette moyenne.
# La silhouette est estimée sur un échantillon commun (maximum 2 000 personnes)
# pour éviter de calculer une matrice de distances trop volumineuse.
k_possibles <- 2:8
taille_echantillon <- min(2000, nrow(donnees_analyse))
indices_echantillon <- sample(seq_len(nrow(donnees_analyse)), taille_echantillon)

evaluer_k <- function(matrice, k) {
  modele <- kmeans(matrice, centers = k, nstart = 20, iter.max = 100)
  groupes_echantillon <- modele$cluster[indices_echantillon]
  matrice_echantillon <- matrice[indices_echantillon, , drop = FALSE]
  sil <- silhouette(groupes_echantillon, dist(matrice_echantillon))
  list(modele = modele, silhouette = mean(sil[, "sil_width"]))
}

evaluation_clinique <- lapply(k_possibles, function(k) evaluer_k(matrice_clinique, k))
evaluation_habitudes <- lapply(k_possibles, function(k) evaluer_k(matrice_habitudes, k))

# Présenter les silhouettes pour rendre le choix du nombre de groupes transparent.
tableau_k <- data.frame(
  k = k_possibles,
  silhouette_clinique = sapply(evaluation_clinique, `[[`, "silhouette"),
  silhouette_habitudes = sapply(evaluation_habitudes, `[[`, "silhouette")
)
cat("\nSilhouette moyenne selon le nombre de groupes :\n")
print(tableau_k, row.names = FALSE, digits = 3)

# Préparer une version visuelle du tableau.
tableau_k_affiche <- rbind(
  data.frame(
    k = tableau_k$k,
    Analyse = "Variables cliniques",
    Silhouette = tableau_k$silhouette_clinique
  ),
  data.frame(
    k = tableau_k$k,
    Analyse = "Habitudes de vie",
    Silhouette = tableau_k$silhouette_habitudes
  )
)
tableau_k_affiche$Valeur <- sprintf("%.3f", tableau_k_affiche$Silhouette)

# Représenter les valeurs sous forme de tableau coloré.
figure_tableau_k <- ggplot(
  tableau_k_affiche,
  aes(x = Analyse, y = factor(k), fill = Silhouette)
) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = Valeur), size = 5) +
  scale_fill_gradient(
    low = "#EAF1EC",
    high = "#9DADA3",
    name = "Silhouette moyenne"
  ) +
  labs(
    title = "Comparaison du coefficient de silhouette",
    x = NULL,
    y = "Nombre de groupes (k)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

# Afficher le tableau.
print(figure_tableau_k)
enregistrer_graphique(
  figure_tableau_k,
  "resultats/k-moyennes/tableau_silhouette.png",
  largeur = 8,
  hauteur = 5
)

# Il faut retenir le k dont la silhouette moyenne est la plus élevée, séparément pour
# chaque ensemble de variables. 
# N.B que les deux segmentations peuvent avoir des k différents.

indice_clinique <- which.max(tableau_k$silhouette_clinique)
indice_habitudes <- which.max(tableau_k$silhouette_habitudes)
k_clinique <- k_possibles[indice_clinique]
k_habitudes <- k_possibles[indice_habitudes]
groupes_cliniques <- evaluation_clinique[[indice_clinique]]$modele$cluster
groupes_habitudes <- evaluation_habitudes[[indice_habitudes]]$modele$cluster
cat("\nk retenu pour les variables cliniques :", k_clinique, "\n")
cat("k retenu pour les habitudes de vie :", k_habitudes, "\n")

# Afficher le nombre d'observations dans chaque groupe des deux segmentations.
cat("\nEffectifs des groupes cliniques :\n")
print(table(groupes_cliniques))
cat("\nEffectifs des groupes d'habitudes de vie :\n")
print(table(groupes_habitudes))

# Mettre les effectifs des deux segmentations dans un tableau.
effectifs_cliniques <- table(groupes_cliniques)
effectifs_habitudes <- table(groupes_habitudes)
effectifs_affiche <- rbind(
  data.frame(
    Analyse = "Variables cliniques",
    Groupe = seq_along(effectifs_cliniques),
    Effectif = as.integer(effectifs_cliniques)
  ),
  data.frame(
    Analyse = "Habitudes de vie",
    Groupe = seq_along(effectifs_habitudes),
    Effectif = as.integer(effectifs_habitudes)
  )
)
effectifs_affiche$Analyse <- factor(
  effectifs_affiche$Analyse,
  levels = c("Variables cliniques", "Habitudes de vie")
)
effectifs_affiche$Groupe <- factor(
  effectifs_affiche$Groupe,
  levels = seq_len(max(effectifs_affiche$Groupe))
)
effectifs_affiche$Pourcentage <- ave(
  effectifs_affiche$Effectif,
  effectifs_affiche$Analyse,
  FUN = function(effectifs) effectifs / sum(effectifs)
)
effectifs_affiche$Etiquette <- paste0(
  format(effectifs_affiche$Effectif, big.mark = " "),
  "\n(", sprintf("%.1f%%", effectifs_affiche$Pourcentage * 100), ")"
)

# Présenter les numéros de groupe et leurs effectifs dans une figure png
figure_effectifs <- ggplot(
  effectifs_affiche,
  aes(x = Groupe, y = Analyse)
) +
  geom_tile(fill = "#EAF1EC", color = "white", linewidth = 1.2) +
  geom_text(aes(label = Etiquette), size = 4.5) +
  labs(
    title = "Effectifs des groupes",
    x = "Numéro du groupe",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

print(figure_effectifs)
enregistrer_graphique(
  figure_effectifs,
  "resultats/k-moyennes/effectifs_groupes.png",
  largeur = 8,
  hauteur = 3.5
)

# Calculons l'indice de Rand ajusté (ARI) (sans d'autres dépendances)
# L'ARI mesure l'accord entre deux partitions en corrigeant la part d'accord 
# attendue par hasard.

calculer_ARI <- function(partition_1, partition_2) {
  tableau <- table(partition_1, partition_2)
  choisir_deux <- function(x) x * (x - 1) / 2
  accords_paires <- sum(choisir_deux(tableau))
  sommes_lignes <- sum(choisir_deux(rowSums(tableau)))
  sommes_colonnes <- sum(choisir_deux(colSums(tableau)))
  total_paires <- choisir_deux(sum(tableau))
  accord_attendu <- sommes_lignes * sommes_colonnes / total_paires
  accord_maximal <- (sommes_lignes + sommes_colonnes) / 2
  denominateur <- accord_maximal - accord_attendu
  if (denominateur == 0) return(NA_real_)
  (accords_paires - accord_attendu) / denominateur
}

# Croiser les groupes cliniques et les groupes d'habitudes de vie.
# Les numéros des groupes sont des identifiants et ne correspondent pas
# nécessairement les uns aux autres.
tableau_croise <- table(
  Groupe_clinique = groupes_cliniques,
  Groupe_habitudes = groupes_habitudes
)
cat("\nTableau croisé des deux segmentations :\n")
print(tableau_croise)

# Calculer l'ARI entre les deux segmentations, qui résume leur concordance.
ari <- calculer_ARI(groupes_cliniques, groupes_habitudes)
cat("\nIndice de Rand ajusté (ARI) entre les segmentations :", round(ari, 5), "\n")

# Préparer une version illustrée du tableau croisé.
# Les pourcentages sont calculés à l'intérieur de chaque groupe clinique
# (chaque ligne totalise 100 %).
tableau_croise_affiche <- as.data.frame(tableau_croise)
names(tableau_croise_affiche) <- c("Groupe_clinique", "Groupe_habitudes", "Effectif")
tableau_croise_affiche$Pourcentage <- ave(
  tableau_croise_affiche$Effectif,
  tableau_croise_affiche$Groupe_clinique,
  FUN = function(effectifs) effectifs / sum(effectifs)
)
tableau_croise_affiche$Etiquette <- paste0(
  tableau_croise_affiche$Effectif,
  "\n(", sprintf("%.1f%%", tableau_croise_affiche$Pourcentage * 100), ")"
)
tableau_croise_affiche$Groupe_clinique <- factor(
  tableau_croise_affiche$Groupe_clinique,
  levels = sort(unique(tableau_croise_affiche$Groupe_clinique)),
  labels = as.character(sort(unique(tableau_croise_affiche$Groupe_clinique)))
)
tableau_croise_affiche$Groupe_habitudes <- factor(
  tableau_croise_affiche$Groupe_habitudes,
  levels = sort(unique(tableau_croise_affiche$Groupe_habitudes)),
  labels = as.character(sort(unique(tableau_croise_affiche$Groupe_habitudes)))
)

figure_tableau_croise <- ggplot(
  tableau_croise_affiche,
  aes(x = Groupe_habitudes, y = Groupe_clinique, fill = Pourcentage)
) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = Etiquette), size = 3.5) +
  scale_fill_gradient(
    low = "#EAF1EC",
    high = "#9DADA3",
    labels = function(x) paste0(round(x * 100, 1), "%"),
    name = "Part du groupe clinique"
  ) +
  labs(
    title = "Concordance des groupes cliniques et des habitudes de vie",
    subtitle = paste0(
      "Effectif et pourcentage dans chaque groupe clinique | ARI = ",
      sprintf("%.5f", ari)
    ),
    x = "Groupes d'habitudes de vie",
    y = "Groupes cliniques"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

print(figure_tableau_croise)
enregistrer_graphique(
  figure_tableau_croise,
  "resultats/k-moyennes/tableau_croise_groupes.png",
  largeur = 10,
  hauteur = 7
)

# Utilisons Diabetes_Risk comme référence externe, sans l'inclure dans les
# variables qui ont servi à former les clusters.
tableau_clinique_risque <- table(
  Groupe_clinique = groupes_cliniques,
  Risque_observe = risque_observe
)
tableau_habitudes_risque <- table(
  Groupe_habitudes = groupes_habitudes,
  Risque_observe = risque_observe
)

cat("\nGroupes cliniques croisés avec Diabetes_Risk :\n")
print(tableau_clinique_risque)
cat("\nGroupes d'habitudes croisés avec Diabetes_Risk :\n")
print(tableau_habitudes_risque)

# Calculer l'ARI entre chaque segmentation et la variable de risque observée.
ari_clinique_risque <- calculer_ARI(groupes_cliniques, risque_observe)
ari_habitudes_risque <- calculer_ARI(groupes_habitudes, risque_observe)
cat("\nARI groupes cliniques vs Diabetes_Risk :", round(ari_clinique_risque, 5), "\n")
cat("ARI groupes d'habitudes vs Diabetes_Risk :", round(ari_habitudes_risque, 5), "\n")

# Tableau comparatif des ARI pour l'affiche.
tableau_ari_affiche <- data.frame(
  Analyse = factor(
    c("Variables cliniques", "Habitudes de vie"),
    levels = c("Variables cliniques", "Habitudes de vie")
  ),
  ARI = c(ari_clinique_risque, ari_habitudes_risque)
)
tableau_ari_affiche$Valeur <- sprintf("%.5f", tableau_ari_affiche$ARI)

# Afficher les deux valeurs dans un format compact et lisible.
figure_tableau_ari <- ggplot(
  tableau_ari_affiche,
  aes(x = Analyse, y = "ARI")
) +
  geom_tile(fill = "#EAF1EC", color = "white", width = 0.95, height = 0.8) +
  geom_text(aes(label = Valeur), size = 6) +
  labs(
    title = "Concordance avec le risque de diabète",
    subtitle = "Un ARI proche de 1 indique une forte concordance.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

print(figure_tableau_ari)
enregistrer_graphique(
  figure_tableau_ari,
  "resultats/k-moyennes/tableau_ari_risque.png",
  largeur = 7,
  hauteur = 3.5
)



# Préparer un tableau de concordance : pour chaque groupe, calculer la part
# des catégories de risque et conserver l'effectif dans chaque case.
preparer_tableau_risque <- function(tableau) {
  donnees_tableau <- as.data.frame(tableau)
  names(donnees_tableau)[1:3] <- c("Groupe", "Risque", "Effectif")
  donnees_tableau$Pourcentage <- ave(
    donnees_tableau$Effectif,
    donnees_tableau$Groupe,
    FUN = function(effectifs) effectifs / sum(effectifs)
  )
  donnees_tableau$Groupe <- factor(
    donnees_tableau$Groupe,
    levels = sort(unique(donnees_tableau$Groupe)),
    labels = as.character(sort(unique(donnees_tableau$Groupe)))
  )
  donnees_tableau$Risque <- factor(
    donnees_tableau$Risque,
    levels = c("Low", "Moderate", "High"),
    labels = c("Faible", "Modéré", "Élevé")
  )
  donnees_tableau$Etiquette <- paste0(
    format(donnees_tableau$Effectif, big.mark = " "),
    "\n(", sprintf("%.1f%%", donnees_tableau$Pourcentage * 100), ")"
  )
  donnees_tableau
}

# Tracer un tableau de concordance pour un ensemble de variables.
tracer_tableau_risque <- function(tableau, titre, nom_groupe, ari) {
  donnees_tableau <- preparer_tableau_risque(tableau)
  ggplot(
    donnees_tableau,
    aes(x = Risque, y = Groupe, fill = Pourcentage)
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = Etiquette), size = 3.5) +
    scale_fill_gradient(
      low = "#F2F7FB",
      high = "#F28E2B",
      labels = function(x) paste0(round(x * 100), "%"),
      name = "Part du groupe"
    ) +
    labs(
      title = titre,
      subtitle = paste0("ARI = ", sprintf("%.5f", ari),
                        " | pourcentages calculés dans chaque groupe"),
      x = "Risque de diabète",
      y = nom_groupe
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
}

# Tableau 1 : groupes cliniques comparés aux catégories de risque.
figure_clinique_risque <- tracer_tableau_risque(
  tableau_clinique_risque,
  "Concordance : groupes cliniques et risque de diabète",
  "Groupe clinique",
  ari_clinique_risque
)
print(figure_clinique_risque)
enregistrer_graphique(
  figure_clinique_risque,
  "resultats/k-moyennes/concordance_clinique_risque.png",
  largeur = 7,
  hauteur = 5
)

# Tableau 2 : groupes d'habitudes de vie comparés aux catégories de risque.
figure_habitudes_risque <- tracer_tableau_risque(
  tableau_habitudes_risque,
  "Concordance : habitudes de vie et risque de diabète",
  "Groupe d'habitudes de vie",
  ari_habitudes_risque
)
print(figure_habitudes_risque)
enregistrer_graphique(
  figure_habitudes_risque,
  "resultats/k-moyennes/concordance_habitudes_risque.png",
  largeur = 7,
  hauteur = 6
)

# Interprétation des résultats - 24 novembre 2026 à 20h40

# La silhouette favorise 6 groupes pour les variables cliniques (0,272) et 8 pour les habitudes (0,357, meilleur k testé).
# La silhouette la plus élevée pour les habitudes indique des groupes mieux séparés selon ce critère que ceux des variables cliniques. 
# L’ARI de 0,00002 indique une concordance presque nulle entre les groupes formés à partir des variables cliniques et ceux formés 
# à partir des habitudes de vie. Les deux segmentations regroupent donc les personnes de façons très différentes.
# Comme 8 est la borne supérieure testée, d'autres valeurs de k pourraient encore être explorées pour les habitudes.
# Comparées au risque observé, les variables cliniques concordent un peu plus (ARI = 0,00728) que les habitudes (ARI = 0,00517).
# Toutefois, ces deux ARI restent très proches de zéro, alors aucune segmentation ne reproduit bien les catégories de risque.
# Les graphiques montrent que le risque « Élevé » domine dans presque tous les groupes, tandis que « Faible » est rare.
# Les groupes obtenus dépendent donc des variables choisies. Notez bien que ce sont des profils exploratoires, pas des diagnostics.


# Les groupes formés à partir des habitudes de vie correspondent-ils aux catégories de risque observées 
# aussi bien que les groupes formés à partir des variables cliniques?

# Non, pas selon nos résultats. Les groupes formés à partir des
# habitudes de vie concordent très peu avec les catégories de Diabetes_Risk (ARI = 0,00517). 
# Bien que les variables cliniques montrent un accord légèrement supérieur ( ARI = 0,00728), 
# celui-ci demeure également très faible. Ainsi, même si les habitudes  de vie permettent de 
# distinguer certains profils, elles ne reproduisent pas les regroupements associés au risque de 
# diabète observés avec les variables cliniques dans cet échantillon. Notez bien que ces
# résultats reflètent uniquement la structure des groupes dans ces
# données et ne remplacent en aucun cas un test diagnostique.
