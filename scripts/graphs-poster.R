library(dplyr)

donnees <- read.csv("./data/raw/diabetes_risk.csv")
donnees[donnees$Physical_Activity_Level == "", "Physical_Activity_Level" ] <- NA
km_clin <- read.csv("./data/processed/groupes_kmeans_clin.csv")
km_hab <- read.csv("./data/processed/groupes_kmeans_hAB.csv")
cart <- read.csv("./data/processed/groupes_cart.csv") |> rename (Patient_ID = X) 
cart<-cart[, c("Patient_ID", "gr_CART_hab", "gr_CART_clin")]

donnees_select <-donnees[c("Patient_ID", "BMI", "HbA1c", "Blood_Glucose", "Smoking_Status", "Physical_Activity_Level", "Diet_Quality")]
donnees_select <- donnees_select[complete.cases(donnees_select),]

groupes_tous <- donnees_select|> 
  left_join(
    km_clin[,c("Patient_ID", "grp_km2_cli", "grp_km6_cli")],
    by="Patient_ID") |> 
  left_join(
    km_hab[,c("Patient_ID", "grp_km_hab")],
    by = "Patient_ID")|>
  left_join(cart, by="Patient_ID")


nrow(donnees_select)
sum(complete.cases(donnees_select))

###
pal <- c("#1F77B4", "#FF7F0E", "#CC79A7", "#8DA0CB", "#009E73", "#FFD92F", "#B2182B")
set.seed(2200)
idx <- sample(nrow(groupes_tous), 2000)                  # un échantillon suffit, sinon c'est lent
df5 <- data.frame(groupes_tous[idx, ])


  g1 <- ggpairs(df5, columns = 2:4,
                mapping = aes(color = factor(grp_km2_cli), alpha = 0.7)) +
    scale_color_manual(values = variee) +
    scale_fill_manual(values = variee) +
    theme_minimal()
  
  
ggsave("33-km2-clin.png", g1)  
  g2 <- ggpairs(df5, 
                columns = 2:4,
                mapping = aes(color = factor(grp_km6_cli), 
                              alpha = 0.7),
                columnLabels = c("IMC", "HbA1c", "Glucose")) +
    scale_color_manual(values = variee) +
    scale_fill_manual(values = variee) +
    theme_minimal()
  ggsave("33-km6-clin.png", g2)
  
  g3 <- ggpairs(df5, columns = 2:4,
                mapping = aes(color = factor(grp_km_hab), alpha = 0.7),
                columnLabels = c("IMC", "HbA1c", "Glucose")) +
    scale_color_manual(values = variee) +
    scale_fill_manual(values = variee) +
    theme_minimal()
  ggsave("33-km-hab.png", g3)
  
  g4 <- ggpairs(df5[!is.na(df5$gr_CART_clin),], columns = 2:4,
                mapping = aes(color = factor(gr_CART_clin), alpha = 0.7),
                columnLabels = c("IMC", "HbA1c", "Glucose")) +
    scale_color_manual(values = variee) +
    scale_fill_manual(values = variee) +
    theme_minimal()
  ggsave("33-cart-clin.png", g4)
  
  g5 <- ggpairs(df5[!is.na(df5$gr_CART_hab),], columns = 2:4,
                mapping = aes(color = factor(gr_CART_hab), alpha = 0.7),
                columnLabels = c("IMC", "HbA1c", "Glucose")) +
    scale_color_manual(values = variee) +
    scale_fill_manual(values = variee) +
    theme_minimal()
  ggsave("33-cart-hab.png", g5)
  
 r<-groupes_tous |> group_by(grp_km6_cli) |> summarise(
   moyenne_bmi = mean(BMI),
   moyenne_hb = mean(HbA1c),
   moyenne_glucose = mean(Blood_Glucose)
 )
  
 
 ###############
 #    ARI      #
 ###############
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
   Groupe_clinique = groupes_tous$grp_km6_cli,
   Groupe_habitudes = groupes_tous$grp_km_hab
 )
 cat("\nTableau croisé des deux segmentations :\n")
 print(tableau_croise)
 
 # Calculer l'ARI entre les deux segmentations, qui résume leur concordance.
 ari <- calculer_ARI(groupes_tous$grp_km6_cli, groupes_tous$grp_km_hab)
 cat("\nIndice de Rand ajusté (ARI) entre les segmentations :", round(ari, 5), "\n")

 ari  