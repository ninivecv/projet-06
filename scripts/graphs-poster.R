library(dplyr)
library(aricode)
source("./scripts/utilitaire.R")

#donnees raw
donnees <- read.csv("./data/raw/diabetes_risk.csv")
donnees[donnees$Physical_Activity_Level == "", "Physical_Activity_Level" ] <- NA

#groupes sortant de l'analyse
km_clin <- read.csv("./data/processed/groupes_kmeans_clin.csv")
km_hab <- read.csv("./data/processed/groupes_kmeans_hAB.csv")
cart <- read.csv("./data/processed/groupes_cart.csv") |> rename (Patient_ID = X) 
cart<-cart[, c("Patient_ID", "gr_CART_hab", "gr_CART_clin")]

donnees_select <-donnees[c("Patient_ID", "BMI", "HbA1c", "Blood_Glucose", "Smoking_Status", "Physical_Activity_Level", "Diet_Quality", "Diabetes_Risk")]
donnees_select <- donnees_select[complete.cases(donnees_select),]

#mise en commun des groupes
groupes_tous <- donnees_select|> 
  left_join(
    km_clin[,c("Patient_ID", "grp_km2_cli", "grp_km6_cli")],
    by="Patient_ID") |> 
  left_join(
    km_hab[,c("Patient_ID", "grp_km_hab")],
    by = "Patient_ID")|>
  left_join(cart, by="Patient_ID")

# verif
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

 
 ###############
 #    ARI      #
 ###############

 ARI(groupes_tous$grp_km6_cli, groupes_tous$grp_km_hab)
 ARI(groupes_tous$grp_km2_cli, groupes_tous$grp_km_hab)
 ARI(groupes_tous$gr_CART_hab[!is.na(groupes_tous$gr_CART_hab)], groupes_tous$gr_CART_clin[!is.na(groupes_tous$gr_CART_hab)])