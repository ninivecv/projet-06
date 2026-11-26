## couleurs du theme de l'affiche




vert_pale <- "#9DADA3"
vert <- "#5C9471"
vert_fonce <- "#3A6B4A"
vert_t_fonce <- "#273E20"
bleu_pale <- "#B6CBDE"
bleu <- "#4B7394"
bleu_fonce <- "#0e3b63"
gris <- "#b1bbb9"

suite_vert <- c(vert_pale, vert, vert_fonce)
suite <- c(vert_pale, vert, vert_fonce, vert_t_fonce, bleu_pale, bleu, bleu_fonce, gris)


donnees <- read.csv("./data/raw/diabetes_risk.csv")
km_clin <- read.csv("./data/processed/groupes_kmeans_clin.csv")
km_hab <- read.csv("./data/processed/groupes_kmeans_hAB.csv")

groupes_tous <- donnees[c("Patient_ID", "BMI", "HbA1c", "Blood_Glucose", "Smoking_Status", "Physical_Activity_Level", "Diet_Quality")] |> 
  left_join(
    km_clin[,c("Patient_ID", "grp_km2_cli", "grp_km6_cli")],
    by="Patient_ID") |> 
  left_join(
    km_hab[,c("Patient_ID", "grp_km_hab")],
    by = "Patient_ID")|>
  left_join(cart, by="Patient_ID")


cart <- read.csv("./data/processed/groupes_cart.csv") |> rename (Patient_ID = X) 
cart<-cart[, c("Patient_ID", "gr_CART_hab", "gr_CART_clin")]


###
pal <- c("#1F77B4", "#FF7F0E", "#CC79A7", "#8DA0CB", "#009E73", "#FFD92F", "#B2182B")
set.seed(2200)
idx <- sample(nrow(groupes_tous), 3000)                  # un échantillon suffit, sinon c'est lent
df5 <- data.frame(groupes_tous[idx, ])

for (groupe in c("grp_km2_cli", "grp_km6_cli", "grp_km_hab", "gr_CART_hab", "gr_CART_clin" )) {

g1 <- ggpairs(df5, columns = 2:4,
              mapping = aes(color = factor(grp_km2_cli), alpha = 0.4)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_minimal()
g2 <- ggpairs(df5, columns = 2:4,
              mapping = aes(color = factor(grp_km6_cli), alpha = 0.4)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_minimal()
g3 <- ggpairs(df5, columns = 2:4,
              mapping = aes(color = factor(grp_km_hab), alpha = 0.4)) +
  scale_color_manual(values = suite) +
  scale_fill_manual(values = suite) +
  theme_minimal()

