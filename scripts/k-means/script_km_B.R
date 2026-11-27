############################################################
##  ANALYSE B K-MOYENNES — Adapté de codes/k_means.R (cours
##  STT-2000, Palmer Penguins)
############################################################

## ---------- 0. PACKAGES (installer une seule fois) ----------

#install.packages(c("cluster", "factoextra", "NbClust", "ggplot2", "GGally", "scales", "klaR"))

library(cluster)      # silhouette()
library(factoextra)   # fviz_nbclust(), fviz_cluster(), fviz_silhouette()
library(NbClust)      # ~30 indices pour choisir k
library(ggplot2)      # figures sur mesure
library(GGally)       # ggpairs() : matrice de nuages de points
library(scales)       # couleurs semi-transparentes
library(klaR)         # méthode k-modes

set.seed(2200)        # reproductibilité (l'algorithme a une part d'aléatoire)

## ---------- 1. CONFIGURATION DES DONNÉES ÉTUDIÉES ----------

## 1a. Charger la base de données "donnees" :

donnees <- read.csv("./data/raw/diabetes_risk.csv")

## 1b. Les 3 facteurs du groupe B :

facteurs_B <- c("Smoking_Status", "Physical_Activity_Level", "Diet_Quality")

## 1c. Déterminer k_souhaite :

# k = 8 est retenu parmi les valeurs testées (de 2 à 8).
# Comme 8 est la limite supérieure de cette plage, tester des valeurs plus
# élevées pourrait révéler une autre solution. Le choix de k reste à interpréter
# avec les critères de regroupement et le sens des profils obtenus.

k_souhaite <- 8           #voir FIGURE 1 : méthode du coude

## ---------- 2. PRÉPARATION DES DONNÉES ----------

# Ce script conserve les personnes complètes pour les variables d'habitudes.
# Le script A filtre selon les variables cliniques. Les lignes conservées
# peuvent donc différer -> Cette limite du code sera déclarée durant la présentation. 

## Garder seulement les lignes complètes sur les facteurs analysés :

d <- donnees[complete.cases(donnees[facteurs_B]), ]

# Les réponses vides sont converties en NA avant le filtrage définitif.
# Le filtrage effectué plus haut sera donc recalculé après ce nettoyage.

## Gestion des NA :
for (v in facteurs_B) donnees[[v]][donnees[[v]] == ""] <- NA
d <- donnees[complete.cases(donnees[facteurs_B]), ]

## Matrice brute et version standardisée (moyenne 0, écart-type 1) :
# *étapes de vérification*
x <- as.data.frame(lapply(d[facteurs_B], factor))
str(x)
n <- nrow(x)
cat("Effectif analysé :", nrow(x), "\n")

x_ech <- x[sample(nrow(x), 5000), ]
n_ech <- nrow(x_ech)

# distance de Hamming normalisée
dist_h <- daisy(x_ech, metric = "gower")

## Résumer du format de l'analyse (martice n lignes et length(facteurs_A) colonnes) :

cat("Effectif analysé :", n, "individus ;", length(facteurs_B), "facteurs\n")

## ---------- 3. CHOISIR LE NOMBRE DE GROUPES (validation de k) ----------

## Méthode du coude + silhouette :

kmodes_best <- function(x_ech, K, n_rep = 5) {
  essais <- lapply(seq_len(n_rep), function(i)
    kmodes(x_ech, modes = K, iter.max = 50))
  couts <- sapply(essais, function(m) sum(m$withindiff))
  essais[[which.min(couts)]]
}

K_candidats <- 1:8
modeles <- lapply(K_candidats, function(K) {
  if (K == 1) return(NULL)
  kmodes_best(x_ech, K)
})

W <- sapply(seq_along(K_candidats), function(j) {
  if (K_candidats[j] == 1) {
    # K = 1 : distance de chaque individu au mode global
    modes1 <- sapply(x_ech, function(col) names(which.max(table(col))))
    sum(mapply(function(col, m) sum(col != m), x_ech, modes1))
  } else sum(modeles[[j]]$withindiff)
})

inertie_totale <- W[1]

silhouettes <- vapply(seq_along(modeles), function(j) {
  if (K_candidats[j] == 1L) return(NA_real_)
  mean(cluster::silhouette(modeles[[j]]$cluster, dist_h)[, "sil_width"])
}, numeric(1))

criteres <- data.frame(K = K_candidats, W = W,
                       R2 = pmax(0, 1 - W / inertie_totale),
                       silhouette = silhouettes)
criteres$CH <- NA_real_
j <- K_candidats > 1L
criteres$CH[j] <- ((inertie_totale - W[j]) / (K_candidats[j] - 1)) /
  (W[j] / (n_ech - K_candidats[j]))

cat("\nCritères pour chaque nombre de groupes :\n")
print(criteres, digits = 4, row.names = FALSE)

## ---------- 4. FIGURES ANALYSE B (3 autres variables) ----------

## FIGURE 1 : méthode du coude (W en fonction de k) -

p1 <- plot(K_candidats, W, type = "b", pch = 19, lwd = 2, col = "#0072B2",
     xlab = "Nombre de clusters K",
     ylab = "Coût intra-classe (distance de Hamming)",
     main = "Méthode du coude : k-modes")

## FIGURE 2 : silhouette moyenne en fonction de k -

p2 <- plot(K_candidats, silhouettes, type = "b", pch = 19, lwd = 2, col = "#D55E00",
     xlab = "Nombre de clusters K", ylab = "Silhouette moyenne",
     main = "Silhouette moyenne : k-modes")

## ---------- 5. AJUSTEMENT FINAL ----------
mod <- modeles[[which(K_candidats == k_souhaite)]]

# Numéroter les groupes par taille décroissante
ordre     <- order(tabulate(mod$cluster, nbins = k_souhaite), decreasing = TRUE)
cl_ech    <- match(mod$cluster, ordre)                 # groupes de l'échantillon
modes_ord <- mod$modes[ordre, , drop = FALSE]          # modes réordonnés

# Assigner chacun des individus au mode le plus proche (Hamming)
assigner <- function(x, modes) {
  X <- as.matrix(x); M <- as.matrix(modes)
  desaccords <- sapply(seq_len(nrow(M)), function(k)
    rowSums(sweep(X, 2, M[k, ], "!=")))
  max.col(-desaccords, ties.method = "first")
}
groupes  <- assigner(x, modes_ord)
d$groupe <- factor(groupes, levels = seq_len(k_souhaite))

# Profils types (modes) et tailles
profils <- data.frame(groupe = seq_len(k_souhaite),
                      n = as.integer(table(d$groupe)),
                      modes_ord, row.names = NULL)
cat("\nProfils types (modes) de chaque groupe :\n")
print(profils, row.names = FALSE)

cat("\nDécomposition du coût :\n")
cat("  Pseudo-R2 =", round(1 - sum(mod$withindiff) / inertie_totale, 3), "\n")

sil <- cluster::silhouette(cl_ech, dist_h)
cat("  Silhouette moyenne =", round(mean(sil[, "sil_width"]), 3), "\n")

## FIGURE 3 : silhouette individuelle (échantillon)
p3 <- fviz_silhouette(sil) +
labs(subtitle = "Barres négatives : individus possiblement mal classés")
print(p3)

## Proportions de chaque modalité dans chaque groupe
prop_long <- do.call(rbind, lapply(facteurs_B, function(v) {
  t <- prop.table(table(groupe = d$groupe, modalite = x[[v]]), margin = 1)
  data.frame(facteur = v, as.data.frame(t, responseName = "prop"))
}))

pal <- c("#1F77B4", "#FF7F0E", "#CC79A7", "#8DA0CB", "#009E73", "#FFD92F", "#B2182B")

## FIGURE 4 : composition de chaque groupe, en barres groupées
p4 <- ggplot(prop_long, aes(x = modalite, y = prop, fill = groupe)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ facteur, scales = "free_x") +
  scale_fill_manual(values = pal[seq_len(k_souhaite)], name = "Profil") +
  labs(title = "Composition de chaque groupe",
       x = "Modalité", y = "Proportion dans le groupe") +
  theme_minimal()
print(p4)

## FIGURE 5 : carte de chaleur des proportions
p5 <- ggplot(prop_long, aes(x = modalite, y = groupe, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(100 * prop), " %")),
            color = "black", size = 4) +
  facet_wrap(~ facteur, scales = "free_x") +
  scale_fill_gradient(low = "white", high = "#2166AC", name = "Proportion") +
  labs(title = "Carte de chaleur des profils", x = "Modalité", y = "Profil") +
  theme_minimal()
print(p5)

### Export des groupes

groupes_test <- d |> rename (grp_km_hab = groupe)
write.csv(groupes_test, "data/processed/groupes_kmeans_hAB.csv")




