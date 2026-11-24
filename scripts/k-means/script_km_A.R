############################################################
##  ANALYSE A K-MOYENNES — Adapté de codes/k_means.R (cours
##  STT-2200, Palmer Penguins)
############################################################

## ---------- 0. PACKAGES (installer une seule fois) ----------

install.packages(c("cluster", "factoextra", "NbClust", "ggplot2", "GGally", "scales"))

library(cluster)      # silhouette()
library(factoextra)   # fviz_nbclust(), fviz_cluster(), fviz_silhouette()
library(NbClust)      # ~30 indices pour choisir k
library(ggplot2)      # figures sur mesure
library(GGally)       # ggpairs() : matrice de nuages de points
library(scales)       # couleurs semi-transparentes

set.seed(2200)        # reproductibilité (l'algorithme a une part d'aléatoire)

## ---------- 1. CONFIGURATION DES DONNÉES ÉTUDIÉES ----------

## 1a. Charger la base de données "donnees" :

donnees <- read.csv("diabetes_risk.csv")

## 1b. Les 3 facteurs du groupe A :

facteurs_A <- c("BMI", "HbA1c", "Blood_Glucose")

## 1c. Nombre de groupes souhaité :

k_souhaite <- 6                     #aussi testé avec k = 2, justification étape 4 (voir FIGURE 2)

## ---------- 2. PRÉPARATION DES DONNÉES ----------

## Garder seulement les lignes complètes sur les facteurs analysés :

d <- donnees[complete.cases(donnees[facteurs_A]), ]

## Matrice brute et version standardisée (moyenne 0, écart-type 1) :

x <- as.matrix(d[facteurs_A])
z <- scale(x)
n <- nrow(z)

inertie_totale <- sum(scale(z, center = TRUE, scale = FALSE)^2)

## Résumer du format de l'analyse (martice n lignes et length(facteurs_A) colonnes) :

cat("Effectif analysé :", n, "individus ;", length(facteurs_A), "facteurs\n")

## ---------- 3. CHOISIR LE NOMBRE DE GROUPES (validation de k) ----------

## Méthode du coude + silhouette :

K_candidats <- 1:8
modeles <- lapply(K_candidats, function(K) {
  kmeans(z, centers = K, nstart = 50, iter.max = 500,
         algorithm = "Lloyd")
})

W <- vapply(modeles, function(m) m$tot.withinss, numeric(1))

silhouettes <- vapply(seq_along(modeles), function(j) {
  if (K_candidats[j] == 1L) return(NA_real_)
  mean(cluster::silhouette(modeles[[j]]$cluster, dist(z))[, "sil_width"])
}, numeric(1))

criteres <- data.frame(K = K_candidats, W = W,
                       R2 = pmax(0, 1 - W / inertie_totale),
                       silhouette = silhouettes)
criteres$CH <- NA_real_
j <- K_candidats > 1L
criteres$CH[j] <- ((inertie_totale - W[j]) / (K_candidats[j] - 1)) /
  (W[j] / (n - K_candidats[j]))

cat("\nCritères pour chaque nombre de groupes :\n")
print(criteres, digits = 4, row.names = FALSE)

## ---------- 4. FIGURES ANALYSE A (3 premières variables) ----------

## FIGURE 1 : méthode du coude (W en fonction de k) -

p1 <- plot(1:8, W,
     type = "b",
     pch = 19,
     col = "blue",
     xlab = "Nombre de clusters (k)",
     ylab = "WSS (Within-Cluster Sum of Squares)",
     main = "Méthode du coude")
print(p1)

## FIGURE 2 : silhouette moyenne en fonction de k -

p2 <- fviz_nbclust(z, kmeans, method = "silhouette", k.max = 8)
print(p2)

## FIGURE 3 : consensus de ~30 indices (NbClust) ; proposée par Claude (IAg) -

nb <- NbClust(z, distance = "euclidean", min.nc = 2, max.nc = 6,
              method = "kmeans", index = "all")

## FIGURE 4 : répartition des votes des indices ; proposée par Claude (IAg) -

par(mfrow = c(1, 1))
barplot(table(nb$Best.nc[1, ]),
        xlab = "Nombre de groupes proposé",
        ylab = "Nombre d'indices en faveur",
        main = "NbClust : consensus des indices")

## ---------- 4. AJUSTEMENT FINAL ----------

## Application de la méthode des k-moyennes avec k_souhaite -
# *k = 2 et k = 6*
km <- kmeans(z, centers = k_souhaite, nstart = 50, iter.max = 100,
             algorithm = "Lloyd")

## Numéroter les groupes selon la moyenne du 1er facteur -

ordre <- order(km$centers[, facteurs_A[1]])
groupes  <- match(km$cluster, ordre)
km$cluster <- groupes

## Centres en unités originales + valeurs importantes (pour interpréter les profils concrètement) -

centres_z <- km$centers[ordre, , drop = FALSE]
centres <- sweep(sweep(centres_z, 2, attr(z, "scaled:scale"), "*"),
                 2, attr(z, "scaled:center"), "+")
profils <- data.frame(
  groupe = seq_len(k_souhaite),
  n = as.integer(table(km$cluster)),
  round(centres, 2)
)
cat("\nProfils moyens dans les unités originales :\n")
print(profils, row.names = FALSE)

cat("\nDécomposition de l'inertie :\n")
cat("  R2 =", round(km$betweenss / km$totss, 3),
    "(part de la variance expliquée par les profils)\n")

sil <- cluster::silhouette(km$cluster, dist(z))
cat("  Silhouette moyenne =", round(mean(sil[, "sil_width"]), 3), "\n")

## FIGURE 5 : projection 2D des individus, colorés par profil (ACP utilisée) -

p5 <- fviz_cluster(km, data = z,
                   ellipse.type = "norm",
                   geom = "point",
                   palette = "jco",
                   main = "Profils issus du k-moyennes (projection ACP)") +
  theme_minimal()
print(p5)

## FIGURE 6 : silhouette individuelle (repère les individus mal classés) -

p6 <- fviz_silhouette(sil) +
  labs(subtitle = "Barres rouges/oranges : individus possiblement mal classés")
print(p6)


## FIGURE 7 : profil moyen de chaque groupe, en barres groupées -
# *seulement deux couleurs pour k = 2*

profils_long <- reshape(profils, direction = "long",
                        varying = facteurs_A, v.names = "moyenne",
                        times = facteurs_A, timevar = "facteur",
                        drop = "n")
p7 <- ggplot(profils_long, aes(x = facteur, y = moyenne, fill = as.factor(groupe))) +
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#CC79A7", "#8DA0CB", "#009E73", "#FFD92F"),
                    name = "Profil") +
  labs(title = "Profil moyen de chaque groupe (unités originales)",
       x = "Facteur", y = "Moyenne du facteur") +
  theme_minimal()
print(p7)

## FIGURE 8 : heat map des centroïdes -

p8 <- ggplot(profils_long,
             aes(x = facteur, y = as.factor(groupe), fill = moyenne)) +
  geom_tile() +
  geom_text(aes(label = round(moyenne, 1)), color = "white", size = 4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, name = "Moyenne") +
  labs(title = "Carte de chaleur des profils",
       x = "Facteur", y = "Profil") +
  theme_minimal()
print(p8)

