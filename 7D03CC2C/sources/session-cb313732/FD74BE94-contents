# Arbres CART — Diabetes Risk Prediction (50K) — STT-2200
# Question : les habitudes de vie détectent-elles le risque de diabète
# aussi bien que les variables cliniques ?
# Classification de Diabetes_Risk (Low / Moderate / High) avec quatre arbres :
# variables médicales et habitudes de vie, chacun en version standard et en
# version rééquilibrée (les trois catégories pèsent autant), comparés sur
# les mêmes plis et le même test.
# Depuis la racine du projet ; données dans data/raw/diabetes_risk.csv.

# 0. FONCTIONS ------------------------------------------------------

compter_feuilles <- function(arbre) {
  sum(arbre$frame$var == "<leaf>")
}

profondeur_arbre <- function(arbre) {
  # Les identifiants rpart sont 1 à la racine, puis 2i et 2i + 1.
  max(floor(log2(as.numeric(rownames(arbre$frame)))))
}

# Exactitude, rappel par classe et exactitude équilibrée (moyenne des
# rappels : vaut 1/3 pour un modèle qui prédit toujours la même classe).
mesurer_classification <- function(reelle, predite) {
  confusion <- table(reelle = reelle, predite = predite)
  rappels <- diag(confusion) / rowSums(confusion)
  c(exactitude = mean(predite == reelle),
    exactitude_equilibree = mean(rappels),
    rappels)
}


# 1. DONNÉES ET PARTAGE REPRODUCTIBLE ------------------------------------

# Même chemin et même nettoyage que le script d'EDA.
diabetes_risk <- read.csv("data/raw/diabetes_risk.csv", stringsAsFactors = FALSE)
activite_brute <- trimws(diabetes_risk$Physical_Activity_Level)
activite_brute[activite_brute == ""] <- NA

# CART n'a pas besoin du codage 1-2-3 : il travaille directement avec
# les catégories. Facteur ORDONNÉ = coupures du type « ≤ Average »,
# qui respectent l'ordre. Facteur NON ordonné = toutes les façons de
# séparer les modalités en deux sous-ensembles (ex. {Never, Current}
# contre {Former}). Le tabagisme est laissé non ordonné : on ne suppose
# pas que « Former » est forcément entre « Never » et « Current ».
donnees <- data.frame(
  # Noms courts, pour que les étiquettes des arbres restent lisibles.
  IMC             = diabetes_risk$BMI,
  HbA1c           = diabetes_risk$HbA1c,
  Glycemie        = diabetes_risk$Blood_Glucose,
  Alim            = factor(diabetes_risk$Diet_Quality,
                           levels = c("Poor", "Average", "Healthy"),
                           ordered = TRUE),
  Activite        = factor(activite_brute,
                           levels = c("Low", "Moderate", "High"),
                           ordered = TRUE),
  Tabac           = factor(diabetes_risk$Smoking_Status,
                           levels = c("Never", "Former", "Current")),
  Diabetes_Risk   = factor(diabetes_risk$Diabetes_Risk,
                           levels = c("Low", "Moderate", "High"))
)

variables_cliniques <- c("IMC", "HbA1c", "Glycemie")
variables_habitudes <- c("Alim", "Activite", "Tabac")

# Les quatre arbres comparés. « reequilibre = TRUE » : probabilités a priori
# égales (1/3 chacune), comme si les trois catégories avaient la même taille.
# Sans ça, High (73 %) écrase tout et Low n'est jamais prédit.
modeles <- list(
  clinique = list(variables = variables_cliniques, reequilibre = FALSE,
                  etiquette = "Médicales", titre = "variables médicales"),
  habitudes = list(variables = variables_habitudes, reequilibre = FALSE,
                   etiquette = "Habitudes", titre = "habitudes de vie"),
  clinique_reeq = list(variables = variables_cliniques, reequilibre = TRUE,
                       etiquette = "Médicales rééq.",
                       titre = "variables médicales (rééquilibré)"),
  habitudes_reeq = list(variables = variables_habitudes, reequilibre = TRUE,
                        etiquette = "Habitudes rééq.",
                        titre = "habitudes de vie (rééquilibré)")
)

# Mêmes individus pour tous les arbres, sinon la comparaison est biaisée.
cas_complets <- complete.cases(donnees)
donnees <- donnees[cas_complets, ]
cat("Lignes exclues pour valeur manquante :", sum(!cas_complets), "\n")
# Choix explicite des cas complets ; pas une solution universelle.
# Si l'on impute, apprendre l'imputation dans chaque pli d'entraînement.

# On crée les différents plis, stratifiés selon la catégorie de risque.
set.seed(2200)
indices_par_risque <- split(seq_len(nrow(donnees)), donnees$Diabetes_Risk)
indices_train <- unlist(lapply(indices_par_risque, function(indices) {
  sample(indices, size = round(0.70 * length(indices)))
}), use.names = FALSE)
train <- donnees[indices_train, ]
test <- donnees[-indices_train, ]
set.seed(2201)
nb_plis <- 5L
plis <- integer(nrow(train))
for (risque in levels(train$Diabetes_Risk)) {
  indices <- which(train$Diabetes_Risk == risque)
  plis[indices] <- sample(rep(seq_len(nb_plis), length.out = length(indices)))
}
print(rbind(entrainement = table(train$Diabetes_Risk),
            test = table(test$Diabetes_Risk)))
cat("Plis de validation, uniquement dans l'entraînement :\n")
print(table(risque = train$Diabetes_Risk, pli = plis))


# 2. RECONSTRUIRE L'ARBRE DANS CHAQUE PLI ----------------------------------

# Candidats plus petits que pour les manchots : avec ~33 000 patients
# d'entraînement, une coupure utile peut améliorer l'erreur de moins de 1 %.
cp_candidats <- c(0, 0.0002, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.10)
controle <- rpart::rpart.control(
  cp = 0, minsplit = 10, minbucket = 5, maxdepth = 10,
  xval = 0, maxsurrogate = 0
)
# cp = 0 : développer l'arbre sous les contraintes de taille.
# xval = 0 : désactiver la CV interne, puisque nos plis sont explicites.
# maxsurrogate = 0 : pas de coupures de remplacement, cas complets ici.
# On ne standardise pas les variables ; CART compare des seuils.

ajuster_cart <- function(donnees, variables, reequilibre = FALSE) {
  # Diabetes_Risk est la réponse ; elle n'est jamais un prédicteur.
  parametres <- list(split = "gini")
  if (reequilibre) {
    # Ordre des niveaux : Low, Moderate, High.
    parametres$prior <- rep(1 / 3, nlevels(donnees$Diabetes_Risk))
  }
  rpart::rpart(
    reformulate(variables, response = "Diabetes_Risk"),
    data = donnees, method = "class",
    parms = parametres, control = controle
  )
}

# Critère de validation cohérent avec ce que l'arbre cherche à faire :
# - arbre standard : taux d'erreur (comme dans le code du cours) ;
# - arbre rééquilibré : erreur ÉQUILIBRÉE = 1 - moyenne des rappels, où
#   chaque catégorie compte pour un tiers, comme dans les a priori.
erreur_cv <- function(reelle, predite, reequilibre) {
  if (reequilibre) {
    1 - mesurer_classification(reelle, predite)[["exactitude_equilibree"]]
  } else {
    mean(predite != reelle)
  }
}

valider_cart <- function(donnees, plis, cp_candidats, variables,
                         reequilibre = FALSE) {
  niveaux_plis <- sort(unique(plis))
  niveaux <- levels(donnees$Diabetes_Risk)
  pertes <- matrix(
    0, nrow = length(niveaux_plis), ncol = length(cp_candidats),
    dimnames = list(niveaux_plis, cp_candidats)
  )
  # Prédictions hors pli de chaque patient d'entraînement, pour chaque cp :
  # nécessaires pour calculer l'erreur équilibrée sur l'ensemble des plis.
  hors_pli <- matrix(NA_integer_, nrow(donnees), length(cp_candidats))
  for (v in seq_along(niveaux_plis)) {
    est_validation <- plis == niveaux_plis[v]
    apprentissage <- donnees[!est_validation, ]
    validation <- donnees[est_validation, ]
    # Étape essentielle : réapprendre les coupures sans le pli de validation.
    grand_arbre <- ajuster_cart(apprentissage, variables, reequilibre)
    for (j in seq_along(cp_candidats)) {
      arbre <- rpart::prune(grand_arbre, cp = cp_candidats[j])
      prediction <- predict(arbre, newdata = validation, type = "class")
      pertes[v, j] <- sum(prediction != validation$Diabetes_Risk)
      hors_pli[est_validation, j] <- as.integer(prediction)
    }
  }
  risque_cv <- apply(hors_pli, 2, function(p) {
    erreur_cv(donnees$Diabetes_Risk, factor(niveaux[p], levels = niveaux),
              reequilibre)
  })
  names(risque_cv) <- cp_candidats
  # En cas d'égalité, le plus grand cp, donc l'arbre le plus simple.
  cp_retenu <- max(cp_candidats[risque_cv == min(risque_cv)])
  list(
    cp = cp_retenu, risque_cv = risque_cv, pertes = pertes,
    effectifs_plis = as.integer(table(factor(plis, niveaux_plis)))
  )
}

cat("\n2. Choisir cp sans consulter les performances du test\n")
validations <- lapply(modeles, function(m) {
  valider_cart(train, plis, cp_candidats, m$variables, m$reequilibre)
})
table_validation <- data.frame(
  cp = cp_candidats,
  sapply(validations, function(v) round(100 * v$risque_cv, 3))
)
names(table_validation)[-1] <- paste0("erreur_pct_", names(modeles))
cat("Erreur de validation (%) ; erreur équilibrée pour les arbres rééquilibrés\n")
print(table_validation, row.names = FALSE)
for (nom in names(validations)) {
  cat("cp retenu,", nom, ":", validations[[nom]]$cp, "\n")
}


# 3. RÉAJUSTER, PUIS ÉVALUER UNE FOIS SUR LE TEST -------------------------

cat("\n3. Modèles finaux et performances sur le test réservé\n")

# Référence : toujours prédire la classe la plus fréquente de l'entraînement.
classe_majoritaire <- names(which.max(table(train$Diabetes_Risk)))
prediction_reference <- factor(rep(classe_majoritaire, nrow(test)),
                               levels = levels(test$Diabetes_Risk))

resultats <- list()
for (nom in names(modeles)) {
  m <- modeles[[nom]]
  selection <- validations[[nom]]
  grand_arbre <- ajuster_cart(train, m$variables, m$reequilibre)
  arbre <- rpart::prune(grand_arbre, cp = selection$cp)
  prediction <- predict(arbre, newdata = test, type = "class")
  resultats[[nom]] <- list(
    arbre = arbre, grand_arbre = grand_arbre,
    cp = selection$cp, risque_cv = selection$risque_cv,
    prediction = prediction
  )
  cat("\n---", nom, ":", compter_feuilles(arbre), "feuilles ; profondeur",
      profondeur_arbre(arbre), "\n")
  confusion <- table(reelle = test$Diabetes_Risk, predite = prediction)
  print(confusion)
  cat(sprintf("Exactitude : %.2f %%\n",
              100 * mean(prediction == test$Diabetes_Risk)))
  cat("Rappel par catégorie de risque (%) :\n")
  print(round(100 * diag(confusion) / rowSums(confusion), 2))
}

comparaison <- rbind(
  reference = mesurer_classification(test$Diabetes_Risk, prediction_reference),
  t(sapply(resultats, function(r) {
    mesurer_classification(test$Diabetes_Risk, r$prediction)
  }))
)
cat("\nComparaison sur le même test réservé (%) :\n")
print(round(100 * comparaison, 2))

# Test de McNemar : H0, les deux arbres ont le même taux d'erreur.
# Valide parce que les deux sont évalués sur les MÊMES patients.
# Il compare le nombre d'erreurs (exactitude), pas l'exactitude équilibrée.
comparer_mcnemar <- function(nom_a, nom_b) {
  correct_a <- resultats[[nom_a]]$prediction == test$Diabetes_Risk
  correct_b <- resultats[[nom_b]]$prediction == test$Diabetes_Risk
  cat("\nMcNemar :", nom_a, "vs", nom_b, "\n")
  print(table(correct_a, correct_b, dnn = c(nom_a, nom_b)))
  test_m <- mcnemar.test(correct_a, correct_b)
  print(test_m)
  test_m
}
test_mcnemar      <- comparer_mcnemar("clinique", "habitudes")
test_mcnemar_reeq <- comparer_mcnemar("clinique_reeq", "habitudes_reeq")

# Règles des arbres et proportions d'une feuille.
arbre_clinique <- resultats$clinique$arbre
cat("\nArbre clinique : règles et effectifs d'entraînement\n")
print(arbre_clinique, digits = 6)
probabilites <- predict(arbre_clinique, newdata = test, type = "prob")
cat("Première observation du test et proportions de sa feuille :\n")
print(test[1, ])
print(probabilites[1, ])
# Une proportion nulle n'est pas la preuve d'une impossibilité.
cat("\nArbre des habitudes rééquilibré : règles\n")
print(resultats$habitudes_reeq$arbre, digits = 4)
# Avec des a priori égaux, les proportions affichées (yprob) sont
# pondérées par les a priori, pas les proportions brutes de la feuille.


# 4. IMPORTANCE DES VARIABLES --------------------------------------------

cat("\n4. Importance des variables dans chaque arbre\n")
cat("Importance calculée sur l'entraînement (sans portée causale) :\n")
for (nom in names(resultats)) {
  cat("\nArbre", nom, "(% de l'importance totale) :\n")
  importance <- resultats[[nom]]$arbre$variable.importance
  if (is.null(importance)) {
    cat("Aucune coupure : l'arbre retenu est réduit à sa racine.\n")
  } else {
    print(round(100 * sort(importance, decreasing = TRUE) / sum(importance), 2))
  }
}
# Lecture : l'importance compare les variables À L'INTÉRIEUR d'un arbre
# (ex. l'HbA1c compte-t-elle plus que la glycémie ?). Pour comparer les
# arbres entre eux, ce sont les performances sur le test qui comptent.
# Une variable absente de l'importance n'a servi à aucune coupure.


# 5. TABLEAU RÉCAPITULATIF -------------------------------------------------

# Une ligne par arbre, plus la référence (classe majoritaire).
# Toutes les performances sont mesurées sur le MÊME test réservé.
resumer_arbre <- function(nom) {
  r <- resultats[[nom]]
  m <- modeles[[nom]]
  mesures <- mesurer_classification(test$Diabetes_Risk, r$prediction)
  importance <- r$arbre$variable.importance
  data.frame(
    Arbre = m$etiquette,
    Critere_VC = if (m$reequilibre) "équilibrée" else "standard",
    cp_retenu = r$cp,
    Feuilles = compter_feuilles(r$arbre),
    Profondeur = profondeur_arbre(r$arbre),
    Erreur_VC_pct = round(100 * min(r$risque_cv), 2),
    Exactitude_test_pct = round(100 * mesures[["exactitude"]], 2),
    Exactitude_equilibree_pct = round(100 * mesures[["exactitude_equilibree"]], 2),
    Rappel_Low_pct = round(100 * mesures[["Low"]], 2),
    Rappel_Moderate_pct = round(100 * mesures[["Moderate"]], 2),
    Rappel_High_pct = round(100 * mesures[["High"]], 2),
    Variable_principale = if (is.null(importance)) "aucune" else
      names(which.max(importance)),
    check.names = FALSE
  )
}

mesures_ref <- mesurer_classification(test$Diabetes_Risk, prediction_reference)
tableau_resultats <- rbind(
  do.call(rbind, lapply(names(modeles), resumer_arbre)),
  data.frame(
    Arbre = "Référence", Critere_VC = "—",
    cp_retenu = NA, Feuilles = 1L, Profondeur = 0L, Erreur_VC_pct = NA,
    Exactitude_test_pct = round(100 * mesures_ref[["exactitude"]], 2),
    Exactitude_equilibree_pct = round(100 * mesures_ref[["exactitude_equilibree"]], 2),
    Rappel_Low_pct = round(100 * mesures_ref[["Low"]], 2),
    Rappel_Moderate_pct = round(100 * mesures_ref[["Moderate"]], 2),
    Rappel_High_pct = round(100 * mesures_ref[["High"]], 2),
    Variable_principale = paste("toujours", classe_majoritaire),
    check.names = FALSE
  )
)

lignes_mcnemar <- c(
  sprintf("McNemar médicales vs habitudes : khi2 = %.2f ; p = %.3g",
          test_mcnemar$statistic, test_mcnemar$p.value),
  sprintf("McNemar rééquilibrés (médicales vs habitudes) : khi2 = %.2f ; p = %.3g",
          test_mcnemar_reeq$statistic, test_mcnemar_reeq$p.value)
)

cat("\n5. Tableau récapitulatif des arbres (test réservé, n =",
    nrow(test), ")\n")
print(tableau_resultats, row.names = FALSE)
cat(lignes_mcnemar, sep = "\n")

# Tableau récapitulatif en graphique (R de base, sans paquet).
# Transposé : une ligne par mesure, une colonne par arbre, plus lisible.
tracer_tableau_resultats <- function() {
  en_texte <- as.data.frame(lapply(tableau_resultats, function(v) {
    ifelse(is.na(v), "—", as.character(v))
  }))
  valeurs <- t(as.matrix(en_texte[, -1]))
  colnames(valeurs) <- tableau_resultats$Arbre
  etiquettes <- c("Critère de validation", "cp retenu", "Feuilles",
                  "Profondeur", "Erreur VC (%)", "Exactitude test (%)",
                  "Exactitude équilibrée (%)", "Rappel Low (%)",
                  "Rappel Moderate (%)", "Rappel High (%)",
                  "Variable principale")
  en_tete <- c("Mesure", colnames(valeurs))
  # Largeur de chaque colonne selon son texte le plus long.
  largeurs <- c(max(nchar(c(en_tete[1], etiquettes))),
                pmax(nchar(colnames(valeurs)), apply(nchar(valeurs), 2, max))) + 3
  bords <- c(0, cumsum(largeurs))
  n_lignes <- nrow(valeurs) + 1L
  
  par(mfrow = c(1, 1), mar = c(4, 1, 3.5, 1))
  plot.new()
  plot.window(xlim = c(0, sum(largeurs)), ylim = c(0, n_lignes))
  for (i in seq_len(n_lignes)) {
    haut <- n_lignes - i + 1
    fond <- if (i == 1L) "#007C78" else if (i %% 2 == 0) "#EEF4F4" else "white"
    rect(0, haut - 1, sum(largeurs), haut, col = fond, border = NA)
    textes <- if (i == 1L) en_tete else c(etiquettes[i - 1], valeurs[i - 1, ])
    couleur <- if (i == 1L) "white" else "black"
    police <- if (i == 1L) 2 else 1
    text(bords[1] + 1, haut - 0.5, textes[1], adj = 0, cex = 0.75,
         col = couleur, font = police)
    text((bords[-1][-1] + bords[-length(bords)][-1]) / 2, haut - 0.5,
         textes[-1], cex = 0.75, col = couleur, font = police)
  }
  # Séparer visuellement les arbres standard, rééquilibrés et la référence.
  segments(bords[c(4, 6)], 0, bords[c(4, 6)], n_lignes, col = "#aab7bb", lwd = 2)
  rect(0, 0, sum(largeurs), n_lignes, border = "#aab7bb")
  title(paste("Arbres CART : résultats sur le test réservé (n =",
              nrow(test), ")"))
  mtext(lignes_mcnemar[1], side = 1, line = 1, cex = 0.75)
  mtext(lignes_mcnemar[2], side = 1, line = 2.2, cex = 0.75)
}

# Export : CSV (pour le rapport) et PNG (même tableau que le panneau Plots).
dir.create(file.path("resultats", "CART"), recursive = TRUE, showWarnings = FALSE)
write.csv(tableau_resultats, file.path("resultats", "CART", "resultats_arbres.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
png(file.path("resultats", "CART", "resultats_arbres.png"),
    width = 2800, height = 1400, res = 180)
tracer_tableau_resultats()
dev.off()


# 6. GRAPHIQUES ------------------------------------------------------------

# Taille de dessin lisible : au plus 20 feuilles.
reduire_pour_dessin <- function(arbre, max_feuilles = 20L) {
  # cptable : une ligne par taille d'arbre (nsplit coupures = nsplit + 1
  # feuilles). On garde la plus grande taille qui reste lisible.
  table_cp <- arbre$cptable
  ligne <- max(which(table_cp[, "nsplit"] + 1 <= max_feuilles))
  rpart::prune(arbre, cp = table_cp[ligne, "CP"])
}

tracer <- function(arbre, titre, sous_titre = "Feuilles : catégorie de risque prédite") {
  par(mfrow = c(1, 1), mar = c(2, 1, 4.2, 1))
  plot(arbre, uniform = TRUE, margin = 0.15)
  # use.n = FALSE : seulement la classe prédite, sans les effectifs dessous.
  text(arbre, use.n = FALSE, pretty = 0, cex = 0.8)
  title(titre)
  mtext(sous_titre, side = 1, line = 0.5, cex = 0.8)
}

dessiner_arbre <- function(arbre, grand_arbre, titre) {
  nb <- compter_feuilles(arbre)
  effectifs <- "Feuilles : catégorie de risque prédite"
  if (nb < 2L) {
    # La validation a retenu la racine seule : aucune coupure ne fait
    # mieux que prédire une seule classe. C'est un résultat en soi.
    par(mfrow = c(1, 1), mar = c(1, 1, 4.2, 1))
    plot.new()
    title(paste("Arbre retenu :", titre))
    text(0.5, 0.6, "Aucune coupure retenue par la validation croisée.", cex = 1.1)
    text(0.5, 0.45, paste("L'arbre prédit toujours :",
                          levels(train$Diabetes_Risk)[arbre$frame$yval[1]]))
    # À titre descriptif seulement : l'arbre complet réduit à une taille
    # lisible, pour voir quelles coupures il AURAIT faites.
    if (compter_feuilles(grand_arbre) >= 2L) {
      tracer(reduire_pour_dessin(grand_arbre, 10L),
             paste("Arbre non élagué (descriptif, NON retenu) :", titre),
             effectifs)
    }
  } else if (nb > 20L) {
    cat("Arbre", titre, ":", nb, "feuilles ; dessin réduit à 20 feuilles,",
        "prédictions faites avec l'arbre complet retenu.\n")
    tracer(reduire_pour_dessin(arbre, 20L),
           paste("Arbre retenu (", nb, "feuilles), 20 premières :", titre),
           effectifs)
  } else {
    tracer(arbre, paste("Arbre de classification retenu :", titre), effectifs)
  }
}

dessiner_cart_diabete <- function() {
  anciens_parametres <- par(no.readonly = TRUE)
  on.exit(par(anciens_parametres), add = TRUE)
  couleurs <- c("#007C78", "#D57535")
  
  # 0. Le tableau récapitulatif, dessiné dans le panneau Plots.
  tracer_tableau_resultats()
  
  # A. Erreur de validation selon cp : un panneau par version, car les
  # critères diffèrent (erreur simple ou erreur équilibrée).
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 4.2, 1))
  groupes <- list(
    standard = list(noms = c("clinique", "habitudes"),
                    titre = "Arbres standard",
                    ylab = "Erreur de validation (%)",
                    reference = 100 * mean(prediction_reference != test$Diabetes_Risk)),
    reequilibre = list(noms = c("clinique_reeq", "habitudes_reeq"),
                       titre = "Arbres rééquilibrés",
                       ylab = "Erreur équilibrée de validation (%)",
                       reference = 100 * (1 - 1 / 3))
  )
  for (g in groupes) {
    pertes <- sapply(validations[g$noms], function(v) 100 * v$risque_cv)
    matplot(seq_along(cp_candidats), pertes, type = "b", lty = 1, lwd = 2,
            pch = 19, col = couleurs, xaxt = "n", xlab = "cp (candidats)",
            ylab = g$ylab, main = g$titre,
            ylim = range(c(pertes, g$reference)))
    axis(1, at = seq_along(cp_candidats), labels = cp_candidats, cex.axis = 0.6)
    for (k in seq_along(g$noms)) {
      retenu <- match(validations[[g$noms[k]]]$cp, cp_candidats)
      points(retenu, pertes[retenu, k], pch = 1, cex = 2, lwd = 2,
             col = couleurs[k])
    }
    abline(h = g$reference, lty = 2, col = "#aab7bb")
    legend("topright", c("Médicales", "Habitudes", "Classe unique"),
           col = c(couleurs, "#aab7bb"), pch = c(19, 19, NA),
           lty = c(1, 1, 2), bty = "n", cex = 0.75)
  }
  
  # B. Un arbre pour chaque modèle, toujours dessiné.
  for (nom in names(resultats)) {
    dessiner_arbre(resultats[[nom]]$arbre, resultats[[nom]]$grand_arbre,
                   modeles[[nom]]$titre)
  }
  invisible(NULL)
}

# Dans RStudio, utiliser les flèches du panneau Plots pour voir chaque arbre.
if (isTRUE(getOption("cart_afficher_graphiques", TRUE))) {
  dessiner_cart_diabete()
}

