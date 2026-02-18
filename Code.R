
# -----------------------------
# IMPORT DES PACKAGES
# -----------------------------

library(readxl)
library(tidyverse)
library(naniar)
library(dplyr)
library(outliers)
library(EnvStats)
library(PerformanceAnalytics)
library(corrplot)
library(reshape2)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(psych)
library(kableExtra)
library(tidytext)

# -----------------------------
# IMPORT DE LA BASE DE DONNÉES
# -----------------------------

Base <- read_xlsx("IGT - Pouvoir de réchauffement global.xlsx")
head(Base) # Première vue des données 
names(Base) # Nom des variables 
nrow(Base) # Nombre de ligne

# -----------------------------
# RENOMMER LES VARIABLES
# -----------------------------

Base <- Base %>%
  rename(
    Industrie = `Industrie hors-énergie`,
    Biomasse = `CO2 biomasse hors-total`,
    Code_INSEE_commune = `INSEE commune`,
    Autres_transports_international = `Autres transports international`,
    Autres_transports = `Autres transports`,
    Nom_commune = Commune)

# -----------------------------
# STRUCTURE DE LA BASE
# -----------------------------

str(Base)



# ---------------------------------------------
# ANALYSE ET TRAITEMENT DES DONNÉES MANQUANTES 
# ---------------------------------------------

Base_non_zero <- Base |>
  select(where(~ any(is.na(.))))  # ne garde que les colonnes avec NA

# Graphique avec gg_miss_var
Données_manquantes <- gg_miss_var(Base_non_zero) +
  labs(
    title = "Graphique 1: Visualisation des données manquantes par variable",
    x = "Variables",
    y = "Valeurs manquantes") +
  theme(
    plot.title = element_text(size = 7, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)) 


sum(is.na(Base$Autres_transports_international))
# 32907 données manquantes 
sum(is.na(Base$Autres_transports))
# 25819 données manquantes 
sum(is.na(Base$Industrie))
#1308
sum(is.na(Base$Energie))
#1308
sum(is.na((Base$Agriculture)))
#62
sum(is.na((Base$Routier)))
#20 
sum(is.na((Base$Résidentiel)))
#6
sum(is.na((Base$Déchets)))
#6

# Suppression des 2 variables avec plus de 80 % de NA

Base <- Base[, !colnames(Base) %in% c(
  "Autres_transports",
  "Autres_transports_international")]

# On s'aperçoit qu'il reste toujours quelques variables avec des NA

gg_miss_var(Base)



# ------------------------------------
# Création de la variable département
# ------------------------------------

Base$departement <- substr(Base$Code_INSEE_commune, 1, 2)

table(Base$departement)

sum(is.na(Base$departement))


# ---------------------------------------------
# Import d'une nouvelle base de donnée (INSEE)
# ---------------------------------------------

Base_INSEE <- read_csv("communes-france-2025.csv")

# On garde seulement 2 variables sur les 47: le nom du département et le code de celui-ci

commune_reduite <- Base_INSEE[, c( "dep_nom",
                                   "dep_code")]

# Dans la base de donnée initiale on garde seulement une ligne pour chaque numéros de département différent

communes_reduite_unique <- commune_reduite |>
  group_by(dep_code) |> 
  slice(1) |>
  ungroup()

# ------------------
# Jointure gauche
# ------------------

Base_dep <- left_join(
  Base,
  communes_reduite_unique,
  by = c("departement" = "dep_code"),
  keep = TRUE)

sum(is.na(Base_dep$dep_nom))

# On supprime les colonnes dont nous n'avons plus besoin

Base_dep <- Base_dep[, !colnames(Base_dep) %in% c(
  "INSEE commune",
  "Commune","departement")]

# -----------------------------
# Regroupement par code postal
# -----------------------------


Base_dep <- Base_dep |>
  group_by(dep_code) |>            
  summarise(
    dep_nom = first(dep_nom),
    Agriculture = mean(Agriculture, na.rm = TRUE),
    Biomasse = mean(Biomasse, na.rm = TRUE),
    Déchets = mean(Déchets, na.rm = TRUE),
    Energie = mean(Energie, na.rm = TRUE),
    Industrie = mean(Industrie, na.rm = TRUE),
    Résidentiel = mean(Résidentiel, na.rm = TRUE),
    Routier = mean(Routier, na.rm = TRUE),
    Tertiaire = mean(Tertiaire, na.rm = TRUE))

# Vérification des données manquantes 
gg_miss_var(Base_dep)

# Identification de la valeur manquante
Base_dep[!complete.cases(Base_dep), ]

# Il s'agit de Paris (intra-muros), étant donné qu'aucune activité agricole n'y est réalisée, on décide d'imputer la valeur 0.

Base_dep <- Base_dep %>% 
  mutate(across(everything(), ~replace_na(., 0)))

str(Base_dep)

variables_quantitatives <- c("Agriculture","Biomasse","Déchets", "Energie","Industrie","Résidentiel","Routier","Tertiaire")      

# Tableau 1 : Statistique descriptive des variables quantitative pour les émissions de C02 par commune (en annexe)
tableau_analyse_univ_quanti_commune <- lapply(names(Base[variables_quantitatives]), function(var) {
  x <- Base[[var]]
  if(is.numeric(x)) {
    data.frame(
      Variable = var,
      Moyenne = mean(x, na.rm = TRUE),
      Médiane = median(x, na.rm = TRUE),
      Minimum = min(x, na.rm = TRUE),
      Maximum = max(x, na.rm = TRUE),
      Ecart_type = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE)}
}) |> bind_rows()

# Tableau 2: Statistiques descriptives des variables quantitatives des émissions de CO2 pour les 96 départements français
tableau_analyse_univ_quanti_dep <- lapply(names(Base_dep[variables_quantitatives]), function(var) {
  x <- Base_dep[[var]]
  if(is.numeric(x)) {
    data.frame(
      Variable = var,
      Moyenne = mean(x, na.rm = TRUE),
      Médiane = median(x, na.rm = TRUE),
      Q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      Q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      Minimum = min(x, na.rm = TRUE),
      Maximum = max(x, na.rm = TRUE),
      Ecart_type = sd(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}) |> bind_rows()

options(scipen = 999)

par(mfrow = c(2,4), mar = c(2,2,1,1))

hist(Base_dep$Agriculture, xlab = "Agriculture", main = "Emission C02 - Agriculture ", col = "lightgreen", cex.main = 0.8)
hist(Base_dep$Biomasse, xlab = "Biomasse", main = " Emission C02 - Biomasse", col = "darkred",cex.main = 0.8)
hist(Base_dep$Déchets, xlab = "Déchets", main = "Emission C02 - Déchets", col = "tomato",cex.main = 0.8)
hist(Base_dep$Energie, xlab = "Energie", main = "Emission C02 - Energie" , col = "hotpink",cex.main = 0.8)
hist(Base_dep$Industrie, xlab = "Industrie", main = "Emission C02 - Industrie ", col = "darkgreen",cex.main = 0.8)
hist(Base_dep$Résidentiel, xlab = "Résidentiel", main = "Emission C02 - Résidentiel ", col = "lightblue",cex.main = 0.8)
hist(Base_dep$Routier, xlab = "Routier", main = "Emission C02 - Routier", col = "mediumvioletred",cex.main = 0.8)
hist(Base_dep$Tertiaire, xlab = "Tertiaire", main = "Emission C02 - Terrtiaire", col = "firebrick",cex.main = 0.8)

#Graphiques 3 : Histogrammes zoomés des variables quantitatives
options(scipen = 999)

par(mfrow = c(2,4), mar = c(2,2,1,1))

hist(Base_dep$Agriculture, xlab = "Agriculture", main = "Emission C02 - Agriculture ", col = "lightgreen", cex.main = 0.8)
hist(Base_dep$Biomasse, xlab = "Biomasse", main = " Emission C02 - Biomasse", col = "darkred",cex.main = 0.8, xlim = c(0, 20000), breaks = 50)
hist(Base_dep$Déchets, xlab = "Déchets", main = "Emission C02 - Déchets", col = "tomato",cex.main = 0.8, xlim = c(0, 10000), breaks = 50)
hist(Base_dep$Energie, xlab = "Energie", main = "Emission C02 - Energie" , col = "hotpink",cex.main = 0.8, xlim = c(0, 10000), breaks = 100)
hist(Base_dep$Industrie, xlab = "Industrie", main = "Emission C02 - Industrie ", col = "darkgreen",cex.main = 0.8, xlim = c(0, 20000), breaks = 50)
hist(Base_dep$Résidentiel, xlab = "Résidentiel", main = "Emission C02 - Résidentiel ", col = "lightblue",cex.main = 0.8, xlim = c(0, 20000), breaks = 50)
hist(Base_dep$Routier, xlab = "Routier", main = "Emission C02 - Routier", col = "mediumvioletred",cex.main = 0.8, xlim = c(0, 20000), breaks = 50)
hist(Base_dep$Tertiaire, xlab = "Tertiaire", main = "Emission C02 - Terrtiaire", col = "firebrick",cex.main = 0.8, xlim = c(0, 20000), breaks = 50)


# Graphiques 4 : Densités des variables quantitatives
options(scipen = 999)

par(mfrow = c(2,4), mar = c(2,2,2,1))

plot(density(Base_dep$Agriculture, na.rm = TRUE),main = "Emission CO2 - Agriculture",xlab = "Agriculture",col = "lightgreen",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Biomasse, na.rm = TRUE),main = "Emission CO2 - Biomasse",xlab = "Biomasse",col = "darkred",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Déchets, na.rm = TRUE),main = "Emission CO2 - Déchets",xlab = "Déchets",col = "tomato",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Energie, na.rm = TRUE),main = "Emission CO2 - Energie",xlab = "Energie",col = "hotpink",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Industrie, na.rm = TRUE), main = "Emission CO2 - Industrie",xlab = "Industrie",col = "darkgreen",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Résidentiel, na.rm = TRUE),main = "Emission CO2 - Résidentiel",xlab = "Résidentiel",col = "lightblue",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Routier, na.rm = TRUE),main = "Emission CO2 - Routier",xlab = "Routier",col = "mediumvioletred",lwd = 2, cex.main = 0.8)

plot(density(Base_dep$Tertiaire, na.rm = TRUE), main = "Emission CO2 - Tertiaire", xlab = "Tertiaire",col = "firebrick",lwd = 2, cex.main = 0.8)

par(mfrow = c(2,4), mar = c(2,2,1,1))

boxplot(Base_dep$Agriculture, xlab = "Agriculture", main = "Boxplot - Agriculture", col = "lightgreen",cex.main = 0.8)
boxplot(Base_dep$Biomasse, xlab = "Biomasse", main = "Boxplot - Biomasse", col = "darkred",cex.main = 0.8)
boxplot(Base_dep$Déchets, xlab = "Déchets", main = "Boxplot - Agriculture", col = "tomato",cex.main = 0.8)
boxplot(Base_dep$Energie, xlab = "Energie", main = "Boxplot - Energie", col = "hotpink",cex.main = 0.8)
boxplot(Base_dep$Industrie, xlab = "Industrie", main = "Boxplot - Industrie", col = "darkgreen",cex.main = 0.8)
boxplot(Base_dep$Résidentiel, xlab = "Résidentiel", main = "Boxplot - Résidentiel", col = "lightblue",cex.main = 0.8)
boxplot(Base_dep$Routier, xlab = "Routier", main = "Boxplot - Routier", col = "mediumvioletred",cex.main = 0.8)
boxplot(Base_dep$Tertiaire, xlab = "Tertiaire", main = "Boxplot - Tertiaire", col = "firebrick",cex.main = 0.8)

# Analyse univariée des variables qualitatives

sum(duplicated(Base_dep$dep_code))
sum(duplicated(Base_dep$dep_nom))

# 2. Analyse bivariée
# Variables quantitative - quantitative


mat <- Base_dep |>
  select(all_of(variables_quantitatives)) |>
  cor(use = "pairwise.complete.obs")


mat |>
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "cor") %>%
  filter(var1 < var2, abs(cor) > 0.3) %>% 
  arrange(desc(abs(cor)))


# Graphiques 6: Corrélation des émissions de CO₂ par secteur (Pearson)
plot.new()
cor(Base_dep[,c("Agriculture","Biomasse","Déchets", "Energie","Industrie","Résidentiel","Routier","Tertiaire")], use="complete.obs",method = c("pearson"))


mydata <- Base_dep[, c("Agriculture","Biomasse","Déchets","Energie","Industrie","Résidentiel","Routier","Tertiaire")]
chart.Correlation(mydata, histogram=TRUE, pch=19,method = c("pearson"))
graph_chart_corr_pearson <- recordPlot()

corr_mat_pearson=cor(mydata,method="pearson")
corrplot(corr_mat_pearson, method = 'number',type="upper",number.cex = 0.5)
graph_corr_number_pearson <- recordPlot()

corrplot(corr_mat_pearson,type="upper")
graph_corr_color_pearson <- recordPlot()


# Graphiques 8 : Corrélation des émissions de CO₂ par secteur (Spearman)
plot.new()
cor(Base_dep[,c("Agriculture","Biomasse","Déchets", "Energie","Industrie","Résidentiel","Routier","Tertiaire")], use="complete.obs",method = c("spearman"))


mydata <- Base_dep[, c("Agriculture","Biomasse","Déchets","Energie","Industrie","Résidentiel","Routier","Tertiaire")]
chart.Correlation(mydata, histogram=TRUE, pch=19,method = c("spearman"))
graph_chart_corr_spearman <- recordPlot()

corr_mat=cor(mydata,method="spearman")
corrplot(corr_mat, method = 'number',type="upper",number.cex = 0.5)
graph_corr_number_spearman <- recordPlot()

corrplot(corr_mat,type="upper")
graph_corr_color_spearman <- recordPlot()


par(mfrow = c(1, 2)) 
plot(Base_dep$Résidentiel, Base_dep$Routier,
     main = "Résidentiel vs Routier",
     xlab = "Emissions Résidentielles",
     ylab = "Emissions Routières",
     pch = 19)
abline(lm(Routier ~ Résidentiel, data = Base_dep), col = "red", lwd = 2)


plot(Base_dep$Agriculture, Base_dep$Résidentiel,
     main = "Agriculture vs Résidentiel",
     xlab = "Emissions Agriculture",
     ylab = "Emissions Résidentielles",
     pch = 19)
abline(lm(Résidentiel ~ Agriculture, data = Base_dep),  col = "red", lwd = 2)

# Variables qualitative - qualitative
length(unique(paste(Base_dep$dep_code, Base_dep$dep_nom)))

# Variables qualitative - quantitative

Base_long <- melt(Base_dep, id.vars = "dep_nom", 
                  measure.vars = c("Agriculture", "Biomasse", "Déchets", 
                                   "Energie", "Industrie", "Résidentiel", 
                                   "Routier", "Tertiaire"))
ggplot(Base_long, aes(x = dep_nom, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Répartition des émissions par secteur et département",
       x = "Département", y = "Émissions (tonnes CO2 eq)")

# Graphiques 11 : Détails émissions de C02 par secteurs et département 

plot <- list()
for (var in variables_quantitatives) {
  
  p <- ggplot(Base_dep, aes(x = reorder(dep_nom, -.data[[var]]),
                            y = .data[[var]])) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = paste("Émissions de CO2 – secteur", var, "par département"),
      x = "Département",
      y = "Émissions (tonnes équivalent CO2)"
    )
  
  plot[[var]] <- p}

plot

#  Heatmap des émissions de CO₂ par secteur et département
ggplot(Base_long, aes(x = variable, y = dep_nom, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Heatmap des émissions par secteur et département",
       x = "Secteur", y = "Département", fill = "Émissions")

# Top 10 des départements les plus émétteurs par secteurs 
Base_long <- melt(Base_dep, id.vars = "dep_nom", 
                  measure.vars = c("Agriculture", "Biomasse", "Déchets", 
                                   "Energie", "Industrie", "Résidentiel", 
                                   "Routier", "Tertiaire"))

# top 10 par secteur
Base_top10 <- Base_long |>
  group_by(variable) |>
  slice_max(value, n = 10) |>  
  ungroup()


# Boxplots
ggplot(Base_top10, 
       aes(x = reorder_within(dep_nom, value, variable),
           y = value, 
           fill = variable)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(title = "Top 10 départements – Boxplots des émissions par secteur",
       x = "Département",
       y = "Émissions (tonnes CO₂ eq)")

# ------------------------
# 3. Analyse factorielle
# ------------------------
# Justification de l'Analyse en composantes principales
# Indice kmo 
kmo_result_ACP1 <- KMO(Base_dep[, variables_quantitatives])
kmo_result_ACP1
# Test shapiro
sapply(Base_dep[, c("Agriculture","Biomasse","Déchets","Energie",
                    "Industrie","Résidentiel","Routier","Tertiaire")],
       function(x) shapiro.test(x)$p.value)
# Test Bartlett 
bartlett_result <- cortest.bartlett(
  cor(Base_dep[, variables_quantitatives], use = "pairwise.complete.obs"),
  n = nrow(Base_dep))
bartlett_result
# Determinant 
déterminant_ACP_1 <- det(corr_mat_pearson)
déterminant_ACP_1

# ACP sur l'enssemble des émissions de CO2 par département 

# --------------------------
# Code département en index
# --------------------------

rownames(Base_dep) <- Base_dep$dep_code

pca_full <- PCA(Base_dep[, variables_quantitatives], scale.unit = TRUE, graph = FALSE)

# -------------------------------------------
# valeurs propres et % de variance expliquée
# -------------------------------------------

pca_full$eig   

fviz_eig(pca_full, 
         addlabels = TRUE,
         main = "Eboulis des valeurs propres") +
  ylim(0, 78)+
  theme(plot.title = element_text(size = 12))

fviz_pca_var(pca_full, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cercle des corrélations - Variables")

pca_full$var$contrib
pca_full$var$cos2

# Graphiques 16 et 17 des contributions des variables à l'axe 1 et 2
Contribution_ACP1_dim1 <- fviz_contrib(pca_full, choice = "var", axes = 1, top = 10)
Contribution_ACP1_dim2 <- fviz_contrib(pca_full, choice = "var", axes = 2, top = 10)

fviz_pca_ind(pca_full, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus (cos2)" )+ ylim(-6.5, 13) + xlim(-3, 17)

pca_full$ind

fviz_pca_biplot(pca_full, 
                repel = TRUE,        
                col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.ind = "cos2")   

# ACP après regroupement des variables fortement corrélées

Base_dep_blocs_corrélé <- Base_dep


# ---------------------------------------------------
# Regroupement des variables corélées dans des blocs 
# ---------------------------------------------------

Base_dep_blocs_corrélé$Bloc_residentiel <- rowMeans(Base_dep_blocs_corrélé[, c("Biomasse","Résidentiel","Tertiaire","Routier")])

Base_dep_blocs_corrélé$Bloc_indus <- rowMeans(Base_dep_blocs_corrélé[, c("Energie","Industrie")])

# Supprimer les variables d'origine intégrées dans les blocs
Base_dep_blocs_corrélé <- Base_dep_blocs_corrélé[, !colnames(Base_dep_blocs_corrélé) %in% c("Biomasse","Résidentiel","Tertiaire","Routier","Energie","Industrie")]


# Code département en index
rownames(Base_dep_blocs_corrélé) <- Base_dep_blocs_corrélé$dep_code


# -------------------
# Vérification KMO 
# -------------------

variables_quantitatives_blocs <- c("Agriculture","Déchets","Bloc_residentiel","Bloc_indus" )
kmo_result_blocs <- KMO(Base_dep_blocs_corrélé[, variables_quantitatives_blocs])
kmo_result_blocs
# Indice KMO supérieur à 0,6, validé 

# -------------------------
# Vérification déterminant 
# -------------------------

corr_mat_blocs <- cor(Base_dep_blocs_corrélé[, variables_quantitatives_blocs], use = "complete.obs", method = "pearson")
det(corr_mat_blocs)
# déterminant supérieur à 0 donc pas de corrélation parfaite (mieux que l'ancien déterminant calculé)

pca_blocs<- PCA(Base_dep_blocs_corrélé[, variables_quantitatives_blocs], scale.unit = TRUE, graph = FALSE)

# Choix du nombre de dimension 

pca_blocs$eig   


Eboulis_blocs <- fviz_eig(pca_blocs, 
                          addlabels = TRUE,
                          main = "Eboulis des valeurs propres") +
  ylim(0, 78)+
  theme(plot.title = element_text(size = 12))

fviz_pca_var(pca_blocs, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cercle des corrélations - Variables")

fviz_pca_ind(pca_blocs, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus")

fviz_pca_ind(pca_blocs, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus") + ylim(-2, 3)+xlim(-1, 2)

# ACP sur les régions 
regions <- read.csv("departements-region.csv",
                    sep = ",",
                    stringsAsFactors = FALSE)

head(regions)

Base_dep$dep_code <- as.character(Base_dep$dep_code)
regions$num_dep <- as.character(regions$num_dep)
Base_dep <- merge(Base_dep,
                  regions[, c("num_dep", "region_name")],
                  by.x = "dep_code",
                  by.y = "num_dep",
                  all.x = TRUE)

Base_region <- Base_dep |>
  group_by(region_name) |>
  summarise(across(where(is.numeric),
                   \(x) mean(x, na.rm = TRUE)))


Base_region$Bloc_residentiel <- rowMeans(Base_region[, c("Biomasse","Résidentiel","Tertiaire","Routier")])

# Supprimer les variables d'origine intégrées dans les blocs
Base_region <- Base_region[, !colnames(Base_region) %in% c("Biomasse","Résidentiel","Tertiaire","Routier","Total_CO2")]


Base_region_acp <- as.data.frame(Base_region)

rownames(Base_region_acp) <- Base_region_acp$region_name

Base_region_acp$region_name <- NULL


variables_quantitatives_blocs_reg <- c("Agriculture","Déchets","Energie","Industrie","Bloc_residentiel")


kmo_result_ACP_région <- KMO(Base_region_acp[, variables_quantitatives_blocs_reg])
kmo_result_ACP_région
# KMO > 0,6

mat_cor <- cor(Base_region_acp, method = "pearson")
mat_cor


det(mat_cor)


reg_acp <- PCA(Base_region_acp,
               scale.unit = TRUE,
               graph = FALSE)

reg_acp$eig

# Eboulis des valeurs propres pour l'ACP par région
fviz_eig(reg_acp,
         addlabels = TRUE,
         main = "Eboulis des valeurs propres")

# Contribution des variables à l’ACP par région
fviz_pca_var(reg_acp, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cercle des corrélations - Variables")

#  Projection des régions sur le plan 
fviz_pca_ind(reg_acp, col.ind = "cos2",
             repel = TRUE,
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus (cos2)")

# ACP ratio des émissions de CO2 par habitant

#| message: false
habs <- read_xlsx("estim-pop-dep-sexe-gca-1975-2026.xlsx", sheet = 2, skip=3 )
habs <- habs[,c(1,2,8)]


Base_dep_habs <- left_join(
  Base_dep_blocs_corrélé,
  habs,
  by = c("dep_code" = "Départements"),
  keep = TRUE)

Base_dep_habs <- Base_dep_habs |>
  select(-Départements, -`...2`) |>
  rename(nbr_habs = `...8`)

rownames(Base_dep_habs) <- Base_dep_habs$dep_code

kmo_result <- KMO(Base_dep_habs[, variables_quantitatives_blocs])
kmo_result
# kmo > 0,6

mat_cor <- cor(Base_dep_habs[sapply(Base_dep_habs, is.numeric)], method = "pearson")
det(mat_cor)

pca_ind <- PCA(Base_dep_habs[, variables_quantitatives_blocs], scale.unit = TRUE, graph = FALSE)

fviz_pca_var(pca_ind, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle des corrélations - Variables")
fviz_pca_ind(pca_ind,  
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus (cos2)")

# ACP sur les ratios d'émissions

# Création de la base
Base_dep$Total_CO2 <- rowSums(Base_dep[, variables_quantitatives])

Base_dep_ratios <- Base_dep
Base_dep_ratios[, variables_quantitatives] <- (Base_dep_ratios[, variables_quantitatives] / Base_dep_ratios$Total_CO2) * 100
rownames(Base_dep_ratios) <- Base_dep_ratios$dep_code

mydata_ratios <- Base_dep_ratios[,c("Agriculture","Biomasse","Déchets","Energie","Industrie","Résidentiel","Routier","Tertiaire")]
corr_mat_pearson_ratios=cor(mydata_ratios,method="pearson")
corrplot(corr_mat_pearson_ratios, method = 'number',type="upper",number.cex = 0.5)
det(corr_mat_pearson_ratios)

variables_quantitatives_ratios <- c("Routier","Biomasse", "Energie","Industrie","Résidentiel","Déchets")

kmo_result_ratios <- KMO(Base_dep_ratios[, variables_quantitatives_ratios])
kmo_result_ratios # kmo > 0,6

mydata_ratios_2 <- Base_dep_ratios[,c("Routier","Biomasse","Déchets","Energie","Industrie","Résidentiel")]
corr_mat_pearson_ratios2=cor(mydata_ratios_2,method="pearson")
det(corr_mat_pearson_ratios2)

pca_ratios <- PCA(Base_dep_ratios[, variables_quantitatives_ratios], scale.unit = TRUE, graph = FALSE)


pca_ratios$eig


fviz_eig(pca_ratios, 
         addlabels = TRUE,
         main = "Eboulis des valeurs propres") +
  ylim(0, 45)+
  theme(plot.title = element_text(size = 12))

fviz_pca_var(pca_ratios, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle des corrélations - Variables")

fviz_contrib(pca_ratios, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_ratios, choice = "var", axes = 2, top = 10)
pca_ratios$var$cos2

fviz_pca_ind(pca_ratios,  
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Carte des individus (cos2)")

