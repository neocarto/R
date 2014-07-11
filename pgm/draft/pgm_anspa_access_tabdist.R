# =============================================
# pgm_anspa_access_tabdist.R
# 
# Objectif: Créer un fichier de distance entre couples
# de lieux et ajoute des variables
# auteurs : Claude Grasland & Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================

# ===================================
# 1. Choisir le dossier
# ===================================

#setwd("F:/Sfax2014/R/tunisie")
setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# ===================================

library(reshape2)  # package de transformation matrice / colonne
library(maptools)  # package de lecture des fichiers .shp 

# ===================================
# 3. Importation du fichier de géométrie
# ===================================

#Import des géométries
list.files("geom")
fdc <- readShapeSpatial("data/TUN/geom/Tunisie_snuts4.shp")
head(fdc@data)

#création d'un dataframe avec les coordonnées des centroides des communes
pt <- cbind(fdc@data[,"id"],as.data.frame(coordinates(fdc)))

#renommage des colonnes de ce dataframe
colnames(pt) <- c("id","x","y")

# ==============================================
# 4. Calcul des distances entre couples de lieux
# ==============================================

# 4.1 Calcul du vecteur des distances

# Calcul des distances à partir des coordonnées
coo<-coordinates(fdc)
# Calcul des distances entre toutes les paires (x,y)
calcul<-dist(coo,method="euclidean")
# conversion des métres en kilomètres 
calcul<-calcul/1000
# arrondis à 2 chiffres après la virgule
calcul<-round(calcul,2)
# vérification du résultat
head(calcul)

# 4.2 Conversion du vecteur des distances en matrice de distance

# passage du type vecteur au type matrice
matdist<-as.matrix(calcul)
# ajout des noms de ligne
row.names(matdist)<-pt$id
# ajout des noms de colonnes
colnames(matdist)<-pt$id
# dimension de la matrice
dim(matdist)
# affichage d'un extrait de la matrice
matdist[1:5,1:5]

# 4.3 Conversion de la matrice des distances tableau colonne

# connversion du mode matrice en mode colonne (package reshape)
coldist<-melt(matdist)
# ajout du nom des colonnes
colnames(coldist)<-c("code_i","code_j","Dij")
# dimension du tableau
dim(coldist)
# affichage d'un extrait du tableau
head(coldist)


# ==========================================
# 5. Ajout d'attributs aux couples de lieux 
# ==========================================

# 5.1 Copie du tableau coldist dans tab_ij
tab_ij<-coldist
head(tab_ij)
tail(tab_ij)

# 5.2 Importation des données supplémenaires 
list.files("data")
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",)
head(donnees)
tail(donnees)
code<-donnees$del
nom<-donnees$del_nom
gou<-donnees$gou_nom
cap<-donnees$cap_gou
tab<-data.frame(code,nom,gou,cap,sup,pop)
write.table(tab,"outputs/tab_lieu.csv", dec=",",sep=";",row.names=FALSE,quote=FALSE)


# 5.3 Choix et collage des attributs origine (i)
code_i<-code
nom_i<-nom
gou_i<-gou
cap_i<-cap
tab_i<-data.frame(code_i,nom_i,gou_i,cap_i)
tab_i$code_i<-as.character(tab_i$code_i)
tab_ij$code_i<-as.character(tab_ij$code_i)
tab_ij<-merge(tab_ij,tab_i,by="code_i",all.X=TRUE,all.Y=FALSE)

head(tab_ij)

# 5.4 Choix et collage des attributs destination (j)
code_j<-code
nom_j<-nom
gou_j<-gou
cap_j<-cap
tab_j<-data.frame(code_j,nom_j,gou_j,cap_j)
tab_ij$code_j<-as.character(tab_ij$code_j)
tab_j$code_j<-as.character(tab_j$code_j)
tab_ij<-merge(tab_ij,tab_j,by="code_j",all.X=TRUE,all.Y=FALSE)

head(tab_ij)


# ======================================================================
# 6. Exportation du tableau de distance et attributs des paires de lieux
# ======================================================================

write.table(tab_ij,"outputs/tab_dist.csv", dec=",",sep=";",row.names=FALSE,quote=FALSE)



