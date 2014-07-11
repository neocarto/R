# =============================================
# pgm_anspa_tabdist_tunisie.R
# 
# Objectif: Créer un fichier de distance entre lieux munis d'attributs
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

library(maptools)
library(RColorBrewer)
library(reshape)


# ===================================
# 3. Importation du fichier de géométrie
# ===================================

#Import des géométries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
head(fdc@data)

#création d'un dataframe avec les coordonnées des centroides des communes
pt <- cbind(fdc@data[,"id"],as.data.frame(coordinates(fdc)))

#renommage des colonnes de ce dataframe
colnames(pt) <- c("id","x","y")

# ==============================================
# 4. Calcul des distances entre couples de lieux
# ==========head(pt====================================
head(pt)
# Calcul des distances à partir des coordonnées

calcul<-dist(c(pt$x,pt$y),method="euclidean")
calcul<-round(calcul/1000,2)

# Conversion en matrice
matdist<-as.matrix(calcul)
row.names(matdist)<-pt$id
colnames(matdist)<-pt$id
matdist[1:5,1:5]

# Conversion en tableau colonne
coldist<-melt(matdist)
colnames(coldist)<-c("i","j","Dij")

head(coldist)


# ==================================================
# 5. Ajout d'attributs sur les couples de lieux
# ==================================================

tab_ij<-coldist
head(tab_ij)
tail(tab_ij)

#Import des données
list.files("data")
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",)
head(donnees)
tail(donnees)

# Choix et collage des attributs origine (i)
i<-donnees$del
del_i<-donnees$del_nom
gou_i<-donnees$gou_nom
reg_i<-donnees$reg_nom

cap_i<-donnees$cap_gou
tab_i<-data.frame(i,del_i,gou_i,reg_i,cap_i)
tab_ij$i<-as.character(tab_ij$i)
tab_i$i<-as.character(tab_i$i)
tab_ij<-merge(tab_ij,tab_i,by="i",all.X=TRUE,all.Y=FALSE)


# Choix et collage des attributs destination (j)
j<-donnees$del
del_j<-donnees$del_nom
gou_j<-donnees$gou_nom
reg_j<-donnees$reg_nom
cap_j<-donnees$cap_gou
tab_j<-data.frame(j,del_j,gou_j,reg_j,cap_j)
tail(tab_j)
tab_ij$j<-as.character(tab_ij$j)
tab_i$j<-as.character(tab_i$j)
tab_ij<-merge(tab_ij,tab_j,by="j",all.X=TRUE,all.Y=FALSE)


head(tab_ij)
# Sauvegarde
write.table(tab_ij,"outputs/tabdist.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)



