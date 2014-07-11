# =============================================
# pgm_anspa_access_distance_moyenne.R
# 
# Objectif: Créer un fichier de distance moyenne
# entre lieux munis d'attributs
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
#setwd("/home/nlambert/Documents/Travaux/Sfax2014/R/tunisie")
setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# ===================================

library(maptools)
library(RColorBrewer)
library(reshape)
library(classInt)

# ======================================
# 3. Chargement du fichier des distances 
# ======================================

# 3.1 Fichier de distance créé par  pgm_anspa_access_tabdist
list.files("outputs")
vilvil<-read.csv( "outputs/tab_dist.csv",header=TRUE,sep=";",dec=",",)
head(vilvil)

# ======================================
# 4. Chargement du fichier des masses 
# ======================================

# 4.1 choix du fichier
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",)
head(donnees)

# 4.2 choix du code du nom et de la masse
code_j<-donnees$del
nom_j<-donnees$del_nom

#Mj<-donnees$POPTO2010
Mj<-donnees$SUP2010

# 4.3 collage de la masse sur le fichier des distances
tab<-data.frame(code_j,Mj)
vilvil<-merge(vilvil,tab,by="code_j",all.X=TRUE,all.Y=FALSE)
head(vilvil)

# 4.4 Préparation du tableau de resultat
vil<-data.frame(code_j,nom_j,Mj)
names(vil)<-c("code","nom","M")
head(vil)





# ============================================
# 5 . Calcul d'accessibilite moyenne pondérée
# ============================================

# calcul de masse x distance
vilvil$MD<-vilvil$Dij*vilvil$Mj
head(vilvil)
# somme des masse x distance
MD<-tapply(vilvil$MD,vilvil$code_i,FUN=sum)
res<-data.frame(row.names(MD),MD)
names(res)<-c("code","MD")
tabres<-merge(vil,res,by="code")


# Distance moyenne = somme(masse x distance) par somme(masse)
tabres$Access<-round(tabres$MD/sum(vil$M),2)
tabres<-tabres[order(tabres$Access),]
head(tabres)
tail(tabres)

# ===================================================
# 6 . Cartographie de l'accessibilite moyenne pondérée
# ===================================================

#Import du fond de carte
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
# Joiunture
fdc@data = data.frame(fdc@data, tabres[match(fdc@data[,"id"], tabres[,"code"]),])
# Choix de la variable
fdc@data$var <- fdc@data$Access
# Nb classes
nbclass<-10
# Choix des couleurs
colours <- brewer.pal(nbclass,"RdBu")
# Discretisation
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
#attribution des couleurs aux régions 
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
# Cartographie
plot(fdc, col=colMap,border="black",lwd=1)
# Ajout des gouvernorats
gouv <- readShapeSpatial("data/TUN/shp/Tunisie_snuts3.shp")
plot(gouv, border="black", lwd=1.5,add=TRUE)
#Habillage
legend(x="topright", legend=leglabs(round(distr,0),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title=" distance moyenne (en km)")
title(main="Accessibilité moyenne à la surface",cex.sub=0.7)

# ==========================
# Sauvegarde résultat
# ==========================

write.table(tabres,"outputs/access_moy_pond.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

