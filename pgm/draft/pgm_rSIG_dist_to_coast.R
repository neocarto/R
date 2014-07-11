# =============================================
# pgm_rSIG_dist_to_coast.R
# 
# Objectif 1: Les fonctionnalités SIG de R : Distance à la cote des delegations
# Objectif 2 : Préparer un tabelau de données pour étudier le relation entre IDR et distance à la côte
# auteurs : Claude Grasland & Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================

#chargement des packages nécessaires
library(RColorBrewer)
library(classInt)
library(rgeos)
library(maptools)

# initialisation
rm(list=ls())
plot.new()
setwd("/home/nlambert/Documents/R/R_neocarto_github")


# ETAPE 1 --------------------------------------
# Calculer la distance entre les délégations et la côte
# ---------------------------------------------


# Import et affichage
centres<-readShapeSpatial("data/TUN/shp/Tunisie_snuts4_centres.shp")
coast<-readShapeSpatial("data/TUN/shp/coast.shp")
plot(coast,col="red", lwd=1.5)
plot(centres,add=TRUE)

# calcul distance points -> ligne 
dist<-gDistance(coast,centres,byid=TRUE)
head(dist)
# Mise en forme du tableau (id, dist)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
head(dist)

# ETAPE 2 --------------------------------------
# Préparer un tableau de données pour étudier
# La relation entre IDR et la distance à la côte
# ---------------------------------------------

# Import du fichier de données
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
donnees<-donnees[,c("del","del_nom","IDRVA2011")]
head(donnees)
# Joindre le tableau de donnée contenant la variable IDR
dist = data.frame(dist, donnees[match(dist[,"id"], donnees[,"del"]),])

# Fabrication du tableau final
dist<-dist[,c("id","del_nom","IDRVA2011","dist")]
colnames(dist)<-c("id","nom","idr","dist")
head(dist)

# Converson de la distance en km
dist$dist<-dist$dist/1000
#arrondi
dist$dist<-round(dist$dist,2)
head(dist)

# Export du tableau
write.table(dist,"outputs/dist_to_coast_delegations.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

# ETAPE 2 --------------------------------------
# Cartographie rapide pour verifie rles resultats
# ---------------------------------------------

fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
fdc@data = data.frame(fdc@data, dist[match(fdc@data[,"id"], dist[,"id"]),])
fdc@data$var <- fdc@data$dist
nbclass<-9
colours <- brewer.pal(nbclass,"YlOrRd")
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="black",lwd=1)
legend(x="topright", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title="(en metres)")
title(main="Distance à la cote",cex.sub=0.7)

