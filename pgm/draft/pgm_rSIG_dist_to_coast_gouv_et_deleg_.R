# =============================================
# pgm_rSIG_dist_to_coast.R
# 
# Objectif: Les fonctionnalités SIG de R : Distance à la cote
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

# Nettoyage
rm(list=ls())
plot.new()
setwd("/home/nlambert/Documents/R/R_neocarto_github")

# Distance à la cote (gouvernorats) ------------------------------------------------
centres<-readShapeSpatial("data/TUN/shp/Tunisie_snuts3_centres.shp")
coast<-readShapeSpatial("data/TUN/shp/coast.shp")
plot(coast,col="red", lwd=1.5)
plot(centres,add=TRUE)
dist<-gDistance(coast,centres,byid=TRUE)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
head(dist)

# Export
write.table(dist,"outputs/dist_to_coast_gouvernorats.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

#Carto
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts3.shp")
fdc@data = data.frame(fdc@data, dist[match(fdc@data[,"id"], dist[,"id"]),])
fdc@data$var <- fdc@data$dist
nbclass<-8
colours <- brewer.pal(nbclass,"YlOrRd")
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="black",lwd=1)


# Distance à la cote (delegations) ------------------------------------------------
centres<-readShapeSpatial("data/TUN/shp/Tunisie_snuts4_centres.shp")
coast<-readShapeSpatial("data/TUN/shp/coast.shp")
plot(coast,col="red", lwd=1.5)
plot(centres,add=TRUE)
dist<-gDistance(coast,centres,byid=TRUE)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
head(dist)


donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
donnees<-donnees[,c("del","del_nom","IDRVA2011")]
dist = data.frame(dist, donnees[match(dist[,"id"], donnees[,"del"]),])
dist<-dist[,c("id","del_nom","IDRVA2011","dist")]
colnames(dist)<-c("id","nom","idr","dist")
head(dist)

# Export
write.table(dist,"outputs/dist_to_coast_delegations.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

#Carto
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
fdc@data = data.frame(fdc@data, dist[match(fdc@data[,"id"], dist[,"id"]),])
fdc@data$var <- fdc@data$dist
nbclass<-8
colours <- brewer.pal(nbclass,"YlOrRd")
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="black",lwd=1)

# export pdf
#dev.copy(pdf,'resul/myplot.pdf')
#dev.off()
