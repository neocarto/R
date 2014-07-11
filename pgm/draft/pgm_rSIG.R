# =============================================
# pgm_rSIG.R
# 
# Objectif: Les fonctionnalités SIG de R
# Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================

#chargement des packages nécessaires
library(rgeos)
library(sp)
library(maptools)
library(reshape2)
library(RColorBrewer)
library(classInt)

# Nettoyage
rm(list=ls())
plot.new()
setwd("/home/nlambert/Documents/R/R_neocarto_github")

#import
fdcOri<-"data/TUN/shp/Tunisie_snuts4.shp"
fdcOri2<-"data/TUN/shp/sebkhas.shp"
delegations<-readShapeSpatial(fdcOri)
sebkhas<-readShapeSpatial(fdcOri2)


# Tester le shp
getinfo.shape(fdcOri)
class(delegations)
head(delegations@data)
head(gIsValid(delegations, byid = TRUE, reason=TRUE))

#extraction d'un polygone
plot(delegations)
poly1<-delegations[delegations@data$id=="TS1124",]
plot(poly1,col="red")


#Extraction des contours
b = gBoundary(poly1)
plot(b)

#buffer
buff<-gBuffer(poly1, byid=TRUE, id=NULL, width=500, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
plot(buff,add=TRUE)

buff<-gBuffer(delegations, byid=TRUE, id=NULL, width=30000, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
plot(buff)

buff<-gBuffer(delegations, byid=FALSE, id=NULL, width=30000, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
plot(buff)

#simplification
simpl1<-gSimplify(delegations, tol=4000, topologyPreserve=TRUE)
simpl2<-gSimplify(delegations, tol=12000, topologyPreserve=TRUE)
simpl3<-gSimplify(delegations, tol=24000, topologyPreserve=TRUE)
par(mfrow=c(1,4))
plot(delegations)
plot(simpl1)
plot(simpl2)
plot(simpl3)
par(mfrow=c(1,1))

#centroide
centres<-gCentroid(delegations, byid=TRUE, id = NULL)
plot(centres)
head(centres@coords)


#Aggregation des géométries
head(delegations@data)
buff<-gBuffer(delegations, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
gouvernorats<-gUnaryUnion(buff,id = delegations@data$id_snuts3)
regions<-gUnaryUnion(buff, id = delegations@data$id_snuts2)
macro<-gUnaryUnion(buff, id = delegations@data$id_snuts1)
country<-gUnaryUnion(buff, id = delegations@data$id_snuts0)
par(mfrow=c(1,5))
plot(delegations)
title(main="Délégations")
plot(gouvernorats)
title(main="Gouvernorats")
plot(regions)
title(main="Regions")
plot(macro)
title(main="Zones")
plot(country)
title(main="Pays")
par(mfrow=c(1,1))


# Verification de la classe des objets créés
class(delegations)
class(gouvernorats)

summary(gouvernorats)

# Associer une table attributaire
gouv_data<-as.data.frame(unique(delegations@data$id_snuts3))
row.names(gouv_data)<-gouv_data[,1]
gouvernorats <-SpatialPolygonsDataFrame(gouvernorats,data=gouv_data,match.ID = TRUE)
colnames(gouvernorats@data)<-"id"
class(gouvernorats)

# Exporter un shapfile
writeSpatialShape(gouvernorats, "outputs/gouvernorats.shp", factor2char = TRUE, max_nchar=254)


# Calcul de distance euclidiennes

dist<-gDistance(centres,byid=TRUE)
row.names(dist)<-delegations@data$id
colnames(dist)<-delegations@data$id

# Conversion matrice ->i,j,var
dist2<-melt(dist,variable.name=1, na.rm=TRUE) 
colnames(dist2)<-c("i","j","dij")

# Conversion i,j,var -> matrice
dist3<-dcast(dist2,i~j,value.var="dij")

#calcul des contiguités
contig<-gIntersects(delegations, byid = TRUE, prepared=TRUE)
row.names(contig)<-delegations@data$id
colnames(contig)<-delegations@data$id
contig2<-melt(contig,variable.name=1,value.name="fij", na.rm=TRUE) 
colnames(contig2)<-c("i","j","cij")

# Distance à la cote (gouvernorats) ------------------------------------------------
centres<-readShapeSpatial("data/TUN/shp/Tunisie_snuts3_centres.shp")
coast<-readShapeSpatial("data/TUN/shp/coast.shp")
plot(centres)
plot(coast,add=TRUE)  
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
plot(centres)
plot(coast,add=TRUE)  
dist<-gDistance(coast,centres,byid=TRUE)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
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

dist



