# =============================================
# pgm_discontinuities.R
# 
# Objectif: Faire une carte de discontinuités
# auteur : Nicolas Lambert, CNRS - UMS RIATE
# version : 1.0 (janvier 2014)
#
# ===============================================

library(rgeos)
library(maptools)
library(raster)
library(proj4)
library(reshape2)


# -----------------------------------------------------------
# [STEP 1] IMPORT DU FOND DE CARTE
# (la projection doit être définie)
# -----------------------------------------------------------

rm(list=ls())
setwd("/home/nlambert/Documents/R/R_neocarto_github")
fdc<-readShapeSpatial("data/EU/shp/nuts2_2006.shp", proj4string=CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
colnames(fdc@data)[1]<-"id"

# -----------------------------------------------------------
# [STEP 2] SIMPLIFICATION DU FOND DE CARTE
# (vecter -> raster -> vecter)
# ATTENTION, TEMPS DE CALCUL ASSEZ LONG (plusieurs minutes) !! 
# -----------------------------------------------------------

# VARIABLES
resolutionc<-1500
resolutionl<-1500

# polys -> raster
r <- raster(ncol=resolutionc, nrow=resolutionl)
extent(r) <- extent(fdc)
myr<-rasterize(fdc, r, field=fdc@data$id,fun='first')
#plot(myr)

# raster -> polys
test<-rasterToPolygons(myr, n=4, na.rm=TRUE, digits=1, dissolve=TRUE)
test<- test[order(test@data$layer),]
test@data$id<-fdc@data$id
fdc<-test
fdc<- fdc[order(fdc@data$id),]
fdc@data<-data.frame(fdc@data$id)
colnames(fdc@data)<-"id"        
head(fdc@data)

# Sauvagarde du shp
writeSpatialShape(fdc, "outputs/fdcnew.shp", factor2char = TRUE, max_nchar=254)


# -----------------------------------------------------------
# [STEP 3] EXTRACTION DES FRONTIERES
# (à partir du fdc propre généré ci-dessus)
# -----------------------------------------------------------

# Création d'un fichier de contiguités
contig<-gIntersects(fdc, byid = TRUE, prepared=TRUE)
row.names(contig)<-fdc@data$id
colnames(contig)<-fdc@data$id
contig<-melt(contig,variable.name=1,value.name="fij", na.rm=TRUE) 
colnames(contig)<-c("i","j","cij")
contig<-contig[contig$cij==TRUE,]
contig<-contig[contig$i!=contig$j,]
contig$id<-paste(contig$i,contig$j,sep = "_")
row.names(contig)<-contig$id

# Polys -> lines

bound<-gBoundary(fdc, byid=TRUE, id = fdc@data$id)
attribut<-fdc@data
row.names(attribut)<-attribut$id
bound<-SpatialLinesDataFrame(bound,attribut, match.ID = TRUE)

# Lines -> borders

nb_errors<-0
for(x in 1:nrow(contig))
{
  polyline1<-bound[bound@data$id==contig$i[x],]
  polyline2<-bound[bound@data$id==contig$j[x],]
  tmp<-gIntersection(polyline1, polyline2)
  if (class(tmp)=="SpatialLines")
  {
    tmp <- spChFIDs(tmp, as.character(contig$id[x]))
    if (exists("borders")){borders<-spRbind(borders,tmp)}
    if (!exists("borders")){borders<-tmp}  
  }

  if (class(tmp)=="SpatialPoints")
  {
  nb_errors<-nb_errors+1
  if (exists("errors")){errors<-spRbind(errors,tmp)}
  if (!exists("errors")){errors<-tmp}  
#  plot(tmp,col="red",add=T)
  }
}
coords<-data.frame(coordinates(errors)[,1],coordinates(errors)[,2])
addAttrToGeom(errors, coords, match.ID=F)
errors<-SpatialPointsDataFrame(errors,coords, match.ID = FALSE)

# Recuparation des attributs
borders<-SpatialLinesDataFrame(borders,contig, match.ID = TRUE)
borders@data<-data.frame(borders@data$id,borders@data$i,borders@data$j)
colnames(borders@data)<-c("id","i","j")


# simplification
#borders<-gSimplify(borders, tol=100000, topologyPreserve=TRUE)


# Sauvagarde du shp
writeSpatialShape(borders, "outputs/borders.shp", factor2char = TRUE, max_nchar=254)
if (exists("errors")){writeSpatialShape(errors, "outputs/errors.shp", factor2char = TRUE, max_nchar=254)}



# -----------------------------------------------------------
# [STEP 4] CARTOGRAPHIE
# (Cette partie du programme est independante)
# -----------------------------------------------------------

library(maptools)
library(RColorBrewer)
library(classInt)

rm(list=ls())
setwd("/home/nlambert/Documents/R/R_neocarto_github")

#-------------------------------------------
# CARTOGRAPHIE
#-------------------------------------------

  
MaxSize<-10 # epaisseur de la plus grosse ligne
LineColor<-"red"
seuil<-1.7 # seules discontinuités relaitivse superieures à "seuil" sont retenues

# 1. IMPORTS  

fdc <- readShapeSpatial("data/EU/shp/nuts2_2006.shp")  
names(fdc@data)[1]<-"id"
borders <- readShapeSpatial("outputs/borders.shp")
donnees<-read.csv( "data/EU/csv/N2.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
donnees$var<-donnees$PIBperinh

# 2. CARTE CHOROPLETHE  
head(fdc@data)
fdc@data = data.frame(fdc@data, donnees[match(fdc@data[,"id"], donnees[,"id"]),])
nbclass<-8
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
colours <- brewer.pal(nbclass,"Greens")
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="black",lwd=0.1)
legend(x="topright", legend=leglabs(round(distr,1),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=0.3,cex=0.4,title="GDP/inh")

# 3. DISCONTINUITES
borders@data = data.frame(borders@data, donnees[match(borders@data[,"i"], donnees[,"id"]),])
borders@data = data.frame(borders@data, donnees[match(borders@data[,"j"], donnees[,"id"]),])
borders@data$disc<-pmax(borders@data$var.1/borders@data$var,borders@data$var/borders@data$var.1)
borders@data<-data.frame(borders@data$id,borders@data$disc)
names(borders)<-c("id","disc")
borders<- borders[order(borders@data$disc,decreasing=TRUE),]
borders@data$size<-(borders@data$disc/max(na.omit((borders@data$disc))))*MaxSize 
borders<- borders[borders@data$disc>seuil,]
plot(borders,col=LineColor, lwd=borders@data$size ,add=T)

rLeg <- quantile(borders@data$size,c(0,0.5,0.80,1),type=1)
rVal <- round(quantile(borders@data$disc,c(0,0.5,0.80,1),type=1),1)

legend("bottomright",legend=rVal, lwd = rLeg, col="red",bty="n",title="Discontinuités",cex=0.4,pt.cex=1)

title(main="Discontinuités de PIB/hab en 2007",sub="Auteur: Nicolas LAMBERT, UMS RIATE, 2014",cex.sub=0.6)
