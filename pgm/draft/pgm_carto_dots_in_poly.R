# =============================================
# pgm_carto_dots_in_poly.R
# 
# Objectif: Dots in polys
# auteurs : Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# ===============================================

# VARIABLES
mygeom<-"../../data/EU/shp/nuts3_2006.shp"
mydata<-"../../data/EU/csv/data_nuts3.csv"
myvar<-"pop_t_2008"
nb<-30  # Un point vaut nb units
#getwd() 
library(maptools)
#setwd("/home/nlambert/Documents/R/carto")
list.files()

fdc <- readShapeSpatial(mygeom)
donnees<-read.csv(mydata ,header=TRUE,sep="\t",dec=",",encoding="latin1",)
colnames(fdc@data)[1]<-"id"
names(donnees)[1]<-"id"
fdc@data <- data.frame(fdc@data, donnees[match(fdc@data[,"id"], donnees[,"id"]),])
fdc@data<-data.frame(fdc@data$id,fdc@data[,myvar])
colnames(fdc@data)<-c("id","var")
fdc@data$var[is.na(fdc@data$var)] <- 0 
fdc@data$nbdots <-as.integer(fdc@data$var/nb)

dots<-dotsInPolys(fdc, as.integer(fdc@data$nbdots),f = "random")
plot(fdc,col="#CCCCCC",border="white")
plot(dots, pch=20, coll="black",add=TRUE,cex=0.01)


