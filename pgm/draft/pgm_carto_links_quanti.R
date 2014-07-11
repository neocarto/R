# =============================================
# pgm_carto_links_quali.R
# Objectif: Cartographier des liens quanti absolus 
# ===============================================
#chargement des packages nécessaires
library(maptools)
rm(list=ls())

# Choisir le dossier

setwd("/home/nlambert/Documents/R/R_neocarto_github")


#variables
maxsize<-10 #epaisseur du plus gros lien

#import des données à cartographier
dt <- read.csv( "data/TUN/csv/Tunisie_delegations flux.csv",header=TRUE,sep="\t",dec=",")
dt <- dt[order(dt$Var,decreasing=TRUE),]
head(dt)

#import du fond de carte
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")


#Calcul des centroides
centres <- cbind(fdc@data[,"id"],as.data.frame(coordinates(fdc)))
colnames(centres) <- c("Code","x","y")
head(centres)

#jointure entre le dataframe des coordonnées des centroides et les données à cartographier

dt = data.frame(dt, centres[match(dt[,"ID1"], centres[,"Code"]),2:3])
dt = data.frame(dt, centres[match(dt[,"ID2"], centres[,"Code"]),2:3])
colnames(dt) <- c("id1","id2","var","x1","y1","x2","y2")
head(dt)

#Taille des flux
dt$size<-(dt$var/max(dt$var))*maxsize

head(dt)

#Display
plot(fdc)
#lines(dt[,(c(dt$x1, dt$y1), c(dt$x2, dt$y2)], lwd = 2)

segments(dt$x1, dt$y1, dt$x2, dt$y2, col="red", lwd=dt$size)
#arrows(dt$x1, dt$y1, dt$x2, dt$y2, col="red", lwd=dt$size, length=0.2)

#symbols(pt[,c("x","y")],circles=pt$var,add=TRUE,bg="#C7E9C0",inches=FALSE)

#legend
rLeg <- quantile(dt$size,c(0,0.5,0.80,1),type=1)
rVal <- quantile(dt$var,c(0,0.5,0.80,1),type=1)
legend("topleft",legend=rVal, lwd = rLeg, col="red",bty="n",title="Taille des flux")


