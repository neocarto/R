# =============================================
# pgm_carto_links_quali.R
# Objectif: Cartographier des types liens 
# ===============================================

#chargement des packages nécessaires
library(maptools)
library(RColorBrewer)
rm(list=ls())

# Choisir le dossier

setwd("/home/nlambert/Documents/R/R_neocarto_github")


#import des données à cartographier
dt <- read.csv( "data/TUN/csv/Tunisie_delegations flux_quali.csv",header=TRUE,sep="\t",dec=",")
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


#Creation d'un nouveau facteur avec les couleurs
colours <- brewer.pal(nlevels(as.factor(dt$var)),"Set1")
dt$col<-as.factor(dt$var)
levels(dt$col)<-colours
colours

#Display
plot(fdc)
cols<-as.character(unique(dt$col))
segments(dt$x1, dt$y1, dt$x2, dt$y2, col=cols, lwd=6)

#legend
rVal <- as.character(unique(dt$var))
legend("topleft",legend=rVal, lwd = 6, col=cols,bty="n",title="Types de flux")

