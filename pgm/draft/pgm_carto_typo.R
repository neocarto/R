# =============================================
# pgm_carto_typo.R
# 
# Objectif: Cartographier une variable qualitative nominale
# auteurs : Claude Grasland & Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================

# ===================================
# 1. Choisir le dossier
# حدد المجلد
# ===================================

#setwd("F:/Sfax2014/R/tunisie")
setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# تحميل حزم مفيدة
# ===================================

library(maptools)
library(RColorBrewer)
library(classInt)

# ===================================
# 3. Import et jointure
# ===================================

#Import des géométries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
#Import des données
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)

#jointure
fdc@data = data.frame(fdc@data, donnees[match(fdc@data[,"id"], donnees[,"del"]),])

# ===================================
# 4. Cartographie
# ===================================

#Choix de la variable à vartographier
fdc@data$var<-fdc@data$reg_nom

# Choix de la palette de couleurs
mycol<-"Set1"

#Creation d'un nouveau facteur avec les couelurs
colours <- brewer.pal(nlevels(as.factor(fdc@data$var)),mycol)
fdc@data$col<-as.factor(fdc@data$var)
levels(fdc@data$col)<-colours

#View(fdc@data)

#attribution des couleurs aux régions 
cols<-as.character(fdc@data$col)

plot(fdc, col=cols)

# ===================================
# 5. Habillage
# ===================================

# ajout d'une couche
reg <- readShapeSpatial("geom/Tunisie_snuts2.shp")
plot(reg,border="#FEE08B",add=TRUE)

#legende
rVal <- as.character(levels(fdc@data$var))
legend("topleft",legend=rVal, fill=colours,bty="n",pt.cex=1,cex=0.7,title="Typologie qualitative")
#Titre
title(main="Types",sub="Auteur: Nicolas LAMBERT, UMS RIATE, 2014",cex.sub=0.7)

#Echelle
xscale<-4100000
yscale<-900000
sizescale<-50000
labelscale<-"50km"
SpatialPolygonsRescale(layout.scale.bar(),offset=c(xscale,yscale),scale=sizescale,fill=c("black"),plot.grid=F)
text(xscale+sizescale/2,yscale,paste(labelscale,"\n\n",sep=""),cex=0.7)
#l <- locator(n=1)   #cliquer dans la fenêtre graphique à l'endroit choisi
#SpatialPolygonsRescale(layout.scale.bar(),offset=c(l$x,l$y),scale=50000,fill=c("black"),plot.grid=F)
#text(l$x+5000/2,l$y,paste("50 km","\n\n",sep=""),cex=0.7)

# Orientation (simple fleche)
xarrow<-4100000
yarrow<-1500000
SpatialPolygonsRescale(layout.north.arrow(2),offset=c(xarrow,yarrow),scale=50000,plot.grid=F)
#l <- locator(n=1)
#SpatialPolygonsRescale(layout.north.arrow(2),offset=c(l$x,l$y),scale=50000,plot.grid=F)



