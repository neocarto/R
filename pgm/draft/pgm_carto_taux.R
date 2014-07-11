# =============================================
# pgm_carto_taux.R
# Nicolas LAMBERT, 2014
#
# ===============================================


setwd("/home/nlambert/Documents/R/R_neocarto_github")
library(maptools)
library(RColorBrewer)
library(classInt)

#output
#svg(filename="resul/map.svg", width=29,height=21,pointsize=40)

#Import des géométries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")

#Import des données
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)


#jointure
fdc@data = data.frame(fdc@data, donnees[match(fdc@data[,"id"], donnees[,"del"]),])
head(fdc@data)

# ===================================
# 4. Cartographie
# ===================================

#Choix de la variable à cartographier

fdc@data$var <- fdc@data$IDRVA2011
head(fdc@data)
#head(fdc@data[,c("POPTO2010","SUP2010","var")])
#fdc@data$var[is.infinite(fdc@data$var)]<-NA
var <- as.vector(na.omit(fdc@data$var))
var_title<-"Indicateur de developpement regional"
var_title_leg<-"indice 0-1"

# Nombre de classes
nbclass<-8

#determination des bornes (manuelement ou automatiquement)
#distr<-c(100,200,500,1000,2000) 
distr <- classIntervals(var,nbclass,style="quantile")$brks
#?classIntervals

# Choix de la palette de couleurs
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)

colours <- brewer.pal(nbclass,"YlOrRd")
colours
brewer.pal.info
#attribution des couleurs aux régions 
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
colMap

#Affichage de la carte
plot(fdc, col=colMap,border="black",lwd=1)


# ===================================
# 5. Habillage
# ===================================

#legende
legend(x="topright", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title=var_title_leg)

#Titre
title(main=var_title,sub="Auteur: Nicolas LAMBERT, UMS RIATE, 2014",cex.sub=0.7)


#ajout d'une couche
sebkhas <- readShapeSpatial("geom/sebkhas.shp")
plot(sebkhas,col="blue",border="#FEE08B",add=TRUE)

# Ajout des gouvernorats avec Labels
gouv <- readShapeSpatial("geom/Tunisie_snuts3.shp")
plot(gouv,border="#000000",lwd=3,add=TRUE)

dots <- readShapeSpatial("geom/Tunisie_snuts3_centres.shp")
pt <- cbind(dots@data[,"id"],as.data.frame(dots@coords))
head(fdc@data)
colnames(pt) <- c("id","x","y")
pt = data.frame(pt, donnees[match(pt[,"id"], donnees[,"gou"]),])
head(pt)
text(pt$x, pt$y , labels = pt$gou_nom,cex=0.6,col="red")

#la fonction bbox donne les coordonnées max et min du fond de carte
x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]


#Echelle
xscale<-x2
yscale<-y1
sizescale<-50000
labelscale<-"50km"
SpatialPolygonsRescale(layout.scale.bar(),offset=c(xscale,yscale),scale=sizescale,fill=c("black"),plot.grid=F)
text(xscale+sizescale/2,yscale,paste(labelscale,"\n\n",sep=""),cex=0.7)
#l <- locator(n=1)   #cliquer dans la fenêtre graphique à l'endroit choisi
#SpatialPolygonsRescale(layout.scale.bar(),offset=c(l$x,l$y),scale=50000,fill=c("black"),plot.grid=F)
#text(l$x+5000/2,l$y,paste("50 km","\n\n",sep=""),cex=0.7)

# Orientation (simple fleche)
xarrow<-x1
yarrow<-y2-(y2-y1)/10
SpatialPolygonsRescale(layout.north.arrow(2),offset=c(xarrow,yarrow),scale=50000,plot.grid=F)
#l <- locator(n=1)
#SpatialPolygonsRescale(layout.north.arrow(2),offset=c(l$x,l$y),scale=50000,plot.grid=F)


#dev.off()

