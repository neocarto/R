# =============================================
# pgm_carto_stock.R
# 
# Objectif: Cartographier une variable quantitative absolue (stock)
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

setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# تحميل حزم مفيدة
# ===================================

library(maptools)
library(RColorBrewer)


# ===================================
# 3. Import et jointure
# ===================================

#Import des géométries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
#class(fdc)
#View(fdc@data)
#head(fdc@data)

#Import des données
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)

#création d'un dataframe avec les coordonnées des centroides des communes
pt <- cbind(fdc@data[,"id"],as.data.frame(coordinates(fdc)))

#renommage des colonnes de ce dataframe
colnames(pt) <- c("id","x","y")
head(pt)

#jointure entre le dataframe des coordonnées des centroides et les données à cartographier
pt = data.frame(pt, donnees[match(pt[,"id"], donnees[,"del"]),])
head(pt)

# ===================================
# 4. Cartographie
# ===================================

# Choix de la variable à cartographier
pt$var<-pt$POPTO2004

#On détermine extension maximale du fond de carte
#la fonction bbox donne les coordonnées max et min du fond de carte
x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]

#surface maximale de la carte
sfdc <- (x2-x1)*(y2-y1)

#somme de la variable à cartographier
sc <- sum(pt$var,na.rm=TRUE)

# Calcul d'une variable taille des cercles
k<-0.2 #la surface des cercles corrspon à 20% de la surface de fdc
pt$size <- sqrt((pt$var*k*sfdc/sc)/pi) #la somme des surfaces des cercles 


# Une variable pour la couleur
mycol<-"red"

# Tri des valeurs pour afficher les petits xcercles en haut
pt <- pt[order(pt$size,decreasing=TRUE),]
head(pt)
# Affichage de la carte
plot(fdc, border="white", col="grey")
symbols(pt[,c("x","y")],circles=pt$size,add=TRUE,bg=mycol,inches=FALSE)


# ===================================
# 5. Habillage
# ===================================


#legende
LegTitle<-"Nombre \nd'habitants\n"
rLeg <- quantile(pt$size,c(1,0.9,0.25,0),type=1,na.rm = TRUE)
rVal <- quantile(pt$var,c(1,0.9,0.25,0),type=1,na.rm = TRUE)
l <- NULL
l$x <- x1
l$y <- y1
xinit <- l$x+rLeg[1]
ypos <- l$y+rLeg
symbols(x=rep(xinit,4),y=ypos,circles=rLeg,add=TRUE,bg=mycol,inches=FALSE)
text(x=rep(xinit,4)+rLeg[1]*1.2,y=(l$y+(2*rLeg)),rVal,cex=0.3,srt=0,adj=0)
for (i in 1:4){  segments (xinit,(l$y+(2*rLeg[i])),xinit+rLeg[1]*1.1,(l$y+(2*rLeg[i])))}
text(x=xinit-rLeg[1],y=(l$y+(2*rLeg[1])),LegTitle,adj=c(0,0),cex=0.6)

#Titre
title(main="Population, 2010",sub="Auteur: Nicolas LAMBERT, UMS RIATE, 2014",cex.sub=0.7)

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



