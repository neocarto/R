# =============================================
# pgm_anspa_access_tunisie.R
# 
# Objectif: Créer un fichier de distance entre lieux munis d'attributs
# auteurs : Claude Grasland & Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================


# ===================================
# 1. Choisir le dossier
# ===================================

#setwd("F:/Sfax2014/R/tunisie")
setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# ===================================

library(maptools)
library(RColorBrewer)
library(reshape2)
library(classInt)

# ===================================
# 3. Définition du fichier des masses 
# ===================================

# choix du fichier
list.files("data")
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",)
head(donnees)

# selection des variables 
code<-donnees$del
nom<-donnees$del_nom
M<-donnees$POPTO2010
vil<-data.frame(code,nom,M)
head(vil)


# =======================================
# 4. Défnition du fichier des distances
# =======================================

# On récupère le fichier créé par le programme anspa_distance
list.files("resul")
tab_ij<-read.csv( "outputs/tabdist.csv",header=TRUE,sep=";",dec=",",)
head(tab_ij)

# collage des attributs destination (j)
tab_j<-data.frame(vil$code,vil$M)
names(tab_j)<-c("j","Mj")
tab_ij<-merge(tab_ij,tab_j,by="j",all.X=TRUE,all.Y=FALSE)

# collage des attributs origine (i)
tab_i<-data.frame(vil$code,vil$M)
names(tab_i)<-c("i","Mi")
tab_ij<-merge(tab_ij,tab_i,by="i",all.X=TRUE,all.Y=FALSE)


# tableau des couples avec masses et distances
vilvil<-tab_ij
head(vilvil)

# =============================================================
# 5. Exemple d'analyse d'une localisation isolée
# =============================================================

# choix des couples de lieux
vilvil2<-vilvil[vilvil$i!=vilvil$j,]

# Choix d'une unité spatiale
head(vil)
select="TS1110"
#select="TS3136"
city<-vilvil[vilvil$i==select,c("i","j","Dij","Mj")]
head(city)


# Fonction cumulative
city$Fj<-100*city$Mj/sum(city$Mj)
city<-city[order(city$Dij),]
n<-dim(city)[1]
city$Fcumj<-city$Fj[1]
city$Mcumj<-city$Mj[1]
for (k in 2:n)
{
  city$Fcumj[k]<-city$Fcumj[k-1]+city$Fj[k]
  city$Mcumj[k]<-city$Mcumj[k-1]+city$Mj[k]
}
head(city)
tail(city)

# visualisation des fréquences cumulées
plot(city$Dij,city$Fcumj,type="l",col="red",main=select)

# visualisation des masses cumulées
plot(city$Dij,city$Mcumj,type="l",col="red",main=select)

# Sauvegarde
# write.table(city,"resul/acc_Carthage.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)


# ===============================================
# 6 . Calcul d'accessibilite moyenne non pondérée
# ===============================================

# choix des couples de lieux
vilvil2<-vilvil[vilvil$i!=vilvil$j,]

# calcul Accessibilité moyenne
X<-tapply(vilvil2$Dij,vilvil2$i,FUN=mean)

# collage du résultat sur le tableau vil
res<-data.frame(row.names(X),X)
names(res)<-c("code","Access")
head(res)
tabres<-merge(vil,res,by="code")
tabres$IndShimble<-sum(tabres$Access)/tabres$Access
# tri du tableau et affichage des résultats
tabres<-tabres[order(tabres$Access),]
head(tabres)
tail(tabres)

# Sauvegarde tableau
write.table(tabres,"output/asccess_simple.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

# ----------- Carto résultat de la variable Access ------------------------------------------------------
#Import du fond de carte
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
# Joiunture
fdc@data = data.frame(fdc@data, tabres[match(fdc@data[,"id"], tabres[,"code"]),])
# Choix de la variable
fdc@data$var <- fdc@data$Access
# Nb classes
nbclass<-8
# Choix des couleurs
colours <- brewer.pal(nbclass,"YlOrRd")
# Discretisation
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
#attribution des couleurs aux régions 
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
# Cartographie
plot(fdc, col=colMap,border="black",lwd=1)
#Habillage
legend(x="topright", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title="")
title(main="Accessibility",cex.sub=0.7)


# ============================================
# 6 . Calcul d'accessibilite moyenne pondérée
# ============================================


# choix des couples de lieux
vilvil2<-vilvil[vilvil$i!=vilvil$j,]

# calcul de l'accessibilité pondérée
vilvil2$MD<-vilvil2$Dij*vilvil2$Mj
MD<-tapply(vilvil2$MD,vilvil2$i,FUN=sum)
res<-data.frame(row.names(MD),MD)
names(res)<-c("code","MD")
tabres<-merge(vil,res,by="code")
tabres$Access<-round(tabres$MD/sum(tabres$M),2)
tabres$IndShimble<-sum(tabres$Access)/tabres$Access
head(tabres)

tabres<-tabres[order(tabres$Access),]

head(tabres)
tail(tabres)

# Sauvegarde
#write.table(tabres,"outputs/access_pond.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)



# ======================================================================
# 8. Calcul de potentiel simple (masse localisée à moins d'une distance)
# ======================================================================

# choix du seuil de distance
seuil<-60


# Elimination des distances supérieures au seuil
vilvil2<-vilvil[vilvil$Dij<seuil,]

# calcul du potentiel
POT<-tapply(vilvil2$Mj,vilvil2$i,FUN=sum)
res<-data.frame(row.names(POT),POT)
names(res)<-c("code","POT")
tabres<-merge(vil,res,by="code")
head(tabres)

# tri et affichage du potentiel
tabres<-tabres[order(tabres$POT),]
head(tabres)
tail(tabres)

# Sauvegarde
#write.table(tabres,"outputs/access_pot_100km.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)


# --------------- cartographie ---------------------------------------------
plot.new()
par(mfrow=c(1,2))
# (1) Carte des masses ------
#Import du fond de carte et extraction des centroides
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
pt <- cbind(fdc@data[,"id"],as.data.frame(coordinates(fdc)))
colnames(pt) <- c("id","x","y")
# Jointure
pt = data.frame(pt, donnees[match(pt[,"id"], donnees[,"del"]),])
# Choix de la variable à cartographier
pt$var<-pt$POPTO2010
#Calcul de la taille des cercles
x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]
sfdc <- (x2-x1)*(y2-y1)
sc <- sum(pt$var,na.rm=TRUE)
k<-0.2 #la surface des cercles corrspon à 20% de la surface de fdc
pt$size <- sqrt((pt$var*k*sfdc/sc)/pi) 
# Cartographie
mycol<-"red"
pt <- pt[order(pt$size,decreasing=TRUE),]
plot(fdc, border="white", col="grey")
symbols(pt[,c("x","y")],circles=pt$size,add=TRUE,bg=mycol,inches=FALSE)
#Habillage
title(main="Population en 2010")
LegTitle<-"nb habs\n"
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


# (2) Carte des potentiels
#Import du fond de carte
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
# Jointure
fdc@data = data.frame(fdc@data, tabres[match(fdc@data[,"id"], tabres[,"code"]),])
# Choix de la variable
fdc@data$var <- fdc@data$POT
# Nb classes
nbclass<-8
# Choix des couleurs
colours <- brewer.pal(nbclass,"YlOrRd")
# Discretisation
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
#attribution des couleurs aux régions 
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
# Cartographie
plot(fdc, col=colMap,border="black",lwd=1)
#Habillage
title(main="Accessibility")
legend(x="bottomleft", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.4,title="")
par(mfrow=c(1,1))



