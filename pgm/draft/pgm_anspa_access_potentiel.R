# =============================================
# pgm_anspa_access_potentiel.R
# 
# Objectif: Créer un fichier de potentiel (masse à moins d'une distance)
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
#setwd("/home/nlambert/Documents/Travaux/Sfax2014/R/tunisie")
setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()


# ===================================
# 2. Charger les  packages utiles
# ===================================

library(maptools)
library(RColorBrewer)
library(reshape)
library(classInt)

# ======================================
# 3. Chargement du fichier des distances 
# ======================================

# 3.1 Fichier de distance créé par  pgm_anspa_access_tabdist
list.files("resul")
vilvil<-read.csv( "output/tab_dist.csv",header=TRUE,sep=";",dec=",",)
head(vilvil)

# ======================================
# 4. Chargement du fichier des masses 
# ======================================

# 4.1 choix du fichier
donnees<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",)
head(donnees)

# 4.2 choix du code du nom et de la masse
code_j<-donnees$del
nom_j<-donnees$del_nom

Mj<-donnees$POPTO2010
#Mj<-donnees$SUP2010



# 4.3 collage de la masse sur le fichier des distances
tab<-data.frame(code_j,Mj)
vilvil<-merge(vilvil,tab,by="code_j",all.X=TRUE,all.Y=FALSE)
head(vilvil)

# 4.4 Préparation du tableau de resultat
vil<-data.frame(code_j,nom_j,Mj)
names(vil)<-c("code","nom","M")
head(vil)



# ======================================================================
# 5. Calcul de potentiel simple (masse localisée à moins d'une distance)
# ======================================================================


# 8.1 choix du seuil de distance
seuil<-40
vilvil2<-vilvil[vilvil$Dij<seuil,]



# calcul du potentiel
POT<-tapply(vilvil2$Mj,vilvil2$code_i,FUN=sum)
res<-data.frame(row.names(POT),POT)
names(res)<-c("code","POT")
tabres<-merge(vil,res,by="code")
head(tabres)

# tri et affichage du potentiel
tabres<-tabres[order(tabres$POT),]
head(tabres)
tail(tabres)

# Sauvegarde
#write.table(tabres,"output/access_pot_100km.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

# ======================================================================
# 5. Cartographie
# ======================================================================

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
# Ajout des gouvernorats
gouv <- readShapeSpatial("data/TUN/shp/Tunisie_snuts3.shp")
plot(gouv, border="black", lwd=1.5,add=TRUE)
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
nbclass<-10
# Choix des couleurs
colours <- brewer.pal(nbclass,"RdBu")
# Discretisation
distr <- classIntervals(fdc@data$var,nbclass,style="quantile")$brks
#attribution des couleurs aux régions 
colMap <- colours[(findInterval(fdc$var,distr,all.inside=TRUE))]
# Cartographie
plot(fdc, col=colMap,border="black",lwd=1)
# Ajout des gouvernorats
gouv <- readShapeSpatial("data/TUN/shp/Tunisie_snuts3.shp")
plot(gouv, border="black", lwd=1.5,add=TRUE)
#Habillage
title(main="Potentiel à moins de 40 km")
legend(x="bottomleft", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.4,title="")
par(mfrow=c(1,1))



# ==========================
# Sauvegarde résultat
# ==========================

write.table(tabres,"outputs/access_potentiel_Population_40km.csv",dec=",",sep=";",row.names=FALSE,quote=FALSE)

