# =============================================
# pgm_stat_regression.R
#
# Objectif: Calcul d'une regression Y = aX+b
# et cartographie des residus
# auteurs : Claude Grasland & Nicolas Lambert
# version : 1.0 (janvier 2014)
#
# (c) Projet CMCU XXX
#  SYFACTE - IDEES - RIATE - GEOGRAPHIE-CITES
#
# ===============================================

# ===================================
# 0. Packages utiles
# ===================================

library(maptools)
library(RColorBrewer)
library(classInt)


# ===================================
# 1. Choisir le dossier
# ===================================

#setwd("C:/Sfax2014/R/matunisie")

setwd("/home/nlambert/Documents/R/R_neocarto_github")
list.files()

# ===================================
# 2. Importer les données statistiques
# ===================================

# examen du dossier data
list.files("data")

# imporation du fichier
donnees<-read.table("outputs/dist_to_coast_delegations.csv",header = TRUE, sep = ";",dec = ",")

# visualisation du début du fichier
head(donnees)

# visualisation de la fin du fichier
tail(donnees)

# visualisation de tout le fichier
#donnees

# ======================================
# 3. Définition des variables à analyser
# ======================================

# 3.1 identifiant des variables (id)
code<-donnees$id

# 3.2 nom des variables (nom)
nom<-donnees$nom

# 3.3 Variable quantitative à expliquer (Y)
Y<-donnees$idr
nomY<-"idr"

# 3.4 Variable quantitative explicative (X)
#X<-donnees$dist
X<-log(donnees$dist)
nomX<-"Distance à la côte (log)"

# 3.5 tableau à étudier
tab<-data.frame (code,nom,Y,X)

# visualisation du début et de la fin
head(tab,3)
tail(tab,3)


# ==============================================================
# 4. Analyse statistique des distributions de X et Y (FIGURE 1)
# ==============================================================

# 4.1 Paramètres statistiques principaux de X et Y

summary(X)
summary(Y)

mean(X)
mean(Y)
sd(X)
sd(Y)

# 4.2 Creation de variables X et Y standardisées par moyenne et écart-type
tab$Ystand<-(Y-mean(Y))/sd(Y)
tab$Xstand<-(X-mean(X))/sd(X)

#Arrond
tab$Ystand<-round(tab$Ystand,2)
tab$Xstand<-round(tab$Xstand,2)
head(tab)


# 4.3 Visualisation statistique de X et Y
plot.new()
par(mfrow=c(2,2))

hist(X,main=nomX,breaks=10)
hist(Y,main=nomY,breaks=10)
boxplot(X,main=nomX, horizontal=TRUE)
boxplot(Y,main=nomY, horizontal=TRUE)

# SAUVEGARDER LA FIGURE 1
dev.copy(png,'outputs/fig1_bivarie.png')
dev.off()

# ===================================================================
# 5. Analyse cartographique des distributions de X et Y (FIGURE2)
# ===================================================================

par(mfrow=c(1,2))

# 5.1 importation et jointure du fichier des geometries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
codecarto<-names(fdc@data)[1]
fdc@data = data.frame(fdc@data, tab[match(fdc@data[,codecarto], tab[,"code"]),])
head(fdc@data)
nbclas=4

# 5.2 Cartographie rapide de X (sans habillage)
distr <- classIntervals(fdc@data$X,nbclas,style="quantile")$brks
colours <- brewer.pal(nbclas,"Reds")
colMap <- colours[(findInterval(fdc@data$X,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="#000000",lwd=0.2)
legend(x="bottomleft", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title="quantiles")
title(main=nomX,sub="Auteur: Claude GRASLAND, Géographie-cités 2014",cex.sub=0.7)

# 5.3 Cartographie rapide de Y (sans habillage)
distr <- classIntervals(fdc@data$Y,nbclas,style="quantile")$brks
colours <- brewer.pal(nbclas,"Blues")
colMap <- colours[(findInterval(fdc@data$Y,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="#000000",lwd=0.2)
legend(x="bottomleft", legend=leglabs(round(distr,2),over="plus de",under="moins de"), fill=colours, bty="n",pt.cex=1,cex=0.7,title="quantiles")
title(main=nomY,sub="Auteur: Claude GRASLAND, Géographie-cités 2014",cex.sub=0.7)

# SAUVEGARDER LA FIGURE 2
dev.copy(png,'outputs/fig2_bivarie.png')
dev.off()


# ===================================================================
# 6. Visualisation de la relation entre X et Y
# ===================================================================

# 6.1 Visualisation rapide
par(mfrow=c(1,1))
plot(X,Y)


# 6.2 Visualisation améliorée 
par(mfrow=c(1,1))
plot(X,Y,
     main="Relation entre X et Y",
     xlab=nomX, # titre horizontal
     ylab=nomY, # titre vertical
     type="p",  # type de point
     pch=20,    # taille des points
     cex=0.7)   # coefficient multiplicateur de taille
     
text(X,Y,
     labels=nom,    # nom à ajouter sur le graphique
     adj=c(0.5,-1), # position par rapport au point
     cex=0.7)       # coefficient multiplicateur de taille


# ===================================================================
# 7. Régression de Y en fonction de X et carte des résidus
# ===================================================================


# 7.1 Calcul du coefficient de corrélation et test de significativité
cor(X,Y)
cor.test(X,Y) # une relatio est signification si p-value < 0.05 (5% d'erreur)

#cor(X,Y,method="spearman")
#cor.test(X,Y,method="spearman")

# 7.2 Paramètres de la droite de régression linéaire Y=aX+b
MonModele <- lm(Y~X)
summary(MonModele)
names(MonModele)

# 7.3 Valeurs estimées et résidus 
tab$Yest<-MonModele$fitted.values
tab$Yres<-MonModele$residuals
tab$Yres_std<-tab$Yres/(sd(tab$Yres))
head(tab)


#===================================================================
# 8. Droite de régression et carte des résidus
# ===================================================================

par(mfrow=c(1,2))

# 8.1 Graphique de régression
plot(X,Y,
     main="Régression",
     xlab=nomX, # titre horizontal
     ylab=nomY, # titre vertical
     type="p",  # type de point
     pch=20,    # taille des points
     cex=0.7)   # coefficient multiplicateur de taille
#text(X,Y,
#     labels=nom,    # nom à ajouter sur le graphique
#     adj=c(0.5,-1), # position par rapport au point
#     cex=0.4)       # coefficient multiplicateur de taille

abline(MonModele,
       col="red")
 
       

# 8.2 importation et jointure du fichier des geometries
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
codecarto<-names(fdc@data)[1]
fdc@data = data.frame(fdc@data, tab[match(fdc@data[,codecarto], tab[,"code"]),])
head(fdc@data)
#distr <- classIntervals(fdc@data$Yres_std,5,style="quantile")$brks
distr<-c(-1000,-2,-1,-0.5,0,0.5,1,2,1000)
colours <- brewer.pal(8,"RdBu")
?brewer.pal
colMap <- colours[(findInterval(fdc@data$Yres_std,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="#000000",lwd=0.2)
legend(x="bottomleft", legend=leglabs(round(distr,2),over="sup. ",under="inf. "), fill=colours, bty="n",pt.cex=1,cex=0.7,title="residus standardisés")
title(main="Residus standardisés",sub="Auteur: Claude GRASLAND, Géographie-cités 2014",cex.sub=0.7)



#===================================================================
# 9. Exportation des résultats de la régression
# ===================================================================


# 9.1 exportation au format .csv (pour Excel)
write.table(tab, "outputs/maregression.csv", sep = ";",dec = ",",row.names=FALSE)

# 9.2 exportation au format .shp (pour SIG)

