# =============================================
# pgm_MTA.R
# 
# Objectif: Cartographie multiscalaire
# auteurs : Nicolas Lambert,UMS RIATE - CNRS
# version : 1.0 (janvier 2014)
#
# ===============================================

library(maptools)
library(rgeos)
library(reshape2)

# ===================================
# 1. Imports et jointure
# ===================================

setwd("/home/nlambert/Documents/R/R_neocarto_github")
fdc <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
donnees<-read.csv( "data/TUN/csv/data_del_2004.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)

names(fdc@data)[1]<-"id"
names(donnees)[1]<-"id"
fdc@data = data.frame(fdc@data, donnees[match(fdc@data[,"id"], donnees[,"id"]),])

# ===================================
# 2. Variables
# ===================================

# Indicateurs & labels
stock1<-fdc@data$pop_t
stock2<-fdc@data$area
ratio<-stock1/stock2
#stock1_label<-"Population totale, 2004 (unit)"
#stock2_label<-"Superficie (unit)"
ratio_label<-"Densité de population, 2004"

# Deviation globale (valeur)
global=sum(stock1)/sum(stock2)

# Deviation intermediaire (champ aggregatif)
medium<-fdc@data$id_snuts2

# Deviation locale (0 = contiguite, sinon distance euclidienne dans l'unité de la carte)
mydist<-3000

# Critere et seuil
critere<-"sup" # "sup" = plus grand que, "inf" = plus petit que 
seuil <-100

# ===================================
# 3. Calculs
# ===================================

# On ne garde que ce qui est utile
fdc@data<-data.frame(fdc@data$id,medium,stock1,stock2,ratio)
colnames(fdc@data)<-c("id","id2","stock1","stock2","ratio")

# 3.1 Calcul deviation global
fdc@data$global<-(fdc@data$ratio/global)*100

# 3.2 Calcul deviation intermédiare
stock1_med<-aggregate(fdc@data$stock1, by = list(id2 = fdc@data$id2), sum, simplify=TRUE)
stock2_med<-aggregate(fdc@data$stock2, by = list(id2 = fdc@data$id2), sum, simplify=TRUE)
med<-data.frame(c(stock1_med,stock2_med))
med$tmp<-(med$x/med$x.1)
med<-data.frame(med$id2,med$tmp)
colnames(med)<-c("id2","medium")
fdc@data = data.frame(fdc@data, med[match(fdc@data[,"id2"], med[,"id2"]),2])
colnames(fdc@data)<-c("id","id2","stock1","stock2","ratio","global","medium")
fdc@data$medium<-(fdc@data$ratio/fdc@data$medium)*100

# 3.3 Calcul déviation locale
if(mydist==0) {  

dist<-gIntersects(fdc, byid = TRUE, prepared=TRUE)
row.names(dist)<-fdc@data$id
colnames(dist)<-fdc@data$id
dist<-melt(dist,variable.name=1,value.name="fij", na.rm=TRUE) 
colnames(dist)<-c("i","j","cij")
dist<-dist[dist$cij==TRUE,]

} else {

centres<-gCentroid(fdc, byid=TRUE, id = NULL)
dist<-gWithinDistance(centres,byid=TRUE,dist=mydist)
row.names(dist)<-fdc@data$id
colnames(dist)<-fdc@data$id
dist<-melt(dist,variable.name=1, na.rm=TRUE) 
colnames(dist)<-c("i","j","cij")
dist<-dist[dist$cij==TRUE,]
}

dist = data.frame(dist, fdc@data[match(dist[,"j"], fdc@data[,"id"]),c("stock1","stock2")])
local_stock1<-aggregate(dist$stock1, by = list(i = dist$i), sum, simplify = TRUE)
local_stock2<-aggregate(dist$stock2, by = list(i = dist$i), sum, simplify = TRUE)
local<-data.frame(local_stock1,local_stock2$x)
colnames(local)<-c("id","stock1","stock2")
local$ratio<-local$stock1/local$stock2
local<-data.frame(local$id,local$ratio)
colnames(local)<-c("id","local")

fdc@data = data.frame(fdc@data, local[match(fdc@data[,"id"], local[,"id"]),2])
colnames(fdc@data)<-c("id","id2","stock1","stock2","ratio","global","medium","local")
fdc@data$local<-(fdc@data$ratio/fdc@data$local)*100

  
# ===================================
# 4. Typologie multiscalaire
# ===================================

# Types
fdc@data$tmp1<-0
fdc@data$tmp2<-0
fdc@data$tmp3<-0
fdc@data$typo<-0

if (critere=="sup")
{
  fdc@data$tmp1[fdc@data$global>=seuil]<-1
  fdc@data$tmp2[fdc@data$medium>=seuil]<-2
  fdc@data$tmp3[fdc@data$local>=seuil]<-4
}

if (critere=="inf")
{
  fdc@data$tmp1[fdc@data$global<=seuil]<-1
  fdc@data$tmp2[fdc@data$medium<=seuil]<-2
  fdc@data$tmp3[fdc@data$local<=seuil]<-4
}

fdc@data$typo<-fdc@data$tmp1+fdc@data$tmp2+fdc@data$tmp3
fdc@data$typo<-as.factor(fdc@data$typo)

# couleurs
colours<-c("white","#fdc785","#ffffab","#fba9b0","#addea6","#ffa100","#fff226","#e30020")
fdc@data$col<-colours[1]
fdc@data$col[fdc@data$typo==1]<-colours[2]
fdc@data$col[fdc@data$typo==2]<-colours[3]
fdc@data$col[fdc@data$typo==3]<-colours[4]
fdc@data$col[fdc@data$typo==4]<-colours[5]
fdc@data$col[fdc@data$typo==5]<-colours[6]
fdc@data$col[fdc@data$typo==6]<-colours[7]
fdc@data$col[fdc@data$typo==7]<-colours[8]

# ===================================
# 5. Cartographie
# ===================================

cols<-as.character(fdc@data$col)
plot(fdc, col=cols)

rVal<-c(" .   .   . ","[X]  .   . "," .  [X]  . ","[X] [X]  . "," .   .  [X]","[X]  .  [X]"," .  [X] [X]","[X] [X] [X]")
legend("bottomleft",legend=rVal, fill=colours,bty="n",pt.cex=1,cex=0.7,title=paste(paste(critere, seuil),"\n[glo] [med] [loc]"))

if(mydist==0){soustitre<-"NB : déviation locale basée sur la contiguité"}
if(mydist>0){soustitre<-paste("NB : déviation locale basée sur la distance : ",mydist)}
title(main=paste("Typologie multiscalaire\n",ratio_label),cex.sub=0.7,sub=soustitre)


