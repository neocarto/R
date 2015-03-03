# FONCTION 1 : CREATION DE DEGRADES
getpal<-function(pal1,n1,pal2=NULL,n2=NULL,middle=F,alphaeffect=F){

# PARAMETRES
alphainit<-30
alpha="FF"
middlecol<-"#F6F6F6"

# CREATION DE LA PALETTE

# 1.Simple gradation ----------------------------

if(is.null(pal2) & is.null(n2)){
  pal<-as.character(unlist(colors[[pal1]][n1]))
  if(alphaeffect==T){
  for ( i in 1:n1-1) {
  alpha<-as.hexmode(floor(alphainit+(255-alphainit)/n1*i))
  pal[i]<-paste(pal[i],alpha,sep="")
  }
  alpha<-as.hexmode(alphainit)
  }
}

# 2. Double gradation ------------------------

if(!is.null(pal2) & !is.null(n2)){
  n<-max(n1,n2)
  pal1<-as.character(unlist(colors[[pal1]][n]))
  pal2<-as.character(unlist(colors[[pal2]][n]))
  
  if(alphaeffect==T){
  for ( i in 1:n-1) {
  alpha<-as.hexmode(floor(alphainit+(255-alphainit)/n*i))
  pal1[i]<-paste(pal1[i],alpha,sep="")
  pal2[i]<-paste(pal2[i],alpha,sep="")
  }
  alpha<-as.hexmode(alphainit)
  }
  
  pal1 <-pal1[1:n1]
  pal1<-rev(pal1)
  pal2 <-pal2[1:n2]
  
  pal<-c(pal1,pal2)
  if(middle==T){pal<-c(pal1,paste(middlecol,alpha,sep=""),pal2)}
}

return(pal)

}

# FONCTION 2 : RECUPERATION ET AFFICHAGE DES DEGRADES
displaypal<-function(mypal)
{
  k<-length(mypal)
  image(1:k, 1, as.matrix(1:k), col =mypal, xlab = paste(k," classes",sep=""), ylab = "", xaxt = "n", yaxt = "n",bty = "n")
}


