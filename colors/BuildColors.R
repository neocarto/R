# --------------------------------------
# Programme pour fabriquer des palettes
# --------------------------------------

# colmin1 : couleur claire de la palette en 2 classes
# colmax1 : couleur foncée de la palette en 2 classes
# colmin2 : couleur claire de la palette en N classes
# colmax2 : couleur foncée de la palette en N classes
# colref : couleur de réference influanceant le dégradé
# N=20 : Nombre de classes max pour la palette


buildpal<-function(colmin1,colmax1,colmin2,colmax2,colref,N=20){
  # On défnie les deux dégradés utils
  degrade_min<-colorRampPalette(c(colmin1,colmin2))
  degrade_max<-colorRampPalette(c(colmax1,colmax2))
  mypal<-list(colref)
  for ( i in 2:N) {
    cols<-colorRampPalette(c(degrade_min(N)[i],colref),bias = 0.8)
    tmp<-cols(3)[2]
    #cols<-colorRampPalette(c(degrade_min(N)[i],tmp1))
    #tmp2<-cols(3)[2]
    pal<-colorRampPalette(c(degrade_min(N)[i],tmp,colref,degrade_max(N)[i]),bias = 0.8)  
    #pal<-colorRampPalette(c(degrade_min(N)[i],colref,degrade_max(N)[i]),bias = 0.8)
    pal<-pal(i)
    mypal<-c(mypal, list(pal))
  }
 return(mypal)
}


