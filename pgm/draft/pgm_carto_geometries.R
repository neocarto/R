# =============================================
# Nicolas LAMBERT
# How to manage geometries with maptools
# ===============================================

# LOAD PACKAGES
library(maptools)

# IMPORT DES COUCHES
tun0 <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts0.shp")
tun1 <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts1.shp")
tun2 <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts2.shp")
tun3 <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts3.shp")
tun4 <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts4.shp")
tun3_centres <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts3_centres.shp")
tun4_centres <- readShapeSpatial("../../data/TUN/shp/Tunisie_snuts4_centres.shp")
sebkhas <- readShapeSpatial("../../data/TUN/shp/sebkhas.shp")

# VISULATISATION 
plot(tun4, border="white", col="grey")
plot(tun3, border="#999999", lwd=1.5,add=TRUE)
plot(sebkhas,col="#FEE08B50",border="#FEE08B",add=TRUE)
plot(tun0, border="#666666",lwd=2,add=TRUE)





