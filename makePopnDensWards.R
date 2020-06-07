## Get census data

library(rgdal)
load("pubs.RData")
load("pubs.sp.RData")

engcmwd <- readOGR("./data/England_cmwd_2011_gen/", "england_cmwd_2011_gen")
proj4string(engcmwd) <- CRS("+init=EPSG:27700")

walcmwd <- readOGR("./data/Wales_cmwd_2011_gen/", "wales_cmwd_2011_gen")
proj4string(walcmwd) <- CRS("+init=EPSG:27700")

sctcmwd <- readOGR("./data/Scotland_caswa_2001/", "scotland_caswa_2001")
proj4string(sctcmwd) <- CRS("+init=EPSG:27700")

plot(engcmwd)
plot(pubs.sp, add=TRUE, pch=16, col=3, cex=0.5)

ukcens$npubs = over(as(pubs.sp, "SpatialPoints"), ukcens)
spplot(ukcens, "npubs")
