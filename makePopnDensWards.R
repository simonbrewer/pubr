## Get census data

library(rgdal)

ukcens <- readOGR("./data/England_cmwd_2011_gen/", "england_cmwd_2011_gen")
proj4string(ukcens) <- CRS("+init=EPSG:27700")

plot(ukcens)
plot(pubs.sp, add=TRUE, pch=16, col=3, cex=0.5)

ukcens$npubs = over(as(pubs.sp, "SpatialPoints"), ukcens)
spplot(ukcens, "npubs")
