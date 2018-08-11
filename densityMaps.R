## Kernel density maps

library(spatstat)
library(sp)
library(maptools)
library(rgdal)
load("pubs.RData")

## Get UK outline
uk = map("world", region="UK", plot=FALSE, fill=TRUE)
uk.sp <- map2SpatialPolygons(uk, IDs=uk$names, proj4string=CRS("+init=EPSG:4326"))
## Reproject to OS projection
uk.sp <- spTransform(uk.sp, CRS("+init=EPSG:27700"))
# Convert boundary to owin object
y <- as(uk.sp, "SpatialPolygons")
# Then convert SpatialPolygons to owin class
uk.owin <- as(uk.sp, "owin")

## Points - first make up as spatial point class
pubs.sp = pubs
coordinates(pubs.sp) <- ~lon+lat
proj4string(pubs.sp) <- CRS("+init=EPSG:4326")
## Reproject to OS projection
pubs.sp <- spTransform(pubs.sp, CRS("+init=EPSG:27700"))

## Check plot
plot(uk.sp)
plot(pubs.sp, add=TRUE, pch=16, cex=0.5)

## Make point pattern object
pubs.crds = coordinates(pubs.sp)
pubs$east = pubs.crds[,1]
pubs$north = pubs.crds[,2]
pub.ppp = ppp(pubs$east, pubs$north, window=uk.owin)

## Plots
plot(pub.ppp)
plot(density(pub.ppp, sigma=50000))
