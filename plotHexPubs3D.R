## Hexgon maps

library(ggplot2)
library(maptools)
library(maps)
library(rgdal)
library(rayshader)

# Load the dataset of points

load("pubs.RData")

## Get UK outline
uk = map("world", region="UK", plot=FALSE, fill=TRUE)
uk.sp <- map2SpatialPolygons(uk, IDs=uk$names, proj4string=CRS("+init=EPSG:4326"))
## Reproject to OS projection
uk.sp <- spTransform(uk.sp, CRS("+init=EPSG:27700"))
uk.gg = fortify(uk.sp)

## Points - first make up as spatial point class
pubs.sp = pubs
coordinates(pubs.sp) <- ~lon+lat
proj4string(pubs.sp) <- CRS("+init=EPSG:4326")
## Reproject to OS projection
pubs.sp <- spTransform(pubs.sp, CRS("+init=EPSG:27700"))
pubs.crds = coordinates(pubs.sp)

## DF
# pdf("pubsHex.pdf")
plot.df = data.frame(long=pubs.crds[,1], lat=pubs.crds[,2])
p <- ggplot(data=plot.df, aes(x=long,y=lat))
p <- p + stat_binhex(binwidth=c(10000,10000))
# p <- p + stat_binhex(bins=200)
p <- p + geom_path(data=uk.gg,aes(group=group), size=0.25) 
p <- p +
  scale_fill_viridis_c(option = "C") +
  coord_equal() +
  labs(x=NULL, y=NULL) 

print(p)
# dev.off()

plot_gg(p, width = 5, height = 4, scale = 300, multicore = FALSE, 
        windowsize = c(1000, 800))

render_camera(fov = 40, zoom = 0.5, theta = 350, phi = 35)

render_depth(focus = 0.6, focallength = 35, clear = FALSE)

