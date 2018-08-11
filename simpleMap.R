## Simple map (removes all the text and metrics)

require(ggplot2)
pubName = "ship"
pubColour = "red"
pubBackground = "black"
sizing = 2

pubTmp <- pubs %>%
  filter(nameClean == pubName) 

map = ggplot(pubTmp) +
  geom_blank(data = NULL, aes(x = 2, y = 60))+
  geom_blank(data = NULL, aes(x = -20, y = 50))

map = map + annotate(geom = "text", x = -12, y = 56.5, ## plotting pub name
                     label = str_wrap(str_to_upper(pubName), width = 5),
                     size = 12, colour = pubColour,
                     hjust = 0.5, vjust = 0, fontface = "bold",
                     lineheight = 0.8) 

#uk map
map = map + geom_path(data = UK, ##plotting map of GB
                      mapping = aes(x = long, y = lat, group = group),
                      colour = pubBackground, lineend = "round", 
                      linejoin = "round", linetype = "solid", 
                      size = 0.5)

map = map +   #plotting actual points
  geom_point(aes(x = lon, y = lat), colour = pubColour, shape = 20, size = sizing) 
#geom_point(aes(x = lon, y = lat), size = 0.001, colour = pubBackground, shape = 20) 

###break line
# map = map + geom_segment(aes(x = -14, xend = -10, y = 56, yend = 56), size = 1, colour = pubBackground)

map = map +   coord_map(projection = "mercator")

map = map + theme(line = element_blank(),
                  text = element_blank(),
                  title = element_blank(),
                  rect = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white", colour = "white"),
                  plot.background = element_rect(fill = "white", colour = "white"))
print(map)
