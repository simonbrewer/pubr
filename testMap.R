require(ggplot2)
pubName = "ship"
pubColour = "red"
pubBackground = "blue"
sizing = 1

pubTmp <- pubs %>%
  filter(nameClean == pubName) 

regionLabel <- namesPerRegion %>% 
  filter(nameClean == pubName) %>%
  arrange(desc(proportion), desc(proportionOfRegion)) %>%
  filter(row_number() == 1) %>%
  mutate(label = paste0(count," in ",district))

typeSummary <- nameTypes %>% 
  ungroup() %>%
  filter(nameClean == pubName) %>%
  #filter(type != "as is") %>%
  #filter(type != "hotel") %>%
  #filter(type != "bar") %>%
  arrange((n)) %>%
  mutate(lat = desc(row_number (n)) + 56) %>%
  mutate(prop = n/sum(n)) %>%
  mutate(prop5 = prop *5) %>%
  mutate(prop5Run = cumsum(prop5)) %>%
  mutate(lonStartText = -14.5) %>%
  mutate(lonStart = lonStartText) %>%
  mutate(lonStart = lag(lonStart+prop5Run, default = -14.5)) %>%
  mutate(lonEnd = lonStart + prop5) %>%
  mutate(type = if_else(type == "as is",  str_replace(type, pattern = "as is", nameClean), paste(nameClean, type, sep = " "))) %>%
  rename(count = n) %>%
  mutate(type = str_to_title(type))

map = ggplot(pubTmp) +
  geom_blank(data = NULL, aes(x = 2, y = 60))+
  geom_blank(data = NULL, aes(x = -20, y = 50))

map = map + annotate(geom = "text", x = -12, y = 56.5, ## plotting pub name
                     label = str_wrap(str_to_upper(pubName), width = 5),
                     size = 14, colour = pubColour,
                     hjust = 0.5, vjust = 0, fontface = "bold",
                     lineheight = 0.8) 

map = map + geom_text(data = regionLabel, 
                      aes(x = lon, y = 49.5, label = label), 
                      size = 4, colour = pubBackground,
                      hjust = 1, vjust = -0.5, 
                      nudge_x = -0.1, nudge_y = -0.2, fontface = "italic")

map = map + geom_segment(data = regionLabel, 
                         aes(x = lon, y = 49.5, xend = lon, yend = lat), 
                         colour = pubBackground, size = 0.5, lineend = "square")

#uk map
map = map + geom_path(data = UK, ##plotting map of GB
                      mapping = aes(x = long, y = lat, group = group),
                      colour = pubBackground, lineend = "round", 
                      linejoin = "round", linetype = "solid", 
                      size = 0.5)

map = map +   #### add typical name
  geom_label(data = pubs %>% 
               filter(nameClean == pubName) %>% filter(lat == max(lat)) %>% mutate(name = str_replace_all(name, " ", "\n")), 
             aes(x = lon+0.3, y = lat+0.3, label = name), 
             size = 3, colour = pubColour,
             hjust = 0, vjust = 0, fontface = "italic", 
             fill = "white", label.size=NA)

map = map + geom_segment(data = pubs %>% 
                           filter(nameClean == pubName) %>% filter(lat == max(lat)), 
                         aes(x = lon+0.01, y = lat+0.01, xend = lon+0.3, yend = lat+0.3), 
                         colour = pubColour)

map = map +   #plotting actual points
  geom_point(aes(x = lon, y = lat), colour = pubColour, shape = 20, size = sizing) 
#geom_point(aes(x = lon, y = lat), size = 0.001, colour = pubBackground, shape = 20) 

###break line
map = map + geom_segment(aes(x = -14, xend = -10, y = 56, yend = 56), size = 1, colour = pubBackground)

map = map + geom_segment(data = typeSummary, 
                         aes(x = lonStart, xend = lonEnd, y = lat, yend = lat), 
                         colour = pubColour, size = 1)

map = map + geom_segment(data = typeSummary, 
                         aes(x = lonStartText, xend = lonStartText+5, y = lat, yend = lat), 
                         colour = pubBackground, size = 0.3, linetype = "dotted")

##add details for types
map = map + geom_text(data = typeSummary, aes(x = lonStartText, y = lat, label = type), 
                      size = 4, colour = pubColour,
                      hjust = 0, vjust = -0.5)

## add numerics for types
map = map + geom_text(data = typeSummary, 
                      aes(x = lonStartText, y = lat, label = count), 
                      size = 6, colour = pubBackground,
                      hjust = 1, vjust = -0.5, 
                      nudge_x = -0.1, nudge_y = -0.2)

map = map +   coord_map(projection = "mercator")

map = map + theme(line = element_blank(),
                  text = element_blank(),
                  title = element_blank(),
                  rect = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white", colour = "white"),
                  plot.background = element_rect(fill = "white", colour = "white"))
print(map)

stop()

#### add labels for top region
geom_text(data = regionLabel, 
          aes(x = lon, y = 49.5, label = label), 
          size = 4, colour = pubBackground,
          hjust = 1, vjust = -0.5, 
          nudge_x = -0.1, nudge_y = -0.2,
          family = "Open Sans", fontface = "italic")+
  
  geom_segment(data = regionLabel, 
               aes(x = lon, y = 49.5, xend = lon, yend = lat), 
               colour = pubBackground, size = 0.5, lineend = "square")+
  
  #uk map
  geom_path(data = UK, ##plotting map of GB
            mapping = aes(x = long, y = lat, group = group),
            colour = pubBackground, lineend = "round", 
            linejoin = "round", linetype = "solid", 
            size = 0.5)+ 
  
  #### add typical name
  geom_label(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)) %>% mutate(name = str_replace_all(name, " ", "\n")), 
             aes(x = lon+0.3, y = lat+0.3, label = name), 
             size = 3, colour = pubColour,
             hjust = 0, vjust = 0, 
             family = "Open Sans", fontface = "italic", 
             fill = "white", label.size=NA)+
  
  geom_segment(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)), 
               aes(x = lon+0.01, y = lat+0.01, xend = lon+0.3, yend = lat+0.3), colour = pubColour)+
  
  #plotting actual points
  geom_point(aes(x = lon, y = lat), colour = pubColour, shape = 20, size = sizing) + 
  #geom_point(aes(x = lon, y = lat), size = 0.001, colour = pubBackground, shape = 20) + 
  
  ###break line
  geom_segment(aes(x = -14, xend = -10, y = 56, yend = 56), size = 1, colour = pubBackground)+
  
  # add type metrics
  geom_segment(data = typeSummary, 
               aes(x = lonStart, xend = lonEnd, y = lat, yend = lat), 
               colour = pubColour, size = 1)+
  
  geom_segment(data = typeSummary, 
               aes(x = lonStartText, xend = lonStartText+5, y = lat, yend = lat), 
               colour = pubBackground, size = 0.3, linetype = "dotted")+
  
  ##add details for types
  geom_text(data = typeSummary, aes(x = lonStartText, y = lat, label = type), 
            size = 4, colour = pubColour,
            hjust = 0, vjust = -0.5,
            family = "Open Sans")+
  
  ## add numerics for types
  geom_text(data = typeSummary, 
            aes(x = lonStartText, y = lat, label = count), 
            size = 6, colour = pubBackground,
            hjust = 1, vjust = -0.5, 
            nudge_x = -0.1, nudge_y = -0.2)+
  # geom_text(data = typeSummary, 
  #           aes(x = lonStartText, y = lat, label = count), 
  #           size = 6, colour = pubBackground,
  #           hjust = 1, vjust = -0.5, 
  #           nudge_x = -0.1, nudge_y = -0.2,
  #           family = "Open Sans", fontface = "italic")+
  
  coord_map(projection = "mercator")+
  
  theme(
    line = element_blank(),
    text = element_blank(),
    title = element_blank(),
    rect = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  ) 




stop()

## CODE BELOW HERE IS FROM ORIGINAL PUB MAPS
makeMeAMap <- function(pubName = "ship", pubColour = "red", sizing = 0.7, pubBackground = "blue") {
  
  
  typeSummary <- nameTypes %>% 
    ungroup() %>%
    filter(nameClean == pubName) %>%
    #filter(type != "as is") %>%
    #filter(type != "hotel") %>%
    #filter(type != "bar") %>%
    arrange((n)) %>%
    mutate(lat = desc(row_number (n)) + 56) %>%
    mutate(prop = n/sum(n)) %>%
    mutate(prop5 = prop *5) %>%
    mutate(prop5Run = cumsum(prop5)) %>%
    mutate(lonStartText = -14.5) %>%
    mutate(lonStart = lonStartText) %>%
    mutate(lonStart = lag(lonStart+prop5Run, default = -14.5)) %>%
    mutate(lonEnd = lonStart + prop5) %>%
    mutate(type = if_else(type == "as is",  str_replace(type, pattern = "as is", nameClean), paste(nameClean, type, sep = " "))) %>%
    rename(count = n) %>%
    mutate(type = str_to_title(type))
  
  
  regionLabel <- namesPerRegion %>% 
    filter(nameClean == pubName) %>%
    arrange(desc(proportion), desc(proportionOfRegion)) %>%
    filter(row_number() == 1) %>%
    mutate(label = paste0(count," in ",district))
  
  ### plot the pubs by name ####
  
  pubs %>%
    filter(nameClean == pubName) %>%
    ggplot() +
    geom_blank(data = NULL, aes(x = 2, y = 60))+
    geom_blank(data = NULL, aes(x = -20, y = 50))+
    
    
    ggplot2::annotate(geom = "text", x = -12, y = 56.5, ## plotting pub name
                      label = str_wrap(str_to_upper(pubName), width = 5),
                      size = 14, colour = pubColour,
                      hjust = 0.5, vjust = 0,
                      family = "Open Sans", fontface = "bold",
                      lineheight = 0.8) +
    
    #### add labels for top region
    geom_text(data = regionLabel, 
              aes(x = lon, y = 49.5, label = label), 
              size = 4, colour = pubBackground,
              hjust = 1, vjust = -0.5, 
              nudge_x = -0.1, nudge_y = -0.2,
              family = "Open Sans", fontface = "italic")+
    
    geom_segment(data = regionLabel, 
                 aes(x = lon, y = 49.5, xend = lon, yend = lat), 
                 colour = pubBackground, size = 0.5, lineend = "square")+
    
    #uk map
    geom_path(data = UK, ##plotting map of GB
              mapping = aes(x = long, y = lat, group = group),
              colour = pubBackground, lineend = "round", 
              linejoin = "round", linetype = "solid", 
              size = 0.5)+ 
    
    #### add typical name
    geom_label(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)) %>% mutate(name = str_replace_all(name, " ", "\n")), 
               aes(x = lon+0.3, y = lat+0.3, label = name), 
               size = 3, colour = pubColour,
               hjust = 0, vjust = 0, 
               family = "Open Sans", fontface = "italic", 
               fill = "white", label.size=NA)+
    
    geom_segment(data = pubs %>% filter(nameClean == pubName) %>% filter(lat == max(lat)), 
                 aes(x = lon+0.01, y = lat+0.01, xend = lon+0.3, yend = lat+0.3), colour = pubColour)+
    
    #plotting actual points
    geom_point(aes(x = lon, y = lat), colour = pubColour, shape = 20, size = sizing) + 
    #geom_point(aes(x = lon, y = lat), size = 0.001, colour = pubBackground, shape = 20) + 
    
    ###break line
    geom_segment(aes(x = -14, xend = -10, y = 56, yend = 56), size = 1, colour = pubBackground)+
    
    # add type metrics
    geom_segment(data = typeSummary, 
                 aes(x = lonStart, xend = lonEnd, y = lat, yend = lat), 
                 colour = pubColour, size = 1)+
    
    geom_segment(data = typeSummary, 
                 aes(x = lonStartText, xend = lonStartText+5, y = lat, yend = lat), 
                 colour = pubBackground, size = 0.3, linetype = "dotted")+
    
    ##add details for types
    geom_text(data = typeSummary, aes(x = lonStartText, y = lat, label = type), 
              size = 4, colour = pubColour,
              hjust = 0, vjust = -0.5,
              family = "Open Sans")+
    
    ## add numerics for types
    geom_text(data = typeSummary, 
              aes(x = lonStartText, y = lat, label = count), 
              size = 6, colour = pubBackground,
              hjust = 1, vjust = -0.5, 
              nudge_x = -0.1, nudge_y = -0.2)+
    # geom_text(data = typeSummary, 
    #           aes(x = lonStartText, y = lat, label = count), 
    #           size = 6, colour = pubBackground,
    #           hjust = 1, vjust = -0.5, 
    #           nudge_x = -0.1, nudge_y = -0.2,
    #           family = "Open Sans", fontface = "italic")+
    
    coord_map(projection = "mercator")+
    
    theme(
      line = element_blank(),
      text = element_blank(),
      title = element_blank(),
      rect = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    ) 
  
  
}

makeMeAMap(pubColour = "red", pubBackground = "blue", sizing = 1, pubName = "ship") 
