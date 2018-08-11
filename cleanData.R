library(tidyverse)
library(geosphere)
library(extrafont)

decimals <- 1

# Read in and process Pub name data ---------------------------------------

pubs <- read_csv(file = "data/osmPubs.csv", col_names = F) 

colnames(pubs) <- c("name", "lat", "lon")

pubs <- pubs %>%
  select(name,  lon, lat) %>%
  #filter(lat != "\\N") %>%
  #filter(lon != "\\N") %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) %>%
  unique() %>%
  mutate(lat0 = round(lat, decimals)) %>%
  mutate(lon0 = round(lon, decimals)) %>%
  mutate(id = row_number())


pubs <- pubs %>%
  mutate(name = stringr::str_to_title(name)) %>%
  mutate(nameClean = str_to_lower(name)) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "&", replacement = "and")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "@", replacement = "at")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "'", replacement = "")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "[^[:alnum:]]", replacement = " ")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = ",", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "[.]", replacement = "")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "\\)", replacement = " ")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "\\(", replacement = " ")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)the(?:$|\\W)", replacement = " ")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)ltd(?:$|\\W)", replacement = " ")) %>%
  
  mutate(nameClean = str_trim(string = nameClean)) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)pub(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)restaurant(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)wet sales only(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)wet sales(?:$|\\W)", replacement = "")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)ph(?:$|\\W)", replacement = " public house ")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)p h(?:$|\\W)", replacement = " public house ")) %>%
  
  mutate(inn = str_detect(nameClean, "(?:^|\\W)inn(?:$|\\W)")) %>%
  mutate(ph = str_detect(nameClean, "(?:^|\\W)public house(?:$|\\W)")) %>%
  mutate(tavern = str_detect(nameClean, "(?:^|\\W)tavern(?:$|\\W)")) %>%
  mutate(bar = str_detect(nameClean, "(?:^|\\W)bar(?:$|\\W)")) %>%
  mutate(new = str_detect(nameClean, "(?:^|\\W)new(?:$|\\W)")) %>%
  mutate(old = str_detect(nameClean, "(?:^|\\W)old(?:$|\\W)")) %>%
  mutate(hotel = str_detect(nameClean, "(?:^|\\W)hotel(?:$|\\W)")) %>%
  
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)hotel(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)public house(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)inn(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)bar(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)tavern(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)old(?:$|\\W)", replacement = "")) %>%
  mutate(nameClean = str_replace_all(nameClean, pattern = "(?:^|\\W)new(?:$|\\W)", replacement = "")) %>%
  
  mutate(asIs = if_else( tavern | bar | ph | hotel | inn, 
                         true = FALSE, false = TRUE)) %>%
  
  mutate(the = str_detect(string = str_to_lower(name), pattern = "(?:^|\\W)the(?:$|\\W)")) %>% # from https://superuser.com/questions/903168/how-should-i-write-a-regex-to-match-a-specific-word
  mutate(ye = str_detect(string = str_to_lower(name), pattern = "(?:^|\\W)ye(?:$|\\W)")) %>%
  mutate(nameClean = str_trim(nameClean)) %>%
  group_by(nameClean)%>%
  mutate(nameFreq = n()) %>%
  ungroup(nameClean)%>%
  arrange(desc(nameFreq)) 


# Calculate Name Metrics --------------------------------------------------
nameFrequencies <- pubs %>%
  group_by(nameClean)%>%
  summarise(nameFreq = n(), 
            maxLat = max(lat), 
            maxLon = max(lon),
            minLat = min(lat), 
            minLon = min(lon), 
            meanLat = mean(lat), 
            meanLon = mean(lon)) %>%
  arrange(desc(nameFreq)) 


ggplot(nameFrequencies %>% filter(str_detect(nameClean, "")) %>% filter(row_number() < 30))+
  geom_col(aes(x = reorder(x = str_to_upper(nameClean), X = nameFreq), y = nameFreq), fill = "red")+
  geom_text(aes(x = reorder(x = str_to_upper(nameClean), X = nameFreq), y = nameFreq+15, label = nameFreq))+
  theme_minimal()+
  coord_flip()+
  ggplot2::ggtitle(label = "Most Common Pub Names")+
  theme(line = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())


# Add typical names -------------------------------------------------------

typicalNames <- pubs %>%
  select(name, nameClean) %>%
  group_by(nameClean,name) %>%
  summarise(count = n()) %>%
  arrange(nameClean, desc(count)) %>%
  group_by(nameClean) %>%
  #mutate(count = n()) %>%
  top_n(n = 1, wt = row_number()) %>%
  arrange(desc(count))%>%
  select(nameClean, nameTypical = name) %>%
  ungroup() 

pubs <- pubs %>%
  left_join(y = typicalNames, by = "nameClean")

rm("typicalNames")


# Categorise based on types -----------------------------------------------

nameTypes <- pubs %>%
  select(nameClean, inn, ph, tavern, bar, hotel, asIs) %>%
  gather(key = "type", value = "TorF", 2:7) %>%
  filter(TorF) %>%
  mutate(type = str_replace(string = type, "ph", "public house")) %>%
  mutate(type = str_replace(string = type, "asIs", "as is")) %>%
  group_by(nameClean, type) %>%
  count() %>%
  arrange(nameClean) 



# Create some data for uk outline -----------------------------------------


UK <- map_data(map = "world", region = "UK") 

library(raster)
UK2 <- getData('GADM', country='United Kingdom', level=0)
UK2 <- spTransform(x = UK2, CRSobj = CRS("+proj=longlat +datum=WGS84") )
UK2 <- fortify(UK2)


# also this looks sexy...
districts <-  getData('GADM', country='United Kingdom', level=2)
districts <- spTransform(districts, CRS("+proj=longlat +datum=WGS84"))

districts$areaSqKM <- area(districts) /1000000
sum(districts$areaSqKM)

# plot(districts)

#districts@data <- districts@data %>% mutate(NAME_2 = if_else(TYPE_2 == "London Borough", true = "London", false = NAME_2)) 

districtsDF <- fortify(districts@data)

pubsSpatial <- pubs %>% 
  dplyr::select(lon, lat)

coordinates(pubsSpatial) <- ~lon+lat
pubsSpatial <- SpatialPoints(pubsSpatial, proj4string = CRS("+proj=longlat +datum=WGS84"))

mergedPubs <-  over(x = pubsSpatial, y = districts, returnList = F)

spacialflat <-  pubsSpatial@coords %>% 
  as_data_frame() %>%
  mutate(id = row_number())

mergedPubs <- mergedPubs %>%
  mutate(id = row_number())


mergedPubs <- left_join(x = mergedPubs, y= spacialflat, by = "id")


detach(package:raster, unload = T)
library(tidyverse)

mergedPubs <- mergedPubs %>% 
  dplyr::select(region = NAME_1, district = NAME_2, lat, lon, areaSqKM)

pubs <-  pubs %>%
  left_join(mergedPubs, by = c("lat", "lon")) %>%
  filter(!is.na(region))

rm(list = c( "districts", "mergedPubs", "pubsSpatial", "spacialflat"))

pubs %>% 
  group_by(district) %>% 
  summarise(n = n(), area = mean(areaSqKM), density = n/area) %>% # View()
  arrange(desc(density)) 

pubs %>% group_by(district) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% filter(row_number() < 20 ) %>%
  ggplot()+
  geom_col(aes(x = reorder(x = (district), X = n), y = n), fill = "red")+
  geom_text(aes(x = reorder(x = (district), X = n), y = n, label = n), colour= "white", hjust = 1.2)+
  theme_minimal()+
  coord_flip()+
  theme(line = element_blank(), axis.title = element_blank())


### omg this works


# Create summaries for districts ------------------------------------------
library(tidyverse)

namesPerRegion <- pubs %>% 
  group_by(nameClean, district, nameFreq) %>%
  summarise(count = n(),lon = mean(lon),lat = mean(lat)) %>%
  arrange(desc(count), nameClean) %>%
  mutate(proportion =  count/nameFreq) %>%
  ungroup()  %>%
  left_join(y = pubs %>% group_by(district) %>% count(), by = "district") %>%
  rename(regionTotal = n) %>%
  mutate(proportionOfRegion = count/regionTotal)

save(pubs, UK, namesPerRegion, file="pubs.RData")
