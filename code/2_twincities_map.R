
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gplots)
library(rlang)
library(maps) #for map data
library(ggmap) # for mapping points on maps
library(RColorBrewer) #for color palettes
library(leaflet) #for highly customizable mapping 
library(ggthemes) #for more themes (including theme_map())
library(readr)
library(sf)

#27123 Ramsey FIP
#27053 Hennepin FIP
#27003 Anoka 
#27019 Carver
#27037 Dakota 
#27139 Scott  
#27163 Washington


path <-  "/Users/research/Desktop/valeska-honors-new/"
mn <- read.csv(paste0(path, "data-clean/mn_panel.csv"))

#filter out 7 counties in the metroarea 
metroarea <- mn %>%
  filter(COUNTYFP==123 |COUNTYFP==53 |COUNTYFP==3 |COUNTYFP==19|COUNTYFP==37 |COUNTYFP==139 |COUNTYFP==163)

twincities <- get_stamenmap(
  bbox = c(left = -93.92, bottom = 44.60, right = -92.50, top = 45.34),
  maptype = "terrain",
  zoom = 12 #give more details
  )

#ramseyhennepin <- ramseyhennepin %>%
  mutate(lon = as.numeric (INTPTLON))
#ramseyhennepin <- ramseyhennepin %>%
  mutate(lat=as.numeric(INTPTLAT))                            

#maps each GEOID (census tract?)
ggmap(twincities) +
  geom_point(
    data = metroarea,
    aes(x= INTPTLON, y= INTPTLAT),
    alpha = 0.6, size = 2) +
  theme_map()

#pollution annotated 
ramseyhennepin <- ramseyhennepin%>%
  mutate(pm_jan =as.numeric(pm_25_01))

ggmap(twincities) +
  geom_point(
    data = ramseyhennepin,
    aes(x= lon, y= lat),
    alpha = 0.6, size = 2) +
  geom_text(ramseyhennepin, aes(x= lon, y= lat,label=pm_jan),
            size = 3, vjust = 0, hjust = -0.5)+
  theme_map() #maybe it was too much?

#cutting out i-94

highway <- get_stamenmap(
  bbox = c(left = -93.2942, bottom = 44.8595, right = -92.9200, top = 45.0454),
  maptype = "terrain",
  zoom = 12 #give more details
)

ggmap(highway) +
  geom_point(
    data = metroarea,
    aes(x= lon, y= lat),
    alpha = 0.6, size = 2) +
  geom_text(ramseyhennepin, aes(x= lon, y= lat,label=pm_jan),
            size = 3, vjust = 0, hjust = -0.5)+
  theme_map()


highway_plain <- get_stamenmap(
  bbox = c(left = -93.2942, bottom = 44.8595, right = -92.9200, top = 45.0454),
  maptype = "toner",
  zoom = 13 #give more details
)

ramseyhennepin <- ramseyhennepin %>%
  mutate(pm_july =as.numeric(pm_25_07))


ggmap(highway_plain) +
  geom_point(
    data = metroarea,
    aes(x= INTPTLON, y= INTPTLAT, color=pm_25_01),
    alpha = 0.6, size = 2, guide="none") +
  theme_map()

#hand code Rondo census tracts

rondo <- get_stamenmap(
  bbox = c(left = -93.1763, bottom = 44.9215, right = -93.0828, top = 44.9680),
  maptype = "toner",
  zoom = 13 #give more details
)
ggmap(rondo) +
  geom_point(
    data = metroarea,
    aes(x= INTPTLON, y= INTPTLAT, color=pm_25_01),
    alpha = 0.6, size = 2, guide="none") +
  geom_label(data = metroarea,aes(x = INTPTLON, y = INTPTLAT, label = TRACTCE))+
  theme_map()

#create buffer zone around the highway

#DO THIS FOR THE HIGHWAY - how can i intersect points along a highway?
#are there highway shapefiles? manually select tracts?

metro_sf <- metroarea %>% 
  st_as_sf(coords = c('INTPTLON', 'INTPTLAT')) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

metroarea_10km <- st_buffer(metro_sf, units::as_units(10,'kilometer'))




