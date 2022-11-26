library(sf)
library(tidyr)
library(zoo)
library(lubridate)
library(fixest)
library(GGally)
library(broom)
library(dplyr)
library(ggmap)
library(viridis)

#1) Read in highway shapefiles and census polygons
highway <- read_sf("/Users/research/Desktop/valeska-honors-new/data-raw/2019_minnesota_roads/tl_2019_27_prisecroads.shp", quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE)
censuspolygons <- read_sf("/Users/research/Desktop/valeska-honors-new/data-raw/census-shapefiles/tracts/tl_2020_27_tract/tl_2020_27_tract.shp", quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE)


#1b) get metro census polygons to intersect them with the highway shapefile
censuspolygons$COUNTYFP <- as.numeric(censuspolygons$COUNTYFP)

metrocensuspolygons <- censuspolygons %>%
  filter(COUNTYFP %in% c(123,53,3,19,37,139,163))

metrocensuspolygons<- metrocensuspolygons %>% mutate(TRACTCE=as.numeric(TRACTCE))

# 1c) doing the Buffer of 600m to set control and treatment groups for polygons 
buffer_600m <- st_buffer(highway, 600)
metrocensuspolygons$HWY600m <-(st_intersects(metrocensuspolygons,buffer_600m) %>% lengths()) > 0


##Get only tracts Centroids to intersect them
metroCentroids<- st_as_sf(metroarea %>% select(GEOID_1,INTPTLAT,INTPTLON),coords = c('INTPTLON','INTPTLAT'))
st_crs(metroCentroids)  = st_crs(buffer_600m)

#plot
highway %>% ggplot() + geom_sf() +
  coord_sf(xlim =st_bbox(metroCentroids)[c(1,3)],ylim = st_bbox(metroCentroids)[c(2,4)])


pollution <-
  left_join(metroarea,
            metrocensuspolygons %>% dplyr::select(TRACTCE,HWY600m),
            by = "TRACTCE")
#Create a quantitative variable that measures distance from a point to the closest highway

st_crs(metroCentroids)  = st_crs(highway)

distToRoads <- st_distance(metroCentroids,highway) %>% units::drop_units() %>% as.matrix() #calculate distance from census tracts to any interstate 

metroarea$MinDistToHwy = distToRoads %>% apply(1,min) #create a variable with the lowest distance to a highway

##create a variable with "Number of highways this census tracts is 1km from"
##metroarea$Num_1kmHWY <- st_intersects(metroCentroids,buffer_1km) %>% lengths() #for each census tractc, the index of the highway that was in the buffer

##Create treatment group
metroarea <- metroarea %>% 
  mutate(CloseHW = ifelse(MinDistToHwy<=600,"close","far"))

##Create control group - maybe it is not needed?
metroarea <- metroarea %>% 
  mutate(FarHW = ifelse(MinDistToHwy>=1500,"far","close"))

##exclude tracts in between 600m and 1500m
metroarea %>% 
  filter(MinDistToHwy<=600 | MinDistToHwy >=1500)


buffer_600m %>% ggplot() + geom_sf() +
  coord_sf(xlim =st_bbox(metroCentroids)[c(1,3)],ylim = st_bbox(metroCentroids)[c(2,4)])

#Potentially do other buffers
#buffer_10km <- st_buffer(highway, 10000) 



#metroarea$HWY1km <-(st_intersects(metroCentroids,buffer_1km) %>% lengths()) > 0
#metroarea$HWY10km <-(st_intersects(metroCentroids,buffer_10km) %>% lengths()) > 0

#metroarea is the dataset I will add the column "HWY1km" to where I have the pollution data, that column will be FALSE/TRUE
#metroCentroids is a dataset only with the census tracts centroids that I'm intersecting with the highway buffer
#buffer_1km is the dataset with the highway line shapefile and the 1km buffer around it

#plotting just the buffer zone (first the whole metro area, then only rondo)
rondo <-  c(left = -93.1763, bottom = 44.9215, right = -93.0828, top = 44.9680)
metroCentroids %>% ggplot() + geom_sf(data = metroCentroids) + 
  geom_sf(data = buffer_600m,alpha=.5,color ='orange') +
  coord_sf(xlim = rondo[c(1,3)],ylim = rondo[c(2,4)])

#create a map with the census tracts polygons drawing
metroCentroids %>% ggplot() + geom_sf(data = metroCentroids) + 
  geom_sf(data = buffer_600m,alpha=.5,color ='orange') +
  geom_sf(data = censuspolygons, alpha=.3)+
  coord_sf(xlim = rondo[c(1,3)],ylim = rondo[c(2,4)])+
  theme_classic()


#Map the Twin Cities
metroCentroids %>% ggplot() + geom_sf(data = metroCentroids) + 
  geom_sf(data = buffer_600m,alpha=.5,color ='orange') +
  geom_sf(data = censuspolygons, alpha=.3)+
  coord_sf(xlim =st_bbox(metroCentroids)[c(1,3)],ylim = st_bbox(metroCentroids)[c(2,4)])+
  theme_classic()

#Take a closer look
zoom<-  c(left = -93.3340, bottom = 44.8768, right = -92.9598, top = 45.0488)
metroCentroids %>% ggplot() + geom_sf(data = metroCentroids) + 
  geom_sf(data = buffer_600m,alpha=.5,color ='orange') +
  geom_sf(data = censuspolygons, alpha=.3)+
  coord_sf(xlim = zoom[c(1,3)],ylim = zoom[c(2,4)])+
  theme_classic()

#Closer look without centroids
ggplot() + geom_sf(data = highway, alpha = 0.8) + 
  geom_sf(data = buffer_600m,alpha=.5,color ='orange') +
  geom_sf(data = censuspolygons, alpha=.2)+
  coord_sf(xlim = zoom[c(1,3)],ylim = zoom[c(2,4)])+
  theme_classic()

#Plot a map with highway lines bolded and pm2.5 with color
ggmap(zoom) +
  geom_point(
    data = metroarea,
    aes(x= INTPTLON, y= INTPTLAT, color=pm_25_01),
    alpha = 0.6, size = 2, guide="none") +
  theme_map()


ggplot() + geom_sf(data = highway, alpha = 1, size =2) + 
  geom_sf(data = censuspolygons, alpha=.2)+
  geom_point(data = metroarea,
    aes(x= INTPTLON, y= INTPTLAT, color=factor(Pollution)),
    alpha = 0.6, size = 2, guide="none") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  scale_fill_continuous(type = "viridis") +
  coord_sf(xlim = zoom[c(1,3)],ylim = zoom[c(2,4)])+
  theme_classic()


ggmap(zoom) +
  stat_contour(data = metroarea, aes(x = INTPTLON, y = INTPLAT, z = Pollution, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth = 100) +
  scale_fill_gradient(name = "Price", low = "green", high = "red") +
  guides(alpha = FALSE)

##Map Parallel trends (census tracts close to highway versus far over time)
metroarea %>% 
  ggplot(aes(x=year, y =pm_25_01, color=CloseHW))+ ##this plots just Jan concentrations
  geom_line()

test %>% 
  ggplot(aes(x=Date, y =`Corrected PM2.5`, color=CloseHW), alpha =0.5)+
  geom_line() +
  theme_classic()

##group by and summerize 
testsummary <- test %>% 
  group_by(HWY600m, Date) %>% 
  summarize(MeanPM2=mean(Pollution))

testsummary$HWY600m <- as.numeric(testsummary$HWY600m)

testsummary%>%  
  ggplot(aes(x=Date, y =MeanPM2, color=HWY600m, group=HWY600m), alpha =0.5)+
  geom_line() +
  geom_smooth(se =FALSE,method='loess',span = .2)+
  geom_vline(xintercept=as.yearmon("2020 2","%Y %m"))+
  theme_classic()

testsummary%>%  
  filter(Date > as.yearmon("2019 12","%Y %m")) %>% 
  ggplot(aes(x=Date, y =MeanPM2, color=CloseHW, group=CloseHW), alpha =0.5)+
  geom_line() +
  geom_smooth(se =FALSE,method='loess',span = .2)+
  geom_vline(xintercept=as.yearmon("2020 2","%Y %m"))+
  theme_classic()

summary(test)

mod <- lm(Pollution ~ Month + TRACTCE, data = test) 
test$resid = mod$residual

test %>%
  ggplot(aes(x = Date, y = resid,color = CloseHW)) +
  geom_line()+
  geom_smooth()+
  theme_classic()

