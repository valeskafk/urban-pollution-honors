rm(list=ls())

library(foreign)
library(reshape2)
library(tools)
library(parallel)
library(raster)
library(ncdf4)
library(rhdf5)
library(data.table)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(tibble)
library(exactextractr)
library(gtools)
library(sf)
library(haven)
library(geos)


##1a) Convert NC files to CSV

#define path
path <- "/Users/research/Desktop/valeska-honors-new/"

##applying to all files
pm_files <-list.files(paste0(path,"/data-raw/washu-pm25/2019"), "*.nc", full.names = FALSE)

#Function to pull in raster files and convert to long csv (lon lat pm)
convert <- function(x){
  nc.brick <- brick(paste0(path, "data-raw/washu-pm25/2019/",x)) 
  nc.df <- as.data.frame(nc.brick[[1]], xy=T)
  nc.df<-as.data.table(nc.df)
  colnames(nc.df) <- c("lon","lat","pm")
  fwrite(nc.df, paste0(path,"/data-raw/washu-pm25/2019/pm-csv/",file_path_sans_ext(x),".csv"))
}
mcmapply(x=pm_files[1:12], convert, mc.cores = 4)


##1b) Subset Spatial Points

##### Here we subset the pollution data to state boundaries in order to make the large datasets more manageable
### We use the Census Tract shapefiles for this process because they contain fewer polygons and are less computationally expensive
vars <- list.dirs(paste0(path,"data-raw/census-shapefiles/tracts/"), full.names = FALSE, recursive = FALSE)

### Because each Year's dataset contains the same set of unique points, we only need to subset one year's 
### We assign a unique ID to each point before subsetting. This allows us to match data to that point later on for each year
### 
points <-as.data.frame(read.csv(paste0(path,"data-raw/washu-pm25/2020/pm-csv/V5GL02.HybridPM25.NorthAmerica.202001-202001.csv" ),header = TRUE))

### We call the indexing column "ID"
points<-tibble::rownames_to_column(points, "ID") 

### we make a spatial points dataframe using the sp package
xy <- cbind(points$lon, points$lat)
pts <- SpatialPoints(xy)
sp_df <- SpatialPointsDataFrame(pts, points)

## We use this function to subset the points to each state. Because each state has it's own shapefile, we can run this process in parallel
subset_points <- function(x){
  #load the state census tracts shapefile
  state <- shapefile(paste0(path,"data-raw/census-shapefiles/tracts/", x,"/", x,".shp"))
  #assign the coordinates to an object
  coords <- coordinates(state)
  #Dissolve the polygons so that only the outer state boundary remains
  ID <- cut(coords[,1], range(coords[,1]), include.lowest=TRUE)
  state_bounds <- unionSpatialPolygons(state,ID)
  # Create a buffer that is the width of the data resolution. 
  #This allows us to capture data from points that fall outside of the boundary but whose extent when rasterized fall within the boundary
  buffer <- gBuffer(state_bounds, width=.01)
  #make sure the coordinatte reference system is the same for the spatial points and the buffered state boundary
  crs(sp_df) <- crs(state_bounds)
  #remove the points that do not touch or fall within the buffered boundary
  subset_pts <-sp_df[buffer, ]
  #create a dataframe with the lat, lon and ID of each point
  
  subset_pts$state <- over(subset_pts,state)
  subset_pts$STATEFP00 <-  subset_pts$state$STATEFP00
  subset_pts$state <- NULL
  
  
  newdf <- as.data.frame(subset_pts)
  setwd(paste0(path,"data-raw/subset-points/"))
  write.csv(newdf, paste0(x,".csv"))
}
#mapply(x=vars, subset_points)
mcmapply(x=vars, subset_points, mc.cores = 4)


##1c) Merge State Pollution
### we now merge pollution from all years to each state file
states <- list.files(paste0(path,"data-raw/subset-points/"), "*.csv", full.names = FALSE)
#dir.create(paste0(path,"data-raw/State Data"))

#2018
pm_merge <- function(x){
  #Call in the first state's subseted points
  months <- list.files(paste0(path,"data-raw/washu-pm25/2018/pm-csv/"), "*.csv", full.names = FALSE)
  z <- read.csv(paste0(path, "data-raw/subset-points/",x))
  #Now we loop through all of the years from 1981 to 2018, adding "CorrectedPM2.5" by unique ID 
  for(j in 1:length(months)){
    pm <- as.data.frame(fread(paste0(path,"data-raw/washu-pm25/2018/pm-csv/", months[j])))
    pm <- tibble::rownames_to_column(pm, "ID")
    colnames(pm)[4] <- paste0("CorrectedPM2.5_",file_path_sans_ext(months[j]))
    z <- merge(x = z, y = pm[ , c("ID", paste0("CorrectedPM2.5_",file_path_sans_ext(months[j])))], by = "ID", all.x=TRUE)
  }
  newdf <- as.data.frame(z)
  setwd(paste0(path,"data-raw/state-data/2018/"))
  write.dta(newdf, paste0("State_",x,"_PM_2018",".dta"))
  gc()
}

##talk about this with gabe: "statefp00 doesnt exist in these tracts?' look at the 1c) file, still running error

mcmapply(x=states,pm_merge,mc.cores = 4)


##1d) Tract Zonal Statistics
list_state_shapes <- mixedsort(sort(list.dirs(paste0(path, "data-raw/census-shapefiles/tracts"), full.names = FALSE, recursive = FALSE)))
state_data <- mixedsort(sort(list.files(paste0(path, "data-raw/state-data/2019/"), "*.dta", full.names = FALSE)))
us_border <- shapefile(paste0(path, "data-raw/census-shapefiles/us/cb_2017_us_nation_5m/cb_2017_us_nation_5m.shp"))

zonal_stats<-function(x,y){
  ###We load the state shapefile and its corresponding file from the State_Data directory which includes all of the long format state data by year for all of the lat, lon points in that state. 
  
  state <- shapefile(paste0(path, "data-raw/census-shapefiles/tracts/", x,"/", x,".shp"))
  pollution <- read.dta(paste0(path, "data-raw/state-data/2018/", y))
  
  #We now build a raster object in order to later conduct zonal statistics
  #The resolution is the horizontal and vertical distance between spatial points
  resolution <-  0.01
  
  xmin <- min(pollution$lon) - (.5 * resolution)
  xmax <- max(pollution$lon) + (.5 * resolution)
  ymin <- min(pollution$lat) - (.5 * resolution)
  ymax <- max(pollution$lat) + (.5 * resolution)
  
  pm_raster <- raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, res = resolution, crs=crs(state))
  
  listnames <- colnames(pollution)
  listnames <- listnames[8:length(listnames)]
  
  state <- intersect(state,us_border)
  sf_state <- st_as_sf(state)
  #This loop produces a raster of PM data for each year from the wide format data above.
  #We then use the exactextract function to calcualte the mean PM 2.5 value in each polygon within the state shapefile (see https://github.com/isciences/exactextractr)
  for(j in 1:length(listnames)){
    state_pm <- rasterize(pollution[, c('lon', 'lat')], pm_raster, pollution[, paste0(listnames[j])], fun=mean)
    state$pm <- exact_extract(state_pm, sf_state, 'mean')
    colnames(state@data)[colnames(state@data) == "pm"] <- paste0(listnames[j])
  }
  final <- as.data.frame(state)
  write.dta(final, paste0(path, "data-clean/tract-zonal-statistics/2018/", file_path_sans_ext(y),".dta"))
}
mcmapply(x=list_state_shapes, y=state_data, zonal_stats, mc.cores=4)

#This loop renames variables so that there are no - 
rename_data_list <- mixedsort(sort(list.files(paste0(path, "data-clean/tract-zonal-statistics/2018/"), full.names = FALSE)))
colClean <- function(x){
  y<-read_dta(paste0(path,"data-clean/tract-zonal-statistics/2018/",x))
  colnames(y) <- gsub("-","_", colnames(y)) 
  write_dta(y,paste0("data-clean/tract-zonal-statistics-clean/2018/",x))
}
mcmapply(x=rename_data_list, colClean, mc.cores=4)

