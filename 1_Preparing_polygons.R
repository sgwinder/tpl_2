#######
### Trust for Public Lands Showcase Project
### Salem, Oregon ROS dataset
#######

library(dplyr) # it's possible that this package could break things downstream.
library(sf)
library(lwgeom)
library(rgdal)
library(raster)
library(ggplot2)

city <- "salem"
setwd(paste0("/home/sgwinder/Documents/TPL/GIS/Data/", city, "/Calculated/"))

#ROS_Salem <- readOGR("GIS/Data/Calculated/", "ROS_WGS_84")
#ROS_Salem@data
#plot(ROS_Salem)
#crs(ROS_Salem)

# reading it in as an sf shape instead for making the grid
ROS_sf <- st_read(".", paste0(city, "_ROS_84"))
st_crs(ROS_sf)

########## create a grid ############

# reference: https://gis.stackexchange.com/questions/88830/overlay-a-spatial-polygon-with-a-grid-and-check-in-which-grid-element-specific-c
# cellsize is in degrees, .1
grid_1 <- st_make_grid(ROS_sf, cellsize = c(.1, .1)) %>%
  st_sf(grid_id = 1:length(.))
st_crs(grid_1)
#st_write(grid_1, paste0(city, "_grid_1.shp"))

# create lables for each grid id (necessary?)
grid_lab <- st_centroid(grid_1) %>% cbind(st_coordinates(.))

# view the polygons and grid overlaid
overlay <- ggplot() +
  geom_sf(data = ROS_sf, fill = "white", lwd = .05) +
  geom_sf(data = grid_1, fill = "transparent", lwd = .3)
#overlay

################ Dissolve ROS layer ############
ROS_dissolved <- st_union(ROS_sf)

#quartz()
#ggplot() +
#  geom_sf(data = test, fill = "white", lwd = .05)

########## Intersect with grid ######
#test <- st_intersection(ROS_dissolved, grid_1)
# correcting invalid geometries
ROS_dis_valid <- st_make_valid(ROS_dissolved)
#st_write(ROS_dis_valid, paste0(city, "_ROS_dissolved_valid.shp"))

ROS_gridded <- st_intersection(grid_1, ROS_dis_valid) # started at 1:01, still going at 4:44 when comp shut.
# < 1 min for pittsburgh, wichita on xps, 
# 6 min for tucson on xps
# 4.75 hr total run time for Salem on Mac, ~the same on xps
# just ran for charleston in <30 sec on xps

#st_write(ROS_gridded, paste0(city, "_ROS_gridded.shp"))

#quartz()
#plot(ROS_gridded)

########## Creating an alternative gridded shapefile that retains ROS details ######
# dissolving
ROS_dissolved_features <- ROS_sf %>%
  group_by(ros_class) %>%
  summarise()

#plot(ROS_dissolved_features)
ROS_valid <- st_make_valid(ROS_dissolved_features)
#st_write(ROS_valid, "ROS_class_diss_valid.shp")

ROS_class_gridded <- st_intersection(grid_1, ROS_valid) 
# started at 3:12 pm, finished at 3:34 - why so much quicker??
st_write(ROS_class_gridded, paste0(city, "_ROS_class_gridded.shp"))

###### Combining 5 gridded ROS regions into a single shapefile #########

setwd("/home/sgwinder/Documents/TPL/GIS/Data/")

salem_ros <- st_read("salem/Calculated", "salem_ROS_class_gridded")
wichita_ros <- st_read("wichita/Calculated/", "wichita_ROS_class_gridded")
pittsburgh_ros <- st_read("pittsburgh/Calculated/", "pittsburgh_ROS_class_gridded")
tucson_ros <- st_read("tucson/Calculated/", "tucson_ROS_class_gridded")
charleston_ros <- st_read("charleston/Calculated/", "charleston_ROS_class_gridded")

# create a column in each sf telling which city it's near
salem_ros <- salem_ros %>% mutate(city = "salem")
wichita_ros <- wichita_ros %>% mutate(city = "wichita")
pittsburgh_ros <- pittsburgh_ros %>% mutate(city = "pittsburgh")
tucson_ros <- tucson_ros %>% mutate(city = "tucson")
charleston_ros <- charleston_ros %>% mutate(city = "charleston")

# make a single sf object with all cities included
five_cities_ros <- rbind(salem_ros, wichita_ros, pittsburgh_ros, tucson_ros, charleston_ros)
#ggplot(five_cities)
st_write(five_cities_ros, "five_cities_AOI_ROS.shp")


######## Combine 5 gridded regions (w/o ROS class) into a single shapefile #####
# to feed to globalrec

salem <- st_read("salem/Calculated", "ROS_gridded")
wichita <- st_read("wichita/Calculated/", "wichita_ROS_gridded")
pittsburgh <- st_read("pittsburgh/Calculated/", "pittsburgh_ROS_gridded")
tucson <- st_read("tucson/Calculated/", "tucson_ROS_gridded")
charleston <- st_read("charleston/Calculated/", "charleston_ROS_gridded")

# create a column in each sf telling which city it's near
salem <- salem %>% dplyr::select(-test) %>% mutate(city = "salem")
wichita <- wichita %>% mutate(city = "wichita")
pittsburgh <- pittsburgh %>% mutate(city = "pittsburgh")
tucson <- tucson %>% mutate(city = "tucson")
charleston <- charleston %>% mutate(city = "charleston")

# make a single sf object with all cities included
five_cities <- rbind(salem, wichita, pittsburgh, tucson, charleston)
#ggplot(five_cities)
st_write(five_cities, "five_cities_AOI.shp")
