#######
### Trust for Public Lands Showcase Project
### Salem, Oregon ROS dataset
#######

library(sf)
library(lwgeom)
library(rgdal)
library(raster)
library(ggplot2)

setwd("/home/sgwinder/Documents/TPL/GIS/Data/Tucson/Calculated/")


#ROS_Salem <- readOGR("GIS/Data/Calculated/", "ROS_WGS_84")
#ROS_Salem@data
#plot(ROS_Salem)
#crs(ROS_Salem)

# reading it in as an sf shape instead for making the grid
ROS_sf <- st_read(".", "tucson_ROS_84")
st_crs(ROS_sf)

########## create a grid ############

# reference: https://gis.stackexchange.com/questions/88830/overlay-a-spatial-polygon-with-a-grid-and-check-in-which-grid-element-specific-c
# cellsize is in degrees, .1
grid_1 <- st_make_grid(ROS_sf, cellsize = c(.1, .1)) %>%
  st_sf(grid_id = 1:length(.))
st_crs(grid_1)
st_write(grid_1, "tucson_grid_1.shp")

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
#st_write(ROS_dis_valid, "tucson_ROS_dissolved_valid.shp")

ROS_gridded <- st_intersection(grid_1, ROS_dis_valid) # started at 4:19, 
# 6 min for tucson on xps
# 4.75 hr total run time for Salem on Mac
# just ran for charleston in <30 sec on xps
#st_write(ROS_gridded, "tucson_ROS_gridded.shp")

#quartz()
#plot(ROS_gridded)

########## Creating an alternative gridded shapefile that retains ROS details ######
# dissolving
ROS_dissolved_features <- ROS_sf %>%
  group_by(ros_class) %>%
  summarise()

plot(ROS_dissolved_features)
ROS_valid <- st_make_valid(ROS_dissolved_features)
st_write(ROS_valid, "ROS_class_diss_valid.shp")

ROS_class_gridded <- st_intersection(grid_1, ROS_valid) 
# started at 3:12 pm, finished at 3:34 - why so much quicker??
st_write(ROS_class_gridded, "ROS_class_gridded.shp")

