#### Creating the predictors which we will compare to viz #####
### Requires 1_preparing_polygons to have been run

library(sf)
library(tidyverse)
library(ggplot2)




##### ROS Class Dummy Variables #####


# importing shapefiles from 1_preparing_polygons & from globalrec workflow
# to calculate areas and ROS stats for each gridcell

ROS_gridded_pid <- st_read("tpl/PUD_out/", "ROS_gridded_pid")
grid <- st_read("tpl/shapefiles/", "grid_1")
ROS_class_gridded <- st_read("tpl/shapefiles/", "ROS_class_gridded")

## calculating the proportion of each grid that is public land
# subsetting the grid so that only cells which contain public land are included
occupied_grids <- ROS_gridded_pid$grid_id
grid_subset <- filter(grid, grid_id %in% occupied_grids)

ROS_gridded_pid$area <- unclass(st_area(ROS_gridded_pid))
ROS_gridded_pid$prop_area <- unclass(ROS_gridded_pid$area/st_area(grid_subset))

#### pulling in PUD data and relating it to area 
avgann <- read.csv("tpl/PUD_out/userdays_avg_annual_bypid.csv")
ROS_gridded_pid_2 <- left_join(ROS_gridded_pid, avgann, by = "pid")

### calculating area of each ROS class in each grid - not using this directly, but am feeding it
# into the ROS_presence below
ROS_class_gridded$area <- unclass(st_area(ROS_class_gridded))
ROS_area <- ROS_class_gridded %>% 
  st_set_geometry(NULL) %>%
  spread(key = ros_class, value = area) %>%
  group_by(grid_id) %>%
  summarise(Backcountry = sum(Backcountry, na.rm=T), 
            FrontCountryI = sum(`Front Country I`, na.rm=T),
            FrontCountryII = sum(`Front Country II`, na.rm = T),
            MidCountry = sum(`Mid-Country`, na.rm = T),
            Rural = sum(Rural, na.rm = T),
            Urban = sum(Urban, na.rm = T),
            Total_area = sum(Backcountry, FrontCountryI, FrontCountryII, MidCountry,
                             Rural, Urban)) %>%
  right_join(ROS_gridded_pid_2, by = "grid_id") %>%
  mutate(log_avg_ann_ud = log(avg_ann_ud + 1))

ROS_presence <- ROS_area %>% 
  mutate_at(vars(Backcountry, FrontCountryI, FrontCountryII, MidCountry, 
                 Rural, Urban), 
            function(x) as.integer(x != 0))



###### Human Modification #####
city <- "wichita"
setwd(paste0("home/sgwinder/Documents/TPL/GIS/Data/", city))

