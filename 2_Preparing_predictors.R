#### Creating the predictors which we will compare to viz #####
### Requires 1_preparing_polygons to have been run

library(sf)
library(tidyverse)
library(ggplot2)
library(raster)

setwd("/home/sgwinder/Documents/TPL/GIS/Data/")

##### ROS Class Dummy Variables #####
# working with entire 5 city shapefiles

# importing shapefiles from 1_preparing_polygons & from globalrec workflow
# to calculate areas and ROS stats for each gridcell

ROS_gridded_pid <- st_read(".", "five_cities_AOI_pid")
grid <- st_read(".", "five_cities_AOI_grid")
ROS_class_gridded <- st_read(".", "five_cities_AOI_ROS")

## calculating the proportion of each grid that is public land
# subsetting the grid so that only cells which contain public land are included
# creating cityxgrid column
ROS_gridded_pid <- ROS_gridded_pid %>% mutate(cityxgrid = paste0(city, "x", grid_id))
grid <- grid %>% mutate(cityxgrid = paste0(city, "x", grid_id))

occupied_grids <- ROS_gridded_pid$cityxgrid
grid_subset <- filter(grid, cityxgrid %in% occupied_grids)

ROS_gridded_pid$area <- unclass(st_area(ROS_gridded_pid))
ROS_gridded_pid$prop_area <- unclass(ROS_gridded_pid$area/st_area(grid_subset))

#### pulling in PUD data and joining it to area 
avgann <- read.csv("../../pud/userdays_avg_annual_bypid.csv")
ROS_gridded_pid_2 <- left_join(ROS_gridded_pid, avgann, by = "pid")

### calculating area of each ROS class in each grid - not using this directly, but am feeding it
# into the ROS_presence below
ROS_class_gridded$area <- unclass(st_area(ROS_class_gridded))
ROS_class_gridded <- ROS_class_gridded %>% mutate(cityxgrid = paste0(city, "x", grid_id))
ROS_area <- ROS_class_gridded %>% 
  st_set_geometry(NULL) %>%
  spread(key = ros_class, value = area) %>%
  group_by(cityxgrid) %>%
  summarise(Backcountry = sum(Backcountry, na.rm=T), 
            FrontCountryI = sum(`Front Country I`, na.rm=T),
            FrontCountryII = sum(`Front Country II`, na.rm = T),
            MidCountry = sum(`Mid-Country`, na.rm = T),
            Rural = sum(Rural, na.rm = T),
            Urban = sum(Urban, na.rm = T),
            Total_area = sum(Backcountry, FrontCountryI, FrontCountryII, MidCountry,
                             Rural, Urban)) %>%
  right_join(ROS_gridded_pid_2, by = "cityxgrid") %>%
  mutate(log_avg_ann_ud = log(avg_ann_ud + 1))

ROS_presence <- ROS_area %>% 
  mutate_at(vars(Backcountry, FrontCountryI, FrontCountryII, MidCountry, 
                 Rural, Urban), 
            function(x) as.integer(x != 0))

############ Water/oceans ######################



###### Human Modification #####
cities <- c("wichita", "pittsburgh", "tucson", "charleston", "salem")
mean_hmod <- tibble(hmod_mean = NA_real_, cityxgrid = NA_character_)

for(city in cities){ # start 10:29 for 5 cities
  #city <- "wichita"
  setwd(paste0("/home/sgwinder/Documents/TPL/GIS/Data/", city))
  hmod <- raster(paste0(city, "_human_mod.tif"))
  city_grid <- st_read("Calculated/", paste0(city, "_ROS_gridded"))
  
  # reproject into wgs 84
  # grab projection info from city_grid
  newproj <- projection(city_grid)
  hmod_84 <- projectRaster(hmod, crs =  newproj)
  
  # convert ROS_gridded sf to a spatial dataframe
  city_grid_sp <- as_Spatial(city_grid)
  
  # calculating the mean hmod for each polygon
  city_mean_hmod <- raster::extract(hmod_84, city_grid_sp, small = TRUE, 
                                    fun = mean, na.rm = TRUE, sp = TRUE)
  #st_write(st_as_sf(city_mean_hmod), paste0("Calculated/", city, "_mean_hmod.shp"))
  
  city_hmod_df <- st_as_sf(city_mean_hmod) %>% 
    st_set_geometry(NULL)  %>%
    mutate(cityxgrid = paste0(city, "x", grid_id))
  
  # manually renaming, since extract seems to call this column diff things at diff times
  colnames(city_hmod_df)[2] <- "hmod_mean"
  city_hmod_df <- dplyr::select(city_hmod_df, hmod_mean, cityxgrid)
   
  mean_hmod <- bind_rows(mean_hmod, city_hmod_df)
}
mean_hmod <- mean_hmod %>% filter(complete.cases(.))

#write_csv(mean_hmod, "/home/sgwinder/Documents/TPL/GIS/Data/five_cities_mean_hmod.csv")

predictors <- ROS_presence %>% left_join(mean_hmod, by = "cityxgrid") %>% 
  dplyr::select(-grid_id, -city, -area)



