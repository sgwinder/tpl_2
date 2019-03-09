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
  right_join(ROS_gridded_pid_2 %>% st_set_geometry(NULL), by = "cityxgrid") %>%
  mutate(log_avg_ann_ud = log(avg_ann_ud + 1))

ROS_presence <- ROS_area %>% 
  mutate_at(vars(Backcountry, FrontCountryI, FrontCountryII, MidCountry, 
                 Rural, Urban), 
            function(x) as.integer(x != 0))

############ Water/oceans ######################
## Note that there are 3 water polygons we are incorporating
## Water lines and water areas represent fresh water, and are intersected with the polygons and
# combined into a single presence/absence score for fresh water in the public lands

water_lines <- st_read("USA_wat/", "USA_water_lines_dcw")
water_bodies <- st_read("USA_wat/", "USA_water_areas_dcw")

# check projection
projection(water_bodies)
projection(water_lines)

water_bodies_int <- st_intersection(ROS_gridded_pid, water_bodies)
water_lines_int <- st_intersection(ROS_gridded_pid, water_lines)

# extracting vectors of cityxgrids that contain freshwater
water_bodies_grids <- c(as.character(water_bodies_int$cityxgrid), 
                        as.character(water_lines_int$cityxgrid))

predictors <- ROS_presence %>% 
  mutate(freshwater = if_else(cityxgrid %in% water_bodies_grids, 1, 0)) %>% 
  dplyr::select(-grid_id, -area)

## Ocean is intersecting with the entire grid, and serves as a proxy for distance to the ocean
ocean <- st_read("water-polygons-split-4326/", "water_polygons")
projection(ocean)

ocean_int <- st_intersection(grid, ocean)
ocean_present <- as.character(ocean_int$cityxgrid)

predictors <- predictors %>% mutate(ocean = if_else(cityxgrid %in% ocean_present, 1, 0))

########## Transportation #####
wgs_84 <- projection(grid)

# Railroads. Every town has a railroads file, so we will first import and combine them
wichita_rail <- st_read("wichita/", "wichita_railroads")
salem_rail <- st_read("salem/", "salem_railroads")
pittsburgh_rail <- st_read("pittsburgh/", "pittsburgh_railroads")
charleston_rail <- st_read("charleston/", "charleston_railroads")
tucson_rail <- st_read("tucson/", "tucson_railroads")

# check projections (all the same, but not wgs_84)
identical(projection(wichita_rail), projection(tucson_rail))

# combine into single dataset
salem_rail_geom <- salem_rail %>% dplyr::select(geometry)
others_rail_geom <- rbind(pittsburgh_rail, charleston_rail, tucson_rail, wichita_rail) %>% 
  dplyr::select(geometry)

railroads <- rbind(salem_rail_geom, others_rail_geom) %>% st_transform(crs = wgs_84)

rail_int <- st_intersection(grid, railroads)
rail_present <- as.character(rail_int$cityxgrid)

predictors <- predictors %>% mutate(railroad = if_else(cityxgrid %in% rail_present, 1, 0))

##### Streets #######
# these are bigger and more complex files, so doing one city at a time
# also, currently projected as albers equal area. We reproject grid to match, so our results 
# are in meters and thus more interpretable and accurate

cities <- c("wichita", "pittsburgh", "tucson", "charleston", "salem")
street_lengths <- tibble(cityxgrid = NA_character_, street_len = NA_real_)

for(city in cities){ # started at 4:50
  streets <- st_read(city, paste0(city, "_streets"))

  street_proj <- st_crs(streets)
  grid_transform <- st_transform(grid, crs = street_proj)
  
  street_int <- st_intersection(grid_transform, streets %>% dplyr::select(geometry))
  
  city_street_lengths <- street_int %>% 
    mutate(length = st_length(street_int)) %>% 
    st_set_geometry(NULL) %>%
    group_by(cityxgrid) %>%
    summarise(street_len = sum(length))
  
  street_lengths <- bind_rows(street_lengths, city_street_lengths)
}

street_lengths <- street_lengths %>% filter(complete.cases(.))

#write_csv(street_lengths, "/home/sgwinder/Documents/TPL/GIS/Data/five_cities_street_lengths.csv")

predictors <- predictors %>% left_join(street_lengths, by = "cityxgrid") 

library(corrgram)
corrgram(predictors[,c("log_avgann", "prop_area", "street_len",
                       "freshwater", "ocean", "railroad")],
         upper.panel = panel.pts, lower.panel = panel.cor, diag.panel = panel.density)
 
###### Human Modification #####
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

predictors <- predictors %>% left_join(mean_hmod, by = "cityxgrid") 



#### still need to incorporate
# Distance to nearest major city
# Transportation (length of streets)
# Land ownership?
# wilderness - none around wichita or pittsburgh

