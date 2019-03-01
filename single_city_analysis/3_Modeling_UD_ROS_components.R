##### Incorporating more pieces (components of ROS class into the TPL Salem, OR model) #####

### Copied over from 2_Modeling_UD.R ###

library(sf)
library(tidyverse)
library(ggplot2)
library(coefplot)
library(corrgram)

setwd("Documents/TPL/")

# creating a function for plotting model diagnostics
modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

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

###### Modeling UD ~ ROS + Area, where each ROS class is a dummy variable #######
ROS_presence <- ROS_area %>% 
  mutate_at(vars(Backcountry, FrontCountryI, FrontCountryII, MidCountry, 
                 Rural, Urban), 
            function(x) as.integer(x != 0))

ROS_dummy_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + FrontCountryI + Rural + 
                        Urban + prop_area, data = ROS_presence)
summary(ROS_dummy_model)
modplot(ROS_dummy_model)

coefplot(ROS_dummy_model, lwdInner = 0, lwdOuter = .5)


#####################################################################
#### Let's incorporate some other potential predictors! 
#####################################################################

### Human modification
## HMod_* are the stats calculated from the "human modification" raster provided by TPL by grid polygon

# let's look at HMod_mean a bit
summary(ROS_presence$HMod_mean)
ROS_presence %>% filter(is.na(HMod_mean))
# these 3 polygons are all tiny slivers with no UD. Let's drop them
ROS_presence <- ROS_presence %>% filter(!is.na(HMod_mean))

# HMod_mean is probably the best
plot(ROS_presence$HMod_mean, ROS_presence$HMod_media)
plot(ROS_presence$HMod_mean, ROS_presence$prop_area)

# definitely a relationship there, but what's the correlation?
cor(ROS_presence$HMod_mean, ROS_presence$prop_area)
corrgram::corrgram(ROS_presence)

# let's just throw it in to the previous model
ROS_HMod_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + FrontCountryI + Rural + 
                        Urban + prop_area + HMod_mean, data = ROS_presence)
summary(ROS_HMod_model)
modplot(ROS_HMod_model)
coefplot(ROS_HMod_model, lwdInner = 0, lwdOuter = .5)
# very interesting. And a better fit than without... but I suspect that we will need to remove
# ROS class from our models once we start feeding them all the ROS components as predictors

###################################################
##### Let's incorporate something about water ####

waterbodies <- st_set_geometry(st_read("GIS/Data/Calculated", "water_areas_intersect"), NULL)
waterlines <- st_set_geometry(st_read("GIS/Data/Calculated", "water_lines_intersect"), NULL)
ocean <- st_set_geometry(st_read("GIS/Data/Calculated", "ocean_intersect"), NULL)

# for the waterbodies and waterlines (rivers & creeks), the imported datasets are those raw .shp files
# intercepted with the globalrec polygons. For this, let's just look at whether or not either a 
# waterbody or a waterline is present in the pid

# the ocean dataset was intercepted with the entire grid, so just says whether or not the GRID included
# coastline
waterbody_summary <- waterbodies %>% group_by(grid_id) %>% summarise(waterbody = TRUE)
waterlines_summary <- waterlines %>% group_by(grid_id) %>% summarise(waterline = TRUE)
ocean_summary <- ocean %>% group_by(grid_id) %>% summarise(ocean = TRUE)

ROS_water <- ROS_presence %>% 
  left_join(waterbody_summary, by = "grid_id") %>%
  left_join(waterlines_summary, by = "grid_id") %>%
  left_join(ocean_summary, by = "grid_id") %>%
  mutate(water = !(is.na(waterbody) & is.na(waterline)), ocean = !is.na(ocean)) %>%
  select(-waterbody, -waterline, -HMod_count, -HMod_sum, -HMod_media)

ROS_HMod_water_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + FrontCountryI + Rural + 
                       Urban + prop_area + HMod_mean + water + ocean, data = ROS_water)
summary(ROS_HMod_water_model)
modplot(ROS_HMod_water_model)
coefplot(ROS_HMod_water_model)

# what about w/o ROS class?
HMod_water_model <- lm(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean, data = ROS_water)
summary(HMod_water_model)
coefplot(HMod_water_model)
# good. this is looking great.
modplot(HMod_water_model)

pairs(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean, data = ROS_water)

############ let's bring in info about transportation ########
street_len_grid <- read_csv("GIS/Data/Calculated/street_length_by_grid.csv")
# sum is the total length of roads in that grid (not just those in the public lands polygons!!)

railroads <- st_set_geometry(st_read("GIS/Data/Calculated", "railroads_grid_intersect"), NULL) %>% 
  group_by(grid_id) %>% summarise(railroad = TRUE)

ROS_transport <- ROS_water %>% 
  left_join(select(street_len_grid, grid_id, sum), by = "grid_id") %>% 
  rename(tot_street_len = sum) %>%
  left_join(railroads, by = "grid_id") %>%
  mutate(streets_stand = tot_street_len/max(tot_street_len, na.rm = TRUE), 
         railroad = !is.na(railroad))

ROS_transport %>% select(tot_street_len, streets_stand)

HMod_water_transport_model <- lm(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean + 
                                   streets_stand + railroad, data = ROS_transport)
summary(HMod_water_transport_model)
modplot(HMod_water_transport_model)
coefplot(HMod_water_transport_model)

pairs(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean + 
        streets_stand + railroad, data = ROS_transport)
# hmm, streets is very lopsided. Let's check out log transforming it

HMod_water_transport_model_2 <- lm(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean + 
                                   log(tot_street_len) + railroad, data = ROS_transport)
summary(HMod_water_transport_model_2)
modplot(HMod_water_transport_model_2)
pairs(log_avg_ann_ud ~ prop_area + HMod_mean + water + ocean + 
        log(tot_street_len) + railroad, data = ROS_transport)
# that's interesting
plot(hist(log(ROS_transport$tot_street_len)))
plot(hist(log(ROS_transport$streets_stand)))
plot(hist(HMod_water_transport_model_2$residuals))
# hmm. log transform looks better for normality but performs much worse in model

ROS_HMod_water_transport_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
                                       FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
                                       water + ocean + streets_stand + railroad, 
                                     data = ROS_transport)
summary(ROS_HMod_water_transport_model)
modplot(ROS_HMod_water_transport_model, ask = F)
coefplot(ROS_HMod_water_transport_model)
corrgram(ROS_transport)
plot(hist(ROS_HMod_water_transport_model$residuals))

###### finally, let's bring in a distance to nearest metro area variable #####
# here I used the NNJoin tool and plugin in QGIS to calculate the minimum distance from each polygon to
# a city of at least 50,000 people. Distances are in decimal degrees, but I will convert to a standardized
# distance anyway, so that this can be comparable

distance <- st_set_geometry(st_read("GIS/Data/Calculated", "dist_to_city_50k"), NULL)

ROS_distance <- ROS_transport %>% left_join(select(distance, grid_id, distance), by = "grid_id") %>%
  mutate(dist_stand = distance/max(distance))
plot(hist(ROS_distance$dist_stand))

ROS_HMod_water_transport_dist_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
                                       FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
                                       water + ocean + streets_stand + railroad + dist_stand, 
                                     data = ROS_distance)
summary(ROS_HMod_water_transport_dist_model)
pairs(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
        FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
        water + ocean + streets_stand + railroad + dist_stand, data = ROS_distance)
plot(ROS_distance$log_avg_ann_ud ~ ROS_distance$dist_stand)
# super interesting - no apparent relationship between UD and distance from city of 50K+

#### let's try now with distance to Portland/Vancouver only (no other cities)

distance_port <- st_set_geometry(st_read("GIS/Data/Calculated", "dist_to_portland"), NULL)

ROS_distance_port <- ROS_transport %>% left_join(select(distance_port, grid_id, distance), by = "grid_id") %>%
  mutate(dist_port_stand = distance/max(distance))

plot(hist(ROS_distance_port$dist_port_stand))

ROS_HMod_water_transport_dist_port_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
                                            FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
                                            water + ocean + streets_stand + railroad + dist_port_stand, 
                                          data = ROS_distance_port)
summary(ROS_HMod_water_transport_dist_port_model)
# cool. that is highly sig
modplot(ROS_HMod_water_transport_dist_port_model)
plot(log_avg_ann_ud ~ dist_port_stand, data = ROS_distance_port)
pairs(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
        FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
        water + ocean + streets_stand + railroad + dist_port_stand, 
      data = ROS_distance_port)
coefplot(ROS_HMod_water_transport_dist_port_model, lwdOuter = .5)

##### that's a good looking model! however, what about multicollinearity?

plot(streets_stand ~ dist_port_stand, data = ROS_distance_port)
cor(ROS_distance_port$streets_stand, ROS_distance_port$dist_port_stand, na.rm = TRUE)
plot(hist(ROS_distance_port$streets_stand))
plot(log_avg_ann_ud ~ streets_stand, data = ROS_distance_port)
plot(HMod_mean ~ streets_stand, data = ROS_distance_port)


############ This is the model we are presenting to TPL 2/7/19 ##########
# let's look at removing railroad since it is insig. 

no_rr_model <-  lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
                     FrontCountryI + Rural + Urban + prop_area + HMod_mean + 
                     water + ocean + streets_stand + dist_port_stand, 
                   data = ROS_distance_port)
summary(no_rr_model)
AIC(no_rr_model, ROS_HMod_water_transport_dist_port_model)

coefplot(no_rr_model, lwdOuter = .5)

final_variables <- ROS_distance_port %>% 
  select(log_avg_ann_ud, Backcountry, MidCountry, FrontCountryII, FrontCountryI,
                             Rural, Urban, prop_area, HMod_mean, water, ocean, streets_stand,
                             dist_port_stand)

corrgram(final_variables, upper.panel = panel.pts, lower.panel = panel.cor, diag.panel = panel.density)
plot(FrontCountryII ~ log_avg_ann_ud, data = ROS_distance_port)

plot(hist(no_rr_model$residuals))

#################################################################

#### What about if we remove urban and frontcountryI and II because of relatively high correlation?
no_uff_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + Rural + prop_area + HMod_mean + 
                     water + ocean + streets_stand + dist_port_stand, 
                   data = ROS_distance_port)
modplot(no_uff_model)         
summary(no_uff_model)
coefplot(no_uff_model)

### Just removing urban? the above model had lower adjusted r^2
no_u_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + 
                   FrontCountryI + Rural + prop_area + HMod_mean + 
                   water + ocean + streets_stand + dist_port_stand, 
                 data = ROS_distance_port)
summary(no_u_model)
coefplot(no_u_model)
