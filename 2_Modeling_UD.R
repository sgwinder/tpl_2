##### Modeling Userdays ######

# Note: code below allows for calculation of proporation of gridcell that is public land,
# but I don't think this is necessary. We should be able to use area directly, since it 
# will only measure the area of the public lands within the grid

# Then again, maybe prop area is good because it better reflects the fact that our polygon
# designations are grid-based rather than meaning-based. Also, using proportion of area means
# that we can directly compare the coefficient estimates because they are then on the same scale

library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(coefplot)

setwd("/Users/macbook/Documents/Work/Recreation/TPL")

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
plot(ROS_gridded_pid$area ~ ROS_gridded_pid$prop_area)     
#hist(ROS_gridded_pid$prop_area)

#### pulling in PUD data and relating it to area 
avgann <- read.csv("tpl/PUD_out/userdays_avg_annual_bypid.csv")
ROS_gridded_pid_2 <- left_join(ROS_gridded_pid, avgann, by = "pid")

#plot(avg_ann_ud ~ prop_area, data = ROS_gridded_pid_2)
#ggplot(data = ROS_gridded_pid_2, aes(x = prop_area, y = avg_ann_ud)) + geom_point()
ggplot(data = ROS_gridded_pid_2, aes(x = area, y = avg_ann_ud)) + geom_point()
ggplot(data = ROS_gridded_pid_2, aes(x = area, y = log(avg_ann_ud + 1))) +
  geom_point()

summary(lm(avg_ann_ud ~ area, data = ROS_gridded_pid_2))
summary(lm(log(avg_ann_ud + 1) ~ area, data = ROS_gridded_pid_2))
# not much of a relationship on its own

#### incorporating ROS classes ####
# calculating area of each class/cell combo
ROS_class_gridded$area <- unclass(st_area(ROS_class_gridded))
ROS_class_gridded$ros_numeric <- recode(ROS_class_gridded$ros_class, Backcountry = 6, `Mid-Country` = 5, 
       `Front Country II` = 4, `Front Country I` = 3, Rural = 2, Urban = 1)
ROS_class_gridded$weighted_score <- ROS_class_gridded$area * ROS_class_gridded$ros_numeric

Wildness_gridded <- ROS_class_gridded %>% 
  st_set_geometry(NULL) %>%
  group_by(grid_id) %>%
  summarise(public_area = sum(area), wildness_score = (sum(weighted_score)/sum(area)))

ROS_calculated <- left_join(ROS_gridded_pid_2, Wildness_gridded, by = "grid_id")

# note that our two area columns are slightly different. Consider looking into why
plot(ROS_calculated$area ~ ROS_calculated$public_area)
ggplot() +
  geom_point(data = ROS_calculated, aes(x=public_area, y = log(avg_ann_ud + 1),
             color = wildness_score))

plot(log(ROS_calculated$avg_ann_ud+1) ~ ROS_calculated$wildness_score)
wildness_model <- lm(log(avg_ann_ud + 1) ~ area + wildness_score, data = ROS_calculated)
summary(wildness_model)
plot(wildness_model)

# making some predictions and adding them to plot
area_seq <- seq(min(ROS_calculated$area), max(ROS_calculated$area), length = 100)
pred_wild = data.frame(area = area_seq, wild_1 = NA, wild_2 = NA, wild_3 = NA, 
                       wild_4 = NA, wild_5 = NA, wild_6 = NA)
for(i in 1:6){
  pred_wild[,i+1] <- predict(wildness_model, newdata = list(area = area_seq, wildness_score = rep(i, 100)))
}
pred_wild_long <- pred_wild %>% gather(key = "wild_score", value = "predictions", -area)
pred_wild_long
ggplot() + 
  geom_line(data = pred_wild_long, aes(x = area, y = predictions, linetype = wild_score)) +
  geom_point(data = ROS_calculated, aes(x=public_area, y = log(avg_ann_ud + 1)))

##### Let's try figuring out area of each ROS class in each polygon as a predictor ####

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

ROS_area_model <- lm(log_avg_ann_ud ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
     Urban + Total_area, data = ROS_area)
summary(ROS_area_model)
plot(ROS_area_model)

pairs(ROS_area[,c(13, 2:8)])


#### What about the presence/absence approach for each ROS class? ####
ROS_presence <- ROS_area %>% 
  mutate_at(vars(Backcountry, FrontCountryI, FrontCountryII, MidCountry, 
                    Rural, Urban), 
               function(x) as.integer(x != 0))
  
ROS_dummy_model <- lm(log_avg_ann_ud ~ Backcountry + MidCountry  + FrontCountryII + FrontCountryI + Rural + 
             Urban + prop_area, data = ROS_presence)
summary(ROS_dummy_model)
plot(ROS_dummy_model)

coefplot(ROS_dummy_model, lwdInner = 0, lwdOuter = .5)


############### 2 step process, p/a then abundance #################
# 1. Are there any PUDs?

ROS_binary <- ROS_presence %>% 
  mutate(avg_ann_ud, photo = as.integer(avg_ann_ud != 0))
mean(ROS_binary$photo)

PUD_logit_model <- glm(photo ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
              Urban + Total_area, data = ROS_binary, family = binomial)
summary(PUD_logit_model)
#plot(PUD_logit_model)
plot(photo ~ Total_area, data = ROS_binary)
ggplot() + geom_point(data = ROS_binary, aes(x=Total_area, y =photo, color = Urban))

# 2. Where there are, what predicts how many?
ROS_photos <- ROS_presence %>%
  filter(avg_ann_ud > 0)
ROS_dummy_present_mod <- lm(log(avg_ann_ud) ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
             Urban + Total_area, data = ROS_photos)
summary(ROS_dummy_present_mod)
plot(ROS_dummy_present_mod)
# qq plot does look a lot better without the 0s (if we don't add 1 before taking the log)

# visualizing, creating new column of 

ggplot() + geom_point(data = ROS_photos, aes(x = Total_area, y = log(avg_ann_ud),
                      color = Backcountry))

ggplot() + geom_histogram(data = ROS_photos, aes(x=log(avg_ann_ud + 1)))
summary(ROS_photos$avg_ann_ud)











######################################################
########### Various junk and trial code #############

#### can we do the 2 step process above all in one (hurdle /zero inflated model?)
library(pscl)
hurdle() # bleh. this wants discrete data

# let's try a dummy model with an exponential distribution
mod_test <- glm(avg_ann_ud ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
     Urban + Total_area, data = ROS_presence, family = Gamma(link = "identity"))


MASS::fitdistr(ROS_presence$avg_ann_ud, "exponential")

# a function from the internet supposed to create qqplot texting for exponential dist
qqexp <-  function(y, line=FALSE, ...) { 
  y <- y[!is.na(y)]
  n <- length(y)
  x <- qexp(c(1:n)/(n+1))
  m <- mean(y)
  if (any(range(y)<0)) stop("Data contains negative values")
  ylim <- c(0,max(y))
  qqplot(x, y, xlab="Exponential plotting position",ylim=ylim,ylab="Ordered sample", ...)
  if (line) abline(0,m,lty=2)
  invisible()
}

qqexp((ROS_presence$avg_ann_ud), line=TRUE)
# doesn't look appropriate. Also, is this just the same as the log transform we've been doing?
qqnorm(log(ROS_presence$avg_ann_ud+1))

qqexp(ROS_presence$avg_ann_ud[ROS_presence$avg_ann_ud > 0])
hist(log(ROS_presence$avg_ann_ud[ROS_presence$avg_ann_ud > 0]))

# i'm not very happy with the log transform - let's see about using a glm instead
mod2 <- glm(avg_ann_ud ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
              Urban + Total_area, data = ROS_area, family = poisson())
# oops, avg_ann_ud is not discrete
summary(ROS_area$avg_ann_ud)
ggplot() + geom_histogram(data = ROS_area, aes(avg_ann_ud), binwidth = .01) +
  xlim(-.5,.5)
ggplot() +
  geom_histogram(data = ROS_area, aes(log_avg_ann_ud))


mod5 <- lm(avg_ann_ud + 1~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
             Urban + Total_area, data = ROS_area)
library(car)
boxCox(mod5)
ggplot() +
  geom_histogram(data = ROS_area, aes((avg_ann_ud + 1)^(-1/2)))
# yep, still doesn't look normal

mod3 <-glm(avg_ann_ud + .001 ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
             Urban + Total_area, data = ROS_area, family = Gamma)

### thoughts for dealing with 0s
# rerun with bigger grid cells
# drop 0s
# note that adding different small amounts before log transform changes results

### exponential model?
mod6 <- glm(avg_ann_ud + 1 ~ Backcountry + FrontCountryI + FrontCountryII + MidCountry + Rural + 
              Urban + Total_area, data = ROS_presence, family = Gamma)
summary(mod6)
plot(mod6)

plot(density(avgann$avg_ann_ud))
