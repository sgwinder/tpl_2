##### Modeling 5 cities two hour drives #####
### Requires output from 2_preparing_predictors


## Side note: useful text about hierarchical models: http://idiom.ucsd.edu/~rlevy/pmsl_textbook/chapters/pmsl_8.pdf

library(tidyverse)
library(ggplot2)
library(lme4)
library(corrgram)

library(rstanarm)
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(tidybayes)
library(ggstance)


modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

su <- summary

setwd("~/Documents/TPL/")
predictors <- read_csv("GIS/Data/five_cities_predictors.csv")

summary(predictors)

# rescale mindist and street_len
### TODO: We need to think about changing the street length calc, since our streets are cropped to
# the extent of the two hour drive polygons, so edge grids are not actually representing total
# lenght of all streets within that gridcell
predictors <- predictors %>% 
  mutate(dist_stand = mindist/max(mindist), 
         street_stand = if_else(is.na(street_len), 0, street_len/max(street_len, na.rm = T)))
predictors$avg_ann_rnd <- round(predictors$avg_ann_ud)


citytp <- "charleston"
corrgram(predictors %>% filter(city == citytp) %>%
           select(log_avg_ann_ud, prop_area, freshwater, ocean, railroad, 
                               wilderness, hmod_mean, dist_stand, street_stand), 
         upper.panel = panel.pts, lower.panel = panel.cor, main = citytp)

#predictors %>% filter(is.na(street_len) )
plot(density(predictors$log_avgann))




## could also do a plot of the salem estimates here vs w/o the other 5 cities (informative?)

### NOTE: Above (now below) code is inappropriate because it forces the grand slope for each parameter to
# be 0. More correct code is here:

nb_mod_bayes_2 <- stan_glmer.nb(avg_ann_rnd ~ ocean + railroad + wilderness +
                                  dist_stand + street_stand + hmod_mean  + freshwater + prop_area +
                                  (ocean + railroad + wilderness + dist_stand + street_stand + 
                                     hmod_mean  + freshwater + prop_area|city), 
                                data = predictors,
                                link = "log",
                                prior = normal(),
                                prior_intercept = normal(),
                                prior_aux = exponential(),
                                prior_covariance = decov(),
                                adapt_delta = .99)
#pairs(nb_mod_bayes_2, regex_pars = "Sigma")

# write it out
#write_rds(nb_mod_bayes_2, "StanModelRuns/nb_mod_bayes_2_99.rds")
nb_mod_bayes_2 <- read_rds("StanModelRuns/nb_mod_bayes_2_99.rds")

launch_shinystan(nb_mod_bayes_2)
summary(nb_mod_bayes_2)
loo(nb_mod_bayes_2)


# looking at posterior predictions vs observed data
pp_check(nb_mod_bayes_2) + scale_x_continuous(limits = c(0, 10))

### I want to plot the residuals on a map
nb_mod_bayes_output <- tibble(cityxgrid = nb_mod_bayes_2$data$cityxgrid, 
                              avg_ann_rnd = nb_mod_bayes_2$data$avg_ann_rnd,
                              resids_nb_mod_bayes = nb_mod_bayes_2$residuals, 
                              fitted_nb_mod_bayes = nb_mod_bayes_2$fitted.values)

## pulling in a gridded map
library(sf)
ROS_gridded_pid <- st_read("GIS/Data/", "five_cities_AOI_pid")
ROS_gridded_pid <- ROS_gridded_pid %>% mutate(cityxgrid = paste0(city, "x", grid_id))

## joining on the model outputs
cities_mod_output <- ROS_gridded_pid %>% left_join(nb_mod_bayes_output, by = "cityxgrid")
#plot(cities_mod_output %>% filter(city == "salem"))

## writing it out to a shapefile so I can click around in QGIS
#st_write(cities_mod_output, "GIS/Data/nb_mod_bayes_2_output.shp")

## Want: plot that shows overall effects of each predictor

# prep the model chains for plotting
posterior <- as.array(nb_mod_bayes_2)
dimnames(posterior)

mcmc_intervals(posterior, pars = c("(Intercept)", "ocean", "railroad", "wilderness",
                                   "dist_stand", "street_stand", "hmod_mean", "freshwater",
                                   "prop_area"))

## Want: some measure of variability between cities by predictor (just a table of median sd? plot?)

mcmc_intervals(posterior, pars = c("Sigma[city:(Intercept),(Intercept)]",
                                   "Sigma[city:ocean,ocean]",
                                   "Sigma[city:railroad,railroad]",
                                   "Sigma[city:wilderness,wilderness]",
                                   "Sigma[city:dist_stand,dist_stand]",
                                   "Sigma[city:street_stand,street_stand]",
                                   "Sigma[city:hmod_mean,hmod_mean]",
                                   "Sigma[city:freshwater,freshwater]",
                                   "Sigma[city:prop_area,prop_area]"))

## Want: some way of saying whether there seem to be specific differences between the cities and how
# they respond to some set of predictors. Possibly by adding together e.g. Ocean and d[ocean, city:charleston]
# to get an estimate of each city's ocean slope

# let's use tidybayes
get_variables(nb_mod_bayes_2)
group_slopes <- nb_mod_bayes_2 %>% 
  gather_draws(`(Intercept)`, ocean, railroad, wilderness, dist_stand, street_stand,
               hmod_mean, freshwater, prop_area)

city_slopes <- nb_mod_bayes_2 %>%
  spread_draws(b[param, city])

# join them together
combined_slopes <- city_slopes %>% 
  left_join(group_slopes, by = c(".chain", ".iteration", ".draw", "param" = ".variable")) %>%
  mutate(city_est = b + `.value`)

combined_qi <- combined_slopes %>%
  median_qi(city_est)

# creating a grouping variable of whether or not the 95% cred int crosses 0
combined_qi <- combined_qi %>% mutate(sig = if_else(.lower < 0 & .upper > 0, FALSE, TRUE))

## plot it
ggplot(combined_qi) +
  geom_pointintervalh(aes(y = param, x = city_est, col = city, alpha = sig),
                      position = position_dodge2v(height = .8, reverse = T)) +
  scale_color_brewer(palette = "Dark2",
                     name = NULL,
                     breaks = c("city:charleston", "city:pittsburgh",
                                "city:salem", "city:tucson", "city:wichita"),
                     labels = c("Charleston", "Pittsburgh", "Salem",
                                "Tucson", "Wichita")) +
  scale_alpha_discrete(NULL, NULL, NULL) +
  vline_0(col = "gray70") +
  scale_y_discrete(name = "Variable",
                   labels = c("(Intercept)" = "Intercept", 
                              "dist_stand" = "Distance to Big City", 
                              "freshwater" = "Freshwater",
                              "hmod_mean" = "Human Modification", 
                              "ocean" = "Ocean", 
                              "prop_area" = "Area", 
                              "railroad" = "Railroad",
                              "street_stand" = "Street Density", 
                              "wilderness" = "Wilderness")) +
  xlab("Coefficient")

#ggsave("figures/5_city_coef_plot_2.png", width = 6.5, height = 7, unit = "in")

### plot just wilderness
ggplot(combined_qi %>% filter(param == "wilderness")) +
  geom_pointintervalh(aes(y = param, x = city_est, col = city, alpha = sig),
                      position = position_dodge2v(height = .8, reverse = T)) +
  scale_color_brewer(palette = "Dark2",
                     name = NULL,
                     breaks = c("city:charleston", "city:pittsburgh",
                                "city:salem", "city:tucson", "city:wichita"),
                     labels = c("Charleston", "Pittsburgh", "Salem",
                                "Tucson", "Wichita")) +
  scale_alpha_discrete(NULL, NULL, NULL) +
  vline_0(col = "gray70") +
  scale_y_discrete(name = "Variable",
                   labels = c("(Intercept)" = "Intercept", 
                              "dist_stand" = "Distance to Big City", 
                              "freshwater" = "Freshwater",
                              "hmod_mean" = "Human Modification", 
                              "ocean" = "Ocean", 
                              "prop_area" = "Area", 
                              "railroad" = "Railroad",
                              "street_stand" = "Street Density", 
                              "wilderness" = "Wilderness")) +
  xlab("Coefficient")
#ggsave("figures/5_city_coef_plot_2_wilderness.png", width = 5, height = 3.5, unit = "in")


### same for area ###
# why is prop_area's sigma so big?

# because in pittsburgh and wichita, having a greater proportion of the gridcell be public
# is very positively related to visitation, while in the other cities it is less so.
# Likely because the other cities have big tracts of public land which don't see much use

ggplot(combined_qi %>% filter(param == "prop_area")) +
  geom_pointintervalh(aes(y = param, x = city_est, col = city),
                      position = position_dodge2v(height = .8, reverse = T)) +
  scale_color_brewer(palette = "Dark2",
                     name = NULL,
                     breaks = c("city:charleston", "city:pittsburgh",
                                "city:salem", "city:tucson", "city:wichita"),
                     labels = c("Charleston", "Pittsburgh", "Salem",
                                "Tucson", "Wichita")) +
  scale_alpha_discrete(NULL, NULL, NULL) +
  vline_0(col = "gray70") +
  scale_y_discrete(name = "Variable",
                   labels = c("(Intercept)" = "Intercept", 
                              "dist_stand" = "Distance to Big City", 
                              "freshwater" = "Freshwater",
                              "hmod_mean" = "Human Modification", 
                              "ocean" = "Ocean", 
                              "prop_area" = "Area", 
                              "railroad" = "Railroad",
                              "street_stand" = "Street Density", 
                              "wilderness" = "Wilderness")) +
  xlab("Coefficient")
#ggsave("figures/5_city_coef_plot_2_area.png", width = 5, height = 3.5, unit = "in")


# etc



#################### Old things ###########

### incorrect Bayesian models + graphs ###
#### real quick, since the model above had a singular fit, which is probably an issue...
## let's try a bayesian model from rstanarm (https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models)
## Note that this is quick a sloppy, so should probably be done more carefully if I"m going for it later

mod1bayes <- stan_lmer(log_avgann ~ (ocean + railroad + wilderness +
                                       dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), data = predictors)
mod1bayes
# write it out
#write_rds(mod1bayes, "StanModelRuns/lm_mod_bayes.rds")
launch_shinystan(mod1bayes)
# wow, what a cool interface!
# the intercept term seems to be having some issues, and could perhaps be dropped
# But more importantly, it's clear that the normality assumption is pretty wildly inappropriate
# Perhaps we could try with a neg bin...

nb_mod_bayes <- stan_glmer.nb(avg_ann_rnd ~ (ocean + railroad + wilderness +
                                               dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), data = predictors)

nb_mod_bayes
launch_shinystan(nb_mod_bayes)
pp_check(nb_mod_bayes) + scale_x_continuous(limits = c(0, 10))
## this posterior predictive check is way better than the normality assuming one above
# intercepts are still a little weird

## Let's save this model out
#write_rds(nb_mod_bayes, "StanModelRuns/nb_mod_bayes.rds")

# or read it in
nb_mod_bayes <- read_rds("StanModelRuns/nb_mod_bayes.rds")


identical(names(nb_mod_bayes$coefficients),  names(mod1bayes$coefficients))
cbind(nb_mod_bayes$coefficients, mod1bayes$coefficients)

### I want to plot the residuals on a map
nb_mod_bayes_output <- tibble(cityxgrid = nb_mod_bayes$data$cityxgrid, 
                              avg_ann_rnd = nb_mod_bayes$data$avg_ann_rnd,
                              resids_nb_mod_bayes = nb_mod_bayes$residuals, 
                              fitted_nb_mod_bayes = nb_mod_bayes$fitted.values)

## pulling in a gridded mpa
library(sf)
ROS_gridded_pid <- st_read("GIS/Data/", "five_cities_AOI_pid")
ROS_gridded_pid <- ROS_gridded_pid %>% mutate(cityxgrid = paste0(city, "x", grid_id))

## joining on the model outputs
cities_mod_output <- ROS_gridded_pid %>% left_join(nb_mod_bayes_output, by = "cityxgrid")
#plot(cities_mod_output %>% filter(city == "salem"))

## writing it out to a shapefile so I can click around in QGIS
#st_write(cities_mod_output, "GIS/Data/nb_mod_bayes_output.shp")

### Let's make a plot showing the posterior predictive distributions (/95% credible intervals) of 
# e.g. each city's response to water

# prep the model chains for plotting
posterior <- as.array(nb_mod_bayes)
dimnames(posterior)

mcmc_intervals(posterior, regex_pars = "ocean ")
mcmc_intervals(posterior, regex_pars = "Intercept) ")
mcmc_intervals(posterior, regex_pars = "freshwater ")
mcmc_intervals(posterior, regex_pars = "railroad ")
mcmc_intervals(posterior, regex_pars = "hmod_mean ")
mcmc_intervals(posterior, regex_pars = "street_stand ")
mcmc_intervals(posterior, regex_pars = "wilderness ")
mcmc_intervals(posterior, regex_pars = "prop_area ")

mcmc_intervals(posterior, regex_pars = "salem")

mcmc_intervals(posterior, regex_pars = " ")

# TODO: want this graph to be organized by predictor, then show each city's response.
# cities should be color coded

credints <- nb_mod_bayes %>% 
  spread_draws(b[param, city]) %>%
  median_qi(estimate = b)


ggplot(credints, aes(y = param, x = estimate, col = city)) +
  geom_halfeyeh()
#geom_pointrange(ymin = '.lower', ymax = '.upper')#, position = position_nudge(y = -.2))

credints <- credints %>% mutate(sig = if_else(.lower < 0 & .upper > 0, FALSE, TRUE))

ggplot(credints) +
  geom_pointintervalh(aes(y = param, x = estimate, col = city, alpha = sig), 
                      position = position_dodge2v(height = .8, reverse = T)) +
  scale_color_brewer(palette = "Dark2",
                     name = NULL,
                     breaks = c("city:charleston", "city:pittsburgh",
                                "city:salem", "city:tucson", "city:wichita"),
                     labels = c("Charleston", "Pittsburgh", "Salem",
                                "Tucson", "Wichita")) +
  scale_alpha_discrete(NULL, NULL, NULL) +
  vline_0(col = "gray70") +
  scale_y_discrete(name = "Variable",
                   labels = c("Intercept", "Distance to Big City", "Freshwater",
                              "Human Modification", "Ocean", "Area", "Railroad",
                              "Street Density", "Wilderness"))

#ggsave("figures/5city_coef_plot.png", width = 6.5, height = 7, unit = "in")




#################################################################
## Other (non Bayesian) directions

mod1 <- lm(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
             dist_stand + street_stand + hmod_mean, data = predictors)
su(mod1); modplot(mod1)

pairs(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
        mindist + street_len + hmod_mean, data = predictors)

mod2 <- lmer(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
               dist_stand + street_stand + hmod_mean + (1|city), data = predictors)
su(mod2); modplot(mod2)
ranef(mod2)

mod3 <- lm(log_avgann ~ -1 + prop_area + freshwater + ocean + railroad + wilderness +
             dist_stand + street_stand + hmod_mean + city, data = predictors)
su(mod3); modplot(mod3)

## adding random slopes/intercepts for each variable
mod2 <- lmer(log_avgann ~ ocean + railroad + wilderness +
               dist_stand + street_stand + hmod_mean  + freshwater + prop_area +
               (ocean + railroad + wilderness +
                  dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), 
             data = predictors)
su(mod2); modplot(mod2)
ranef(mod2)
predictors$mod2preds <- fitted(mod2)

ggplot(predictors, aes(x = prop_area, y = log_avgann, group = city)) +
  geom_point(alpha = .3) +
  geom_line(aes(y = mod2preds)) +
  facet_wrap(~city)
# too much else going on - I think these lines are so scribbly because there are multiple predictors



## why don't we actually just do one predictor here, to create some plots
mod5 <- lmer(log_avgann ~ hmod_mean + (hmod_mean|city), data = predictors)
su(mod5)
ranef(mod5)
predictors$mod5preds <- fitted(mod5)

ggplot(predictors, aes(x = hmod_mean, y = log_avgann, group = city)) +
  geom_point(alpha = .3) +
  geom_line(aes(y = mod5preds)) +
  facet_wrap(~city)



ggplot(predictors, aes(x = hmod_mean, y = log_avgann)) +
  geom_point(alpha = .3) +
  facet_wrap(~city)

ggplot(predictors, aes(x = street_stand, y = log_avgann)) +
  geom_point(alpha = .3) +
  facet_wrap(~city)

ggplot(predictors, aes(x = dist_stand, y = log_avgann)) +
  geom_point(alpha = .3) +
  facet_wrap(~city)
# should we do max dist on a city by city basis?

ggplot(predictors, aes(x = freshwater, y = log_avgann)) +
  geom_point(alpha = .3) +
  facet_wrap(~city) 

ggplot(predictors) +
  geom_histogram(aes(x = log_avgann)) +
  facet_wrap(~city)

ggplot(predictors) +
  geom_histogram(aes(x = avg_ann_ud^(1/3))) +
  facet_wrap(~city)

corrgram(predictors[, c("log_avgann", "freshwater", "ocean", "railroad", "wilderness", 
                        "dist_stand", "street_stand")],
         upper.panel = panel.pts(col = predictors$city))

## simply doing interaction terms between city and other predictors
mod4 <- lm(log_avgann ~ -1 + freshwater*city + ocean*city + railroad*city +
             wilderness*city + dist_stand*city + street_stand*city, data = predictors)
su(mod4); modplot(mod4)

