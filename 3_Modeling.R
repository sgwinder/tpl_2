##### Modeling 5 cities two hour drives #####
### Requires output from 2_preparing_predictors
library(tidyverse)
library(ggplot2)
library(lme4)
library(corrgram)


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

#predictors %>% filter(is.na(street_len) )
plot(density(predictors$log_avgann))

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
mod2 <- lmer(log_avgann ~ -1 + (ocean + railroad + wilderness +
               dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), data = predictors)
su(mod2); modplot(mod2)
ranef(mod2)
predictors$mod2preds <- fitted(mod2)

ggplot(predictors, aes(x = prop_area, y = log_avgann, group = city)) +
  geom_point(alpha = .3) +
  geom_line(aes(y = mod2preds)) +
  facet_wrap(~city)
# too much else going on - I think these lines are so scribbly because there are multiple predictors

#### real quick, since the model above had a singular fit, which is probably an issue...
## let's try a bayesian model from rstanarm (https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models)
## Note that this is quick a sloppy, so should probably be done more carefully if I"m going for it later
library(rstanarm)
options(mc.cores = parallel::detectCores())
mod1bayes <- stan_lmer(log_avgann ~ (ocean + railroad + wilderness +
                           dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), data = predictors)
mod1bayes
launch_shinystan(mod1bayes)
# wow, what a cool interface!
# the intercept term seems to be having some issues, and could perhaps be dropped
# But more importantly, it's clear that the normality assumption is pretty wildly inappropriate
# Perhaps we could try with a neg bin...
predictors$avg_ann_rnd <- round(predictors$avg_ann_ud)

nb_mod_bayes <- stan_glmer.nb(avg_ann_rnd ~ (ocean + railroad + wilderness +
                                              dist_stand + street_stand + hmod_mean  + freshwater + prop_area|city), data = predictors)






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

