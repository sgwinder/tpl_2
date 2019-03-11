##### Modeling 5 cities two hour drives #####
modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

su <- summary


mod1 <- lm(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
             mindist + street_len + hmod_mean, data = predictors)
su(mod1); modplot(mod1)

pairs(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
        mindist + street_len + hmod_mean, data = predictors)
library(lme4)

mod2 <- lmer(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
               mindist + street_len + hmod_mean + (1|city), data = predictors)
su(mod2); modplot(mod2)

mod3 <- lm(log_avgann ~ prop_area + freshwater + ocean + railroad + wilderness +
             mindist + street_len + hmod_mean + city, data = predictors)
su(mod3); modplot(mod3)
