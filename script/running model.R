####PRELIMINARY MODEL

#libraries
library(ggplot2)
library(brms)
library(plyr)
library(tidyverse)
library(pollimetry)
library(glmmTMB)
library(DHARMa)

#glmmTMB

forage.traits.2 <- forage.traits[!is.na(forage.traits$distance_type)==TRUE,]
forage.traits.3 <- forage.traits.2[!is.na(forage.traits.2$Metric)==TRUE,]%>%droplevels()

#remove some metrics

forage.traits.4 <- forage.traits.3[!forage.traits.3$distance_type%in%
                                     c("max_50","max_90",
                                       "max_95","Minimum"),]

forage.traits.4$distance_type <- revalue(forage.traits.4$distance_type,
                                         c("Mode" = "Central",
                                           "Mean" = "Central",
                                           "Median" = "Central",
                                           "Typical" = "Central"))
#log ITD and distance
forage.traits.4$log.dist <- log(forage.traits.4$Distance)
forage.traits.4$log.it <- log(forage.traits.4$ITD.y)

#Split to Mean and Max dataframes
forage_split <- split(forage.traits.4,forage.traits.4$distance_type)

#Mean model
mean.model <- glmmTMB(Distance~ITD.y+
                        (1|Metric/Authors)+(1|Genus.species),
                      family="gaussian",
                      data=forage_split$Central)

plot(simulateResiduals(mean.model)) #bad


mean.log.model <- glmmTMB(log.dist~log.it+
                            (1|Metric/Authors)+(1|Genus.species),
                          family="gaussian",
                          data=forage_split$Central)

plot(simulateResiduals(mean.log.model))

MuMIn::r.squaredGLMM(mean.log.model)


mean.gam.model <- glmmTMB(Distance~ITD.y+
                            (1|Metric/Authors)+(1|Genus.species),
                          family=Gamma(link="log"),
                          data=forage_split$Central)

plot(simulateResiduals(mean.gam.model))

MuMIn::r.squaredGLMM(mean.gam.model)


mean.log.euc.model <- glmmTMB(log.dist~log.it*Eusociality.y+
                  (1|Metric/Authors)+(1|Genus.species),
                  family="gaussian",
                  data=forage_split$Central)


mean.log.euc.model.2 <- glmmTMB(log.dist~log.it+Eusociality.y+
                                (1|Metric/Authors)+(1|Genus.species),
                              family="gaussian",
                              data=forage_split$Central)


mean.log.euc.model.3 <- glmmTMB(log.dist~log.it+Eusociality.y+
                                (+log.it|Metric)+(1|Authors)+(1|Genus.species),
                              family="gaussian",
                              data=forage_split$Central)

AIC(mean.log.euc.model,
    mean.log.euc.model.2,
    mean.log.euc.model.3)

plot(simulateResiduals(mean.log.euc.model))

mean.gam.euc.model <- glmmTMB(Distance~ITD.y*Eusociality.y+
                                (1|Metric/Authors)+(1|Genus.species),
                              family=Gamma(link="log"),
                              data=forage_split$Central)

plot(simulateResiduals(mean.gam.euc.model))

AIC(mean.log.model,mean.log.euc.model)

AIC(mean.gam.model,mean.gam.euc.model)

summary(mean.log.euc.model)
summary(mean.log.euc.model)

#max model
max.log.model <- glmmTMB(log.dist~log.it+
                       (1|Metric/Authors)+(1|Genus.species),
                     family="gaussian",
                     data=forage_split$Max)

max.gam.model <- glmmTMB(Distance~ITD.y+
                            (1|Metric/Authors)+(1|Genus.species),
                family=Gamma(link="log"),
                data=forage_split$Max)

max.log.euc.model <- glmmTMB(log.dist~log.it*Eusociality.y+
                       (1|Metric/Authors)+(1|Genus.species),
                   family="gaussian",
                     data=forage_split$Max)

max.log.euc.model.2 <- glmmTMB(log.dist~log.it+Eusociality.y+
                               (1|Metric/Authors)+(1|Genus.species),
                             family="gaussian",
                             data=forage_split$Max)

max.log.euc.model.3 <- glmmTMB(log.dist~log.it+Eusociality.y+
                                 (1+log.it|Metric)+(1|Genus.species)+(1|Authors),
                               family="gaussian",
                               data=forage_split$Max)

library(ggeffects)
ggpredict(max.log.euc.model.3,
          terms = c("log.it", "Metric"),
          type="re") %>% plot()

AIC(max.log.euc.model,
    max.log.euc.model.2,
    max.log.euc.model.3)

plot(simulateResiduals(max.log.model))
plot(simulateResiduals(max.gam.model))
plot(simulateResiduals(max.log.euc.model))
plot(simulateResiduals(max.log.euc.model.2))
plot(simulateResiduals(max.log.euc.model.3))

MuMIn::r.squaredGLMM(max.log.euc.model.2)

ggplot(forage_split$Max,aes(x=log.it,y=log.dist,col=Eusociality.y))+
  geom_point()

