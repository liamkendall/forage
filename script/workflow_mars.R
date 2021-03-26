#Foraging distance meta-analysis

#removed mean estimates for homing as I am unsure what they actually say

#removed males, and queens

#assumed most unknown sex = female
#i.e. homing experiments, nest plant associations in oligo species

#B. flavifrons given ITD of 4 until we get measurement

#libraries
library(plyr)
library(tidyverse)
library(brms)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(ggeffects)

##dataframes
distances <- read.csv("data/species_distances.csv",
                      sep=";",dec=",",
                      stringsAsFactors = FALSE)
colnames(distances)[1] = "Authors"

distances$Distance
str(distances)

#log distances
distances$log.dist <- log(distances$Distance)

#merge with traits
traits <- read.csv("data/species_traits.csv",
                   sep=";",dec=",",
                   stringsAsFactors = FALSE)

#log distance and Itd
distances$log.dist<-log(distances$Distance)
traits$log.it<-log(traits$ITD)

#join
forage.traits <- distances%>%
  left_join(traits[,2:9],by=c("Genus_species"))%>%
  filter(!is.na(ITD)==T)%>%#minus 3 
  filter(!Sex_Imp%in%c("Male","Female_Queen"))%>%#minus 3 
  filter(!distance_type%in% #minus 10
  c("max_50","max_90",
    "max_95","Minimum"))%>%
  filter(!is.na(Metric)==T)



#check


##
forage.traits$distance_type2 <- 
  revalue(forage.traits$distance_type,
          c("Mode" = "Central",
            "Mean" = "Central",
            "Median" = "Central",
            "Typical" = "Central"))


table(forage.traits$distance_type2)

#homing studies - remove means - remove waggle dance for comparative 
forage.traits.2 <- forage.traits%>%
#  filter(!(Metric%in%c("homing")&
#           distance_type2%in%"Central"))%>% #minus 17
  filter(!Metric%in%c("waggle dance"))

#max dataframe 158
forage.traits.max <- forage.traits.2%>%
  filter(distance_type2%in%"Max")

#mean dataframe 111
forage.traits.mean <- forage.traits.2%>%
  filter(distance_type2%in%"Central") %>%
  filter(!Distance==20)

#max and mean solitary
forage.traits.max.sol<-forage.traits.max%>%
  filter(social_two%in%"Solitary")%>%
  filter(!Lecty%in%"unclear")

forage.traits.mean.sol<-forage.traits.mean%>%
  filter(social_two%in%"Solitary")%>%
  filter(!Lecty%in%"unclear")


#mean model

mean.model.4 <- glmmTMB(log(Distance)~log(ITD)+
                         (1|Authors)+
                         (1|Metric)+(1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])

mean.model.5 <- glmmTMB(log(Distance)~log(ITD)+
                         (1|Metric/Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])

mean.model.6 <- glmmTMB(log(Distance)~log(ITD)+
                         (0+log(ITD)|Metric)+(1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])


AIC(mean.model.4,mean.model.5,mean.model.6) #marginal differences # 5 bedt

plotResiduals(simulateResiduals(mean.model.4))
testResiduals(simulateResiduals(mean.model.4))

plotResiduals(simulateResiduals(mean.model.5))
testResiduals(simulateResiduals(mean.model.5))

plotResiduals(simulateResiduals(mean.model.6))
testResiduals(simulateResiduals(mean.model.6))

mean.model.7 <- glmmTMB(log(Distance)~log(ITD)*social_two+
                         (0+log(ITD)|Metric)+(1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])
summary(mean.model.7)

mean.model.8 <- glmmTMB(log(Distance)~log(ITD)+social_two+
                         (0+log(ITD)|Metric)+
                         (1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])

plotResiduals(simulateResiduals(mean.model.8))
testResiduals(simulateResiduals(mean.model.8))

AIC(mean.model.7,
    mean.model.8) #marginal difference

#####
mean.model.9 <- glmmTMB(log(Distance)~log(ITD)+social_tree+
                         (0+log(ITD)|Metric)+
                         (1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.mean[-10,])

summary(mean.model.9)

plotResiduals(simulateResiduals(mean.model.9))
testResiduals(simulateResiduals(mean.model.9))

#check interaction
mean.model.10 <- glmmTMB(log(Distance)~log(ITD)*social_tree+
                          (0+log(ITD)|Metric)+
                          (1|Authors)+
                          (1|Genus_species),
                        family="gaussian",
                        data=forage.traits.mean[-10,])

AIC(mean.model.8,
    mean.model.9,
    mean.model.10) #three groups better


AIC(mean.model.4,
    mean.model.5,
    mean.model.6,
    mean.model.7,
    mean.model.8,
    mean.model.9,
    mean.model.10)



