###set-up

#run once

library(stringr)
library(brms)
library(plyr)
library(dplyr)
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(pollimetry)
library(phytools)
library(ape)
library(performance)
library(kableExtra)
library(tidybayes)
library(ggstance)
library(colorspace)
library(ggpubr)

##dataframes

distances <- read.csv("data/species_distances_final.csv",
                      sep=";",dec=",",
                      stringsAsFactors = FALSE)
colnames(distances)[1] = "Authors"

distances$spp<-distances$Genus_species
distances$obs<-1:nrow(distances)

#merge with traits
traits <- read.csv("data/species_traits_cs.csv",
                   sep=";",dec=",",
                   stringsAsFactors = FALSE)

data("pollimetry_dataset")
traits[traits$Genus_species%in%"Megachile_femorata","ITD"] = mean(pollimetry_dataset[pollimetry_dataset$Genus%in%"Megachile" & pollimetry_dataset$Sex%in%"Female","IT"])

#M. femorata = itd = average of other Megachile from pollimetry dataset

#log distance and ITD
distances$log.dist<-log(distances$Distance)
traits$log.it<-log(traits$ITD)


distances$Genus<-word(distances$Genus_species,1,sep="_")

distances$limits <- ifelse(distances$Metric2%in%c("feeder","homing"),"physio","funct")
#join
forage.traits <- distances%>%
  left_join(traits[,-1],by=c("Genus_species"))%>%
  filter(!is.na(ITD)==T)%>%#minus 3 
  filter(!Sex_Imp%in%c("Male"))%>%#minus 3 
  filter(!Notes1%in%c("dispersal distance"))%>%
  filter(!Metric2%in%c(NA))%>%
  filter(!distance_type2%in%c("Observed"))%>%
  mutate(Sex_Imp=revalue(Sex_Imp,c("Female_Queen" = "Female"))) #one study with queen foraging so assumed the same

#observed distances
forage.traits.obs <- distances%>%
  left_join(traits[,-1],by=c("Genus_species"))%>%
  filter(!is.na(ITD)==T)%>%#minus 3 
  filter(!Sex_Imp%in%c("Male"))%>%#minus 3 
  filter(!Notes1%in%c("dispersal distance"))%>%
  filter(!Metric2%in%c(NA))%>%
  filter(distance_type2%in%c("Observed"))

#Removed males, queens
#Minimum is pollen mapping study with "minimum distance to a given pollen source" = incompatible

#Aggregate all mean / median / mode / typical estimates
#forage.traits$distance_type2 <- 
#  revalue(forage.traits$distance_type,
#          c("max_50" = "Central", #homing 50% 
##            "Mode" = "Central",
##            "Mean" = "Central",
#            "Median" = "Central",
#            "Typical" = "Central",
##            "max_90" = "Max", #90% 
#            "max_95" = "Max")) #95% homing 

#max dataframe 158
forage.traits.max <- forage.traits%>%
  filter(distance_type2%in%"Max")

#mean dataframe 111
forage.traits.mean <- forage.traits%>%
  filter(distance_type2%in%"Typical")

#max and mean solitary
forage.traits.max.sol<-forage.traits.max%>%
  filter(social_tree%in%"Solitary")%>%
  filter(!Lecty%in%c("unclear",NA))

forage.traits.mean.sol<-forage.traits.mean%>%
  filter(social_tree%in%"Solitary")%>%
  filter(!Lecty%in%c("unclear",NA))

##phylo setup (same as pollimetry)

#tree
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")

##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]%>%
  root(outgroup="Tachysphex")%>%
  as.phylo

bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal)

bee.max.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits.max$Genus)))

bee.mean.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                         unique(forage.traits.mean$Genus)))

bee.max.tree$node.label=NULL
bee.mean.tree$node.label=NULL

#species from trait dataframe
max.species=unique(as.character(forage.traits.max$Genus_species))
mean.species=unique(as.character(forage.traits.mean$Genus_species))
all.species=unique(as.character(forage.traits$Genus_species))

bee.max.spp.tree <- bee.max.tree
bee.mean.spp.tree <- bee.mean.tree

bee.all.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits$Genus)))

## Will's suggestion
bee.all.tree$tip.label<-paste(bee.all.tree$tip.label,"_xyz",sep="")
for(i in 1:length(all.species)){
  bee.all.tree<-add.species.to.genus(bee.all.tree,all.species[i],
                                     where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.all.tree$tip.label)
bee.all.tree<-drop.tip(bee.all.tree,bee.all.tree$tip.label[ii])
bee.all.tree$node.label=NULL


## Will's suggestion
bee.max.spp.tree$tip.label<-paste(bee.max.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(max.species)){
  bee.max.spp.tree<-add.species.to.genus(bee.max.spp.tree,max.species[i],
                                         where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.max.spp.tree$tip.label)
bee.max.spp.tree<-drop.tip(bee.max.spp.tree,bee.max.spp.tree$tip.label[ii])
bee.max.spp.tree$node.label=NULL

#mean
bee.mean.spp.tree$tip.label<-paste(bee.mean.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(mean.species)){
  bee.mean.spp.tree<-add.species.to.genus(bee.mean.spp.tree,mean.species[i],
                                          where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.mean.spp.tree$tip.label)
bee.mean.spp.tree<-drop.tip(bee.mean.spp.tree,bee.mean.spp.tree$tip.label[ii])
bee.mean.spp.tree$node.label=NULL

bee.max.spp.tree
bee.mean.spp.tree