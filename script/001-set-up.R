###set-up

#run once

library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(colorspace)
library(brms)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(ggstance)
library(ggtext)
library(Manu)
library(ModelMetrics)
library(patchwork)
library(pollimetry)
library(phytools)
library(ape)
library(performance)
library(reshape2)
library(stringr)
library(tidybayes)

##dataframe
forage.ranges <- read.csv("data/range-traits-df.csv",
                      sep=";",dec=",",
                      stringsAsFactors = FALSE)

#log distance and ITD
forage.ranges$log.dist<-log(forage.ranges$distance)

#Aggregate all mean / median / mode / typical estimates
forage.ranges$range.type <- revalue(forage.ranges$range.type,
          c("max_50" = "Typical", #homing 50% 
            "Mode" = "Typical",
            "Mean" = "Typical",
            "Median" = "Typical",
            "Typical" = "Typical",
            "Minimum" = "Observed",
            "max_90" = "Max", #90% 
            "max_95" = "Max")) #95% homing 

#treat NAs of metric/range type as "observed ranges"
forage.ranges$range.type <- ifelse(is.na(forage.ranges$metric)==T,
                                     "Observed",forage.ranges$range.type)
forage.ranges$range.type <- ifelse(is.na(forage.ranges$range.type)==T,
                                   "Observed",forage.ranges$range.type)

#join
forage.ranges2 <- forage.ranges%>%
  filter(!is.na(itd)==T)%>%
  filter(!sex%in%c("Male"))%>%
  filter(!notes%in%c("dispersal distance"))#%>%

forage.traits <- forage.ranges2%>%
  filter(!range.type%in%c("Observed"))

#observed distances
forage.traits.obs <- forage.ranges%>%
  filter(!is.na(itd)==T)%>%
  filter(!sex%in%c("Male"))%>%
  filter(!notes%in%c("dispersal distance"))%>%
  filter(range.type%in%c("Observed"))

#Maximum dataframe
forage.traits.max <- forage.traits%>%
  filter(range.type%in%"Max")

#Typical dataframe
forage.traits.mean <- forage.traits%>%
  filter(range.type%in%"Typical")

##Phylo setup (same as pollimetry)

#tree
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")

##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]%>%
  root(outgroup="Tachysphex")%>%
  as.phylo

bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal)

bee.max.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits.max$genus)))

bee.mean.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                         unique(forage.traits.mean$genus)))

bee.max.tree$node.label=NULL
bee.mean.tree$node.label=NULL


###READY!
