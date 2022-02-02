###set-up

#run once
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(colorspace)
library(performance)
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
library(reshape2)
library(stringr)
library(tidybayes)
library(grid)
library(gtable)
library(ggh4x)

##dataframe
forage.ranges <- read.csv("data/ranges-df.csv",
                      sep=";",dec=",",
                      stringsAsFactors = FALSE)

#log distance and ITD
forage.ranges$log.dist<-log(forage.ranges$distance)

#Aggregate all mean / median / mode / typical estimates
forage.ranges$range.type <- revalue(forage.ranges$range.type,
          c("Mode" = "Typical",
            "Mean" = "Typical",
            "Median" = "Typical",
            "Typical" = "Typical",
            "Minimum" = "Unclassifiable"))

#treat NAs of metric/range type as "observed ranges"
forage.ranges$range.type <- ifelse(is.na(forage.ranges$metric)==T,
                                     "Unclassifiable",forage.ranges$range.type)
forage.ranges$range.type <- ifelse(is.na(forage.ranges$range.type)==T,
                                   "Unclassifiable",forage.ranges$range.type)

#join
forage.traits <- forage.ranges%>%
  filter(!range.type%in%c("Unclassifiable"))

#observed distances
forage.traits.obs <- forage.ranges%>%
  filter(range.type%in%c("Unclassifiable"))

####

forage.traits$range.4 <- ifelse(forage.traits$range.type%in%"Max"
                                       &forage.traits$measure.type%in%"Realised",
                                      "Max-Realised",
                         ifelse(forage.traits$range.type%in%"Max"
                                       &forage.traits$measure.type%in%"Potential",
                                      "Max-Potential",
                         ifelse(forage.traits$range.type%in%"Typical"
                                      &forage.traits$measure.type%in%"Potential",
                                      "Typ-Potential",
                                "Typ-Realised")))

forage.traits$range.4 <- factor(forage.traits$range.4,
                                levels=c("Typ-Realised",
                                         "Typ-Potential",
                                         "Max-Realised",
                                         "Max-Potential"))

##Phylogeny setup

#tree
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")

##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]%>%
  root(outgroup="Tachysphex")%>%
  as.phylo

bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal)

bee.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits$genus)))


bee.tree$node.label=NULL

###READY!
