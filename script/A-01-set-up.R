###set-up

#run once
library(plyr)
library(dplyr)
library(forcats)
library(colorspace)
library(tidyverse)
library(performance)
library(brms)
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
forage.ranges$dist.km <- forage.ranges$distance/1000
forage.ranges$log.dist<-log(forage.ranges$dist.km)
forage.ranges$dist10<-log10(forage.ranges$dist.km)

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

#body weight
#run B-02-bodysize2-function.R 

forage.spp <- forage.traits %>% 
  filter(!duplicated(species)) %>% 
  select(family,species,itd) %>% 
  mutate(Sex="Female") %>% 
  rename(IT=itd,
         Family=family,
         Species=species)

forage.spp.bm <- bodysize2(forage.spp,type="sex",taxa="bee")

forage.traits.bm <- forage.traits %>% 
  left_join(forage.spp.bm %>% 
              dplyr::select(-c(IT,Family,Sex)),by=c("species" = "Species"))

forage.traits.bm$wgt10 <- log10(forage.traits.bm$wgt)

##Phylogeny setup

#tree
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")

##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]%>%
  root(outgroup="Tachysphex")%>%
  as.phylo

bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal,control=list(tol = 1e-8,
                                                             iter.max = 1e4,
                                                             eval.max = 1e4,
                                                             nb.rate.cat = 10,
                                                     dual.iter.max = 20,
                                                     epsilon = 1e-6))
bee.gen.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                    unique(forage.traits$genus)))
bee.gen.tree$node.label=NULL
bee.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits$genus)))
##add species tips
bee.tree$tip.label<-paste(bee.tree$tip.label,"_dum",sep="")
for(i in 1:length(forage.spp$Species)){
  bee.tree<-add.species.to.genus(bee.tree,forage.spp$Species[i],
                                 where="root")
}
## prune out these same taxa
ii<-grep("dum",bee.tree$tip.label)
bee.tree<-drop.tip(bee.tree,bee.tree$tip.label[ii])

plot(bee.tree)
setdiff(forage.spp$Species,bee.tree$tip.label)

is.ultrametric(bee.tree)
range(bee.tree$edge.length)
bee.tree$node.label=NULL
###READY!
