#max / mean eusociality model for reporting to others

#libraries
library(plyr)
library(tidyverse)
library(brms)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(ggeffects)
library(phyr)

#three levels of eusociality

###intro plots
##Metric
ggplot(forage.traits.max,aes(x=ITD,y=Distance,col=Metric))+
  geom_point(size=3)+theme_bw()+
  stat_smooth(method="lm")+
  facet_wrap(~Metric)
##
#need to remove pollen mapping and host plant association or aggregate with nest plant association ("associative studies")

forage.traits.max$Metric2 <- revalue(forage.traits.max$Metric,
                                     c("pollen mapping" = "associative",
                                       "host plant association" = "associative",
                                       "nest plant association" = "associative"))

table(forage.traits.max$Metric2)

ggplot(forage.traits.max,aes(x=ITD,y=Distance,col=social_tree))+
  geom_point(size=3)+theme_bw()+
  stat_smooth(method="lm")+
  facet_wrap(~Metric2)+
  ylim(0,25000)

##
ggplot(forage.traits.max,aes(x=ITD,y=Distance,col=social_tree))+
  geom_point(size=3)+theme_bw()+
  stat_smooth(method="lm")+
  facet_wrap(~social_tree)

#####
#random slope with 0 intercept
euc.tree.max.model.rslope<- glmmTMB(log.dist~log.it+social_tree+
                         (0+log.it|Metric2)+
                         (1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.max)

#random slope and intercept
euc.tree.max.model.int.rslope<- glmmTMB(log.dist~log.it+social_tree+
                                          (1+log.it|Metric2)+
                                          (1|Authors)+
                                          (1|Genus_species),
                                        family="gaussian",
                                        data=forage.traits.max)

#compare random effects
rslope.ranef <- ranef(euc.tree.max.model.rslope)
rslope.int.ranef <- ranef(euc.tree.max.model.int.rslope)

rslope.ranef$cond$Metric2
rslope.int.ranef$cond$Metric2

euc.tree.max.model.nested<- glmmTMB(log.dist~log.it+social_tree+
                                      (1|Metric2/Authors)+
                                      (1|Genus_species),
                                    family="gaussian",
                                    data=forage.traits.max)

euc.tree.max.model.crossed<- glmmTMB(log.dist~log.it+social_tree+
                                      (1|Metric2)+(1|Authors)+
                                      (1|Genus_species),
                                    family="gaussian",
                                    data=forage.traits.max)

euc.tree.max.model.nested.species<- glmmTMB(log.dist~log.it+social_tree+
                                       (1|Metric2/Genus_species)+(1|Authors),
                                     family="gaussian",
                                     data=forage.traits.max)

#Metric2 in the fixed
euc.tree.max.model.fixed<- glmmTMB(log.dist~log.it+social_tree+Metric2+(1|Authors)+
                                       (1|Genus_species),
                                     family="gaussian",
                                     data=forage.traits.max)

euc.tree.max.model.fixed.interaction<- glmmTMB(log.dist~log.it+social_tree+Metric2+
                                                 log.it:Metric2+
                                                 (1|Authors)+
                                     (1|Genus_species),
                                   family="gaussian",
                                   data=forage.traits.max)
#Residuals tests

testResiduals(simulateResiduals(euc.tree.max.model.rslope))
testResiduals(simulateResiduals(euc.tree.max.model.int.rslope))
testResiduals(simulateResiduals(euc.tree.max.model.nested))
testResiduals(simulateResiduals(euc.tree.max.model.crossed))

#all ok

#compare AIC
AIC(euc.tree.max.model.rslope,
    euc.tree.max.model.int.rslope,
    euc.tree.max.model.nested,
    euc.tree.max.model.nested.species,
    euc.tree.max.model.crossed,
    euc.tree.max.model.fixed,
    euc.tree.max.model.fixed.interaction)

#random slope with zero intercept best fitting model 
summary(euc.tree.max.model.fixed.interaction)

#plot results with lines for 

plot(ggpredict(euc.tree.max.model.fixed.interaction,
  terms =c("log.it","Metric2"),
  ci.lvl = 0.95,
  type = "re"))+
  geom_point(data=forage.traits.max,
             aes(x=log.it,y=log.dist,col=Metric2),inherit.aes = F)


###
#Hhmmmmmm

library(phytools)
library(ape)
library(phyr)

##LOAD TREE
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")
##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]%>%
  root(outgroup="Tachysphex")%>%
  as.phylo()
bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal)

bee.max.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(word(forage.traits.max$Genus_species,
                                                    1,
                                                    sep="_"))))
bee.mean.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                         unique(word(forage.traits.mean$Genus_species,
                                                     1,
                                                     sep="_"))))
#species from trait dataframe
max.species=unique(as.character(forage.traits.max$Genus_species))
mean.species=unique(as.character(forage.traits.mean[-10,]$Genus_species))

#add species tips
bee.max.spp.tree <- bee.max.tree
bee.max.spp.tree$tip.label<-paste(bee.max.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(max.species)){
  bee.max.spp.tree<-add.species.to.genus(bee.max.spp.tree,max.species[i],
                                     where="root")
}
## prune out these same taxa
ii<-grep("_xyz",bee.max.spp.tree$tip.label)
bee.max.spp.tree<-drop.tip(bee.max.spp.tree,bee.max.spp.tree$tip.label[ii])

bee.max.spp.tree$node.label=NULL

#mean
bee.mean.tree$tip.label<-paste(bee.mean.tree$tip.label,"_xyz",sep="")
for(i in 1:length(mean.species)){
  bee.mean.tree<-add.species.to.genus(bee.mean.tree,mean.species[i],
                                      where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.mean.tree$tip.label)
bee.mean.tree<-drop.tip(bee.mean.tree,bee.mean.tree$tip.label[ii])

bee.mean.tree$node.label=NULL


###RUN MODEL
euc.tree.max.model.fixed.phylo<- pglmm(log.dist~log.it+social_tree+Metric2+
                                         (1|Authors)+
                                         (1|Genus_species__),
                                       cov_ranef = list(Genus_species = bee.max.tree),
                                       family="gaussian",
                                       data=forage.traits.max)
euc.tree.max.model.fixed.int.phylo<- pglmm(log.dist~log.it+social_tree+Metric2+
                                                 log.it:Metric2+
                                                 (1|Authors)+
                                                 (1|Genus_species__),
                                         cov_ranef = list(Genus_species = bee.max.tree),
                                         family="gaussian",
                                         data=forage.traits.max)
euc.tree.max.model.fixed.phylo$AIC
euc.tree.max.model.fixed.int.phylo$AIC
