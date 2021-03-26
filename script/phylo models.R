####phylogenetic models
#first with genera tree

library(phytools)
library(ape)
library(phyr)

#####load tree

####
max.model.9 <- glmmTMB(log(Distance)~log(ITD)+social_tree+
                         (0+log(ITD)|Metric)+
                         (1|Authors)+
                         (1|Genus_species),
                       family="gaussian",
                       data=forage.traits.max)

cor(fitted(max.model.9),residuals(max.model.9))

##LOAD TREE
bee.trees=read.tree(file="data/hedtke_genera_tree.txt")
##Use tree 1 (376 genera) #Genera-level phylogney
bee.mcmc=bee.trees[[1]]
range(bee.mcmc$edge.length) #all positive
bee.mcmc=root(bee.mcmc,outgroup="Tachysphex")
range(bee.mcmc$edge.length) #all positive
bee.mcmc=as.phylo(bee.mcmc)

bee.cal<-makeChronosCalib(bee.mcmc)
bee.mcmc=chronos(bee.mcmc,calibration = bee.cal)

is.ultrametric(bee.mcmc)
plot(bee.mcmc)
range(bee.mcmc$edge.length)

bee.max.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                    unique(word(forage.traits.max$Genus_species,
                                                1,
                                                sep="_"))))

bee.mean.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(word(forage.traits.mean[-10,]$Genus_species,
                                                    1,
                                                    sep="_"))))
plot(bee.max.tree)
plot(bee.mean.tree)

#species from trait dataframe
max.species=unique(as.character(forage.traits.max$Genus_species))

mean.species=unique(as.character(forage.traits.mean[-10,]$Genus_species))

## Will's suggestion
bee.max.tree$tip.label<-paste(bee.max.tree$tip.label,"_dum",sep="")
for(i in 1:length(max.species)){
  bee.max.tree<-add.species.to.genus(bee.max.tree,max.species[i],
                                 where="root")
}
## prune out these same taxa
ii<-grep("dum",bee.max.tree$tip.label)
bee.max.tree<-drop.tip(bee.max.tree,bee.max.tree$tip.label[ii])

bee.max.tree$node.label=NULL

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

testResiduals(simulateResiduals(max.social2.pglmm))
resids <- DHARMa::simulateResiduals(max.social2.pglmm, plot = FALSE)
#> Warning in checkModel(fittedModel): DHARMa: fittedModel not in class of
#> supported models. Absolutely no guarantee that this will work!
plot(resids)
###RUN MODELS
max.social2.pglmm<-pglmm(log.dist~log.it+
                          social_two+
                          (1|Authors)+
                          (1|Metric)+
                          (log.it|Metric)+
                          (1|Genus_species__),
                        cov_ranef = list(Genus_species = bee.max.tree),
                        family="gaussian",
                        data=forage.traits.max)

max.social3.pglmm<-pglmm(log.dist~log.it+
        social_tree+
        (1|Authors)+
        (1|Metric)+
        (log.it|Metric)+
        (1|Genus_species__),
      cov_ranef = list(Genus_species = bee.max.tree),
      family="gaussian",
      data=forage.traits.max)

max.social2.pglmm$AIC
max.social3.pglmm$AIC

AIC(max.social2.pglmm)
identify(residuals(max.social3.pglmm),
              fitted(max.social3.pglmm))

plot(residuals(max.social3.pglmm),
     fitted(max.social3.pglmm))

summary(max.social3.pglmm)

max.social4.pglmm<-pglmm(log.dist~log.it+
                            social_tree+
                            (1|Authors)+
                            (1|Metric)+
                            (log.it|Metric)+
                            (1|Genus_species__),
                          cov_ranef = list(Genus_species = bee.max.tree),
                          family="gaussian",
                          data=forage.traits.max[-112,])

summary(max.social4.pglmm)

MuMIn::r.squaredGLMM(max.social4.pglmm)

###mean models
mean.social3.pglmm<-pglmm(log.dist~log.it+
                           social_tree+
                           (1|Authors)+
                           (1|Metric)+
                           (log.it|Metric)+
                           (1|Genus_species__),
                         cov_ranef = list(Genus_species = bee.mean.tree),
                         family="gaussian",
                         data=forage.traits.mean[-10,])

mean.social3.pglmm$AIC
AIC(mean.model.9)
