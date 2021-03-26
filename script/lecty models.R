#lecty models

bee.max.sol.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(word(forage.traits.max.sol$Genus_species,
                                                    1,
                                                    sep="_"))))

bee.mean.sol.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                         unique(word(forage.traits.mean.sol$Genus_species,
                                                     1,
                                                     sep="_"))))
plot(bee.max.sol.tree)
plot(bee.mean.sol.tree)

#species from trait dataframe
max.sol.species=unique(as.character(forage.traits.max.sol$Genus_species))

mean.sol.species=unique(as.character(forage.traits.mean.sol$Genus_species))

## Will's suggestion
bee.max.sol.tree$tip.label<-paste(bee.max.sol.tree$tip.label,"_xyz",sep="")
for(i in 1:length(max.sol.species)){
  bee.max.sol.tree<-add.species.to.genus(bee.max.sol.tree,max.sol.species[i],
                                     where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.max.sol.tree$tip.label)
bee.max.sol.tree<-drop.tip(bee.max.sol.tree,bee.max.sol.tree$tip.label[ii])

bee.max.sol.tree$node.label=NULL

#mean.sol
bee.mean.sol.tree$tip.label<-paste(bee.mean.sol.tree$tip.label,"_xyz",sep="")
for(i in 1:length(mean.sol.species)){
  bee.mean.sol.tree<-add.species.to.genus(bee.mean.sol.tree,mean.sol.species[i],
                                      where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.mean.sol.tree$tip.label)
bee.mean.sol.tree<-drop.tip(bee.mean.sol.tree,bee.mean.sol.tree$tip.label[ii])
bee.mean.sol.tree$node.label=NULL

#max lecty pglmm
C <- ape::vcv.phylo(bee.max.sol.tree)
D <- ape::vcv.phylo(bee.mean.sol.tree)

#max lecty brm

max.lecty.priors <- prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

hist(rnorm(1000,mean=0,sd=5))
hist(rnorm(1000,mean=-1,sd=1))

forage.traits.max.sol$spp <- forage.traits.max.sol$Genus_species

max.lecty.phylo.brm.1<- brm(log.dist~log.it*Lecty+
                            (1|Metric2/Authors)+
                            (1|spp)+
                            (1|gr(Genus_species, cov = A)),
                          prior = max.lecty.priors,
                          iter = 2000,
                          data2 = list(A = A),
                          cores=4,
                          family="gaussian",
                          control = list(adapt_delta=0.99,
                                         max_treedepth=15),
                          data=forage.traits.max.sol)


max.lecty.phylo.brm.2<- brm(log.dist~log.it+Lecty+
                              (1|Metric2/Authors)+
                              (1|spp)+
                              (1|gr(Genus_species, cov = A)),
                            prior = max.lecty.priors,
                            iter = 2000,
                            data2 = list(A = A),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.99,
                                           max_treedepth=15),
                            data=forage.traits.max.sol)

max.lecty.phylo.brm.1 <- add_criterion(max.lecty.phylo.brm.1,criterion="loo")
max.lecty.phylo.brm.2 <- add_criterion(max.lecty.phylo.brm.2,criterion="loo")

loo_compare(max.lecty.phylo.brm.1,
            max.lecty.phylo.brm.2)
