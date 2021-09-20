#lecty models
bee.max.sol.tree<-drop.tip(bee.max.tree,setdiff(bee.max.tree$tip.label,
                                                forage.traits.max.sol$Genus))
bee.max.sol.tree$node.label=NULL

Mlg <- ape::vcv.phylo(bee.max.sol.tree)

forage.traits.max.sol$Lecty <- relevel(factor(forage.traits.max.sol$Lecty),ref="Polylectic")


max.lecty.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,2),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

max.lecty.phylo.brm.1<- brm(log.dist~ITD*Lecty+
                              (ITD|Metric2)+
                              (1|Authors)+
                              (1|spp)+
                              (1|gr(Genus, cov = Mlg)),
                            prior = max.lecty.priors,
                            iter = 4000,
                            data2 = list(Mlg = Mlg),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.999,
                                           max_treedepth=15),
                            data=forage.traits.max.sol)

max.lecty.phylo.brm.2<- brm(log.dist~ITD+Lecty+
                              (ITD|Metric2)+
                              (1|Authors)+
                              (1|spp)+
                              (1|gr(Genus, cov = Mlg)),
                            prior = max.lecty.priors,
                            iter = 4000,
                            data2 = list(Mlg = Mlg),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.999,
                                           max_treedepth=15),
                            data=forage.traits.max.sol)


max.lecty.phylo.brm.3<- brm(log.dist~ITD*limits*Lecty+
                              (ITD|Metric2)+
                              (1|Authors)+
                              (1|spp)+
                              (1|gr(Genus, cov = Mlg)),
                            prior = max.lecty.priors,
                            iter = 4000,
                            data2 = list(Mlg = Mlg),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.999,
                                           max_treedepth=15),
                            data=forage.traits.max.sol)

max.lecty.phylo.brm.4<- brm(log.dist~ITD+Lecty+limits+
                              (ITD|Metric2)+
                              (1|Authors)+
                              (1|spp)+
                              (1|gr(Genus, cov = Mlg)),
                            prior = max.lecty.priors,
                            iter = 4000,
                            data2 = list(Mlg = Mlg),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.999,
                                           max_treedepth=15),
                            data=forage.traits.max.sol)

#mean



bee.mean.sol.tree<-drop.tip(bee.mean.tree,setdiff(bee.mean.tree$tip.label,
                                                  forage.traits.mean.sol$Genus))
bee.mean.sol.tree$node.label=NULL
plot(bee.mean.sol.tree)

Melg <- ape::vcv.phylo(bee.mean.sol.tree)

forage.traits.mean.sol$Lecty <- relevel(factor(forage.traits.mean.sol$Lecty),ref="Polylectic")

mean.lecty.priors <- prior(normal(0,5),class="Intercept")+
  prior(normal(0,2),class="b")+
  #prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

mean.lecty.phylo.brm.1<- brm(log.dist~ITD*Lecty+
                               (ITD|Metric2)+
                               (1|Authors)+
                               (1|spp)+
                               (1|gr(Genus, cov = Melg)),
                             prior = mean.lecty.priors,
                             iter = 4000,
                             data2 = list(Melg = Melg),
                             cores=4,
                             family="gaussian",
                             control = list(adapt_delta=0.999,
                                            max_treedepth=15),
                             data=forage.traits.mean.sol)

mean.lecty.phylo.brm.2<- brm(log.dist~ITD+Lecty+
                               (ITD|Metric2)+
                               (1|Authors)+
                               (1|spp)+
                               (1|gr(Genus, cov = Melg)),
                             prior = mean.lecty.priors,
                             iter = 4000,
                             data2 = list(Melg = Melg),
                             cores=4,
                             family="gaussian",
                             control = list(adapt_delta=0.999,
                                            max_treedepth=15),
                             data=forage.traits.mean.sol)

mean.lecty.phylo.brm.3<- brm(log.dist~ITD*limits*Lecty+
                               (ITD|Metric2)+
                               (1|Authors)+
                               (1|spp)+
                               (1|gr(Genus, cov = Melg)),
                             prior = mean.lecty.priors,
                             iter = 4000,
                             data2 = list(Melg = Melg),
                             cores=4,
                             family="gaussian",
                             control = list(adapt_delta=0.999,
                                            max_treedepth=15),
                             data=forage.traits.mean.sol)

mean.lecty.phylo.brm.4<- brm(log.dist~ITD+Lecty+limits+
                               (ITD|Metric2)+
                               (1|Authors)+
                               (1|spp)+
                               (1|gr(Genus, cov = Melg)),
                             prior = mean.lecty.priors,
                             iter = 4000,
                             data2 = list(Melg = Melg),
                             cores=4,
                             family="gaussian",
                             control = list(adapt_delta=0.999,
                                            max_treedepth=15),
                             data=forage.traits.mean.sol)
