#models

#set-up
options(brms.backend="cmdstanr")

#maximum models

#Phylo
Agen <- ape::vcv.phylo(bee.max.tree)
#func vs physio models

max.func.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

max.IT.brm <- brm(log.dist~ITD+
                    (1|Authors)+
                    (1|Metric2)+
                    (1|spp)+
                    (1|gr(Genus, cov = Agen)),
                  data2 = list(Agen = Agen),
                  cores=4,
                  iter = 2000,
                  prior = max.func.priors,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.max)

#physio vs functional model
max.func.phylo.brm<- brm(log.dist~ITD*limits+
                           (1|Authors)+
                           (1|Metric2)+
                           (1|spp)+
                           (1|gr(Genus, cov = Agen)),
                         data2 = list(Agen = Agen),
                         cores=4,
                         iter = 2000,
                         prior = max.func.priors,
                         family="gaussian",
                         control = list(adapt_delta=0.999,
                                        max_treedepth=15),
                         data=forage.traits.max)

max.func.euc.full.phylo.brm<- brm(log.dist~ITD*limits*social_tree+
                                    (1|Authors)+
                                    (1|Metric2)+
                                    (1|spp)+
                                    (1|gr(Genus, cov = Agen)),
                                  data2 = list(Agen = Agen),
                                  cores=4,
                                  iter = 2000,
                                  prior = max.func.priors,
                                  family="gaussian",
                                  control = list(adapt_delta=0.999,
                                                 max_treedepth=15),
                                  data=forage.traits.max)

#####
##typical models

#Phylo
Bg <- ape::vcv.phylo(bee.mean.tree)

#func model
mean.func.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

mean.IT.brm<- brm(log.dist~ITD+
                    (1|Authors)+
                    (1|Metric2)+
                    (1|spp)+
                    (1|gr(Genus, cov = Bg)),
                  iter = 2000,
                  prior = mean.func.priors,
                  data2 = list(Bg = Bg),
                  cores=4,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.mean)

#physio vs functional model
mean.func.phylo.brm<- brm(log.dist~ITD*limits+
                            (1|Authors)+
                            (1|Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Bg)),
                          data2 = list(Bg = Bg),
                          cores=4,
                          iter = 2000,
                          prior = mean.func.priors,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.mean)

mean.func.euc.full.phylo.brm<- brm(log.dist~ITD*limits*social_tree+
                                     (1|Authors)+
                                     (1|Metric2)+
                                     (1|spp)+
                                     (1|gr(Genus, cov = Bg)),
                                   data2 = list(Bg = Bg),
                                   cores=4,
                                   iter = 2000,
                                   prior = mean.func.priors,
                                   family="gaussian",
                                   control = list(adapt_delta=0.999,
                                                  max_treedepth=15),
                                   data=forage.traits.mean)

icc(mean.IT.brm,by_group=T)
icc(mean.func.phylo.brm,by_group=T)
icc(mean.func.euc.full.phylo.brm,by_group=T)

icc(max.IT.brm,by_group=T)
icc(max.func.phylo.brm,by_group=T)
icc(max.func.euc.full.phylo.brm,by_group=T)

####model performance
model.list <- list(max.IT.brm,
                   max.func.phylo.brm,
                   max.func.euc.full.phylo.brm,
                   mean.IT.brm,
                   mean.func.phylo.brm,
                   mean.func.euc.full.phylo.brm)

model.r2s <- do.call("rbind", lapply(model.list,function(x) r2(x)))
model.r2s
write.csv(summary(max.IT.brm),"max basic.csv")

#save all model objects
#save(max.IT.brm,file="model_outputs/max.IT.brm.rdata")
#save(max.func.phylo.brm,file="model_outputs/max.func.phylo.brm.rdata")
max.euc.model <- max.func.euc.full.phylo.brm
save(max.euc.model,file="model_outputs/max.euc.model.rdata")

#save(mean.IT.brm,file="model_outputs/mean.IT.brm.rdata")
#save(mean.func.phylo.brm,file="model_outputs/mean.func.phylo.brm.rdata")
typ.euc.model <- mean.func.euc.full.phylo.brm
save(typ.euc.model,file="model_outputs/typ.euc.model.rdata")

