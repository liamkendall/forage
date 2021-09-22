####Predictive foraging range models

#set-up
options(brms.backend="cmdstanr")

####Maximum models

#Phylo VCV matrix
Agen <- ape::vcv.phylo(bee.max.tree)

####Priors

max.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

####Model 1
max.IT.brm <- brm(log.dist~itd+
                    (1|publication)+
                    (1|metric)+
                    (1|species)+
                    (1|gr(genus, cov = Agen)),
                  data2 = list(Agen = Agen),
                  cores=4,
                  iter = 2000,
                  prior = max.priors,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.max)

##Model 2
max.measure.brm<- brm(log.dist~itd*measure.type+
                           (1|publication)+
                           (1|metric)+
                           (1|species)+
                           (1|gr(genus, cov = Agen)),
                         data2 = list(Agen = Agen),
                         cores=4,
                         iter = 2000,
                         prior = max.priors,
                         family="gaussian",
                         control = list(adapt_delta=0.999,
                                        max_treedepth=15),
                         data=forage.traits.max)

#Model 3
max.social.brm<- brm(log.dist~itd*measure.type*sociality+
                                    (1|publication)+
                                    (1|metric)+
                                    (1|species)+
                                    (1|gr(genus, cov = Agen)),
                                  data2 = list(Agen = Agen),
                                  cores=4,
                                  iter = 2000,
                                  prior = max.priors,
                                  family="gaussian",
                                  control = list(adapt_delta=0.999,
                                                 max_treedepth=15),
                                  data=forage.traits.max)

#####
##typical models
#####

#Phylo VCV matrix
Bg <- ape::vcv.phylo(bee.mean.tree)

#pirors
typ.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

#model 1
typ.IT.brm<- brm(log.dist~itd+
                    (1|publication)+
                    (1|metric)+
                    (1|species)+
                    (1|gr(genus, cov = Bg)),
                  iter = 2000,
                  prior = typ.priors,
                  data2 = list(Bg = Bg),
                  cores=4,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.mean)

#model 2
typ.measure.brm<- brm(log.dist~itd*measure.type+
                         (1|publication)+
                         (1|metric)+
                         (1|species)+
                         (1|gr(genus, cov = Bg)),
                       data2 = list(Bg = Bg),
                       cores=4,
                       iter = 2000,
                       prior = typ.priors,
                       family="gaussian",
                       control = list(adapt_delta=0.999,
                                      max_treedepth=15),
                       data=forage.traits.mean)

#model 3
typ.social.brm<- brm(log.dist~itd*measure.type*sociality+
                        (1|publication)+
                        (1|metric)+
                        (1|species)+
                        (1|gr(genus, cov = Bg)),
                      data2 = list(Bg = Bg),
                      cores=4,
                      iter = 2000,
                      prior = typ.priors,
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.mean)
                                   
###posterior predictive checks
pp_check(typ.IT.brm)
pp_check(typ.measure.brm)
pp_check(typ.social.brm)

pp_check(max.IT.brm)
pp_check(max.measure.brm)
pp_check(max.social.brm)

##ICC
icc(typ.IT.brm,by_group=T)
icc(typ.measure.brm,by_group=T)
icc(typ.social.brm,by_group=T)

icc(max.IT.brm,by_group=T)
icc(max.measure.brm,by_group=T)
icc(max.social.brm,by_group=T)

#r2
r2(typ.IT.brm)
r2(typ.measure.brm)
r2(typ.social.brm)

r2(max.IT.brm)
r2(max.measure.brm)
r2(max.social.brm)

####model performance
model.list <- list(max.IT.brm,
                   max.measure.brm,
                   max.social.brm,
                   typ.IT.brm,
                   typ.measure.brm,
                   typ.social.brm)

model.r2s <- do.call("rbind", lapply(model.list,function(x) r2(x)))

model.r2s
#save model 3 objects
save(max.social.brm,file="model_outputs/max.social.model.rdata")
save(typ.social.brm,file="model_outputs/typ.social.model.rdata")

