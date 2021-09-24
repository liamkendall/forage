####Predictive foraging range models

#set-up
options(brms.backend="cmdstanr")

####Maximum models

#Phylo VCV matrix
Agen <- ape::vcv.phylo(bee.max_tree)

####Priors

max_priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

####Model 1
max_IT_mod <- brm(log.dist~itd+
                    (1|publication)+
                    (1|metric)+
                    (1|species)+
                    (1|gr(genus, cov = Agen)),
                  data2 = list(Agen = Agen),
                  cores=4,
                  iter = 2000,
                  prior = max_priors,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.max)

##Model 2
max_measure_mod<- brm(log.dist~itd*measure.type+
                           (1|publication)+
                           (1|metric)+
                           (1|species)+
                           (1|gr(genus, cov = Agen)),
                         data2 = list(Agen = Agen),
                         cores=4,
                         iter = 2000,
                         prior = max_priors,
                         family="gaussian",
                         control = list(adapt_delta=0.999,
                                        max_treedepth=15),
                         data=forage.traits.max)

#Model 3
max_social_mod<- brm(log.dist~itd*measure.type*sociality+
                                    (1|publication)+
                                    (1|metric)+
                                    (1|species)+
                                    (1|gr(genus, cov = Agen)),
                                  data2 = list(Agen = Agen),
                                  cores=4,
                                  iter = 2000,
                                  prior = max_priors,
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
typ_priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

#model 1
typ_IT_mod<- brm(log.dist~itd+
                    (1|publication)+
                    (1|metric)+
                    (1|species)+
                    (1|gr(genus, cov = Bg)),
                  iter = 2000,
                  prior = typ_priors,
                  data2 = list(Bg = Bg),
                  cores=4,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.mean)

#model 2
typ_measure_mod<- brm(log.dist~itd*measure.type+
                         (1|publication)+
                         (1|metric)+
                         (1|species)+
                         (1|gr(genus, cov = Bg)),
                       data2 = list(Bg = Bg),
                       cores=4,
                       iter = 2000,
                       prior = typ_priors,
                       family="gaussian",
                       control = list(adapt_delta=0.999,
                                      max_treedepth=15),
                       data=forage.traits.mean)

#model 3
typ_social_mod<- brm(log.dist~itd*measure.type*sociality+
                        (1|publication)+
                        (1|metric)+
                        (1|species)+
                        (1|gr(genus, cov = Bg)),
                      data2 = list(Bg = Bg),
                      cores=4,
                      iter = 2000,
                      prior = typ_priors,
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.mean)
                                   
###posterior predictive checks
pp_check(typ_IT_mod)
pp_check(typ_measure_mod)
pp_check(typ_social_mod)

pp_check(max_IT_mod)
pp_check(max_measure_mod)
pp_check(max_social_mod)

##ICC
icc(typ_IT_mod,by_group=T)
icc(typ_measure_mod,by_group=T)
icc(typ_social_mod,by_group=T)

icc(max_IT_mod,by_group=T)
icc(max_measure_mod,by_group=T)
icc(max_social_mod,by_group=T)

#r2
r2(typ_IT_mod)
r2(typ_measure_mod)
r2(typ_social_mod)

r2(max_IT_mod)
r2(max_measure_mod)
r2(max_social_mod)

####model performance
model.list <- list(max_IT_mod,
                   max_measure_mod,
                   max_social_mod,
                   typ_IT_mod,
                   typ_measure_mod,
                   typ_social_mod)

model.r2s <- do.call("rbind", lapply(model.list,function(x) r2(x)))

model.r2s

#save model 3 objects
save(max_social_mod,file="model_outputs/max_social.mod.rdata",compress="xz")
save(typ_social_mod,file="model_outputs/typ_social_mod.rdata",compress="xz")
