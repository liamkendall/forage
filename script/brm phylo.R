#glmmtmb, phyr and brms - similar results mostly - expect with specification of random effects

library(brms)
library(cmdstanr)
install_cmdstan()

Ag <- ape::vcv.phylo(bee.max.tree)

options(brms.backend = "cmdstanr")
#cmdstanr::set_cmdstan_path(path = NULL)


#Check structure with interaction model

#nested effects
max.simp.priors <- prior(normal(6,1),class="Intercept")+
  #prior(normal(-1,1),class="b")+
  prior(normal(1,1),class="b",coef="log.it")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

max.phylo.brm<- brm(log.dist~log.it+
                            (log.it|Metric2)+
                            (1|Authors)+
                            (1|spp)+
                            (1|gr(Genus, cov = Ag)),
                          data2 = list(Ag = Ag),
                          cores=4,
                          iter = 2000,
                          #prior = max.simp.priors,
                          family="gaussian",
                          control = list(adapt_delta=0.9,
                                         max_treedepth=15),
                          data=forage.traits.max)

max.phylo.brm.2<- brm(log.dist~ITD+
                      (ITD|Metric2)+
                      (1|Authors)+
                      (1|spp)+
                      (1|gr(Genus, cov = Ag)),
                    data2 = list(Ag = Ag),
                    cores=4,
                    iter = 2000,
                    prior = prior(normal(5,2),class="Intercept")+
                    prior(normal(0,1),class="b",coef="ITD")+
                      prior(normal(0,1),class="sd")+
                      prior(normal(0,1),class="sigma"),
                    family="gaussian",
                    control = list(adapt_delta=0.999,
                                   max_treedepth=15),
                    data=forage.traits.max)

max.phylo.brm.3<- brm(Distance~ITD+
                        (ITD|Metric2)+
                        (1|Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = Ag)),
                      data2 = list(Ag = Ag),
                      cores=4,
                      iter = 2000,
                      #prior = max.simp.priors[-2,],
                      family=Gamma(link="log"),
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.max)

max.phylo.brm.4<- brm(Distance~ITD+
                        (ITD|Metric2)+
                        (1|Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = Ag)),
                      data2 = list(Ag = Ag),
                      cores=4,
                      iter = 2000,
                      #prior = max.simp.priors[-2,],
                      family=exponential(link="log"),
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.max)

max.phylo.brm.5<- brm(Distance~ITD+
                        (ITD|Metric2)+
                        (1|Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = Ag)),
                      data2 = list(Ag = Ag),
                      cores=4,
                      iter = 2000,
                      #prior = max.simp.priors[-2,],
                      family="lognormal",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.max)

max.phylo.brm.4<- brm(Distance~log.it+
                        (log.it|Metric2)+
                        (1|Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = Ag)),
                      data2 = list(Ag = Ag),
                      cores=4,
                      iter = 2000,
                      #prior = max.simp.priors[-2,],
                      family="gaussian",
                      control = list(adapt_delta=0.9,
                                     max_treedepth=15),
                      data=forage.traits.max)

max.phylo.brm <- add_criterion(max.phylo.brm,
                               criterion="loo")
max.phylo.brm.2 <- add_criterion(max.phylo.brm.2,
                                 criterion="loo")
max.phylo.brm.2a <- add_criterion(max.phylo.brm.2a,
                                 criterion="loo")
max.phylo.brm.3 <- add_criterion(max.phylo.brm.3,
                               criterion="loo")
max.phylo.brm.4 <- add_criterion(max.phylo.brm.4,
                                 criterion="loo")

loo_compare(max.phylo.brm,max.phylo.brm.2,max.phylo.brm.2a,max.phylo.brm.3,max.phylo.brm.4)

#save basic models
save(max.phylo.brm,file = "model_outputs/basic_log_dist_log_it.rdata")
save(max.phylo.brm.2,file = "model_outputs/basic_log_dist_raw_it.rdata")
save(max.phylo.brm.3,file = "model_outputs/basic_raw_dist_raw_it.rdata")
save(max.phylo.brm.4,file = "model_outputs/basic_raw_dist_log_it.rdata")

performance::r2(max.phylo.brm)
performance::r2(max.phylo.brm.2)
performance::r2(max.phylo.brm.2a)
performance::r2(max.phylo.brm.3)
performance::r2(max.phylo.brm.4)
performance::r2(max.phylo.brm.5)


pp_check(max.phylo.brm)
pp_check(max.phylo.brm.2)
pp_check(max.phylo.brm.3)
pp_check(max.phylo.brm.5)
loo_compare(max.phylo.brm.2,max.phylo.brm.2a)
#un-logged ITD = most predictive

#assessment of eusociality models

max.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(-1,1),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1.25),class="sd")+
  prior(normal(0,1),class="sigma")

max.euc.phylo.brm.1<- brm(log.dist~ITD+social_tree+
                               (1|Authors)+
                               (1|Metric2)+
                               (1|spp)+
                              (1|gr(Genus, cov = Ag)),
                          prior = max.priors,iter = 2000,
                             data2 = list(Ag = Ag),cores=4,
                             family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                             data=forage.traits.max)

#nested effects
max.euc.phylo.brm.2<- brm(log.dist~ITD+social_tree+
                               (1|Metric2/Authors)+
                            (1|spp)+
                            (1|gr(Genus, cov = Ag)),
                          data2 = list(Ag = Ag),
                          cores=4,
                          iter = 2000,
                          prior = max.priors,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.max)



#random slope
max.euc.phylo.brm.3<- brm(log.dist~ITD+social_tree+
                               (1|Authors)+
                               (ITD|Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Ag)),
                          iter = 2000,
                          prior = max.priors,
                          data2 = list(Ag = Ag),
                          cores=4,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.max)

max.euc.phylo.brm.3a<- brm(log.dist~ITD*social_tree+
                             (1|Authors)+
                             (ITD|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Ag)),
                           data2 = list(Ag = Ag),
                           cores=4,
                           iter = 2000,
                           prior = max.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.max)

max.euc.phylo.brm.4<- brm(log.dist~ITD+social_tree+
                            (1|Authors)+
                            (ITD||Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Ag)),
                          prior = max.priors,
                          iter=2000,
                          data2 = list(Ag = Ag),
                          cores=4,
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          family="gaussian",
                          data=forage.traits.max)


max.euc.phylo.brm.1 <- add_criterion(max.euc.phylo.brm.1,criterion="kfold")
max.euc.phylo.brm.2 <- add_criterion(max.euc.phylo.brm.2,criterion="kfold")
max.euc.phylo.brm.3 <- add_criterion(max.euc.phylo.brm.3,criterion="kfold")
max.euc.phylo.brm.3a <- add_criterion(max.euc.phylo.brm.3a,criterion="kfold")
max.euc.phylo.brm.4 <- add_criterion(max.euc.phylo.brm.4,criterion="kfold")

loo_compare(max.euc.phylo.brm.1,
            max.euc.phylo.brm.2,
            max.euc.phylo.brm.3a,
            max.euc.phylo.brm.3,
            max.euc.phylo.brm.4,criterion = "kfold")

pp_check(max.euc.phylo.brm.2,nsamples=1000)

#SAVE PRIMARY MODEL OBJECTS
performance::r2_bayes(max.euc.phylo.brm.1)
performance::r2_bayes(max.euc.phylo.brm.2)
performance::r2_bayes(max.euc.phylo.brm.3a)
performance::r2_bayes(max.euc.phylo.brm.3)
performance::r2_bayes(max.euc.phylo.brm.4)

save(max.euc.phylo.brm.3,file = "model_outputs/basic_log_dist_raw_it_eu.rdata")
save(max.euc.phylo.brm.3a,file = "model_outputs/basic_log_dist_raw_it_eu_int.rdata")



max.func.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(-1,1),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="b",coef="ITD:limitsphysio")+
  prior(normal(0,1.25),class="sd")+
  prior(normal(0,1),class="sigma")
#physio vs functional model
max.func.phylo.brm<- brm(log.dist~ITD*limits+
                             (1|Authors)+
                             (ITD|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Ag)),
                           data2 = list(Ag = Ag),
                           cores=4,
                           iter = 2000,
                           prior = max.func.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.max)

max.func.euc.full.phylo.brm<- brm(log.dist~ITD*limits*social_tree+
                           (1|Authors)+
                           (ITD|Metric2)+
                           (1|spp)+
                           (1|gr(Genus, cov = Ag)),
                         data2 = list(Ag = Ag),
                         cores=4,
                         iter = 2000,
                         prior = max.func.priors,
                         family="gaussian",
                         control = list(adapt_delta=0.999,
                                        max_treedepth=15),
                         data=forage.traits.max)

max.func.euc.red.phylo.brm<- brm(log.dist~ITD*limits+social_tree+
                             (1|Authors)+
                             (ITD|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Ag)),
                           data2 = list(Ag = Ag),
                           cores=4,
                           iter = 2000,
                           prior = max.func.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.max)
#metric model
max.metric.phylo.brm<- brm(log.dist~ITD*Metric2+
                            (1|Authors)+
                            (1|spp)+
                            (1|gr(Genus, cov = Ag)),
                          data2 = list(Ag = Ag),
                          cores=4,
                          iter = 2000,
                          prior = max.priors,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.max)

max.metric.phylo.brm.2<- brm(log.dist~ITD+Metric2+
                             (1|Authors)+
                             (1|spp)+
                             (1|gr(Genus, cov = Ag)),
                           data2 = list(Ag = Ag),
                           cores=4,
                           iter = 2000,
                           prior = max.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.max)

max.metric.phylo.brm<- add_criterion(max.metric.phylo.brm,criterion = "loo")
max.metric.phylo.brm.2<- add_criterion(max.metric.phylo.brm.2,criterion = "loo")

save(max.metric.phylo.brm,file = "model_outputs/basic_log_dist_raw_it_metric.rdata")
save(max.metric.phylo.brm.2,file = "model_outputs/basic_log_dist_raw_it_metric_int.rdata")

loo_compare(max.metric.phylo.brm,max.metric.phylo.brm.2)

conditional_effects(max.metric.phylo.brm)
###############################
#####PAIRWISE DIFFERENCES#####
#############################
library(emmeans)
library(tidybayes)
max.metric.pw<- max.metric.phylo.brm %>%
  emtrends( ~ Metric2,var="ITD") %>% 
  gather_emmeans_draws()

max.metric.cld <- max.metric.pw %>% 
  ungroup() %>% ## to get rid of unneeded columns
  select(.value, Metric2, .draw) %>% 
  spread(Metric2, .value) %>% 
  select(-.draw) %>% ## we need to get rid of all columns not containing draws
  cld_pmatrix()


###Compute HDI intervals  - reality check against CLD
max.metric.hdi<-max.metric.pw %>% 
  ungroup %>% 
  compare_levels(.value, by = Metric2) %>% 
  mean_hdci()

ggplot(max.metric.hdi,aes(x=.value,y=Metric2))+
  geom_errorbarh(aes(xmin=.lower,xmax=.upper))+
  geom_point()

#mean models
Bg <- ape::vcv.phylo(bee.mean.tree)
Bg



#JUST ITD MODEL
#nested effects
mean.simp.priors <- prior(normal(0,5),class="Intercept")+
  #prior(normal(-1,1),class="b")+
  prior(normal(0,1),class="b",coef="log.it")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

mean.phylo.brm<- brm(log.dist~log.it+
                       (1|Authors)+
                       (log.it|Metric2)+
                       (1|spp)+
                       (1|gr(Genus, cov = Bg)),
                    data2 = list(Bg = Bg),
                    cores=4,
                    iter = 2000,
                    prior = mean.simp.priors,
                    family="gaussian",
                    control = list(adapt_delta=0.999,
                                   max_treedepth=15),
                    data=forage.traits.mean)

mean.phylo.brm.2<- brm(log.dist~ITD+
                         (1|Authors)+
                         (ITD|Metric2)+
                         (1|spp)+
                         (1|gr(Genus, cov = Bg)),
                      data2 = list(Bg = Bg),
                      cores=4,
                      iter = 2000,
                      prior = mean.simp.priors[-2,],
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.mean)

mean.phylo.brm.3<- brm(Distance~ITD+
                         (1|Authors)+
                         (ITD|Metric2)+
                         (1|spp)+
                         (1|gr(Genus, cov = Bg)),
                      data2 = list(Bg = Bg),
                      cores=4,
                      iter = 2000,
                      prior = mean.simp.priors[-2,],
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.mean)

mean.phylo.brm.4<- brm(Distance~log.it+
                         (1|Authors)+
                         (log.it|Metric2)+
                         (1|spp)+
                         (1|gr(Genus, cov = Bg)),
                      data2 = list(Bg = Bg),
                      cores=4,
                      iter = 2000,
                      prior = mean.simp.priors[-2,],
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.mean)

mean.phylo.brm <- add_criterion(mean.phylo.brm,
                               criterion="loo")
mean.phylo.brm.2 <- add_criterion(mean.phylo.brm.2,
                                 criterion="loo")
mean.phylo.brm.3 <- add_criterion(mean.phylo.brm.3,
                                 criterion="loo")
mean.phylo.brm.4 <- add_criterion(mean.phylo.brm.4,
                                 criterion="loo")

#save basic models
save(mean.phylo.brm,file = "model_outputs/mean/mean_basic_log_dist_log_it.rdata")
save(mean.phylo.brm.2,file = "model_outputs/mean/mean_basic_log_dist_raw_it.rdata")
save(mean.phylo.brm.3,file = "model_outputs/mean/mean_basic_raw_dist_raw_it.rdata")
save(mean.phylo.brm.4,file = "model_outputs/mean/mean_basic_raw_dist_log_it.rdata")

performance::r2(mean.phylo.brm)
performance::r2(mean.phylo.brm.2)
performance::r2(mean.phylo.brm.3)
performance::r2(mean.phylo.brm.4)

pp_check(mean.phylo.brm.2,nsamples=100,"violin_grouped",group = "Metric2")
pp_check(mean.phylo.brm.2)

#un-logged ITD = better

#assessment of eusociality models

mean.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(-1,1),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

mean.euc.phylo.brm.1<- brm(log.dist~ITD+social_tree+
                            (1|Authors)+
                            (1|Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Bg)),
                          prior = mean.priors,iter = 2000,
                          data2 = list(Bg = Bg),cores=4,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.mean)

#nested effects
mean.euc.phylo.brm.2<- brm(log.dist~ITD+social_tree+
                            (1|Metric2/Authors)+
                            (1|spp)+
                            (1|gr(Genus, cov = Bg)),
                          data2 = list(Bg = Bg),
                          cores=4,
                          iter = 2000,
                          prior = mean.priors,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.mean)

mean.euc.phylo.brm.2a<- brm(log.dist~ITD*social_tree+
                             (1|Metric2/Authors)+
                             (1|spp)+
                             (1|gr(Genus, cov = Bg)),
                           data2 = list(Bg = Bg),
                           cores=4,
                           iter = 2000,
                           prior = mean.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.mean)

#random slope
mean.euc.phylo.brm.3<- brm(log.dist~ITD+social_tree+
                            (1|Authors)+
                            (ITD|Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Bg)),
                          iter = 2000,
                          prior = mean.priors,
                          data2 = list(Bg = Bg),
                          cores=4,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits.mean)

mean.euc.phylo.brm.3a<- brm(log.dist~ITD*social_tree+
                             (1|Authors)+
                             (ITD|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Bg)),
                           iter = 2000,
                           prior = mean.priors,
                           data2 = list(Bg = Bg),
                           cores=4,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.mean)

mean.euc.phylo.brm.4<- brm(log.dist~ITD+social_tree+
                            (1|Authors)+
                            (ITD||Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Bg)),
                          prior = mean.priors,
                          iter=2000,
                          data2 = list(Bg = Bg),
                          cores=4,
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          family="gaussian",
                          data=forage.traits.mean)

hypothesis(mean.euc.phylo.brm.2,"Intercept + social_treeSolitary < Intercept")
hypothesis(mean.euc.phylo.brm.2,"Intercept + social_treePrimitivelyEusocial < Intercept")

mean.euc.phylo.brm.1 <- add_criterion(mean.euc.phylo.brm.1,criterion="loo")

mean.euc.phylo.brm.2 <- add_criterion(mean.euc.phylo.brm.2,criterion="loo")

mean.euc.phylo.brm.3 <- add_criterion(mean.euc.phylo.brm.3,criterion="loo")
mean.euc.phylo.brm.3a <- add_criterion(mean.euc.phylo.brm.3a,criterion="loo")

mean.euc.phylo.brm.4 <- add_criterion(mean.euc.phylo.brm.4,criterion="loo")

loo_compare(
  mean.euc.phylo.brm.1,
  mean.euc.phylo.brm.2,
  mean.euc.phylo.brm.3,
  mean.euc.phylo.brm.3a,
  mean.euc.phylo.brm.4)

pp_check(mean.euc.phylo.brm.3,nsamples=1000)

#SAVE PRIMARY MODEL OBJECTS

performance::r2_bayes(mean.euc.phylo.brm.1)
performance::r2_bayes(mean.euc.phylo.brm.2)
performance::r2_bayes(mean.euc.phylo.brm.3a)
performance::r2_bayes(mean.euc.phylo.brm.3)
performance::r2_bayes(mean.euc.phylo.brm.4)

save(mean.euc.phylo.brm.2,file = "model_outputs/mean/mean_basic_log_dist_raw_it_eu.rdata")
save(mean.euc.phylo.brm.2a,file = "model_outputs/mean/mean_basic_log_dist_raw_it_eu_int.rdata")

#func model
mean.func.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(-1,1),class="b")+
  prior(normal(0,1),class="b",coef="ITD:limitsphysio")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")
#physio vs functional model
mean.func.phylo.brm<- brm(log.dist~ITD*limits+
                           (1|Authors)+
                           (ITD|Metric2)+
                           (1|spp)+
                           (1|gr(Genus, cov = Ag)),
                         data2 = list(Ag = Ag),
                         cores=4,
                         iter = 2000,
                         prior = mean.func.priors,
                         family="gaussian",
                         control = list(adapt_delta=0.999,
                                        max_treedepth=15),
                         data=forage.traits.mean)

mean.func.euc.full.phylo.brm<- brm(log.dist~ITD*limits*social_tree+
                                    (1|Authors)+
                                    (ITD|Metric2)+
                                    (1|spp)+
                                    (1|gr(Genus, cov = Ag)),
                                  data2 = list(Ag = Ag),
                                  cores=4,
                                  iter = 2000,
                                  prior = mean.func.priors,
                                  family="gaussian",
                                  control = list(adapt_delta=0.999,
                                                 max_treedepth=15),
                                  data=forage.traits.mean)

mean.func.euc.red.phylo.brm<- brm(log.dist~ITD*limits+social_tree+
                                   (1|Authors)+
                                   (ITD|Metric2)+
                                   (1|spp)+
                                   (1|gr(Genus, cov = Ag)),
                                 data2 = list(Ag = Ag),
                                 cores=4,
                                 iter = 2000,
                                 prior = mean.func.priors,
                                 family="gaussian",
                                 control = list(adapt_delta=0.999,
                                                max_treedepth=15),
                                 data=forage.traits.mean)

#metric model
mean.metric.phylo.brm<- brm(log.dist~ITD*Metric2+
                             (1|Authors)+
                             (1|spp)+
                             (1|gr(Genus, cov = Bg)),
                           data2 = list(Bg = Bg),
                           cores=4,
                           iter = 2000,
                           prior = mean.priors,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.mean)

mean.metric.phylo.brm.2<- brm(log.dist~ITD+Metric2+
                               (1|Authors)+
                               (1|spp)+
                               (1|gr(Genus, cov = Bg)),
                             data2 = list(Bg = Bg),
                             cores=4,
                             iter = 2000,
                             prior = mean.priors,
                             family="gaussian",
                             control = list(adapt_delta=0.999,
                                            max_treedepth=15),
                             data=forage.traits.mean)

mean.metric.phylo.brm<- add_criterion(mean.metric.phylo.brm,criterion = "loo")
mean.metric.phylo.brm.2<- add_criterion(mean.metric.phylo.brm.2,criterion = "loo")

loo_compare(mean.metric.phylo.brm,mean.metric.phylo.brm.2)
save(mean.metric.phylo.brm,file = "model_outputs/mean/mean_basic_log_dist_raw_it_metric.rdata")
save(mean.metric.phylo.brm.2,file = "model_outputs/mean/mean_basic_log_dist_raw_it_metric_int.rdata")



#####MODELS OF LECTY
#just solitary species
#same random structure?

#phylo
library(ape)
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

max.lecty.phylo.brm.1 <- add_criterion(max.lecty.phylo.brm.1,criterion="loo")
max.lecty.phylo.brm.2 <- add_criterion(max.lecty.phylo.brm.2,criterion="loo")

loo_compare(max.lecty.phylo.brm.1,
            max.lecty.phylo.brm.2)

pp_check(max.lecty.phylo.brm.2)

performance::r2(max.lecty.phylo.brm.1)

library(performance)



#mean

bee.mean.sol.tree<-drop.tip(bee.mean.tree,setdiff(bee.mean.tree$tip.label,
                                                forage.traits.mean.sol$Genus))
bee.mean.sol.tree$node.label=NULL

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

mean.lecty.phylo.brm.1 <- add_criterion(mean.lecty.phylo.brm.1,criterion="loo")
mean.lecty.phylo.brm.2 <- add_criterion(mean.lecty.phylo.brm.2,criterion="loo")

loo_compare(mean.lecty.phylo.brm.1,
            mean.lecty.phylo.brm.2)
