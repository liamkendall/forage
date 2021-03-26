###modelling workflow

library(brms)
library(cmdstanr)
options(brms.backend = "cmdstanr")

#all together model

Cg <- ape::vcv.phylo(bee.all.tree)

#weak priors
all.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  #prior(normal(1,1),class="b",coef="log.it")+
  prior(normal(0,1),class="sd")+
  prior(normal(0,1),class="sigma")

##check random structure with full model
#specifically structure of metric

all.phylo.random.slope.brm<- brm(log.dist~distance_type2*ITD*social_tree+
                            (1|Authors)+
                            (ITD|Metric2)+
                            (1|spp)+
                            (1|gr(Genus, cov = Cg)),
                          iter = 4000,
                          prior = all.priors,
                          data2 = list(Cg = Cg),
                          cores=4,
                          family="gaussian",
                          control = list(adapt_delta=0.999,
                                         max_treedepth=15),
                          data=forage.traits)

hypothesis(all.phylo.random.slope.brm,"Intercept-(Intercept+social_treePrimitivelyEusocial)<0")

all.phylo.crossed.brm<- brm(log.dist~distance_type2*ITD*social_tree+
                      (1|Authors)+
                      (1|Metric2)+
                      (1|spp)+
                      (1|gr(Genus, cov = Cg)),
                    iter = 4000,
                    prior = all.priors,
                    data2 = list(Cg = Cg),
                    cores=4,
                    family="gaussian",
                    control = list(adapt_delta=0.999,
                                   max_treedepth=15),
                    data=forage.traits)

all.phylo.nested.brm <- brm(log.dist~distance_type2*ITD*social_tree+
                        (1|Metric2/Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = Cg)),
                      iter = 4000,
                      prior = all.priors,
                      data2 = list(Cg = Cg),
                      cores=4,
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits)

all.phylo.random.slope.brm <- add_criterion(all.phylo.random.slope.brm,criterion="loo")
all.phylo.crossed.brm <- add_criterion(all.phylo.crossed.brm,criterion="loo")
all.phylo.nested.brm <- add_criterion(all.phylo.nested.brm,criterion="loo")

loo_compare(all.phylo.random.slope.brm,
            all.phylo.crossed.brm,
            all.phylo.nested.brm)

#posterior predictive check
pp_check(all.phylo.random.slope.brm,nsamples=1000) #good

save(all.phylo.random.slope.brm,file = "model_outputs/all.phylo.random.slope.brm.rdata")
save(all.phylo.crossed.brm,file = "model_outputs/all.phylo.crossed.brm.rdata")
save(all.phylo.nested.brm,file = "model_outputs/all.phylo.nested.brm.rdata")

#go with random slope

##reduced models
# - three-way interaction
all.phylo.reduced.1.brm <- brm(log.dist~distance_type2+ITD+social_tree+
                        social_tree:ITD+
                        distance_type2:ITD+
                        social_tree:distance_type2+
                      (1|Authors)+
                      (ITD|Metric2)+
                      (1|spp)+
                      (1|gr(Genus, cov = Cg)),
                    iter = 2000,
                    prior = all.priors,
                    data2 = list(Cg = Cg),
                    cores=4,
                    family="gaussian",
                    control = list(adapt_delta=0.999,
                                   max_treedepth=15),
                    data=forage.traits)

###
#significant distance type sociality interaction - keep
hypothesis(all.phylo.reduced.1.brm,"distance_type2Typical:social_treeSolitary > 0")
hypothesis(all.phylo.reduced.1.brm,"distance_type2Typical:social_treePrimitivelyEusocial > 0")

#NS distance type ITD interaction - remove
hypothesis(all.phylo.reduced.1.brm,"distance_type2Typical:ITD < 0")

#significant slope - solitary group
hypothesis(all.phylo.reduced.1.brm,"ITD:social_treePrimitivelyEusocial < 0")
hypothesis(all.phylo.reduced.1.brm,"ITD:social_treeSolitary < 0")

all.phylo.reduced.2.brm<- brm(log.dist~distance_type2+ITD+social_tree+
                        social_tree:ITD+
                        #distance_type2:ITD+ #removed (as above)
                        social_tree:distance_type2+
                        (1|Authors)+
                        (ITD|Metric2)+
                        (1|spp)+
                        (1|gr(Genus, cov = Cg)),
                      iter = 4000,
                      prior = all.priors,
                      data2 = list(Cg = Cg),
                      cores=4,
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits)

all.phylo.reduced.3.brm<- brm(log.dist~distance_type2+ITD+social_tree+
                         # distance_type2:social_tree+
                          (1|Authors)+
                          (ITD|Metric2)+
                          (1|spp)+
                          (1|gr(Genus, cov = Cg)),
                        iter = 4000,
                        prior = all.priors,
                        data2 = list(Cg = Cg),
                        cores=4,
                        family="gaussian",
                        control = list(adapt_delta=0.999,
                                       max_treedepth=15),
                        data=forage.traits)


all.phylo.reduced.1.brm <- add_criterion(all.phylo.reduced.1.brm,criterion="loo")
all.phylo.reduced.2.brm <- add_criterion(all.phylo.reduced.2.brm,criterion="loo")
all.phylo.reduced.3.brm <- add_criterion(all.phylo.reduced.3.brm,criterion="loo")

loo_compare(all.phylo.random.slope.brm,all.phylo.reduced.3.brm,all.phylo.reduced.2.brm,all.phylo.reduced.1.brm)

save(all.phylo.reduced.1.brm,file = "model_outputs/all.phylo.reduced.1.brm.rdata")
save(all.phylo.reduced.2.brm,file = "model_outputs/all.phylo.reduced.2.brm.rdata")
save(all.phylo.reduced.3.brm,file = "model_outputs/all.phylo.reduced.3.brm.rdata")

performance::r2(all.phylo.reduced.1.brm)
performance::r2(all.phylo.reduced.2.brm)
performance::r2(all.phylo.reduced.3.brm)


conditional_effects(all.phylo.brm.2)
plot(all.phylo.brm)
pp_check(all.phylo.reduced.2.brm,nsamples=1000)

all.phylo.bmetric.brm<- brm(log.dist~distance_type2+ITD*Metric2+
                          (1|Authors)+
                          (1|spp)+
                          (1|gr(Genus, cov = Cg)),
                        iter = 4000,
                        prior = all.priors,
                        data2 = list(Cg = Cg),
                        cores=4,
                        family="gaussian",
                        control = list(adapt_delta=0.999,
                                       max_treedepth=15),
                        data=forage.traits)

all.phylo.bmetric.brm.2<- brm(log.dist~distance_type2+ITD+Metric2+
                              (1|Authors)+
                              (1|spp)+
                              (1|gr(Genus, cov = Cg)),
                            iter = 4000,
                            prior = all.priors,
                            data2 = list(Cg = Cg),
                            cores=4,
                            family="gaussian",
                            control = list(adapt_delta=0.999,
                                           max_treedepth=15),
                            data=forage.traits)

all.phylo.bmetric.brm <- add_criterion(all.phylo.bmetric.brm,criterion = "loo")
all.phylo.bmetric.brm.2 <- add_criterion(all.phylo.bmetric.brm.2,criterion = "loo")

loo_compare(all.phylo.bmetric.brm,all.phylo.bmetric.brm.2)
conditional_effects(all.phylo.bmetric.brm)

forage.traits$limits <- ifelse(forage.traits$Metric2%in%c("feeder","homing"),"physio","funct")

table(forage.traits$limits,forage.traits$Metric2)

all.phylo.limits.brm<- brm(log.dist~distance_type2+ITD+social_tree+limits+
                                limits:ITD+
                                limits:distance_type2+
                                limits:social_tree+
                                (1|Authors)+
                                (1|Metric2)+
                                (1|spp)+
                                (1|gr(Genus, cov = Cg)),
                              iter = 4000,
                              prior = all.priors,
                              data2 = list(Cg = Cg),
                              cores=4,
                              family="gaussian",
                              control = list(adapt_delta=0.999,
                                             max_treedepth=15),
                              data=forage.traits)

all.phylo.limits.brm <- add_criterion(all.phylo.limits.brm,criterion="loo")

loo_compare(all.phylo.limits.brm,all.phylo.limits.brm.2)

performance::r2(all.phylo.limits.brm)

conditional_effects(all.phylo.limits.brm)
all.phylo.limits.brm


all.phylo.limits.red.brm<- brm(log.dist~distance_type2+ITD+social_tree+limits+
                             limits:ITD+
                             limits:distance_type2+
                            # limits:social_tree+
                             (1|Authors)+
                             (1|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Cg)),
                           iter = 4000,
                           prior = all.priors,
                           data2 = list(Cg = Cg),
                           cores=4,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits)

library(plyr)
library(dplyr)
forage.traits.sol <- forage.traits%>%
  filter(social_tree%in%"Solitary")%>%
  filter(Lecty%in%c("Polylectic","Oligolectic"))
                                    

all.lecty.brm<- brm(log.dist~distance_type2*ITD*Lecty+
                             (1|Authors)+
                             (ITD|Metric2)+
                             (1|spp)+
                             (1|gr(Genus, cov = Cg)),
                           iter = 4000,
                           prior = all.priors,
                           data2 = list(Cg = Cg),
                           cores=4,
                           family="gaussian",
                           control = list(adapt_delta=0.999,
                                          max_treedepth=15),
                           data=forage.traits.sol)

conditional_effects(all.lecty.brm)

hypothesis(all.lecty.brm,"ITD+ITD:LectyPolylectic > 0")


all.lecty.brm.2<- glmmTMB(log.dist~distance_type2+ITD*Lecty+
                      (1|Authors)+
                      (ITD|Metric2),
                    #  (1|spp),
                      #(1|gr(Genus, cov = Cg)),
                   # iter = 2000,
                   # prior = all.priors,
                   # data2 = list(Cg = Cg),
                    #cores=4,
                    family="gaussian",
                    #control = list(adapt_delta=0.999,
                    #               max_treedepth=15),
                    data=forage.traits.sol)

all.lecty.brm.3<- brm(log.dist~distance_type2+ITD+Lecty+
                        (1|Authors)+
                        (ITD|Metric2)+
                        (1|spp)+
                        (1|gr(Genus, cov = Cg)),
                      iter = 2000,
                      prior = all.priors,
                      data2 = list(Cg = Cg),
                      cores=4,
                      family="gaussian",
                      control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                      data=forage.traits.sol)


