####Predictive foraging range models

#set-up
options(brms.backend="cmdstanr")

####All model

#Phylo VCV matrix
phy.vcv <- ape::vcv.phylo(bee.tree)
#Phylo VCV matrix
phy.g.vcv <- ape::vcv.phylo(bee.gen.tree)

####Priors
all_priors <- prior(normal(4,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

all_10_priors <- prior(normal(-2, 2),class="Intercept")+
  prior(normal(0,2),class="b")+
#  prior(normal(0,1),class="sd",group="species")+
  prior(normal(0,0.5),class="sd")
#forage.traits.bm$spp <- forage.traits.bm$species

####Model 1
all_IT_bm_mod <- brm(dist10~range.4*wgt10+
                       (1|publication)+
                       (1|metric)+
                       (1|species)+
                       (1|gr(genus, cov = phy.g.vcv)),
                     data2 = list(phy.g.vcv = phy.g.vcv),
                  cores=4,
                  iter = 3000,
                  prior = all_10_priors,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits.bm)

##Model 2
all_social_bm_mod<- brm(dist10~range.4*wgt10*sociality+
                          (1|publication)+
                          (1|metric)+
                          (1|species)+
                          (1|gr(genus, cov = phy.g.vcv)),
                        data2 = list(phy.g.vcv = phy.g.vcv),
                        cores=4,
                        iter = 3000,
                        prior = all_10_priors,
                        family="gaussian",
                        control = list(adapt_delta=0.999,
                                       max_treedepth=15),
                        data=forage.traits.bm)

###posterior predictive checks
pp_check(all_IT_bm_mod)
pp_check(all_social_bm_mod)

#r2
r2(all_IT_bm_mod)
r2(all_social_bm_mod)

library(emmeans)
emtrends(all_social_bm_mod, pairwise ~ range.4*sociality, var = "wgt10")

#save model objects
save(all_IT_bm_mod,
     file="model_outputs/all_IT_bm_mod.rdata",
     compress="xz")
save(all_social_bm_mod,
     file="model_outputs/all_social_bm_mod.rdata",
     compress="xz")
