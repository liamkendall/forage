####Predictive foraging range models

#set-up
options(brms.backend="cmdstanr")

####All model

#Phylo VCV matrix
phy.vcv <- ape::vcv.phylo(bee.tree)

####Priors
all_priors <- prior(normal(4,2),class="Intercept")+
  prior(normal(0,1),class="b")+
  prior(normal(0,1),class="sd")

####Model 1
all_IT_mod <- brm(log.dist~range.4*itd+
                    (1|publication)+
                    (1|metric)+
                    (1|species)+
                    (1|gr(genus, cov = phy.vcv)),
                  data2 = list(phy.vcv = phy.vcv),
                  cores=4,
                  iter = 3000,
                  prior = all_priors,
                  family="gaussian",
                  control = list(adapt_delta=0.999,
                                 max_treedepth=15),
                  data=forage.traits)

##Model 2
all_social_mod<- brm(log.dist~range.4*itd*sociality+
                       (1|publication)+
                       (1|metric)+
                       (1|species)+
                       (1|gr(genus, cov = phy.vcv)),
                     data2 = list(phy.vcv = phy.vcv),
                     cores=4,
                     iter = 3000,
                     prior = all_priors,
                     family="gaussian",
                     control = list(adapt_delta=0.999,
                                     max_treedepth=15),
                     data=forage.traits)

###posterior predictive checks
pp_check(all_IT_mod)
pp_check(all_social_mod)

#r2
r2(all_IT_mod)
r2(all_social_mod)

#save model objects
save(all_IT_mod,
     file="model_outputs/all_IT_mod.rdata",
     compress="xz")
save(all_social_mod,
     file="model_outputs/all_social_mod.rdata",
     compress="xz")
