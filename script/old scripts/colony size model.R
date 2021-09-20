#colony size model
forage.traits.soc.max <- forage.traits.max%>%
  filter(social_tree%in%"Highly Eusocial")#%>%
filter(!Genus_species%in%"Trigona_spinipes")%>%
  droplevels()



forage.traits.soc.max$ln.col <- log(forage.traits.soc.max$Average)
forage.traits.soc.max$scale.col <- scale(forage.traits.soc.max$Average)

#phylo
bee.max.col.tree<-drop.tip(bee.max.tree,
                           setdiff(bee.max.tree$tip.label,
                                   forage.traits.soc.max$Genus))
bee.max.col.tree$node.label=NULL
CoLg <- ape::vcv.phylo(bee.max.col.tree)

max.size.priors <- prior(normal(5,2),class="Intercept")+
  prior(normal(0,0.5),class="b")+
  prior(normal(0,1),class="b",coef="ITD")+
  prior(normal(0,1.25),class="sd")+
  prior(normal(0,1),class="sigma")

max.size.sb.brm<- brm(round(Average,0)~ITD*taxa+
                        (1|Authors)+
                        (1|spp)+
                        (1|gr(Genus, cov = CoLg)),
                      data2 = list(CoLg = CoLg),
                      cores=4,
                      iter = 2000,
                      # prior = max.size.priors[-3,],
                      family="poisson",
                      control = list(adapt_delta=0.99,
                                     max_treedepth=15),
                      data=forage.traits.soc.max[!duplicated(forage.traits.soc.max[,c("spp"),]),])

forage.traits.soc.max$taxa <- ifelse(forage.traits.soc.max$Genus%in%"Apis","Apis","SB")

options(na.action = "na.fail")
MuMIn::dredge(lm(log.dist~ITD*limits*scale.col,data=forage.traits.soc.max))
MuMIn::dredge(lm(log.dist~ITD*limits*ln.col,data=forage.traits.soc.max))

summary(lm(log.dist~ITD+ln.col,data=forage.traits.soc.max))



max.ln.size.phylo.brm<- brm(log.dist~ITD*limits+
                              (1|Authors)+
                              (ITD|Metric2)+
                              (1|spp)+
                              (1|gr(Genus, cov = CoLg)),
                            data2 = list(CoLg = CoLg),
                            cores=4,
                            iter = 2000,
                            prior = max.func.priors,
                            family="gaussian",
                            control = list(adapt_delta=0.8,
                                           max_treedepth=10),
                            data=forage.traits.soc.max)

