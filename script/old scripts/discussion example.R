#dicussion example

example.preds.disc <- data.frame(spp=c(NA,NA,NA,NA),
                              Genus=c(NA,NA,NA,NA),
                              social_tree=c("Solitary","Solitary","Solitary","Solitary"),
                              limits=c("physio","physio","funct","funct"),
                              Authors=c(NA,NA,NA,NA),
                              Metric2=c(NA,NA,NA,NA),
                              ITD=c(2,4,2,4))

example.preds.disc

pred.draws.disc.nr <- max.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = example.preds.disc,
                      re_formula=NA,
                      transform=exp)
#pred.draws.disc.nr

max_disc_qi <- median_hdci(pred.draws.disc.nr,.value=.prediction)%>%
  select(social_tree,limits,.value,.lower,.upper)

max_disc_qi

#phys diff
(max_disc_qi[4,3]-max_disc_qi[3,3])/max_disc_qi[3,3]
max_disc_qi[4,3]/max_disc_qi[3,3]

#func diff
(max_disc_qi[2,3]-max_disc_qi[1,3])/max_disc_qi[1,3]



pred.draws.mean.disc.nr <- mean.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = example.preds.disc,
                      re_formula=NA,
                      transform=exp)
#pred.draws.disc.nr

mean_disc_qi <- median_hdci(pred.draws.mean.disc.nr,.value=.prediction)%>%
  select(social_tree,limits,.value,.lower,.upper)
mean_disc_qi
max_disc_qi
