##graphs

max.func.euc.full.phylo.brm
max.func.euc.red.phylo.brm

performance::r2(max.func.euc.full.phylo.brm)
performance::r2(max.func.euc.red.phylo.brm)


##Calculate RMSE for all models

forage.traits.obs[forage.traits.obs$spp%in%"Megachile_nana","Lecty"] = "Polylectic"

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(predict(max.func.euc.full.phylo.brm,
             newdata = forage.traits.obs,
             re_formula = ~(1|spp)+(1|Genus),
             allow_new_levels=T),
     forage.traits.obs$log.dist)
RMSE(predict(max.func.euc.full.phylo.brm,
             re_formula = ~(1|spp)+(1|Genus)),
     forage.traits.max$log.dist)

RMSE(predict(max.func.euc.full.phylo.brm,
             newdata = forage.traits.obs,
             re_formula = ~(1|Genus),
             allow_new_levels=T),
     forage.traits.obs$log.dist)

RMSE(predict(max.func.euc.full.phylo.brm,
             re_formula = ~(1|Genus),
             allow_new_levels=T),
     forage.traits.max$log.dist)

RMSE(predict(max.func.euc.full.phylo.brm,
             newdata = forage.traits.obs,
             re_formula = NA,
             allow_new_levels=T),
     forage.traits.obs$log.dist)

RMSE(predict(max.func.euc.full.phylo.brm,
             re_formula = NA,
             allow_new_levels=T),
     forage.traits.max$log.dist)



RMSE(predict(mod.list[[2]],newdata = forage.traits.obs,allow_new_levels=T),
     #re_formula = ~(ITD | Metric2))[1],
     forage.traits.obs$log.dist) 
RMSE(predict(mod.list[[3]],newdata = forage.traits.obs,allow_new_levels=T),
     #re_formula = ~(ITD | Metric2))[1],
     forage.traits.obs$log.dist) 




RMSE(predict(mod.list[[4]],newdata = forage.traits.obs,allow_new_levels=T),
     #re_formula = ~(ITD | Metric2))[1],
     forage.traits.obs$log.dist) 
RMSE(predict(mod.list[[5]],newdata = forage.traits.obs,allow_new_levels=T),
     #re_formula = ~(ITD | Metric2))[1],
     forage.traits.obs$log.dist) 
RMSE(predict(mod.list[[6]],newdata = forage.traits.obs,allow_new_levels=T),
     #re_formula = ~(ITD | Metric2))[1],
     forage.traits.obs$log.dist) 





# + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[2]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[3]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[4]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[5]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[6]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[7]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[8]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist)




RMSE(predict(mod.list[[9]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[10]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[5]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))
RMSE(predict(mod.list[[6]],newdata = forage.traits.obs,
             re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist) # + (1 | spp) #+ (1 | gr(Genus, cov = Ag)))










rmse.list <- lapply(mod.list,function (x)
  RMSE(predict(x,newdata = forage.traits.obs,
               re_formula = ~(ITD | Metric2))[1],forage.traits.obs$log.dist)
  )
mod.list
