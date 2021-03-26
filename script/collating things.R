#collation

max.mod.list <- list(max.IT.brm,
max.euc.phylo.full.brm,
max.euc.phylo.reduced.brm,
max.metric.phylo.brm,
max.lecty.phylo.brm.1,
max.lecty.phylo.brm.2)

lapply(max.mod.list,function(x) save(x,
                                     file=paste0(print(quote(x)))))

save(max.IT.brm,file="max.IT.brm.rdata")
save(max.euc.phylo.full.brm,file="max.euc.phylo.full.brm.rdata")
save(max.euc.phylo.reduced.brm,file="max.euc.phylo.reduced.brm.rdata")
save(max.metric.phylo.brm,file="max.metric.phylo.brm.rdata")
save(max.lecty.phylo.brm.1,file="max.lecty.phylo.brm.1.rdata")
save(max.lecty.phylo.brm.2,file="max.lecty.phylo.brm.2.rdata")
save(max.func.phylo.brm,file="max.func.phylo.brm.rdata")
save(max.func.euc.full.phylo.brm,file="max.func.euc.full.phylo.brm.rdata")
save(max.func.euc.red.phylo.brm,file="max.func.euc.red.phylo.brm.rdata")

save(mean.IT.brm,file="mean.IT.brm.rdata")
save(mean.euc.phylo.full.brm,file="mean.euc.phylo.full.brm.rdata")
save(mean.euc.phylo.reduced.brm,file="mean.euc.phylo.reduced.brm.rdata")
save(mean.metric.phylo.brm,file="mean.metric.phylo.brm.rdata")
save(mean.lecty.phylo.brm.1,file="mean.lecty.phylo.brm.1.rdata")
save(mean.lecty.phylo.brm.2,file="mean.lecty.phylo.brm.2.rdata")
save(mean.func.phylo.brm,file="mean.func.phylo.brm.rdata")
save(mean.func.euc.full.phylo.brm,file="mean.func.euc.full.phylo.brm.rdata")
save(mean.func.euc.red.phylo.brm,file="mean.func.euc.red.phylo.brm.rdata")

exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1])
exp(fixef(max.IT.brm)[1,1]+(fixef(max.IT.brm)[2,1]*2))
exp(fixef(max.IT.brm)[1,1]+(fixef(max.IT.brm)[2,1]*3))
exp(fixef(max.IT.brm)[1,1]+(fixef(max.IT.brm)[2,1]*4))


(exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)-
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*1))/
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*1)*100

(exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*4)-
    exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*3))/
  exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*3)*100


(exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*6)-
    exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*4))/
  exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*4)*100


max.euc.phylo.reduced.brm
(exp(fixef(max.euc.phylo.reduced.brm)[1,1]+fixef(max.euc.phylo.reduced.brm)[2,1]*traits[traits$Genus_species%in%"Apis_mellifera",]$ITD)-
  exp(fixef(max.euc.phylo.reduced.brm)[1,1]+fixef(max.euc.phylo.reduced.brm)[4,1]+fixef(max.euc.phylo.reduced.brm)[2,1]*traits[traits$Genus_species%in%"Osmia_pedicornis",]$ITD))/
  exp(fixef(max.euc.phylo.reduced.brm)[1,1]+fixef(max.euc.phylo.reduced.brm)[4,1]+fixef(max.euc.phylo.reduced.brm)[2,1]*traits[traits$Genus_species%in%"Osmia_pedicornis",]$ITD)*100

max.euc.phylo.reduced.brm

(exp((fixef(max.IT.brm)[2,1]*2))-
exp(fixef(max.IT.brm)[2,1]*1))/
  (exp(fixef(max.IT.brm)[2,1]*1))


#difference between amx and typical
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)-exp(fixef(mean.IT.brm)[1,1]+fixef(mean.IT.brm)[2,1]*2)

exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*4)-exp(fixef(mean.IT.brm)[1,1]+fixef(mean.IT.brm)[2,1]*4)


write.tree(bee.mcmc,file="data/bee distance tree.txt")
read.tree(file="data/bee distance tree.txt")

#standardised examples
#2mm 4mm bees
max.IT.fitted <- fitted(max.IT.brm,newdata=data.frame(ITD=c(2,4)),re_formula=NA)

mean.IT.fitted <- fitted(mean.IT.brm,newdata=data.frame(ITD=c(2,4)),re_formula=NA)

phys.preds <- data.frame(ITD=c(2,4,2,4),
           limits=c("physio","physio","funct","funct"))

max.IT.func.fitted <- cbind(phys.preds,fitted(max.func.phylo.brm,
                             newdata=phys.preds,
                             re_formula=NA))

mean.IT.func.fitted <- cbind(phys.preds,fitted(mean.func.phylo.brm,
                              newdata=phys.preds,
                              re_formula=NA))

#eusocial trends with physiological and functional
#3.30 (honeybee size)
euc.phys.preds <- data.frame(ITD=c(3.3,3.3,3.3,3.3,3.3,3.3),
                         limits=c("physio","physio","physio","funct","funct","funct"),
                         social_tree=c("Highly Eusocial","Primitively Eusocial","Solitary",
                                       "Highly Eusocial","Primitively Eusocial","Solitary"))
max.IT.euc.func.fitted <- cbind(euc.phys.preds,fitted(max.func.euc.red.phylo.brm,
                             newdata=euc.phys.preds,
                             re_formula=NA))
mean.IT.euc.func.fitted <- cbind(euc.phys.preds,fitted(mean.func.euc.red.phylo.brm,
                             newdata=euc.phys.preds,
                              re_formula=NA))

#max overall
exp(max.IT.fix[1,1]+max.IT.fix[2,1]*2)
exp(mean.IT.fix[1,1]+mean.IT.fix[2,1]*2)
#physio
exp(max.func.fixef[1,1]+max.func.fixef[3,1]+max.func.fixef[2,1]*2+max.func.fixef[4,1]*2)
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)
#functional
exp(max.func.fixef[1,1]+max.func.fixef[2,1]*2)

exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)

#max social
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)
#max primitively social
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)
#max solitary
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)

exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)
exp(fixef(max.IT.brm)[1,1]+fixef(max.IT.brm)[2,1]*2)
