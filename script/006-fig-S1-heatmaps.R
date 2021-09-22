##
##Model comparisons
##Heat maps of MAE between new and pre-existing models
##

forage.traits.max.gl <- forage.traits.max
forage.traits.max.gl$distance <- forage.traits.max.gl$distance/1000

forage.traits.max.gl$gl1 <- foragedist(forage.traits.max.gl$itd)[,1] #max homing
forage.traits.max.gl$gl2 <- foragedist(forage.traits.max.gl$itd)[,3] #max feeder
forage.traits.max.gl$nm1 <- predict(max.social.brm,forage.traits.max.gl,
                                    re_formula = ~(1|species)+(1|genus),
                                    transform = exp)[,1]/1000 #
forage.traits.max.gl$nm2 <- predict(max.social.brm,forage.traits.max.gl,
                                    re_formula = ~(1|genus),
                                    transform = exp)[,1]/1000 #
forage.traits.max.gl$nm3 <- predict(max.social.brm,forage.traits.max.gl,
                                    re_formula = NA,
                                    transform = exp)[,1]/1000 #

forage.traits.max.phy <- forage.traits.max.gl%>%
  filter(measure.type%in%"Potential")
forage.traits.max.func <- forage.traits.max.gl%>%
  filter(measure.type%in%"Realised")

mae.p.df <- rbind(ModelMetrics::mae(forage.traits.max.phy$distance,
    forage.traits.max.phy$gl1),
    ModelMetrics::mae(forage.traits.max.phy$distance,
    forage.traits.max.phy$nm1),
    ModelMetrics::mae(forage.traits.max.phy$distance,
    forage.traits.max.phy$nm2),
    ModelMetrics::mae(forage.traits.max.phy$distance,
    forage.traits.max.phy$nm3))

mae.f.df <- rbind(
  ModelMetrics::mae(forage.traits.max.func$distance,
    forage.traits.max.func$gl1),
  ModelMetrics::mae(forage.traits.max.func$distance,
    forage.traits.max.func$nm1),
  ModelMetrics::mae(forage.traits.max.func$distance,
    forage.traits.max.func$nm2),
  ModelMetrics::mae(forage.traits.max.func$distance,
    forage.traits.max.func$nm3))


forage.traits.mean.gl <- forage.traits.mean
forage.traits.mean.gl$gl1 <- foragedist(forage.traits.mean.gl$itd)[,2] #mean homing
forage.traits.mean.gl$nm1 <- predict(typ.social.brm,forage.traits.mean.gl,
                                    re_formula = ~(1|species)+(1|genus),
                                    transform = exp)[,1]/1000 #
forage.traits.mean.gl$nm2 <- predict(typ.social.brm,forage.traits.mean.gl,
                                    re_formula = ~(1|genus),
                                    transform = exp)[,1]/1000 #
forage.traits.mean.gl$nm3 <- predict(typ.social.brm,forage.traits.mean.gl,
                                    re_formula = NA,
                                    transform = exp)[,1]/1000 #

forage.traits.mean.phy <- forage.traits.mean.gl%>%
  filter(measure.type%in%"Potential")
forage.traits.mean.func <- forage.traits.mean.gl%>%
  filter(measure.type%in%"Realised")


m.mae.p.df <- rbind(ModelMetrics::mae(forage.traits.mean.phy$distance/1000,
                      forage.traits.mean.phy$gl1),
                    ModelMetrics::mae(forage.traits.mean.phy$distance/1000,
                      forage.traits.mean.phy$nm1),
                    ModelMetrics::mae(forage.traits.mean.phy$distance/1000,
                      forage.traits.mean.phy$nm2),
                    ModelMetrics::mae(forage.traits.mean.phy$distance/1000,
                      forage.traits.mean.phy$nm3))

m.mae.f.df <- rbind(
  ModelMetrics::mae(forage.traits.mean.func$distance/1000,
      forage.traits.mean.func$gl1),
  ModelMetrics::mae(forage.traits.mean.func$distance/1000,
      forage.traits.mean.func$nm1),
  ModelMetrics::mae(forage.traits.mean.func$distance/1000,
      forage.traits.mean.func$nm2),
  ModelMetrics::mae(forage.traits.mean.func$distance/1000,
      forage.traits.mean.func$nm3))

distmat.p<-as.data.frame(apply(as.matrix(mae.p.df),1,function(x) mae.p.df-x))
distmat.f<-as.data.frame(apply(as.matrix(mae.f.df),1,function(x) mae.f.df-x))

mp=as.matrix(distmat.p)
mf=as.matrix(distmat.f)

colnames(mp)=c("GL","Phylo+species","Phylo","FE only")
rownames(mp)=c("GL","Phylo+species","Phylo","FE only")
colnames(mf)=c("GL","Phylo+species","Phylo","FE only")
rownames(mf)=c("GL","Phylo+species","Phylo","FE only")


m.distmat.p<-as.data.frame(apply(as.matrix(m.mae.p.df),1,function(x) m.mae.p.df-x))
m.distmat.f<-as.data.frame(apply(as.matrix(m.mae.f.df),1,function(x) m.mae.f.df-x))

m.mp=as.matrix(m.distmat.p)
m.mf=as.matrix(m.distmat.f)

colnames(m.mp)=c("GL","Phylo+species","Phylo","FE only")
rownames(m.mp)=c("GL","Phylo+species","Phylo","FE only")
colnames(m.mf)=c("GL","Phylo+species","Phylo","FE only")
rownames(m.mf)=c("GL","Phylo+species","Phylo","FE only")

new.palette=colorRampPalette(c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020")
                             ,space="rgb")

m.mp[upper.tri(m.mp)]=NA
m.mf[upper.tri(m.mf)]=NA
mp[upper.tri(mp)]=NA
mf[upper.tri(mf)]=NA

mean.hm <- rbind(cbind(limits=c("Potential"),melt(m.mp)),
cbind(limits=c("Realised"),melt(m.mf)))

range(m.mp,na.rm = T)
range(m.mf,na.rm = T)
range(mf,na.rm = T)
range(mp,na.rm = T)

rng = range(c((mf), (mp)),na.rm=T)
#rng



mean.hm.gg <- ggplot(mean.hm,aes(Var1, Var2, fill= value))+
  geom_tile(col="black")+
  theme_bw()+
  ggtitle("A) Typical foraging range")+
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low="#ca0020", mid="#f7f7f7", high="#0571b0", 
                       midpoint=mean(rng),
                       na.value = NA,
                       breaks=seq(-100,100,4), #breaks in the scale bar
                       limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size=11,angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
        legend.position = "none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = unit(c(0.5,4,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  labs(name="ΔMAE (km)",
       fill="ΔMAE (km)")+
  facet_wrap(~limits)


max.hm <- rbind(cbind(limits=c("Potential"),melt(mp)),
                 cbind(limits=c("Realised"),melt(mf)))
max.hm.gg <- ggplot(max.hm,aes(Var1, Var2, fill= value))+
  geom_tile(col="black")+
  theme_bw()+
  ggtitle("B) Maximum foraging range")+
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low="#ca0020", mid="#f7f7f7", high="#0571b0", 
                       midpoint=mean(rng),
                       na.value = NA,
                       breaks=c(0.5,-0.5,-1.5,-2.5,-3.5),
                       limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=11),
        legend.text = element_text(size=10),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size=11,angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
        legend.position = "right",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = unit(c(0.5,4,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  labs(name="ΔMAE (km)",#Δ
       fill="ΔMAE (km)")+
  facet_wrap(~limits)

ggsave(mean.hm.gg/
  max.hm.gg+plot_layout(guides = "collect"),
  file="plots/Figure S1.pdf",
  device = cairo_pdf,
  width=8,
  height=8,
  units="in")

ggsave(mean.hm.gg/
         max.hm.gg+plot_layout(guides = "collect"),
       file="plots/Figure S1.jpg",
       device = "jpg",
       width=8,
       height=8,
       units="in")


####Percentage differences
#max potential
(mae.p.df[2]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[3]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[4]-mae.p.df[1])/mae.p.df[1]

#typ potential
(m.mae.p.df[2]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[3]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[4]-m.mae.p.df[1])/m.mae.p.df[1]

#max realised
(mae.f.df[2]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[3]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[4]-mae.f.df[1])/mae.f.df[1]

#typ realised
(m.mae.f.df[2]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[3]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[4]-m.mae.f.df[1])/m.mae.f.df[1]

