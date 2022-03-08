##
##Model comparisons
##Heat maps of MAE between new and pre-existing models
##
forage.traits.gl <- forage.traits.bm

forage.traits.gl$gl1 <- ifelse(forage.traits.gl$range.type=="Max",
                               foragedist(forage.traits.gl$itd)[,1],
                               foragedist(forage.traits.gl$itd)[,2]) #max or mean homing

forage.traits.gl$nm1 <- 10^(predict(all_social_bm_mod,forage.traits.gl,
                                    re_formula = ~(1|species)+(1|genus)
                                    #transform = exp
)[,1]) #
forage.traits.gl$nm2 <- 10^(predict(all_social_bm_mod,forage.traits.gl,
                                    re_formula = ~(1|genus)
                                    #transform = exp
)[,1]) #
forage.traits.gl$nm3 <- 10^(predict(all_social_bm_mod,forage.traits.gl,
                                    re_formula = NA
                                    #transform = exp
)[,1]) #

forage.traits.max.phy <- forage.traits.gl%>%
  filter(measure.type%in%"Potential" &
           range.type%in%"Max")
forage.traits.max.func <- forage.traits.gl%>%
  filter(measure.type%in%"Realised"&
           range.type%in%"Max")

mae.p.df <- rbind(ModelMetrics::mae(forage.traits.max.phy$dist.km,
                                    forage.traits.max.phy$gl1),
                  ModelMetrics::mae(forage.traits.max.phy$dist.km,
                                    forage.traits.max.phy$nm1),
                  ModelMetrics::mae(forage.traits.max.phy$dist.km,
                                    forage.traits.max.phy$nm2),
                  ModelMetrics::mae(forage.traits.max.phy$dist.km,
                                    forage.traits.max.phy$nm3))

mae.f.df <- rbind(
  ModelMetrics::mae(forage.traits.max.func$dist.km,
                    forage.traits.max.func$gl1),
  ModelMetrics::mae(forage.traits.max.func$dist.km,
                    forage.traits.max.func$nm1),
  ModelMetrics::mae(forage.traits.max.func$dist.km,
                    forage.traits.max.func$nm2),
  ModelMetrics::mae(forage.traits.max.func$dist.km,
                    forage.traits.max.func$nm3))

forage.traits.mean.phy <- forage.traits.gl%>%
  filter(measure.type%in%"Potential" &
           range.type %in%"Typical")
forage.traits.mean.func <- forage.traits.gl%>%
  filter(measure.type%in%"Realised"&
           range.type %in%"Typical")

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

colnames(mp)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
rownames(mp)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
colnames(mf)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
rownames(mf)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")


m.distmat.p<-as.data.frame(apply(as.matrix(m.mae.p.df),1,function(x) m.mae.p.df-x))
m.distmat.f<-as.data.frame(apply(as.matrix(m.mae.f.df),1,function(x) m.mae.f.df-x))

m.mp=as.matrix(m.distmat.p)
m.mf=as.matrix(m.distmat.f)

colnames(m.mp)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
rownames(m.mp)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
colnames(m.mf)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")
rownames(m.mf)=c("Greenleaf et al. (2007)","Phylo + species","Phylo","FE only")

new.palette=colorRampPalette(c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020")
                             ,space="rgb")

m.mp[upper.tri(m.mp)]=NA
m.mf[upper.tri(m.mf)]=NA
mp[upper.tri(mp)]=NA
mf[upper.tri(mf)]=NA

mean.hm <- rbind(cbind(limits=c("Potential"),melt(m.mp)),
                 cbind(limits=c("Realized"),melt(m.mf)))

range(m.mp,na.rm = T)
range(m.mf,na.rm = T)
range(mf,na.rm = T)
range(mp,na.rm = T)

rng = range(c((mf), (mp)),na.rm=T)
#rng

mean.hm.gg <- ggplot(mean.hm,aes(Var1, Var2, fill= value))+
  geom_tile(width=1)+
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
        axis.text = element_text(family="Times"),
        axis.title = element_text(family="Times"),
        title = element_text(family="Times"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size=11,angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold",family="Times"),
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
                cbind(limits=c("Realized"),melt(mf)))
max.hm.gg <- ggplot(max.hm,aes(Var1, Var2, fill= value))+
  geom_tile(width=1)+
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
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text = element_text(family="Times"),
        axis.title = element_text(family="Times"),
        title = element_text(family="Times"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size=11,angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold",family="Times"),
        legend.position = "none",
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

mean.hm.gg/
  max.hm.gg+plot_layout(guides = "collect")

ggsave(mean.hm.gg/
         max.hm.gg+plot_layout(guides = "collect"),
       file="plots/Figure S3.pdf",
       device = cairo_pdf,
       width=8,
       height=8,
       units="in",
       dpi=600)

ggsave(mean.hm.gg/
         max.hm.gg+plot_layout(guides = "collect"),
       file="plots/Figure S3.jpg",
       device = "jpg",
       width=8,
       height=8,
       units="in",
       dpi=600)


####Percentage differences
#typ potential
(m.mae.p.df[2]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[3]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[4]-m.mae.p.df[1])/m.mae.p.df[1]

#max potential
(mae.p.df[2]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[3]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[4]-mae.p.df[1])/mae.p.df[1]

#typ realised
(m.mae.f.df[2]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[3]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[4]-m.mae.f.df[1])/m.mae.f.df[1]

#max realised
(mae.f.df[2]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[3]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[4]-mae.f.df[1])/mae.f.df[1]



