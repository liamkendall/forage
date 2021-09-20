##
library(pollimetry)
library(lattice)
library(ModelMetrics)

library(patchwork)

gl.comps <- pollimetry_dataset%>%
          filter(Superfamily%in%"Apoidea")%>%
          filter(Sex%in%"Female")%>%
            group_by(Genus,Species)%>%
          summarise(ITD=mean(IT))
  
gl.max <- foragedist(max.spp$IT)[,1]
gl.typ <- foragedist(max.spp$IT)[,2]

forage.traits.max.gl <- forage.traits.max
forage.traits.max.gl$gl1 <- foragedist(forage.traits.max.gl$ITD)[,1] #max homing
forage.traits.max.gl$gl2 <- foragedist(forage.traits.max.gl$ITD)[,3] #max feeder
forage.traits.max.gl$nm1 <- predict(max.func.euc.full.phylo.brm,forage.traits.max.gl,
                                    re_formula = ~(1|spp)+(1|Genus),
                                    transform = exp)[,1]/1000 #
forage.traits.max.gl$nm2 <- predict(max.func.euc.full.phylo.brm,forage.traits.max.gl,
                                    re_formula = ~(1|Genus),
                                    transform = exp)[,1]/1000 #
forage.traits.max.gl$nm3 <- predict(max.func.euc.full.phylo.brm,forage.traits.max.gl,
                                    re_formula = NA,
                                    transform = exp)[,1]/1000 #

forage.traits.max.phy <- forage.traits.max.gl%>%
  filter(limits%in%"physio")
forage.traits.max.func <- forage.traits.max.gl%>%
  filter(limits%in%"funct")

mae.p.df <- rbind(mae(forage.traits.max.phy$Distance/1000,
    forage.traits.max.phy$gl1),
mae(forage.traits.max.phy$Distance/1000,
    forage.traits.max.phy$nm1),
mae(forage.traits.max.phy$Distance/1000,
    forage.traits.max.phy$nm2),
mae(forage.traits.max.phy$Distance/1000,
    forage.traits.max.phy$nm3))

mae.f.df <- rbind(
mae(forage.traits.max.func$Distance/1000,
    forage.traits.max.func$gl1),
mae(forage.traits.max.func$Distance/1000,
    forage.traits.max.func$nm1),
mae(forage.traits.max.func$Distance/1000,
    forage.traits.max.func$nm2),
mae(forage.traits.max.func$Distance/1000,
    forage.traits.max.func$nm3))


forage.traits.mean.gl <- forage.traits.mean
forage.traits.mean.gl$gl1 <- foragedist(forage.traits.mean.gl$ITD)[,2] #mean homing
forage.traits.mean.gl$nm1 <- predict(mean.func.euc.full.phylo.brm,forage.traits.mean.gl,
                                    re_formula = ~(1|spp)+(1|Genus),
                                    transform = exp)[,1]/1000 #
forage.traits.mean.gl$nm2 <- predict(mean.func.euc.full.phylo.brm,forage.traits.mean.gl,
                                    re_formula = ~(1|Genus),
                                    transform = exp)[,1]/1000 #
forage.traits.mean.gl$nm3 <- predict(mean.func.euc.full.phylo.brm,forage.traits.mean.gl,
                                    re_formula = NA,
                                    transform = exp)[,1]/1000 #

forage.traits.mean.phy <- forage.traits.mean.gl%>%
  filter(limits%in%"physio")
forage.traits.mean.func <- forage.traits.mean.gl%>%
  filter(limits%in%"funct")


m.mae.p.df <- rbind(mae(forage.traits.mean.phy$Distance/1000,
                      forage.traits.mean.phy$gl1),
                  mae(forage.traits.mean.phy$Distance/1000,
                      forage.traits.mean.phy$nm1),
                  mae(forage.traits.mean.phy$Distance/1000,
                      forage.traits.mean.phy$nm2),
                  mae(forage.traits.mean.phy$Distance/1000,
                      forage.traits.mean.phy$nm3))

m.mae.f.df <- rbind(
  mae(forage.traits.mean.func$Distance/1000,
      forage.traits.mean.func$gl1),
  mae(forage.traits.mean.func$Distance/1000,
      forage.traits.mean.func$nm1),
  mae(forage.traits.mean.func$Distance/1000,
      forage.traits.mean.func$nm2),
  mae(forage.traits.mean.func$Distance/1000,
      forage.traits.mean.func$nm3))

mae.p.df
mae.f.df

(mae.p.df[3]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[4]-mae.p.df[1])/mae.p.df[1]
(mae.p.df[5]-mae.p.df[1])/mae.p.df[1]

(m.mae.p.df[2]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[3]-m.mae.p.df[1])/m.mae.p.df[1]
(m.mae.p.df[4]-m.mae.p.df[1])/m.mae.p.df[1]


(mae.f.df[3]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[4]-mae.f.df[1])/mae.f.df[1]
(mae.f.df[5]-mae.f.df[1])/mae.f.df[1]

(m.mae.f.df[2]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[3]-m.mae.f.df[1])/m.mae.f.df[1]
(m.mae.f.df[4]-m.mae.f.df[1])/m.mae.f.df[1]

distmat.p<-as.data.frame(apply(as.matrix(mae.p.df),1,function(x) mae.p.df-x))
distmat.f<-as.data.frame(apply(as.matrix(mae.f.df),1,function(x) mae.f.df-x))

mp=as.matrix(distmat.p)
mf=as.matrix(distmat.f)

colnames(mp)=c("GL1","Phylo+Spp","Phylo","FE only")
rownames(mp)=c("GL1","Phylo+Spp","Phylo","FE only")
colnames(mf)=c("GL1","Phylo+Spp","Phylo","FE only")
rownames(mf)=c("GL1","Phylo+Spp","Phylo","FE only")


m.distmat.p<-as.data.frame(apply(as.matrix(m.mae.p.df),1,function(x) m.mae.p.df-x))
m.distmat.f<-as.data.frame(apply(as.matrix(m.mae.f.df),1,function(x) m.mae.f.df-x))

m.mp=as.matrix(m.distmat.p)
m.mf=as.matrix(m.distmat.f)

colnames(m.mp)=c("GL1","Phylo+Spp","Phylo","FE only")
rownames(m.mp)=c("GL1","Phylo+Spp","Phylo","FE only")
colnames(m.mf)=c("GL1","Phylo+Spp","Phylo","FE only")
rownames(m.mf)=c("GL1","Phylo+Spp","Phylo","FE only")

new.palette=colorRampPalette(c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020")
                             ,space="rgb")


m.mp[upper.tri(m.mp)]=NA
m.mf[upper.tri(m.mf)]=NA
mp[upper.tri(mp)]=NA
mf[upper.tri(mf)]=NA

p1=levelplot(mp,main="A) Potential range",
             col.regions=new.palette,
             border="black",
             scales=list(y=(list(cex=1)), 
                         tck = c(1,0), 
                         x=list(cex=1, 
                                rot=90)), 
             xlab="", ylab="", 
             legend=list(top=list(fun=grid::textGrob("ΔMAE", y=0, x=1.075))))
p1

p2=levelplot(mf,main="B) Realised range",
             col.regions=new.palette,
             border="black",
             scales=list(y=(list(cex=1)), 
                         tck = c(1,0), 
                         x=list(cex=1, 
                                rot=90)), 
             xlab="", ylab="", 
             legend=list(top=list(fun=grid::textGrob("ΔMAE", y=0, x=1.075))))
p2


p3=levelplot(m.mp,main="A) Potential range",
             col.regions=new.palette,
             border="black",
             scales=list(y=(list(cex=1)), 
                         tck = c(1,0), 
                         x=list(cex=1, 
                                rot=90)), 
             xlab="", ylab="", 
             legend=list(top=list(fun=grid::textGrob("ΔMAE", y=0, x=1.075))))
p3

p4=levelplot(m.mf,main="B) Realised range",
             col.regions=new.palette,
             border="black",
             scales=list(y=(list(cex=1)), 
                         tck = c(1,0), 
                         x=list(cex=1, 
                                rot=90)), 
             xlab="", ylab="", 
             legend=list(top=list(fun=grid::textGrob("ΔMAE", y=0, x=1.075))))
p4
library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2) #6 x 10


library(reshape2)
library(ggplot2)

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
  file="heatmaps MAE.pdf",
  device = cairo_pdf,
  width=8,
  height=8,
  units="in")

ggsave(mean.hm.gg/
         max.hm.gg+plot_layout(guides = "collect"),
       file="heatmaps MAE.jpg",
       device = "jpg",
       width=8,
       height=8,
       units="in")
plot(cbind(ggplotGrob(mean.hm.gg),
ggplotGrob(max.hm.gg)))

