#visualising phylogeny
library(ggtree)
library(treeio)
library(phyloseq)
library(ggridges)
library(dplyr)
library(ggtree)


#species from trait dataframe
max.species=unique(as.character(forage.traits.max$Genus_species))
mean.species=unique(as.character(forage.traits.mean$Genus_species))
all.species=unique(as.character(forage.traits$Genus_species))

bee.max.spp.tree <- bee.max.tree
bee.mean.spp.tree <- bee.mean.tree

bee.mcmc


bee.all.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits$Genus)))

## Will's suggestion
bee.all.tree$tip.label<-paste(bee.all.tree$tip.label,"_xyz",sep="")
for(i in 1:length(all.species)){
  bee.all.tree<-add.species.to.genus(bee.all.tree,all.species[i],
                                     where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.all.tree$tip.label)
bee.all.tree<-drop.tip(bee.all.tree,bee.all.tree$tip.label[ii])
bee.all.tree$node.label=NULL


## Will's suggestion
bee.max.spp.tree$tip.label<-paste(bee.max.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(max.species)){
  bee.max.spp.tree<-add.species.to.genus(bee.max.spp.tree,max.species[i],
                                         where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.max.spp.tree$tip.label)
bee.max.spp.tree<-drop.tip(bee.max.spp.tree,bee.max.spp.tree$tip.label[ii])
bee.max.spp.tree$node.label=NULL

#mean
bee.mean.spp.tree$tip.label<-paste(bee.mean.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(mean.species)){
  bee.mean.spp.tree<-add.species.to.genus(bee.mean.spp.tree,mean.species[i],
                                          where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.mean.spp.tree$tip.label)
bee.mean.spp.tree<-drop.tip(bee.mean.spp.tree,bee.mean.spp.tree$tip.label[ii])
bee.mean.spp.tree$node.label=NULL

bee.max.spp.tree
bee.mean.spp.tree


forage.summaries <- forage.traits[,c("Family","Genus_species","distance_type2","social_tree","ITD","Distance")]%>%
  group_by(Family,Genus_species,distance_type2,social_tree)%>%
  summarise(mean_dist=mean(Distance),
            mean_log_dist=mean(log(Distance)),
            sd_dist=sd(Distance),
            n_dist=n())#%>%
  tibble::column_to_rownames("Genus_species")

forage.summaries.max <- forage.summaries%>%
  filter(distance_type2%in%"Max")%>%
  ungroup()%>%
  select(Genus_species,mean_log_dist)%>%
  tibble::column_to_rownames("Genus_species")%>%
  t()

forage.summaries.mean <- forage.summaries%>%
  filter(distance_type2%in%"Typical")%>%
  ungroup()%>%
  select(Genus_species,mean_log_dist)%>%
  tibble::column_to_rownames("Genus_species")%>%
  t()


names(forage.summaries.max) <- colnames(forage.summaries.max)

names(forage.summaries.mean) <- colnames(forage.summaries.mean)

setdiff(bee.max.spp.tree$tip.label,rownames(forage.summaries.max))
setdiff(colnames(forage.summaries.max),bee.max.spp.tree$tip.label)

contMap(bee.max.spp.tree,x=forage.summaries.max)
contMap(bee.mean.spp.tree,x=forage.summaries.mean)
head(forage.summaries)

boxplot(forage.summaries$mean_dist~forage.summaries$distance_type2)


?geom_joy

ggplot(forage.summaries,aes(x=mean_dist,y=Genus_species,col=distance_type2))+
  geom_point()+
  scale_x_log10()+
  geom_errorbarh(aes(xmin=mean_dist-(sd_dist/sqrt(n_dist)),
                     xmax=mean_dist+(sd_dist/sqrt(n_dist))))
?contMap

p <- ggtree(mergedGP) + 
  geom_tippoint(aes(color=Phylum), size=1.5)

facet_plot(p, panel="Abundance", data=melt_simple, 
           geom_joy, mapping = aes(x=val,group=label, 
                                   fill=Phylum), 
           color='grey80', lwd=.3)


forage.traits

#A7473A"
"#4B5F6C"

max.cols=c(
           "Maximum"="#A7473A",
           "Typical"="#4B5F6C", #dark blue
           
           "Observed"="#A8B9CB"
           )

forage.traits.plot <- rbind.fill(forage.traits,forage.traits.obs)
forage.traits.plot$distance_type2 <- revalue(forage.traits.plot$distance_type2,
                                             c("Max" = "Maximum"))

forage.traits.plot$Genus2 <- ifelse(forage.traits.plot$spp%in%"Apis_mellifera","Apis mellifera",forage.traits.plot$Genus)
forage.traits.plot$Genus2 <- ifelse(forage.traits.plot$spp%in%"Bombus_terrestris","Bombus terrestris",forage.traits.plot$Genus2)

forage.traits.plot$group2 <- ifelse((forage.traits.plot$social_tree%in%"Highly Eusocial"&
                                      !forage.traits.plot$Genus%in%c("Apis","Bombus")),"Apjdae",forage.traits.plot$Family)


forage.traits.plot2 <- forage.traits.plot %>%
  arrange(group2, Genus2) %>%               # sort your dataframe
  mutate(Genus2 = factor(Genus2, unique(Genus2)))

forage.traits.plot2$Family

fam.labs <- data.frame(Genus2=c("Andrena","Anthophora","Colletes","Lasioglossum","Chelostoma","Dasypoda"),
Family=c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae","Melittidae"))


forage.sum.plots <- ggplot(forage.traits.plot2,
                           aes(x=Distance/1000,
                         y=forcats::fct_rev(Genus2),
                         col=distance_type2,
                         label=Family,
                         fill=distance_type2))+
  geom_jitter(pch=21,col="black",height=0.1,width=0,size=2)+
  #scale_x_log10()+
  geom_density_ridges(rel_min_height=.01,alpha=0.1,scale = 0.5)+
  theme_bw()+ 
  #scale_y_continuous(position = "right")+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3,
        #  panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=10,face="italic"),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        strip.text = element_text(),
        legend.position = "bottom",
        legend.direction="vertical",
        panel.spacing = unit(0.5,"lines"),
        #legend.box = "horizontal",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=max.cols) + 
  scale_colour_manual(values=max.cols) +
  xlab("Foraging range (km)")+
  ylab("Taxa")+
  #xlim(0,25)+
  guides(pch = "none")+
  labs(name="Distance type",
       fill="Distance type",
       col="Distance type")+
  geom_text(data=fam.labs,
            inherit.aes = F,
            aes(y = Genus2,label=Family), x = 27,
            hjust = 0, col="black", size = 4) +
  coord_cartesian(#xlim = c(0, 10), # This focuses the x-axis on the range of interest
                  clip = 'off') + 
 # scale_x_log10()+  # This keeps the labels from disappearing
  theme(plot.margin = unit(c(1,3,1,1), "lines"))
  

    
    
    
    
    geom_bracket(
      xmin = "Andrena",
      xmax = "Panurgus", y.position = 30,
      label = "Andrena",col="black",
      coord.flip = F,
      inherit.aes = F,
      tip.length = c(0.2, 0.02)
    )+
#library(ggpubr)
forage.sum.plots

ggsave(forage.sum.plots,file="forage summaries.pdf",
       device="pdf",
       width=4,
       height=8)


forage.traits.plot2$distance_type2 <- factor(forage.traits.plot2$distance_type2,levels=c("Maximum","Typical","Observed"))

#forage.sum.plots2 <- 
p <- ggplot(forage.traits.plot2,
                           aes(y=Distance/1000,
                               x=forcats::fct_rev(Genus2),
                               col=distance_type2,
                              # width = ..density..,
                               label=Family,
                               fill=distance_type2))+
  geom_boxplot(col="darkgrey",fill="lightgrey", width=0.2,
               position = position_nudge(x = 0.4, y = 0))+
  geom_jitter(pch=21,col="black",width=0.1)+
geom_bracket(
    xmin = "Andrena",
    xmax = "Panurgus",
    y.position = log10(30),
    label = "",col="black",
    coord.flip = F,
    inherit.aes = F,
    tip.length = c(0.02, 0.02))+
  geom_bracket(
    xmin = "Anthophora",
    xmax = "Trigona", y.position = log10(30),
    label = "",col="black",
    coord.flip = F,
    inherit.aes = F,
    tip.length = c(0.02, 0.02))+
  geom_bracket(
    xmin = "Colletes",
    xmax = "Hylaeus", y.position = log10(30),
    label = "",col="black",
    coord.flip = F,
    inherit.aes = F,
    tip.length = c(0.02, 0.02))+
  geom_bracket(
    xmin = "Lasioglossum",
    xmax = "Nomia", y.position = log10(30),
    label = "",col="black",
    coord.flip = F,
    inherit.aes = F,
    tip.length = c(0.02, 0.02))+
  geom_bracket(
    xmin = "Chelostoma",
    xmax = "Rhodanthidium", y.position = log10(30),
    label = "",
    col="black",
    coord.flip = F,
    inherit.aes = F,
    tip.length = c(0.02, 0.02))+
  theme_bw()+
theme(plot.title = element_text(hjust = 0,face="bold",size=14),
      legend.box.background = element_rect(colour = "black"),
      legend.title = element_text(size=12,face="bold"),
      legend.text = element_text(size=11),
      aspect.ratio = 3,
      #  panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      text=element_text(),
      axis.ticks.length = unit(0,"mm"),
      legend.title.align=0.5,
      axis.text.y = element_text(size=10,face="italic"),
      axis.text.x = element_text(size=10),
      axis.title.y = element_text(size=12),
      axis.title.x = element_text(size=12),
      strip.text = element_text(),
      legend.position = "right",
      legend.direction="vertical",
      panel.spacing = unit(0.5,"lines"),
      plot.margin = unit(c(1,1,1,1), "lines"),
      #legend.box = "horizontal",
      legend.background = element_blank(),
      panel.border = element_rect(color = "black",
                                  fill = NA, size = 0.4))+
  scale_fill_manual(values=max.cols) + 
  scale_colour_manual(values=max.cols)+


  ylab("Foraging range (km)")+
  xlab("Taxa")+
  guides(pch = "none")+
  labs(name="Distance type",
       fill="Distance type",
       col="Distance type")+
geom_text(data=fam.labs,
          inherit.aes = F,nudge_x=0.2,
          aes(x = Genus2,label=Family), y = log10(50),
          hjust = 0, col="black", size = 3.5,fontface="bold")+
  coord_flip(clip = 'off')
  
p

  #geom_vridgeline(stat="ydensity")#, trim=FALSE, alpha = 0.85, scale = 2)

 
sum.grob <- ggplotGrob(p+
  scale_y_log10())

plot(sum.grob)


ggsave(sum.grob,file="foraging summary.pdf",
       width=6,
       height=9,
       units="in")
