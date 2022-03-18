###FIGURE S1A-B
library(dplyr)

max.species <- forage.traits.bm%>%
  dplyr::select(genus,species,wgt10,sociality)%>%
  filter(!duplicated(species))

max.species$species <- gsub(" ","_",max.species$species)

max.species.l <- data.frame(rbind(max.species,max.species),
                            range.4=c(rep("Max-Potential",nrow(max.species)),
                                      rep("Max-Realised",nrow(max.species)),
                                      rep("Typ-Potential",nrow(max.species)),
                                      rep("Typ-Realised",nrow(max.species))))

max.species.l$max_set_p <- ifelse(max.species.l$species%in%c(forage.traits%>%
                                                             filter(range.4%in%"Max-Potential")%>%
                                                             dplyr::select(species))$species,
                              "IN",
                              "OUT")

max.species.l$max_set_r <- ifelse(max.species.l$species%in%c(forage.traits%>%
                                                             filter(range.4%in%"Max-Realised")%>%
                                                               dplyr::select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ_set_p <- ifelse(max.species.l$species%in%c(forage.traits%>%
                                                             filter(range.4%in%"Typ-Potential")%>%
                                                               dplyr::select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ_set_r <- ifelse(max.species.l$species%in%c(forage.traits%>%
                                                             filter(range.4%in%"Typ-Realised")%>%
                                                               dplyr::select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ.set<-paste(max.species.l$typ_set_p,max.species.l$typ_set_r,sep="_")
max.species.l$max.set<-paste(max.species.l$max_set_p,max.species.l$max_set_r,sep="_")

species.draws.1.r <- all_social_bm_mod%>%
  add_epred_draws(newdata = max.species.l,
                      re_formula = ~(1|species)+(1|genus),
                      allow_new_levels=T)

species.draws.2.r <- all_social_bm_mod%>%
  add_epred_draws(newdata = max.species.l,
                      re_formula = ~(1|genus),
                      allow_new_levels=T)

species.draws.3.r <- all_social_bm_mod%>%
  add_epred_draws(newdata = max.species.l,
                      re_formula = NA,
                      allow_new_levels=T)

species.1.qi <- median_hdci(species.draws.1.r,.value=10^(.epred))%>%
  dplyr::select(max_set_p,max_set_r,typ_set_r,typ_set_p,
         species,sociality,range.4,.value,.lower,.upper,wgt10,typ.set,max.set)
species.2.qi <- median_hdci(species.draws.2.r,.value=10^(.epred))%>%
  dplyr::select(max_set_p,max_set_r,typ_set_r,typ_set_p,
         species,sociality,range.4,.value,.lower,.upper,wgt10,typ.set,max.set)
species.3.qi <- median_hdci(species.draws.3.r,.value=10^(.epred))%>%
  dplyr::select(max_set_p,max_set_r,typ_set_r,typ_set_p,
         species,sociality,range.4,.value,.lower,.upper,wgt10,typ.set,max.set)

species.1.qi$model <- "Model_1"
species.2.qi$model <- "Model_2"
species.3.qi$model <- "Model_3"

species.mean.values <- rbind.fill(species.1.qi,
                          species.2.qi,
                          species.3.qi)%>%
  dplyr::select(max_set_r,max_set_p,typ_set_r,typ_set_p,species,sociality,range.4,
         .value,.lower,.upper,wgt10,model,typ.set,max.set)

species.mean.values$species <- gsub("_"," ",species.mean.values$species)

species.values.typ <- species.mean.values%>%
  filter(range.4%in%c("Typ-Potential","Typ-Realised"))%>%
  filter(!typ.set%in%c("OUT_OUT"))

species.values.typ$model.set.r <- paste(species.values.typ$typ_set_r,species.values.typ$model)
species.values.typ$model.set.p <- paste(species.values.typ$typ_set_p,species.values.typ$model)

model.cols.typ.set <- c("IN Model_1" = "#757b16",
                    "IN Model_2" = "#2F638F",
                    "IN Model_3" = "#490B0A",
                    "OUT Model_1" = lighten("#757b16",amount = 0.5),
                    "OUT Model_2" = lighten("#2F638F",amount = 0.5),
                    "OUT Model_3" = lighten("#490B0A",amount = 0.5))

species.values.typ.p <- species.values.typ%>%
  filter(range.4%in%"Typ-Potential")%>%
  arrange(desc(wgt10))

forage.traits.bm$species <- gsub("_"," ",forage.traits.bm$species)

pd <- position_dodgev(height = 0.35)

typ.all.sp.p <- ggplot(species.values.typ.p,
                       aes(y=reorder(species,wgt10),
                            col=model.set.p))+
  #coord_cartesian(xlim=c(0,16),clip="off")+
  scale_x_log10(breaks=c(0.01,0.1,1,10,100),
                labels = function(x) format(x, scientific = FALSE))+
  geom_errorbarh(aes(xmin=.lower,
                     xmax=.upper,
                     col=model.set.p),
                 inherit.aes = T,
                 position = pd,
                 height=0.1)+
  geom_jitter(data=forage.traits.bm%>%
               #filter(range.type%in%"Typical")%>%
               filter(range.4%in%"Typ-Potential"),
             aes(x=dist.km,
                 y=reorder(species,wgt10),
                 col=range.type,fill=range.type),alpha=0.75,
             height=0.3,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 fill=model.set.p,
                 pch=range.4),
             size=2.5,col="black",
             position = pd)+
  #geom_text(data=too.high.text,aes(y=reorder(species,wgt10),x=20,
  #                                 label=text),size=3,
  #           hjust="right",fontface="bold",
  #           inherit.aes = F)+
  xlab("Potential range (km)")+
  ylab("Taxa")+
  labs(name="Metric type",
       fill="Metric type",
       col="Metric type")+
  scale_color_manual(values=model.cols.typ.set)+
  scale_fill_manual(values=model.cols.typ.set)+
  scale_shape_manual(values=c(21,21))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3.5,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text=element_text(family="Times"),
        axis.title=element_text(family="Times"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=8,face="italic"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold",family="Times"),
        title = element_text(size=13,face="bold",family="Times"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  ggtitle("A) Typical foraging range")

typ.all.sp.r <- ggplot(species.values.typ%>%
                         filter(range.4%in%"Typ-Realised"),aes(y=reorder(species,wgt10),
                                                           col=model.set.r))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
        col=model.set.r),
    inherit.aes = T,
    position = pd,
    height=0.1)+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1,10))+
  geom_jitter(data=forage.traits.bm%>%
                #filter(range.type%in%"Typical")%>%
                filter(range.4%in%"Typ-Realised"),
              aes(x=dist.km,
                  y=reorder(species,wgt10),
                  col=range.type,
                  fill=range.type),alpha=0.75,
              height=0.3,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #y=species,
                 fill=model.set.r,
                 pch=range.4),
             size=2.5,col="black",
             position = pd)+
  xlab("Realized range (km)")+
  ylab("Taxa")+
  labs(name="Metric type",
       fill="Metric type",
       col="Metric type")+
  scale_color_manual(values=model.cols.typ.set)+
  scale_fill_manual(values=model.cols.typ.set)+
  scale_shape_manual(values=c(21,21))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3.5,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,      
        axis.text=element_text(family="Times"),
        axis.title=element_text(family="Times"),  
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold",family="Times"),
        legend.position = "none",
        title = element_text(size=13,face="bold",family="Times"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))

species.values.max <- species.mean.values%>%
  filter(range.4%in%c("Max-Potential","Max-Realised"))%>%
  filter(!max.set%in%c("OUT_OUT"))

species.values.max$model.set.p <- paste(species.values.max$max_set_p,species.values.max$model)
species.values.max$model.set.r <- paste(species.values.max$max_set_r,species.values.max$model)

model.cols.max.set <- c("IN Model_1" = "#757b16",
                        "IN Model_2" = "#2F638F",
                        "IN Model_3" = "#490B0A",
                        "OUT Model_1" = lighten("#757b16",amount = 0.5),
                        "OUT Model_2" = lighten("#2F638F",amount = 0.5),
                        "OUT Model_3" = lighten("#490B0A",amount = 0.5))

species.values.max2 <- species.values.max%>%
  filter(range.4%in%"Max-Potential")%>%
  arrange(desc(wgt10))


max.all.sp.p <- ggplot(species.values.max2,aes(y=reorder(species,wgt10),
                          col=model.set.p,
                          fill=model.set.p))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
       # y=species,
        col=model.set.p),
   # inherit.aes = F,
    position = pd,
    height=0.1)+
  #coord_cartesian(xlim=c(0,23))+
  scale_x_log10()+
  geom_jitter(data=forage.traits.bm%>%
               filter(range.4%in%"Max-Potential"),
             aes(x=dist.km,
                 y=reorder(species,wgt10),
                 col=range.type,
                 fill=range.type),alpha=0.75,
             height=0.3,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 #   y=species,
                 fill=model.set.p,
                 pch=range.4),
             size=2.5,col="black",
             position = pd)+
  xlab("Potential range (km)")+
  ylab("Taxa")+
  labs(name="Metric type",
       fill="Metric type",
       col="Metric type")+
  scale_color_manual(values=model.cols.max.set)+
  scale_fill_manual(values=model.cols.max.set)+
  scale_shape_manual(values=c(21,21))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3.5,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text=element_text(family="Times"),
        axis.title=element_text(family="Times"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=8,face="italic"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold",family="Times"),
        legend.position = "none",
        title = element_text(size=13,face="bold",family="Times"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  ggtitle("B) Maximum foraging range")

max.all.sp.p

max.all.sp.r <- ggplot(species.values.max%>%
                         filter(range.4%in%"Max-Realised"),
                       aes(y=reorder(species,wgt10),
                           col=model.set.r,
                           fill=model.set.r))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
      #  y=species,
        col=model.set.r),
   # inherit.aes = F,
    position = pd,
    height=0.1)+
  #coord_cartesian(xlim=c(0,100))+
  scale_x_log10()+
  geom_jitter(data=forage.traits.bm%>%
               #filter(range.type%in%"Maximum")%>%
                filter(range.4%in%"Max-Realised"),
              aes(x=dist.km,
                  y=reorder(species,wgt10),
                  col=range.type,
                  fill=range.type),alpha=0.75,
              height=0.3,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #       y=species,
                 fill=model.set.r,
                 pch=range.4),
             size=2.5,col="black",
             position = pd)+
  xlab("Realized range (km)")+
  ylab("Taxa")+
  labs(name="Metric type",
       fill="Metric type",
       col="Metric type")+
  scale_color_manual(values=model.cols.max.set)+
  scale_fill_manual(values=model.cols.max.set)+
  scale_shape_manual(values=c(21,21))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3.5,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text=element_text(family="Times"),
        axis.title=element_text(family="Times"),
        axis.text.x = element_text(size=11),        
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        title = element_text(size=13,face="bold",family="Times"),
        strip.text =  element_text(size=13,face="bold",family="Times"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))



typ.all.sp.p.gg <- ggplotGrob(typ.all.sp.p)
typ.all.sp.r.gg <- ggplotGrob(typ.all.sp.r)
max.all.sp.p.gg <- ggplotGrob(max.all.sp.p)
max.all.sp.r.gg <- ggplotGrob(max.all.sp.r)

sp.typ.plots <- cbind(typ.all.sp.p.gg,typ.all.sp.r.gg)
sp.max.plots <- cbind(max.all.sp.p.gg,max.all.sp.r.gg)

ggsave(sp.typ.plots,file="plots/Figure S4A.jpg",
       device = "jpg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi=600)
ggsave(sp.typ.plots,file="plots/Figure S4A.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi=600)

ggsave(sp.max.plots,file="plots/Figure S4B.jpg",
       device = "jpg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi=600)

ggsave(sp.max.plots,file="plots/Figure S4B.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi=600)



      