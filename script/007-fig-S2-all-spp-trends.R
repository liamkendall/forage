###FIGURE S1A-B

max.species <- forage.traits%>%
  select(genus,species,itd,sociality)%>%
  filter(!duplicated(species))

max.species.l <- data.frame(rbind(max.species,max.species),measure.type=
                 c(rep("Potential",nrow(max.species)),
                       rep("Realised",nrow(max.species))))

max.species.l$max_set_p <- ifelse(max.species.l$species%in%c(forage.traits.max%>%
                                                             filter(measure.type%in%"Potential")%>%
                                                             select(species))$species,
                              "IN",
                              "OUT")

max.species.l$max_set_r <- ifelse(max.species.l$species%in%c(forage.traits.max%>%
                                                             filter(measure.type%in%"Realised")%>%
                                                             select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ_set_p <- ifelse(max.species.l$species%in%c(forage.traits.mean%>%
                                                             filter(measure.type%in%"Potential")%>%
                                                             select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ_set_r <- ifelse(max.species.l$species%in%c(forage.traits.mean%>%
                                                             filter(measure.type%in%"Realised")%>%
                                                             select(species))$species,
                              "IN",
                              "OUT")

max.species.l$typ.set<-paste(max.species.l$typ_set_p,max.species.l$typ_set_r,sep="_")
max.species.l$max.set<-paste(max.species.l$max_set_p,max.species.l$max_set_r,sep="_")

species.draws.max.1.r <- max.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = ~(1|species)+(1|genus),
                      allow_new_levels=T,
                      transform=exp)

species.draws.max.2.r <- max.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = ~(1|genus),
                      allow_new_levels=T,
                      transform=exp)

species.draws.max.3.r <- max.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = NA,
                      allow_new_levels=T,
                      transform=exp)

species.max.1.qi <- median_hdci(species.draws.max.1.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)
species.max.2.qi <- median_hdci(species.draws.max.2.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)
species.max.3.qi <- median_hdci(species.draws.max.3.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)

species.draws.mean.1.r <- typ.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = ~(1|species)+(1|genus),
                      allow_new_levels=T,
                      transform=exp)

species.draws.mean.2.r <- typ.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = ~(1|genus),
                      allow_new_levels=T,
                      transform=exp)

species.draws.mean.3.r <- typ.social.brm%>%
  add_predicted_draws(newdata = max.species.l,
                      re_formula = NA,
                      allow_new_levels=T,
                      transform=exp)

species.mean.1.qi <- median_hdci(species.draws.mean.1.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)

species.mean.2.qi <- median_hdci(species.draws.mean.2.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)

species.mean.3.qi <- median_hdci(species.draws.mean.3.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,species,sociality,measure.type,.value,.lower,.upper,itd,typ.set,max.set)

species.max.1.qi$model <- "Max_1"
species.max.2.qi$model <- "Max_2"
species.max.3.qi$model <- "Max_3"

species.mean.1.qi$model <- "Typ_1"
species.mean.2.qi$model <- "Typ_2"
species.mean.3.qi$model <- "Typ_3"

species.mean.values <- rbind.fill(species.max.1.qi,
                          species.max.2.qi,
                          species.max.3.qi,
                          species.mean.1.qi,
                          species.mean.2.qi,
                          species.mean.3.qi)%>%
  select(max_set_r,max_set_p,typ_set_r,typ_set_p,species,sociality,measure.type,.value,.lower,.upper,itd,model,typ.set,max.set)

species.mean.values$species <- gsub("_"," ",species.mean.values$species)

species.mean.values[,8:10] <- species.mean.values[,8:10]/1000

species.values.typ <- species.mean.values%>%
  filter(model%in%c("Typ_1","Typ_2","Typ_3"))%>%
  filter(!typ.set%in%c("OUT_OUT"))

species.values.typ$model.set.r <- paste(species.values.typ$typ_set_r,species.values.typ$model)
species.values.typ$model.set.p <- paste(species.values.typ$typ_set_p,species.values.typ$model)

table(species.values.typ$model.set.r)


model.cols.typ.set <- c("IN Typ_1" = "#757b16",
                    "IN Typ_2" = "#2F638F",
                    "IN Typ_3" = "#490B0A",
                    "OUT Typ_1" = lighten("#757b16",amount = 0.5),
                    "OUT Typ_2" = lighten("#2F638F",amount = 0.5),
                    "OUT Typ_3" = lighten("#490B0A",amount = 0.5))

too.high.typ <- species.values.typ%>%
  filter(measure.type%in%"Potential")%>%
  arrange(desc(itd))

too.high.typ <- too.high.typ[1:9,]

mean(too.high.typ[1:3,".upper"])
mean(too.high.typ[4:6,".upper"])
mean(too.high.typ[7:9,".upper"])

species.values.typ2 <- species.values.typ%>%
  filter(measure.type%in%"Potential")%>%
  arrange(desc(itd))

species.values.typ2[1:9,".upper"] = 16.8
#species.values.typ2[1:6,".lower"] = 16
#species.values.typ2[1:6,".value"] = NA

forage.traits.plot2$species <- gsub("_"," ",forage.traits.plot2$species)

typ.all.sp.p <- ggplot(species.values.typ2,
                       aes(y=reorder(species,itd),
                            col=model.set.p))+
  coord_cartesian(xlim=c(0,16),clip="off")+
  geom_errorbarh(aes(xmin=.lower,
                     xmax=.upper,
                     col=model.set.p),
                 inherit.aes = T,
                 position = pd,
                 height=0.1)+
  geom_jitter(data=forage.traits.plot2%>%
               filter(range.type%in%"Typical")%>%
               filter(measure.type%in%"Potential"),
             aes(x=distance/1000,
                 y=reorder(species,itd),
                 col=range.type,fill=range.type),alpha=0.75,
             height=0.25,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 fill=model.set.p,
                 pch=measure.type),
             size=2,col="black",
             position = pd)+
  #geom_text(data=too.high.text,aes(y=reorder(species,itd),x=20,
  #                                 label=text),size=3,
  #           hjust="right",fontface="bold",
  #           inherit.aes = F)+
  xlab("Potential foraging range (km)")+
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
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=8,face="italic"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  ggtitle("B) Typical foraging range")
typ.all.sp.p

typ.all.sp.r <- ggplot(species.values.typ%>%
                         filter(measure.type%in%"Realised"),aes(y=reorder(species,itd),
                                                           col=model.set.r))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
        col=model.set.r),
    inherit.aes = T,
    position = pd,
    height=0.1)+
  geom_jitter(data=forage.traits.plot2%>%
                filter(range.type%in%"Typical")%>%
                filter(measure.type%in%"Realised"),
              aes(x=distance/1000,
                  y=reorder(species,itd),
                  col=range.type,
                  fill=range.type),alpha=0.75,
              height=0.25,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #y=species,
                 fill=model.set.r,
                 pch=measure.type),
             size=2,col="black",
             position = pd)+
  xlab("Realised foraging range (km)")+
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
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))

species.values.max <- species.mean.values%>%
  filter(model%in%c("Max_1",
                    "Max_2",
                    "Max_3"))%>%
  filter(!max.set%in%c("OUT_OUT"))

species.values.max$model.set.p <- paste(species.values.max$max_set_p,species.values.max$model)
species.values.max$model.set.r <- paste(species.values.max$max_set_r,species.values.max$model)

model.cols.max.set <- c("IN Max_1" = "#757b16",
                        "IN Max_2" = "#2F638F",
                        "IN Max_3" = "#490B0A",
                        "OUT Max_1" = lighten("#757b16",amount = 0.5),
                        "OUT Max_2" = lighten("#2F638F",amount = 0.5),
                        "OUT Max_3" = lighten("#490B0A",amount = 0.5))

too.high.max <- species.values.max%>%
  filter(measure.type%in%"Potential")%>%
  filter(model%in%c("Max_1",
                   "Max_2",
                   "Max_3"))%>%
  group_by(species)%>%
  arrange(desc(itd))

too.high.max <- too.high.max[1:30,]

round(mean(too.high.max$.value[1:3]))
round(mean(too.high.max$.upper[1:3]))
round(mean(too.high.max[too.high.max$species%in%"Eufriesea surinamensis",]$.upper))
round(mean(too.high.max[too.high.max$species%in%"Xylocopa virginica",]$.upper))
round(mean(too.high.max$.upper[10:12]))
round(mean(too.high.max$.upper[13:15]))
round(mean(too.high.max$.upper[16:18]))
round(mean(too.high.max$.upper[19:21]))


too.high.max$text <- paste(c(round(too.high.max$.value[1]),"","","","","","")," (",
                           round(too.high.max$.upper),")",sep="")


species.values.max2 <- species.values.max%>%
  filter(measure.type%in%"Potential")%>%
  arrange(desc(itd))

#species.values.max2[1:7,".upper"]=NA
#species.values.max2[1,".value"]=NA

max.all.sp.p <- ggplot(species.values.max2,aes(y=reorder(species,itd),
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
  #geom_text(data=too.high.max,aes(y=reorder(species,itd),x=22,
  #                                 label=text),size=3,
  #            hjust="right",
  #          fontface="bold",
  #           inherit.aes = F)+
  coord_cartesian(xlim=c(0,23))+
  geom_jitter(data=forage.traits.plot2%>%
               filter(range.type%in%"Maximum")%>%
               filter(measure.type%in%"Potential"),
             aes(x=distance/1000,
                 y=reorder(species,itd),
                 col=range.type,
                 fill=range.type),alpha=0.75,
             height=0.25,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 #   y=species,
                 fill=model.set.p,
                 pch=measure.type),
             size=2,col="black",
             position = pd)+
  xlab("Potential foraging range (km)")+
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
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=8,face="italic"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  ggtitle("A) Maximum foraging range")

max.all.sp.p

max.all.sp.r <- ggplot(species.values.max%>%
                         filter(measure.type%in%"Realised"),aes(y=reorder(species,itd),
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
  geom_jitter(data=forage.traits.plot2%>%
                filter(range.type%in%"Maximum")%>%
                filter(measure.type%in%"Realised"),
              aes(x=distance/1000,
                  y=reorder(species,itd),
                  col=range.type,
                  fill=range.type),alpha=0.75,
              height=0.25,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #       y=species,
                 fill=model.set.r,
                 pch=measure.type),
             size=2,col="black",
             position = pd)+
  xlab("Realised foraging range (km)")+
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
        axis.text.x = element_text(size=11),        
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=13,face="bold"),
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

ggsave(sp.max.plots,file="plots/Figure S2A.jpeg",
       device = "jpeg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)
ggsave(sp.max.plots,file="plots/Figure S2A.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)

ggsave(sp.typ.plots,file="plots/Figure S2B.jpeg",
       device = "jpeg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)
ggsave(sp.typ.plots,file="plots/Figure S2B.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)


      