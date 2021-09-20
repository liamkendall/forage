###FIGURE S1A-B

max.spp <- traits%>%
  filter(Genus_species%in%union(levels(factor(forage.traits.max$spp)),levels(factor(forage.traits.mean$spp))))%>%
  mutate(Genus=word(Genus_species,1,sep="_"))%>%
  mutate(spp=Genus_species)

max.spp$social_tree <- ifelse(is.na(max.spp$social_tree)==T,
                               "Solitary",
                               max.spp$social_tree)

max.spp.l <- data.frame(rbind(max.spp,max.spp),limits=
                 c(rep("physio",nrow(max.spp)),
                       rep("funct",nrow(max.spp))))

max.spp.l$max_set_p <- ifelse(max.spp.l$Genus_species%in%c(forage.traits.max%>%
                                                             filter(limits%in%"physio")%>%
                                                             select(spp))$spp,
                              "IN",
                              "OUT")
max.spp.l$max_set_r <- ifelse(max.spp.l$Genus_species%in%c(forage.traits.max%>%
                                                             filter(limits%in%"funct")%>%
                                                             select(spp))$spp,
                              "IN",
                              "OUT")

max.spp.l$typ_set_p <- ifelse(max.spp.l$Genus_species%in%c(forage.traits.mean%>%
                                                             filter(limits%in%"physio")%>%
                                                             select(spp))$spp,
                              "IN",
                              "OUT")

max.spp.l$typ_set_r <- ifelse(max.spp.l$Genus_species%in%c(forage.traits.mean%>%
                                                             filter(limits%in%"funct")%>%
                                                             select(spp))$spp,
                              "IN",
                              "OUT")

max.spp.l$typ.set<-paste(max.spp.l$typ_set_p,max.spp.l$typ_set_r,sep="_")
max.spp.l$max.set<-paste(max.spp.l$max_set_p,max.spp.l$max_set_r,sep="_")


spp.draws.max.1.r <- max.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = ~(1|spp)+(1|Genus),
                      allow_new_levels=T,
                      transform=exp)

spp.draws.max.2.r <- max.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = ~(1|Genus),
                      allow_new_levels=T,
                      transform=exp)

spp.draws.max.3.r <- max.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = NA,
                      allow_new_levels=T,
                      transform=exp)

spp.max.1.qi <- median_hdci(spp.draws.max.1.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)
spp.max.2.qi <- median_hdci(spp.draws.max.2.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)
spp.max.3.qi <- median_hdci(spp.draws.max.3.r,.value=.prediction)%>%
  select(max_set_p,max_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)

spp.draws.mean.1.r <- mean.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = ~(1|spp)+(1|Genus),
                      allow_new_levels=T,
                      transform=exp)

spp.draws.mean.2.r <- mean.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = ~(1|Genus),
                      allow_new_levels=T,
                      transform=exp)

spp.draws.mean.3.r <- mean.func.euc.full.phylo.brm%>%
  add_predicted_draws(newdata = max.spp.l,
                      re_formula = NA,
                      allow_new_levels=T,
                      transform=exp)

spp.mean.1.qi <- median_hdci(spp.draws.mean.1.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)

spp.mean.2.qi <- median_hdci(spp.draws.mean.2.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)

spp.mean.3.qi <- median_hdci(spp.draws.mean.3.r,.value=.prediction)%>%
  select(typ_set_p,typ_set_r,spp,social_tree,limits,.value,.lower,.upper,ITD,typ.set,max.set)

spp.max.1.qi$model <- "Max_1"
spp.max.2.qi$model <- "Max_2"
spp.max.3.qi$model <- "Max_3"

spp.mean.1.qi$model <- "Typ_1"
spp.mean.2.qi$model <- "Typ_2"
spp.mean.3.qi$model <- "Typ_3"

spp.mean.values <- rbind.fill(spp.max.1.qi,
                          spp.max.2.qi,
                          spp.max.3.qi,
                          spp.mean.1.qi,
                          spp.mean.2.qi,
                          spp.mean.3.qi)%>%
  select(max_set_r,max_set_p,typ_set_r,typ_set_p,spp,social_tree,limits,.value,.lower,.upper,ITD,model,typ.set,max.set)

spp.mean.values$spp <- gsub("_"," ",spp.mean.values$spp)

spp.mean.values[,8:10] <- spp.mean.values[,8:10]/1000

spp.mean.values$limits <- revalue(spp.mean.values$limits,
                              c("physio" = "Potential",
                                "funct" = "Realised"))

spp.values.typ <- spp.mean.values%>%
  filter(model%in%c("Typ_1","Typ_2","Typ_3"))%>%
  filter(!typ.set%in%c("OUT_OUT"))

spp.values.typ$model.set.r <- paste(spp.values.typ$typ_set_r,spp.values.typ$model)
spp.values.typ$model.set.p <- paste(spp.values.typ$typ_set_p,spp.values.typ$model)

table(spp.values.typ$model.set.r)


model.cols.typ.set <- c("IN Typ_1" = "#757b16",
                    "IN Typ_2" = "#2F638F",
                    "IN Typ_3" = "#490B0A",
                    "OUT Typ_1" = lighten("#757b16",amount = 0.5),
                    "OUT Typ_2" = lighten("#2F638F",amount = 0.5),
                    "OUT Typ_3" = lighten("#490B0A",amount = 0.5))

too.high.typ <- spp.values.typ%>%
  filter(limits%in%"Potential")%>%
  #filter(model%in%"Typ_!")%>%
  #filter(social_tree%in%"Primitively Eusocial")%>%
  arrange(desc(ITD))#%>%top_n(8)

too.high.typ <- too.high.typ[1:9,]

too.high.typ$text <- paste("(",
                           round(too.high.typ$.upper),")",sep="")

too.high.text <- too.high.typ[!duplicated(too.high.typ$spp),]

too.high.text$text <- c()
too.high.text$text[1] <- paste("(",
                               round(mean(too.high.typ$.upper[1:3])),")",sep="")
too.high.text$text[2] <- paste("(",
                               round(mean(too.high.typ$.upper[4:6])),")",sep="")
too.high.text$text[3] <- paste("(",
                               round(mean(too.high.typ$.upper[7:9])),")",sep="")

spp.values.typ2 <- spp.values.typ%>%
  filter(limits%in%"Potential")%>%
  arrange(desc(ITD))

spp.values.typ2[1:9,".upper"] = 16.8
#spp.values.typ2[1:6,".lower"] = 16
#spp.values.typ2[1:6,".value"] = NA

forage.traits.plot2$spp <- gsub("_"," ",forage.traits.plot2$Genus_species)

typ.all.sp.p <- ggplot(spp.values.typ2,
                       aes(y=reorder(spp,ITD),
                            col=model.set.p))+
  coord_cartesian(xlim=c(0,16),clip="off")+
  geom_errorbarh(aes(xmin=.lower,
                     xmax=.upper,
                     col=model.set.p),
                 inherit.aes = T,
                 position = pd,
                 height=0.1)+
  geom_jitter(data=forage.traits.plot2%>%
               filter(distance_type2%in%"Typical")%>%
               filter(limits%in%"physio"),
             aes(x=Distance/1000,
                 y=reorder(spp,ITD),
                 col=distance_type2,fill=distance_type2),alpha=0.75,
             height=0.25,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 fill=model.set.p,
                 pch=limits),
             size=2,col="black",
             position = pd)+
  #geom_text(data=too.high.text,aes(y=reorder(spp,ITD),x=20,
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
  ggtitle("A) Typical foraging range")
typ.all.sp.p

typ.all.sp.r <- ggplot(spp.values.typ%>%
                         filter(limits%in%"Realised"),aes(y=reorder(spp,ITD),
                                                           col=model.set.r))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
        #y=spp,
        col=model.set.r),
    inherit.aes = T,
    position = pd,
    height=0.1)+
  geom_jitter(data=forage.traits.plot2%>%
                filter(distance_type2%in%"Typical")%>%
                filter(limits%in%"funct"),
              aes(x=Distance/1000,
                  y=reorder(spp,ITD),
                  col=distance_type2,
                  fill=distance_type2),alpha=0.75,
              height=0.25,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #y=spp,
                 fill=model.set.r,
                 pch=limits),
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

spp.values.max <- spp.mean.values%>%
  filter(model%in%c("Max_1",
                    "Max_2",
                    "Max_3"))%>%
  filter(!max.set%in%c("OUT_OUT"))

spp.values.max$model.set.p <- paste(spp.values.max$max_set_p,spp.values.max$model)
spp.values.max$model.set.r <- paste(spp.values.max$max_set_r,spp.values.max$model)

model.cols.max.set <- c("IN Max_1" = "#757b16",
                        "IN Max_2" = "#2F638F",
                        "IN Max_3" = "#490B0A",
                        "OUT Max_1" = lighten("#757b16",amount = 0.5),
                        "OUT Max_2" = lighten("#2F638F",amount = 0.5),
                        "OUT Max_3" = lighten("#490B0A",amount = 0.5))

too.high.max <- spp.values.max%>%
  filter(limits%in%"Potential")%>%
  filter(model%in%c("Max_1",
                   "Max_2",
                   "Max_3"))%>%
  #filter(social_tree%in%"Primitively Eusocial")%>%
  arrange(desc(ITD))#%>%top_n(8)

too.high.max <- too.high.max[1:30,]

round(mean(too.high.max$.value[1:3]))
round(mean(too.high.max$.upper[1:3]))
round(mean(too.high.max$.upper[4:6]))
round(mean(too.high.max$.upper[7:9]))
round(mean(too.high.max$.upper[10:12]))
round(mean(too.high.max$.upper[13:15]))
round(mean(too.high.max$.upper[16:18]))
round(mean(too.high.max$.upper[19:21]))

round(mean(too.high.max$.upper[19:21]))

too.high.max$text <- paste(c(round(too.high.max$.value[1]),"","","","","","")," (",
                           round(too.high.max$.upper),")",sep="")


spp.values.max2 <- spp.values.max%>%
  filter(limits%in%"Potential")%>%
  arrange(desc(ITD))

#spp.values.max2[1:7,".upper"]=NA
#spp.values.max2[1,".value"]=NA

max.all.sp.p <- ggplot(spp.values.max2,aes(y=reorder(spp,ITD),
                          col=model.set.p,
                          fill=model.set.p))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
       # y=spp,
        col=model.set.p),
   # inherit.aes = F,
    position = pd,
    height=0.1)+
  #geom_text(data=too.high.max,aes(y=reorder(spp,ITD),x=22,
  #                                 label=text),size=3,
  #            hjust="right",
  #          fontface="bold",
  #           inherit.aes = F)+
  coord_cartesian(xlim=c(0,23))+
  geom_jitter(data=forage.traits.plot2%>%
               filter(distance_type2%in%"Maximum")%>%
               filter(limits%in%"physio"),
             aes(x=Distance/1000,
                 y=reorder(spp,ITD),
                 col=distance_type2,
                 fill=distance_type2),alpha=0.75,
             height=0.25,width = 0,
             pch=21,col="black")+
  geom_point(aes(x=.value,
                 #   y=spp,
                 fill=model.set.p,
                 pch=limits),
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
  ggtitle("B) Maximum foraging range")

max.all.sp.p

max.all.sp.r <- ggplot(spp.values.max%>%
                         filter(limits%in%"Realised"),aes(y=reorder(spp,ITD),
                                                           col=model.set.r,
                                                           fill=model.set.r))+
  geom_errorbarh(
    aes(xmin=.lower,
        xmax=.upper,
      #  y=spp,
        col=model.set.r),
   # inherit.aes = F,
    position = pd,
    height=0.1)+
  #coord_cartesian(xlim=c(0,100))+
  geom_jitter(data=forage.traits.plot2%>%
                filter(distance_type2%in%"Maximum")%>%
                filter(limits%in%"funct"),
              aes(x=Distance/1000,
                  y=reorder(spp,ITD),
                  col=distance_type2,
                  fill=distance_type2),alpha=0.75,
              height=0.25,width = 0,
              pch=21,col="black")+
  geom_point(aes(x=.value,
                 #       y=spp,
                 fill=model.set.r,
                 pch=limits),
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

ggsave(sp.typ.plots,file="all species typical.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)

ggsave(sp.max.plots,file="all species maximum.pdf",
       device = cairo_pdf, 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)

ggsave(sp.typ.plots,file="all species typical.jpeg",
       device = "jpeg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)

ggsave(sp.max.plots,file="all species maximum.jpeg",
       device = "jpeg", 
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 300)

      