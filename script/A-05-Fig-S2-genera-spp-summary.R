#Figure 1 - Summary of all measurements we have

forage.traits.plot <- forage.traits #rbind.fill(,forage.traits.obs)
forage.traits.plot$range.type <- revalue(forage.traits.plot$range.type,
                                             c("Max" = "Maximum"))

forage.traits.plot$genus2 <- ifelse(forage.traits.plot$species%in%"Apis_mellifera",
                                    "Apis mellifera",forage.traits.plot$genus)
forage.traits.plot$genus2 <- ifelse(forage.traits.plot$species%in%"Bombus_terrestris",
                                   "Bombus terrestris",forage.traits.plot$genus2)

forage.traits.plot$group2 <- ifelse((forage.traits.plot$sociality%in%"Highly Eusocial"&
                                      !forage.traits.plot$genus%in%c("Apis","Bombus")),
                                    "Apidae",forage.traits.plot$family)

forage.traits.plot2 <- forage.traits.plot %>%
  arrange(group2, genus2) %>%         
  mutate(genus2 = factor(genus2, unique(genus2)))

fam.labs <- data.frame(genus2=c("Andrena","Anthophora","Colletes",
                                "Lasioglossum","Chelostoma","Dasypoda"),
                       family=c("Andrenidae","Apidae","Colletidae",
                                "Halictidae","Megachilidae",
                                "Melittidae"))

forage.traits.plot2$range.type <- factor(forage.traits.plot2$range.type,
                                         levels=c("Maximum","Typical","Observed"))

forage.traits.plot2$range.4 <- ifelse((forage.traits.plot2$range.type%in%"Maximum"
                                      &forage.traits.plot2$measure.type%in%"Realised"),
                                      "Realized maximum",
                                      ifelse((forage.traits.plot2$range.type%in%"Maximum"
                                              &forage.traits.plot2$measure.type%in%"Potential"),
                                             "Potential maximum",
                                      ifelse((forage.traits.plot2$range.type%in%"Typical"
                                                     &forage.traits.plot2$measure.type%in%"Potential"),
                                             "Potential typical","Realized typical")))

four.pal <- c("Potential typical" = "#CABEE9",
              "Potential maximum" = "#FAE093", 
              "Realized typical"= "#D04E59", 
              "Realized maximum" = "#2F3D70")

forage.traits.plot2$range.4 <- factor(forage.traits.plot2$range.4,
                                      levels=c("Potential typical",
                                               "Potential maximum",
                                               "Realized typical",
                                               "Realized maximum"))

sum.plot <- ggplot(forage.traits.plot2,
                           aes(y=distance/1000,
                               x=fct_rev(genus2),
                               col=range.4,
                               label=family,
                               fill=range.4
                         ))+
  #geom_boxplot(col="darkgrey",fill="lightgrey", width=0.2,
  #             position = position_nudge(x = 0.4, y = 0))+
  geom_jitter(pch=21,col="white",width=0.2,size=3,alpha=1 #,fill="#CABEE9"
              )+
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
    xmax = "Xylocopa", y.position = log10(30),
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
 # geom_bracket(
#    xmin = "Lasioglossum",
#    xmax = "Lasioglossum", y.position = log10(30),
#    label = "",col="black",
#    coord.flip = F,
#    inherit.aes = F,
#    tip.length = c(0.02, 0.02))+
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
      text = element_text(family = 'Times'),
      axis.ticks.length = unit(0,"mm"),
      legend.title.align=0.5,
      axis.text.y = element_text(size=10,face="italic"),
      axis.text.x = element_text(size=10),
      axis.title.y = element_text(size=12,face="bold"),
      axis.title.x = element_text(size=12,face="bold"),
      strip.text = element_text(),
      legend.position = "right",
      legend.direction="vertical",
      panel.spacing = unit(0.5,"lines"),
      plot.margin = unit(c(1,3,1,0.25), "lines"),
      #legend.box = "horizontal",
      legend.background = element_blank(),
      panel.border = element_rect(color = "black",
                                  fill = NA, size = 0.4))+
   scale_fill_manual(values=four.pal) + 
   scale_colour_manual(values=four.pal) + 


  ylab("Foraging range (km)")+
  xlab("Taxa")+
  guides(pch = "none")+
  labs(name="Range type",
       fill="Range type",
       col="Range type")+
geom_text(data=fam.labs,
          inherit.aes = F,nudge_x=0.2,
          aes(x = genus2,label=family), y = log10(50),
          family="Times",
          hjust = 0, col="black", size = 3.5,fontface="bold")+
  coord_flip(clip = 'off')

sum.grob <- ggplotGrob(sum.plot+
  scale_y_log10())

ggsave(sum.grob,file="plots/Figure S2.pdf",
       width=7,
       height=9,
       units="in",
       dpi=600)

ggsave(sum.grob,file="plots/Figure S2.jpg",
       width=7,
       height=9,
       units="in",
       dpi=600)
