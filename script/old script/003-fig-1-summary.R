#Figure 1 - Summary of all measurements we have

max.cols=c("Maximum"="#A7473A",
           "Typical"="#4B5F6C", #dark blue
           "Observed"="#A8B9CB")

forage.traits.plot <- rbind.fill(forage.traits,forage.traits.obs)
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

fam.labs <- data.frame(genus2=c("Andrena","Anthophora","Colletes","Lasioglossum","Chelostoma","Dasypoda"),
                       family=c("Andrenidae","Apidae","Colletidae","Halictidae","Megachilidae","Melittidae"))

forage.traits.plot2$range.type <- factor(forage.traits.plot2$range.type,
                                         levels=c("Maximum","Typical","Observed"))

sum.plot <- ggplot(forage.traits.plot2,
                           aes(y=distance/1000,
                               x=forcats::fct_rev(genus2),
                               col=range.type,
                               label=family,
                               fill=range.type))+
  geom_boxplot(col="darkgrey",fill="lightgrey", width=0.2,
               position = position_nudge(x = 0.4, y = 0))+
  geom_jitter(pch=21,col="black",width=0.1,size=2)+
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
      aspect.ratio = 2.5,
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
  labs(name="Range type",
       fill="Range type",
       col="Range type")+
geom_text(data=fam.labs,
          inherit.aes = F,nudge_x=0.2,
          aes(x = genus2,label=family), y = log10(50),
          hjust = 0, col="black", size = 3.5,fontface="bold")+
  coord_flip(clip = 'off')

sum.grob <- ggplotGrob(sum.plot+
  scale_y_log10())

ggsave(sum.grob,file="plots/figure 1.pdf",
       width=6,
       height=9,
       units="in")

ggsave(sum.grob,file="plots/Figure 1.jpg",
       width=6,
       height=9,
       units="in")
