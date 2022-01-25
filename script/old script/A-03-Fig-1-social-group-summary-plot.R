
forage.plot <- forage.traits

forage.traits %>% 
  filter(measure.type%in%"Potential" &
           sociality%in%"Solitary")

 #sample sizes
forage.stats<-forage.traits%>%
group_by(measure.type,sociality,range.4)%>%
  dplyr::summarise(median = median(distance),
            #sd=sd(log.dist),
            lower.q=quantile(distance,0.25,names=F),
            upper.q=quantile(distance,0.75,names=F),
            n = n())

forage.stats[,4:6] <-round(forage.stats[,4:6])
forage.stats[,4:6] <-forage.stats[,4:6]/1000

forage.stats$n2 <- paste0("(",forage.stats$n,")")

forage.stats$range.4 <- revalue(forage.stats$range.4,
                               c("Typ-Realised" = "Realized typical",
                                 "Max-Realised" = "Realized maximum",
                                 "Typ-Potential" = "Potential typical",
                                 "Max-Potential" = "Potential maximum"))

forage.stats$range.4 <- factor(forage.stats$range.4,levels=c("Potential maximum",
                                                           "Potential typical",
                                                           "Realized maximum",
                                                           "Realized typical"))

forage.plot$sociality<-factor(forage.plot$sociality,
                                 levels=c("Solitary",
                                          "Primitively Eusocial",
                                          "Highly Eusocial"))


forage.plot$range.4 <- revalue(forage.plot$range.4,
                                   c("Typ-Realised" = "Realized typical",
                                     "Max-Realised" = "Realized maximum",
                                     "Typ-Potential" = "Potential typical",
                                     "Max-Potential" = "Potential maximum"))

forage.plot$range.4 <- factor(forage.plot$range.4,levels=c("Potential typical",
                                                           "Potential maximum",
                                                           "Realized typical",
                                                           "Realized maximum"))


forage.stats$range.4 <- factor(forage.stats$range.4,levels=c("Potential typical",
                                                           "Potential maximum",
                                                           "Realized typical",
                                                           "Realized maximum"))

four.pal <- c("Potential typical" = "#4F651D",
              "Potential maximum" = "#798C8B", 
              "Realized typical"= "#2F638F", 
              "Realized maximum" = "#490B0A")

dodge.height=0.9

group.sum.plot <- ggplot(forage.stats,aes(y=median,
                        x=sociality,
                        col=forcats::fct_rev(range.4),
                        fill=forcats::fct_rev(range.4)))+
  geom_point(data=forage.plot,
             aes(y=distance/1000),
             show.legend = F,
             alpha=0.25,
             col="white",
             position = position_jitterdodge(dodge.width = dodge.height,
                                             jitter.width = 0.3),
             size=3,
             pch=21)+
  geom_errorbar(aes(ymin=lower.q,
                    ymax=upper.q),show.legend=F,
                size=1,#col="black",
                position = position_dodge(width = dodge.height),
                width=0.3)+
  geom_point(show.legend = T,#inherit.aes = F,
             alpha=1,col="black",
             position = position_dodge(width = dodge.height),
             size=4,
             pch=21)+
  theme_bw()+
  geom_text(data=forage.stats,
            aes(label=n2,y=40),
            show.legend = F,
            col="black",
            family="Times",
  position = position_dodge(width=dodge.height))+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1.5,
        strip.background = element_blank(),
        text = element_text(family = 'Times'),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=12,face="bold"),
        axis.title.x = element_text(size=12,face="bold"),
        strip.text = element_text(),
        legend.position = "right",
        legend.direction="vertical",
        panel.spacing = unit(0.5,"lines"),
        plot.margin = unit(c(1,3,1,0.25), "lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  xlab("Degree of sociality")+
  ylab("Foraging range (km)")+
  scale_y_continuous(limits = c(0.02, 50), trans = "log10")+
  scale_color_manual(values=four.pal,
                     limits = c("Potential typical",
                                "Potential maximum",
                                "Realized typical",
                                "Realized maximum"))+
  scale_fill_manual(values=four.pal,
                    limits =c("Potential typical",
                              "Potential maximum",
                              "Realized typical",
                              "Realized maximum"))+
labs(name="Range type",
     fill="Range type",
     col="Range type")+
  coord_flip()
group.sum.plot

ggsave(group.sum.plot,file="plots/Figure 1 revised 22.pdf",
       device="pdf",
       width=7,
       height=6,
       units = "in",
       dpi=600)

ggsave(group.sum.plot,file="plots/Figure 1 revised 22.jpg",
       device="jpg",
       width=7,
       height=6,
       units = "in",
       dpi=600)
