forage.medians<-forage.traits%>%
  group_by(measure.type,sociality,range.type)%>%
  summarise(median_range = mean(log.dist),
            sd=sd(log.dist),
            n = n())%>%
  mutate(lower.ci = median_range-(qnorm(0.975)*sd/sqrt(n)),
         upper.ci = median_range+(qnorm(0.975)*sd/sqrt(n)))

forage.medians[,c("median_range","lower.ci","upper.ci")] <-exp(forage.medians[,c("median_range",
                    "lower.ci",
                    "upper.ci")])

forage.medians$range.type<- revalue(forage.medians$range.type,
                                    c("Max" = "Maximum"))

forage.medians$sociality<-factor(forage.medians$sociality,
                                 levels=c("Solitary",
                                          "Primitively Eusocial",
                                          "Highly Eusocial"))

group.sum.plot <- ggplot(forage.medians,#%>%filter(measure.type%in%"Realised"),
       aes(x=median_range/1000,y=sociality,
                         col=range.type))+
  geom_point(show.legend = T,
             position = pd,size=3)+
  geom_errorbarh(aes(xmin=lower.ci/1000,
                     xmax=upper.ci/1000),
                 width=0.2,
                 position = pd,
                 show.legend = T)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        strip.text = element_text(size=13,face="bold"),
        legend.position = "bottom",
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4)) +
  ylab("Degree of sociality")+
  xlab("Foraging range (km)")+
  scale_color_manual(values=c("#4F651D","#798C8B"))+
labs(name="Foraging range type",
     fill="Foraging range type",
     col="Foraging range type")+
  facet_wrap(~measure.type,scales="free_x",
             ncol=2)
group.sum.plot


ggsave(group.sum.plot,file="plots/Figure S1.pdf",
       device="pdf",
       width=6,
       height=4,
       units = "in",
       dpi = 300)

ggsave(group.sum.plot,file="plots/Figure S1.jpg",
       device="jpg",
       width=6,
       height=4,
       units = "in",
       dpi = 300)
