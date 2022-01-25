
#set up dot plots
#####
forage.plot <- forage.traits

forage.plot$social2 <- substr(forage.plot$sociality, 1, 5)

substr(forage.plot$sociality, 1, 5)
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

dodge.height=0.9

forage.stats$social2 <- substr(forage.stats$sociality, 1, 5)

soc.pal2 <-  c("Highl" = lighten("#4F651D",amount = 0.5),
               "Primi" = lighten("#490B0A",amount = 0.5),
               "Solit" = lighten("#798C8B",amount = 0.5),
               "Highly Eusocial"="#4F651D",
               "Primitively Eusocial"="#490B0A",          
               "Solitary"="#798C8B")

soc.pal <-  c("Highly Eusocial"="#4F651D",
"Primitively Eusocial"="#490B0A",          
"Solitary"="#798C8B")

#set up regression plots
#####
func.euc.preds.1 <- rbind(cbind(fitted(all_social_mod,re_formula = NA),
                                forage.traits,
                                gl=ifelse(forage.traits$range.type%in%"Max"==T,
                                          foragedist(forage.traits$itd,type="GrMhd"),
                                          foragedist(forage.traits$itd,type="GrThd"))))


func.euc.preds.1$Estimate <- exp(func.euc.preds.1$Estimate)/1000
func.euc.preds.1$Q2.5 <- exp(func.euc.preds.1$Q2.5)/1000
func.euc.preds.1$Q97.5 <- exp(func.euc.preds.1$Q97.5)/1000
func.euc.preds.1$social2 <- substr(func.euc.preds.1$sociality, 1, 5)

func.euc.preds.1$sociality <- factor(func.euc.preds.1$sociality,
                                     c("Highly Eusocial",
                                       "Solitary",
                                       "Primitively Eusocial"
                                     ))

func.euc.preds.1$range.4 <- revalue(func.euc.preds.1$range.4,
                                    c("Typ-Realised" = "Realized typical",
                                      "Max-Realised" = "Realized maximum",
                                      "Typ-Potential" = "Potential typical",
                                      "Max-Potential" = "Potential maximum"))

range_labels <- func.euc.preds.1 %>%
  mutate(itd = 1, y = 45) %>%
  select(range.4,itd,y) %>% 
  distinct(range.4,itd,y)

range_labels$range.4 <- factor(range_labels$range.4,
                               levels=c("Potential typical","Potential maximum",
                                        "Realized typical","Realized maximum"))

func.euc.preds.1$range.4 <- factor(func.euc.preds.1$range.4,
                                   levels=c("Potential typical","Potential maximum",
                                            "Realized typical","Realized maximum"))

######
######
range.tl <- c("Potential typical")
range.tr <- c("Potential maximum")
range.bl <- c("Realized typical")
range.br <- c("Realized maximum")

###theme objects
#####
line.theme <- theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        text = element_text(family = 'Times'),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        #text=element_text(),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=12,face="bold"),
        plot.margin = unit(c(0.25,0.25,0.25,0.25), "lines"),
        #axis.title.x = element_text(size=12),
        #strip.text = element_text(size=13,face="bold"),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))
line.scale <-   scale_y_log10(breaks=c(0.1,1,10),
                              limits=c(min(range(func.euc.preds.1$Q2.5)),
                                       max(range(func.euc.preds.1$Q97.5))))

#min(range(func.euc.preds.1$Q2.5))

point.theme <- theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 3.5,
        strip.background = element_blank(),
        text = element_text(family = 'Times'),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(),
        legend.position = "right",
        legend.direction="vertical",
        panel.spacing = unit(0.5,"lines"),
        plot.margin = unit(c(0.25,0.25,0.25,0.25), "lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))

#######
#Potential typical
range.tl
######
range.tl.line.plot <- ggplot(func.euc.preds.1 %>% 
                         filter(range.4%in%range.tl),
                       aes(y=Estimate,x=itd,
                           col=sociality,
                           fill=sociality)) +  
  xlab("ITD (mm)") + 
  ylab("Foraging range (km)") + 
  line.scale+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=itd,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=0.5)+
  geom_line(aes(),size=1.5,show.legend = F)+
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=2.5,shape=21,
             col="black",
             show.legend = F,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=itd,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = range_labels%>% 
              filter(range.4%in%range.tl),
            inherit.aes = F,
            hjust="left",
            family="Times") +
  line.theme+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal,
                      guide = guide_legend(reverse = F)) +
  scale_x_continuous(limits=c(1,7.7),breaks=c(1,3,5,7))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
 # facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")

range.tl.point.plot <- ggplot(forage.stats %>% 
                           filter(range.4%in%range.tl),
                         aes(y=median,
                             x=sociality,
                             col=sociality,
                             fill=sociality))+
    geom_point(data=forage.plot%>% 
                 filter(range.4%in%range.tl),
               aes(y=distance/1000,fill=social2),
               show.legend = F,
              # alpha=0.25,
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
    geom_point(show.legend = F,#inherit.aes = F,
               alpha=1,col="black",
               position = position_dodge(width = dodge.height),
               size=5,
               pch=21)+
    theme_bw()+
    geom_text(data=forage.stats%>% 
                filter(range.4%in%range.tl),
              aes(label=n,y=0.035),
              show.legend = F,
              col="black",
              fontface="bold",
              family="Times",
              size=4,##
              position = position_dodge(width=dodge.height))+
  point.theme+
    xlab("Degree of sociality")+
    ylab("Foraging range (km)")+
  line.scale+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal2,
                      guide = guide_legend(reverse = F)) +
    labs(name="Range type",
         fill="Range type",
         col="Range type")

######
#Potential maximum
range.tr
######
range.tr.line.plot <- ggplot(func.euc.preds.1 %>% 
                               filter(range.4%in%range.tr),
                             aes(y=Estimate,x=itd,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("ITD (mm)") + 
  ylab("Foraging range (km)") + 
  line.scale+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=itd,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=0.5)+
  geom_line(aes(),size=1.5,show.legend = F)+
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=2.5,shape=21,
             col="black",
             show.legend = F,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=itd,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = range_labels%>% 
              filter(range.4%in%range.tr),
            inherit.aes = F,
            hjust="left",
            family="Times") +
  line.theme+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal,
                      guide = guide_legend(reverse = F)) +
  scale_x_continuous(limits=c(1,7.7),breaks=c(1,3,5,7))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
# facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")

range.tr.point.plot <- ggplot(forage.stats %>% 
                                filter(range.4%in%range.tr),
                              aes(y=median,
                                  x=sociality,
                                  col=sociality,
                                  fill=sociality))+
  geom_point(data=forage.plot%>% 
               filter(range.4%in%range.tr),
             aes(y=distance/1000,fill=social2),
             show.legend = F,
            # alpha=0.25,
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
  geom_point(show.legend = F,#inherit.aes = F,
             alpha=1,col="black",
             position = position_dodge(width = dodge.height),
             size=5,
             pch=21)+
  theme_bw()+
  geom_text(data=forage.stats%>% 
              filter(range.4%in%range.tr),
            aes(label=n,y=0.035),
            show.legend = F,
            col="black",
            fontface="bold",
            family="Times",
            size=4,##
            position = position_dodge(width=dodge.height))+
  point.theme+
  xlab("Degree of sociality")+
  ylab("Foraging range (km)")+
  line.scale+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal2,
                      guide = guide_legend(reverse = F)) +
  labs(name="Range type",
       fill="Range type",
       col="Range type")



######
#Realized typical
range.bl
#####
range.bl.line.plot <- ggplot(func.euc.preds.1 %>% 
                               filter(range.4%in%range.bl),
                             aes(y=Estimate,x=itd,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("ITD (mm)") + 
  ylab("Foraging range (km)") + 
  line.scale+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=itd,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=0.5)+
  geom_line(aes(),size=1.5,show.legend = F)+
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=2.5,shape=21,
             col="black",
             show.legend = F,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=itd,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = range_labels%>% 
              filter(range.4%in%range.bl),
            inherit.aes = F,
            hjust="left",
            family="Times") +
  line.theme+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal,
                      guide = guide_legend(reverse = F)) +
  scale_x_continuous(limits=c(1,7.7),breaks=c(1,3,5,7))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
# facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")

range.bl.point.plot <- ggplot(forage.stats %>% 
                                filter(range.4%in%range.bl),
                              aes(y=median,
                                  x=sociality,
                                  col=sociality,
                                  fill=sociality))+
  geom_point(data=forage.plot%>% 
               filter(range.4%in%range.bl),
             aes(y=distance/1000,fill=social2),
             show.legend = F,
            # alpha=0.25,
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
  geom_point(show.legend = F,#inherit.aes = F,
             alpha=1,col="black",
             position = position_dodge(width = dodge.height),
             size=5,
             pch=21)+
  theme_bw()+
  geom_text(data=forage.stats%>% 
              filter(range.4%in%range.bl),
            aes(label=n,y=0.035),
            show.legend = F,
            col="black",
            size=4,##
            fontface="bold",
            family="Times",
            position = position_dodge(width=dodge.height))+
  point.theme+
  xlab("Degree of sociality")+
  ylab("Foraging range (km)")+
  line.scale+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal2,
                      guide = guide_legend(reverse = F)) +
  labs(name="Range type",
       fill="Range type",
       col="Range type")






######
#realised maximum
range.br
######

range.br.line.plot <- ggplot(func.euc.preds.1 %>% 
                               filter(range.4%in%range.br),
                             aes(y=Estimate,x=itd,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("ITD (mm)") + 
  ylab("Foraging range (km)") + 
  line.scale+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=itd,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=0.5)+
  geom_line(aes(),size=1.5,show.legend = F)+
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=2.5,shape=21,
             col="black",
             show.legend = F,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=itd,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = range_labels%>% 
              filter(range.4%in%range.br),
            inherit.aes = F,
            hjust="left",
            family="Times") +
  line.theme+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  scale_fill_manual(values=soc.pal2,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal,
                      guide = guide_legend(reverse = F)) +
  scale_x_continuous(limits=c(1,7.7),breaks=c(1,3,5,7))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
# facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")

range.br.point.plot <- ggplot(forage.stats %>% 
                                filter(range.4%in%range.br),
                              aes(y=median,
                                  x=sociality,
                                  col=sociality,
                                  fill=sociality))+
  geom_point(data=forage.plot%>% 
               filter(range.4%in%range.br),
             aes(y=distance/1000,
                 x=sociality,fill=social2),
             show.legend = F,
            # inherit.aes = F,
             col="white",
            # fill=c("Highl" = "9CB378","Primi" = "#B17B7A","Solit"="#B3C6C5"),
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
             alpha=1,
             col="black",
             position = position_dodge(width = dodge.height),
             size=5,
             pch=21)+
  theme_bw()+
  geom_text(data=forage.stats%>% 
              filter(range.4%in%range.br),
            aes(label=n,y=0.035),
            fontface="bold",
            show.legend = F,
            col="black",
            family="Times",
            size=4,
            position = position_dodge(width=dodge.height))+
  point.theme+
  theme(legend.position = c(0.5, 0.1),
        legend.direction = "horizontal")+
  #  legend.title = element_text(size=10,face="bold"),
  #  legend.text = element_text(size=8))+
  xlab("Degree of sociality")+
  ylab("Foraging range (km)")+
  line.scale+
  scale_colour_manual(values=soc.pal2,guide="none")+
                   # guide = guide_legend(override.aes = list(labels = c("Primitively Eusocial",
                   #                                                     "Solitary","Highly eusocial")) ) ) + 
  scale_fill_manual(values=soc.pal2,
                      breaks=c("Primitively Eusocial",
                               "Solitary",
                               "Highly Eusocial"))+
              #        guide = guide_legend(override.aes = list(labels = c("Primitively Eusocial",
              #                                                            "Solitary","Highly eusocial")) ) ) + 
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
range.br.point.plot
#arrange plots
######
library(patchwork)

super.plot <- (range.tl.line.plot|range.tl.point.plot|
                 range.tr.line.plot|range.tr.point.plot)/
              (range.bl.line.plot|range.bl.point.plot|
               range.br.line.plot|range.br.point.plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
super.plot


ggsave(super.plot,file="plots/Figure 1 final.pdf",
       device="pdf",
       width=9,
       height=7,
       units = "in",
       dpi=600)

ggsave(super.plot,file="plots/Figure 1 final.jpg",
       device="jpg",
       width=9,
       height=7,
       units = "in",
       dpi=600)
