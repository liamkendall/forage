###Main plot

func.euc.preds.1 <- rbind(cbind(fitted(all_social_mod,re_formula = NA),
                                forage.traits,
                                gl=ifelse(forage.traits$range.type%in%"Max"==T,
                                          foragedist(forage.traits$itd,type="GrMhd"),
                                          foragedist(forage.traits$itd,type="GrThd"))))


func.euc.preds.1$Estimate <- exp(func.euc.preds.1$Estimate)/1000
func.euc.preds.1$Q2.5 <- exp(func.euc.preds.1$Q2.5)/1000
func.euc.preds.1$Q97.5 <- exp(func.euc.preds.1$Q97.5)/1000

#colour palette
soc.pal=c("Highly Eusocial"="#A7473A",
          "Primitively Eusocial"="#4B5F6C",          
          "Solitary"="#B09B37")

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


new.all.plot <- ggplot(func.euc.preds.1,
         aes(y=exp(Estimate)/1000,x=itd,
             col=sociality,
             fill=sociality)) +  
  xlab("ITD (mm)") + 
  ylab("Foraging range (km)") + 
  scale_y_continuous(trans="log10")+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=itd,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=0.5)+
  geom_line(aes(),size=1.5,show.legend = TRUE)+
  geom_ribbon(aes(ymin=exp(Q2.5)/1000,
                  ymax=exp(Q97.5)/1000),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_point(aes(y=distance/1000),
             size=2.5,shape=21,
             col="black",
             show.legend = TRUE,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=itd,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = range_labels,
            inherit.aes = F,
            hjust="left",
            family="Times"
          #  vjust = 1
            ) +
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
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
        #axis.title.x = element_text(size=12),
        #strip.text = element_text(size=13,face="bold"),
        strip.text = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=soc.pal,
                    guide = guide_legend(reverse = F)) + 
  scale_colour_manual(values=soc.pal,
                      guide = guide_legend(reverse = F)) +
  scale_x_continuous(limits=c(1,7.7),breaks=c(1,3,5,7))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")+
  facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")

ggsave(new.all.plot,file="plots/Figure 2 revised 22.pdf",
       device="pdf",
       width=7,
       height=7,
       units = "in",
       dpi=600)

ggsave(new.all.plot,file="plots/Figure 2 revised 22.jpg",
       device="jpg",
       width=7,
       height=7,
       units = "in",
       dpi=600)
