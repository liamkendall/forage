###plots from brms objects
library(ggplot2)
library(Manu)

#panel plot 3 x 2
#Maximum - Typical
#overall
overall.pred <- rbind(cbind(fitted(max.IT.brm,re_formula = NA),forage.traits.max),
                      cbind(fitted(mean.IT.brm,re_formula = NA),forage.traits.mean))
#eusocial
eusocial.preds <- rbind(cbind(fitted(max.euc.phylo.reduced.brm,re_formula = NA),forage.traits.max),
                        cbind(fitted(mean.euc.phylo.reduced.brm,re_formula = NA),forage.traits.mean))

#physio vs functional
func.preds <- rbind(cbind(fitted(max.func.phylo.brm,re_formula = NA),forage.traits.max),
                        cbind(fitted(mean.func.phylo.brm,re_formula = NA),forage.traits.mean))
func.euc.preds <- rbind(cbind(fitted(max.func.euc.red.phylo.brm,re_formula = NA),forage.traits.max),
                    cbind(fitted(mean.func.euc.red.phylo.brm,re_formula = NA),forage.traits.mean))

overall.plot <- ggplot(overall.pred,aes(y=(Estimate),x=ITD,fill="black")) +  
  xlab(NULL) + 
  ylab("ln Distance (m)") + 
  geom_ribbon(aes(ymin=(Q2.5),
                  ymax=(Q97.5)),
              alpha = 0.5,show.legend = FALSE,colour = NA)+
  geom_point(aes(y=log.dist),size=2.5,shape=21,
             show.legend = FALSE,alpha=0.6)+
  geom_line(size=1,show.legend = FALSE)+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.spacing = unit(0.5,"lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=get_pal("Kereru")) + 
  scale_colour_manual(values=get_pal("Kereru")) + 
  guides(linetype = FALSE)+
facet_wrap(~distance_type2)
overall.plot

euc.plot <- ggplot(eusocial.preds,aes(y=(Estimate),x=ITD)) +  
  xlab(NULL) + 
  ylab("ln Distance (m)") + 
  geom_ribbon(aes(ymin=(Q2.5),
                  ymax=(Q97.5),col=social_tree,fill=social_tree),
              alpha = 0.5,show.legend = FALSE,colour = NA)+
  geom_point(aes(y=log.dist,fill=social_tree),col="black",size=2.5,shape=21,
             show.legend = TRUE,alpha=0.6)+
  geom_line(aes(col=social_tree,fill=social_tree),size=1.5,show.legend = TRUE)+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text = element_blank(),
        panel.spacing = unit(0.5,"lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=get_pal("Kereru")[2:4]) + 
  scale_colour_manual(values=get_pal("Kereru")[2:4]) + 
  guides(linetype = FALSE)+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")+
  facet_wrap(~distance_type2)
euc.plot
func.preds$limits <- revalue(func.preds$limits,c("funct" = "Functional",
                                                 "physio" = "Physiological"))

func.plot <- ggplot(func.preds,aes(y=Estimate,x=ITD)) +  
  xlab("Body size (ITD, mm)") + 
  ylab("ln Distance (m)") + 
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5,col=limits,fill=limits),
              alpha = 0.5,show.legend = FALSE,colour = NA)+
  geom_point(aes(y=log.dist,fill=limits),col="black",size=2.5,shape=21,
             show.legend = TRUE,alpha=0.6)+
  geom_line(aes(col=limits,fill=limits),size=1,show.legend = TRUE)+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text = element_blank(),
        panel.spacing = unit(0.5,"lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=get_pal("Kereru")[5:6]) + 
  scale_colour_manual(values=get_pal("Kereru")[5:6]) + 
  guides(linetype = FALSE)+
  labs(name="Foraging constraint",
       fill="Foraging constraint",
       col="Foraging constraint")+
  facet_wrap(~distance_type2)
func.plot

labels <- data.frame(limits=c("physio","physio","funct","funct"),
           distance_type2=c("Max","Typical","Max","Typical"),
           label=c("Max - Physiological","Typical - Physiological","Max - Functional","Max - Functional"),
           ITD=c(1,1,1,1),
           Estimate=c(11,11,11,11))

func.euc.preds$grp <- paste(func.euc.preds$distance_type2,func.euc.preds$social_tree)

func2.plot <- ggplot(func.euc.preds,aes(y=Estimate,x=ITD,col=social_tree,fill=social_tree)) +  
  xlab("Body size (ITD, mm)") + 
  ylab("ln Distance (m)") + 
  geom_ribbon(aes(ymin=Q2.5,
                  ymax=Q97.5),
              alpha = 0.5,show.legend = FALSE,colour = NA)+
  geom_point(aes(y=log.dist),col="black",size=2.5,shape=21,
             show.legend = TRUE,alpha=0.6)+
  geom_text(data=labels,aes(label = label,y=Estimate,x=ITD),inherit.aes = FALSE,hjust = 0,fontface="bold")+
  geom_line(aes(),size=1,show.legend = TRUE)+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        strip.text = element_blank(),
        panel.spacing = unit(0.5,"lines"),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=get_pal("Kereru")[1:6]) + 
  scale_colour_manual(values=get_pal("Kereru")[1:6]) + 
  guides(linetype = FALSE)+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")+
  facet_wrap(~limits+distance_type2)
func2.plot

three_plot <- overall.plot/euc.plot/func.plot
three_plot
ggsave(three_plot,file="three_plot.pdf",width=7,height=10,device="pdf")
