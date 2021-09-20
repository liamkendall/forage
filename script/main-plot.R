###Main plot
library(ggplot2)
library(Manu)
library(pollimetry)

func.euc.preds.1 <- rbind(cbind(fitted(max.func.euc.full.phylo.brm,re_formula = NA),
                                forage.traits.max,
                                gl=foragedist(forage.traits.max$ITD,type="GrMhd")),
                        cbind(fitted(mean.func.euc.full.phylo.brm,re_formula = NA),
                              forage.traits.mean,
                              gl=foragedist(forage.traits.mean$ITD,type="GrThd")))

func.euc.preds.1$limits <- revalue(func.euc.preds.1$limits,
                                   c("physio" = "Potential",
                                     "funct" = "Realised"))

func.euc.preds.2$limits <- revalue(func.euc.preds.2$limits,
                                   c("physio" = "Potential",
                                     "funct" = "Realised"))

soc.pal=c("Highly Eusocial"="#A7473A",
          "Primitively Eusocial"="#4B5F6C",          
          "Solitary"="#B09B37")

func.euc.preds.1$social_tree <- factor(func.euc.preds.1$social_tree,
                                       levels=c("Highly Eusocial",
                                                "Solitary",
                                                "Primitively Eusocial"))

new.max.plot <- 
  ggplot(func.euc.preds.1%>%filter(distance_type2%in%"Max"),
         aes(y=exp(Estimate)/1000,x=ITD,
             col=social_tree,
             fill=social_tree)) +  
  xlab("ITD (mm)") + 
  ylab("Maximum range (km)") + 
  scale_y_continuous(trans="log10")+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=ITD,y=gl),inherit.aes=F,linetype="dashed",alpha=0.5)+
  geom_point(aes(y=Distance/1000),,
             size=1.5,shape=21,
             col="black",
             show.legend = TRUE,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = TRUE)+
  geom_ribbon(aes(ymin=exp(Q2.5)/1000,
                  ymax=exp(Q97.5)/1000),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
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
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=12),
        axis.title.x = element_blank(),
        strip.text = element_text(size=13,face="bold"),
        legend.position = "none",
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=soc.pal) + 
  scale_colour_manual(values=soc.pal) +
  # scale_linetype_manual(values=c(1,22))+
  xlim(1,8)+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")+
  facet_wrap(~limits,ncol=3,nrow=1)#,scales="free_x")
new.max.plot

new.typ.plot <- 
  ggplot(func.euc.preds.1%>%filter(distance_type2%in%"Typical"),
         aes(y=exp(Estimate)/1000,x=ITD,
             col=social_tree,
             fill=social_tree)) +  
  xlab("ITD (mm)") + 
  ylab("Typical range (km)") + 
  scale_y_continuous(trans="log10")+
  geom_line(aes(x=ITD,y=gl),inherit.aes=F,linetype="dashed",alpha=0.5)+
  xlim(1,8)+
  scale_shape_manual(values=c(21,22))+
  geom_point(aes(y=Distance/1000),#width=0.1,height=0.1,
             size=1.5,shape=21,
             col="black",
             show.legend = TRUE,alpha=1)+
  geom_line(aes(),size=1.5,show.legend = TRUE)+
  #geom_line(data=func.euc.preds.2%>%filter(distance_type2%in%"Typical"),aes(),size=1.5,show.legend = TRUE)+
  geom_ribbon(aes(ymin=exp(Q2.5)/1000,
                  ymax=exp(Q97.5)/1000),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  #geom_text(data=new.typ.labels,aes(label = label,y=Estimate,x=ITD),
  #          inherit.aes = FALSE,hjust = 0,fontface="bold")+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        #  panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text=element_text(),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        strip.text = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  scale_fill_manual(values=soc.pal) + 
  scale_colour_manual(values=soc.pal) +
  #scale_linetype_manual(values=c(1,22))+
  guides(pch = "none")+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")+
  facet_wrap(~limits,ncol=2,nrow=1)


new.typ.plot

new.max.p <- ggplotGrob(new.max.plot)
new.typ.p <- ggplotGrob(new.typ.plot)

ggsave(rbind(new.max.p,new.typ.p),file="new faceted main.pdf",
       device="pdf",
       width=6,
       height=6,
       units = "in",
       dpi = 300)

ggsave(rbind(new.max.p,new.typ.p),file="new faceted main.jpg",
       device="jpg",
       width=6,
       height=6,
       units = "in",
       dpi = 300)

ggplot(forage.traits.max,aes(x=limits,y=Distance,col=social_tree))+
  geom_boxplot()+
  scale_y_log10()

ggplot(forage.traits.mean,aes(x=limits,y=Distance,col=social_tree))+
  geom_boxplot()+
  scale_y_log10()

View(func.euc.preds.1%>%
  group_by(distance_type2,social_tree,limits)%>%
  summarise(range=range(ITD)))
