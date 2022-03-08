
#set up dot plots
#####
forage.plot <- forage.traits.bm

forage.plot$social2 <- substr(forage.plot$sociality, 1, 5)

substr(forage.plot$sociality, 1, 5)
#sample sizes
forage.stats<-forage.traits.bm%>%
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
#set up prediction grid
pred.grid <-  as.data.frame(matrix(ncol = 3, nrow = 1200))
colnames(pred.grid) <- c("sociality", "range.4", "wgt10")
pred.grid$range.4 <- rep(levels(forage.traits.bm$range.4),300)
pred.grid$sociality <- rep(levels(factor(forage.traits.bm$sociality)),400)
pred.grid$wgt10 <- seq(min(forage.traits.bm$wgt10),max(forage.traits.bm$wgt10),length.out =1200)
pred.grid

pred.full.df <- cbind(pred.grid,fitted(all_social_bm_mod,pred.grid,re_formula = NA)) %>% 
  select(-Est.Error)


forage.limits <- forage.traits.bm %>% 
  group_by(sociality,range.4) %>% 
  summarise(min=min(wgt10),
            max=max(wgt10))

pred.full.df.cut <- pred.full.df %>% 
  left_join(forage.limits) %>% 
  group_by(sociality,range.4) %>% 
  mutate(Est=ifelse(wgt10>max,NA,Estimate),
         Q2.5=ifelse(wgt10>max,NA,Q2.5),
         Q97.5=ifelse(wgt10>max,NA,Q97.5))%>% 
  mutate(Est=ifelse(wgt10<min,NA,Est),
         Q2.5=ifelse(wgt10<min,NA,Q2.5),
         Q97.5=ifelse(wgt10>max,NA,Q97.5))

pred.full.df.cut$Est <- (10^(pred.full.df.cut$Est))
pred.full.df.cut$Q2.5 <- (10^(pred.full.df.cut$Q2.5))
pred.full.df.cut$Q97.5 <- (10^(pred.full.df.cut$Q97.5))


pred.full.df.cut$sociality <- factor(pred.full.df.cut$sociality,
                                     c("Highly Eusocial",
                                       "Solitary",
                                       "Primitively Eusocial"
                                     ))

pred.full.df.cut$range.4 <- revalue(pred.full.df.cut$range.4,
                                    c("Typ-Realised" = "Realized typical",
                                      "Max-Realised" = "Realized maximum",
                                      "Typ-Potential" = "Potential typical",
                                      "Max-Potential" = "Potential maximum"))
pred.full.df.cut$social2 <- substr(pred.full.df.cut$sociality, 1, 5)

func.euc.preds.1 <- forage.traits.bm

func.euc.preds.1$gl=ifelse(func.euc.preds.1$range.type%in%"Max"==T,
          foragedist(func.euc.preds.1$itd,type="GrMhd"),
          foragedist(func.euc.preds.1$itd,type="GrThd"))

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
  mutate(wgt10 = 3, y = 50) %>%
  select(range.4,wgt10,y) %>% 
  distinct(range.4,wgt10,y)

#range_labels[c(2,4),"y"]=25

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

line.scale.y <-   scale_y_log10(breaks=c(0.1,1,10),
                                limits=c(0.007,50))

#line.scale.x <-  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
#                             labels = scales::trans_format("log10", scales::math_format(10^.x)))

line.scale.x <-  scale_x_continuous(breaks=c(0,50,100,150,200),
                                    limits=c(0,201))#trans="log10",
#line.scale.x <-  scale_x_continuous(breaks=c(3,9,30,100),limits=c(3,201))
#line.scale.x <-  scale_x_log10(breaks=c(3,10,30,100),
#              limits=c(3,201))

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

func.euc.preds.1$sociality <- factor(func.euc.preds.1$sociality,
                                     levels=c("Highly Eusocial",
                                              "Primitively Eusocial",
                                              "Solitary"))

func.euc.preds.1$social2 <- factor(func.euc.preds.1$social2,
                                   levels=c("Highl",
                                            "Primi",
                                            "Solit"))


pred.full.df.cut$sociality <- factor(pred.full.df.cut$sociality,
                                     levels=c("Highly Eusocial",
                                              "Primitively Eusocial",
                                              "Solitary"))

pred.full.df.cut$social2 <- factor(pred.full.df.cut$social2,
                                   levels=c("Highl",
                                            "Primi",
                                            "Solit"))

forage.stats$sociality <- factor(forage.stats$sociality,
                                 levels=c("Highly Eusocial",
                                          "Primitively Eusocial",
                                          "Solitary"))

forage.stats$social2 <- factor(forage.stats$social2,
                               levels=c("Highl",
                                        "Primi",
                                        "Solit"))

forage.plot$sociality <- factor(forage.plot$sociality,
                                levels=c("Highly Eusocial",
                                         "Primitively Eusocial",
                                         "Solitary"))

forage.plot$social2 <- factor(forage.plot$social2,
                              levels=c("Highl",
                                       "Primi",
                                       "Solit"))

#######
#Potential typical
range.tl
######

plot.tl <- func.euc.preds.1 %>% 
  filter(range.4%in%range.tl)

plot.tl$sociality <- factor(plot.tl$sociality,
                            levels=c("Solitary","Primitively Eusocial","Highly Eusocial"))
plot.tl$social2 <- factor(plot.tl$social2,
                          levels=c("Solit","Primi","Highl"))

ribbon.tl <- pred.full.df.cut%>% 
  filter(range.4%in%range.tl)

ribbon.tl$sociality <- factor(ribbon.tl$sociality,
                              levels=c("Solitary","Primitively Eusocial","Highly Eusocial"))
ribbon.tl$social2 <- factor(ribbon.tl$social2,
                            levels=c("Solit","Primi","Highl"))

range.tl.line.plot <- ggplot(plot.tl,
                             aes(y=distance,x=10^wgt10,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("Body mass (mg)") + 
  ylab("Foraging range (km)") + 
  line.scale.x+
  line.scale.y+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=10^wgt10,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=1)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=3,shape=21,
             col="white",
             show.legend = F,alpha=1)+
  geom_ribbon(data=ribbon.tl,
              aes(ymin=Q2.5,y=Est,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_line(data=ribbon.tl,
            aes(y=Est),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=wgt10,
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
  #scale_x_log10(breaks=c(3,10,30,100))+
  #scale_x_continuous(limits=c(0,230),breaks=c(0,50,100,150,200))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
range.tl.line.plot
#####
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
  line.scale.y+
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

plot.tr <- func.euc.preds.1 %>% 
  filter(range.4%in%range.tr)

plot.tr$sociality <- factor(plot.tr$sociality,
                            levels=c("Primitively Eusocial","Solitary","Highly Eusocial"))
plot.tr$social2 <- factor(plot.tr$social2,
                          levels=c("Primi","Solit","Highl"))

ribbon.tr <- pred.full.df.cut%>% 
  filter(range.4%in%range.tr)

ribbon.tr$sociality <- factor(ribbon.tr$sociality,
                              levels=c("Primitively Eusocial","Solitary","Highly Eusocial"))
ribbon.tr$social2 <- factor(ribbon.tr$social2,
                            levels=c("Primi","Solit","Highl"))

range.tr.line.plot <- ggplot(plot.tr,
                             aes(y=distance,x=10^wgt10,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("Body mass (mg)") + 
  ylab("Foraging range (km)") + 
  line.scale.x+
  line.scale.y+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=10^wgt10,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=1)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=3,shape=21,
             col="white",
             show.legend = F,alpha=1)+
  geom_ribbon(data=ribbon.tr,
              aes(ymin=Q2.5,y=Est,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_line(data=ribbon.tr,
            aes(y=Est),
            size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=wgt10,
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
  #scale_x_continuous(limits=c(0,230),breaks=c(0,50,100,150,200))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
# facet_wrap(~range.4,ncol=2,nrow=2,scales="fixed")
#####
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
  line.scale.y+
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

plot.bl <- func.euc.preds.1 %>% 
  filter(range.4%in%range.bl)

plot.bl$sociality <- factor(plot.bl$sociality,
                            levels=c("Highly Eusocial","Solitary","Primitively Eusocial"))

plot.bl$social2 <- factor(plot.bl$social2,
                          levels=c("Highl","Solit","Primi"))

ribbon.bl <- pred.full.df.cut%>% 
  filter(range.4%in%range.bl)

ribbon.bl$sociality <- factor(ribbon.bl$sociality,
                              levels=c("Highly Eusocial","Solitary","Primitively Eusocial"))
ribbon.bl$social2 <- factor(ribbon.bl$social2,
                            levels=c("Highl","Solit","Primi"))

range.bl.line.plot <- ggplot(plot.bl,
                             aes(y=distance,x=10^wgt10,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("Body mass (mg)") + 
  ylab("Foraging range (km)") + 
  line.scale.x+
  line.scale.y+
  scale_shape_manual(values=c(21,22))+
  geom_line(aes(x=10^wgt10,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=1)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=3,shape=21,
             col="white",
             show.legend = F,alpha=1)+
  geom_ribbon(data=ribbon.bl,
              aes(ymin=Q2.5,y=Est,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_line(data=ribbon.bl,
            aes(y=Est),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=wgt10,
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
  # scale_x_continuous(limits=c(0,230),breaks=c(0,50,100,150,200))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")
range.bl.line.plot
#####

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
  line.scale.y+
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
plot.br <- func.euc.preds.1 %>% 
  filter(range.4%in%range.br)

plot.br$sociality <- factor(plot.br$sociality,
                            levels=c("Highly Eusocial","Solitary","Primitively Eusocial"))

plot.br$social2 <- factor(plot.br$social2,
                          levels=c("Highl","Solit","Primi"))

ribbon.br <- pred.full.df.cut%>% 
  filter(range.4%in%range.br)

ribbon.br$sociality <- factor(ribbon.br$sociality,
                              levels=c("Highly Eusocial","Solitary","Primitively Eusocial"))
ribbon.br$social2 <- factor(ribbon.br$social2,
                            levels=c("Highl","Solit","Primi"))

range.br.line.plot <- ggplot(plot.br,
                             aes(y=distance,x=10^wgt10,
                                 col=sociality,
                                 fill=sociality)) +  
  xlab("Body mass (mg)") + 
  ylab("Foraging range (km)") + 
  line.scale.x+
  line.scale.y+
  scale_shape_manual(values=c(21,22))+
  
  geom_line(aes(x=10^wgt10,y=gl),inherit.aes=F,
            linetype="dashed",
            col="grey",
            alpha=1)+
  geom_point(aes(y=distance/1000,fill=social2),
             size=3,shape=21,
             col="white",
             show.legend = F,alpha=1)+
  geom_ribbon(data=ribbon.br,
              aes(ymin=Q2.5,y=Est,
                  ymax=Q97.5),col=NA,
              alpha = 0.2,
              show.legend = FALSE)+
  geom_line(data=ribbon.br,
            aes(y=Est),size=1.5,show.legend = F)+
  theme_bw() +
  geom_text(aes(label = range.4,
                x=wgt10,
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
  #scale_x_continuous(limits=c(0,230),breaks=c(0,50,100,150,200))+
  labs(name="Eusociality",
       fill="Eusociality",
       col="Eusociality")

range.br.line.plot

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
  xlab("Degree of sociality")+
  ylab("Foraging range (km)")+
  line.scale.y+
  scale_colour_manual(values=soc.pal2,guide="none")+
  scale_fill_manual(values=soc.pal2,
                    breaks=c("Highly Eusocial",
                             "Primitively Eusocial",
                             "Solitary"))+
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


ggsave(super.plot,file="plots/Figure 1.pdf",
       device="pdf",
       width=9,
       height=7,
       units = "in",
       dpi=600)

ggsave(super.plot,file="plots/Figure 1.jpg",
       device="jpg",
       width=9,
       height=7,
       units = "in",
       dpi=600)

