###sample sizes within publications
forage.traits.nplot <- forage.traits

forage.traits.nplot$range.4 <- revalue(forage.traits.nplot$range.4,
                                 c("Typ-Realised" = "Realized typical",
                                   "Max-Realised" = "Realized maximum",
                                   "Typ-Potential" = "Potential typical",
                                   "Max-Potential" = "Potential maximum"))

forage.traits.nplot$range.4 <- factor(forage.traits.nplot$range.4,levels=c("Potential maximum",
                                                               "Potential typical",
                                                               "Realized maximum",
                                                               "Realized typical"))

forage.traits.nplot$range.4.2 <- substr(forage.traits.nplot$range.4, 5, 12)

forage.traits.nplot$range.4.2 <- factor(forage.traits.nplot$range.4.2,levels=c("ntial ma",
                                                                           "ntial ty",
                                                                           "ized max",
                                                                           "ized typ"))


forage.publ.n <- forage.traits %>% 
  filter(!is.na(N)) %>% 
  group_by(range.4,sociality) %>%
  dplyr::summarise(median = median(N),
                   #sd=sd(log.dist),
                   lower.q=quantile(N,0.25,names=F),
                   upper.q=quantile(N,0.75,names=F),
                   n.publ = n()) 

forage.publ.n$range.4 <- revalue(forage.publ.n$range.4,
                                c("Typ-Realised" = "Realized typical",
                                  "Max-Realised" = "Realized maximum",
                                  "Typ-Potential" = "Potential typical",
                                  "Max-Potential" = "Potential maximum"))

forage.publ.n$range.4 <- factor(forage.publ.n$range.4,levels=c("Potential maximum",
                                                             "Potential typical",
                                                             "Realized maximum",
                                                             "Realized typical"))
forage.publ.n <- 
  forage.publ.n%>% 
  left_join(forage.stats %>% 
              ungroup %>% 
              dplyr::select(range.4,sociality,n)
            ,by=c("range.4","sociality"))


forage.publ.n$n_perc <- (forage.publ.n$n-forage.publ.n$n.publ)/forage.publ.n$n*100


library(ggplot2)

four.pal.4 <- c("Potential typical" = "#CABEE9",
              "Potential maximum" = "#FAE093", 
              "Realized typical"= "#D04E59", 
              "Realized maximum" = "#2F3D70",
              "ntial ty" = lighten("#CABEE9",amount = 0.5),
              "ntial ma" = lighten("#FAE093",amount = 0.5),
              "ized typ"= lighten("#D04E59",amount = 0.5),
              "ized max" = lighten("#2F3D70",amount = 0.5))


sample.size.plot <- ggplot(forage.publ.n,aes(y=median,
                         x=sociality,
                         col=range.4,
                         fill=range.4))+
  geom_point(data=forage.traits.nplot,
             aes(y=N,fill=range.4.2),
             show.legend = F,
             # alpha=0.25,
             col="white",
             position = position_jitterdodge(dodge.width = dodge.height,
                                             jitter.width = 0.3),
             size=3,
             pch=21)+
  geom_errorbar(aes(ymin=lower.q,
                    ymax=upper.q),
                show.legend=F,
                size=1,
                position = position_dodge(width = dodge.height),
                width=0.3)+
  geom_point(show.legend = T,
             alpha=1,col="black",
             position = position_dodge(width = dodge.height),
             size=5,
             pch=21)+
  theme_bw()+
  xlab("Degree of sociality")+
  ylab("Sample size")+
  scale_y_log10()+
  scale_fill_manual(values=four.pal.4,
                    breaks=c("Potential typical",
                             "Potential maximum",
                             "Realized typical",
                             "Realized maximum"))+#,guide="none")+ 
  scale_colour_manual(values=four.pal.4,
                      guide="none")+ 
  labs(name="Range type",
       fill="Range type",
       col="Range type")+
  #point.theme+
  theme(aspect.ratio = 1,
        plot.margin = unit(c(0.25,0.25,2,0.25), "lines"),
        axis.ticks = element_blank(),
        legend.title = element_text(size=12,family="Times",face="bold"),
        axis.title.y = element_text(size=12,family="Times",face="bold"),
        axis.title.x = element_text(size=12,family="Times",face="bold"),
        axis.text.y = element_text(size=10,family="Times"),
        legend.text = element_text(size=9,family="Times"),
        axis.text.x = element_text(size=10,family="Times"))



ggsave(sample.size.plot,file="plots/Figure S5.pdf",
       device="pdf",
       width=7,
       height=6,
       units = "in",
       dpi=600)

ggsave(sample.size.plot,file="plots/Figure S5.jpg",
       device="jpg",
       width=7,
       height=6,
       units = "in",
       dpi=600)
