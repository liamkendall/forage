####
#demonstrating predictions
####

limit.cols=c("Potential"="#955F47","Realised"="#A8B9CB")
limit.cols=c("Potential"="#44781E","Realised"="#2C3B75")

example.preds.1 <- data.frame(species=c("Apis_mellifera","Apis_mellifera",
                                        "Osmia_cornifrons","Osmia_cornifrons",
                                    "Bombus_bifarius","Bombus_bifarius"),
                              genus=c("Apis","Apis",
                                      "Osmia","Osmia",
                                      "Bombus","Bombus"),
                              sociality=c("Highly Eusocial","Highly Eusocial",
                                            "Solitary","Solitary",
                                            "Primitively Eusocial","Primitively Eusocial"),
                              measure.type=c("Potential","Realised",
                                             "Potential","Realised",
                                             "Potential","Realised"),
                              publication=c(NA,NA,NA,NA,NA,NA),
                              metric=c(NA,NA,NA,NA,NA,NA),
                              itd=c(2.9,2.9,2.9,2.9,2.9,2.9))

pred.draws.max.1.r <- max.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|species)+(1|genus),
                      transform=exp)

pred.draws.max.2.r <- max.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|genus),
                  transform=exp)

pred.draws.max.3.r <- max.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = NA,
                      transform=exp)

pred.draws.max.1.r$model <- "Max_1"
pred.draws.max.2.r$model <- "Max_2"
pred.draws.max.3.r$model <- "Max_3"

max.1.qi <- median_hdci(pred.draws.max.1.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

max.2.qi <- median_hdci(pred.draws.max.2.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

max.3.qi <- median_hdci(pred.draws.max.3.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

pred.draws.mean.1.r <- typ.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|species)+(1|genus),
                      transform=exp)

pred.draws.mean.2.r <- typ.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|genus),
                      transform=exp)

pred.draws.mean.3.r <- typ.social.brm%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = NA,
                      transform=exp)


pred.draws.mean.1.r$model <- "Typ_1"
pred.draws.mean.2.r$model <- "Typ_2"
pred.draws.mean.3.r$model <- "Typ_3"

mean.1.qi <- median_hdci(pred.draws.mean.1.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

mean.2.qi <- median_hdci(pred.draws.mean.2.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

mean.3.qi <- median_hdci(pred.draws.mean.3.r,.value=.prediction)%>%
  select(sociality,measure.type,.value,.lower,.upper)

max.1.qi$model <- "Max_1"
max.2.qi$model <- "Max_2"
max.3.qi$model <- "Max_3"

mean.1.qi$model <- "Typ_1"
mean.2.qi$model <- "Typ_2"
mean.3.qi$model <- "Typ_3"

##differences for apis
(max.1.qi[2,3]-max.3.qi[2,3])/max.3.qi[2,3]
(max.1.qi[1,3]-max.3.qi[1,3])/max.3.qi[1,3]

max.1.qi[2,5]-max.3.qi[2,5]

#difference between funct and physio
(max.1.qi[2,3]-max.1.qi[1,3])/max.1.qi[1,3]
(max.2.qi[2,3]-max.2.qi[1,3])/max.2.qi[1,3]
(max.3.qi[2,3]-max.3.qi[1,3])/max.3.qi[1,3]

(max.1.qi[4,3]-max.1.qi[3,3])/max.1.qi[3,3]
(max.2.qi[4,3]-max.2.qi[3,3])/max.2.qi[3,3]
(max.3.qi[4,3]-max.3.qi[3,3])/max.3.qi[3,3]

(max.1.qi[6,3]-max.1.qi[5,3])/max.1.qi[5,3]
(max.2.qi[6,3]-max.2.qi[5,3])/max.2.qi[5,3]
(max.3.qi[6,3]-max.3.qi[5,3])/max.3.qi[5,3]

(mean.1.qi[2,3]-mean.1.qi[1,3])/mean.1.qi[1,3]
(mean.2.qi[2,3]-mean.2.qi[1,3])/mean.2.qi[1,3]
(mean.3.qi[2,3]-mean.3.qi[1,3])/mean.3.qi[1,3]

(mean.1.qi[4,3]-mean.1.qi[3,3])/mean.1.qi[3,3]
(mean.2.qi[4,3]-mean.2.qi[3,3])/mean.2.qi[3,3]
(mean.3.qi[4,3]-mean.3.qi[3,3])/mean.3.qi[3,3]

(mean.1.qi[6,3]-mean.1.qi[5,3])/mean.1.qi[5,3]
(mean.2.qi[6,3]-mean.2.qi[5,3])/mean.2.qi[5,3]
(mean.3.qi[6,3]-mean.3.qi[5,3])/mean.3.qi[5,3]

(max.1.qi[4,3]-max.1.qi[3,3])/max.1.qi[3,3]
(max.1.qi[6,3]-max.1.qi[5,3])/max.1.qi[5,3]


#physio
range((max.1.qi[2,3]-max.1.qi[4,3])/max.1.qi[4,3],
      (max.2.qi[2,3]-max.2.qi[4,3])/max.2.qi[4,3],
      (max.3.qi[2,3]-max.3.qi[4,3])/max.3.qi[4,3],
      (max.1.qi[1,3]-max.1.qi[3,3])/max.1.qi[3,3],
      (max.2.qi[1,3]-max.2.qi[3,3])/max.2.qi[3,3],
      (max.3.qi[1,3]-max.3.qi[3,3])/max.3.qi[3,3])

range((max.1.qi[2,3]-max.1.qi[6,3])/max.1.qi[6,3],
      (max.2.qi[2,3]-max.2.qi[6,3])/max.2.qi[6,3],
      (max.3.qi[2,3]-max.3.qi[6,3])/max.3.qi[6,3],
      (max.1.qi[1,3]-max.1.qi[5,3])/max.1.qi[5,3],
      (max.2.qi[1,3]-max.2.qi[5,3])/max.2.qi[5,3],
      (max.3.qi[1,3]-max.3.qi[5,3])/max.3.qi[5,3])

range((max.1.qi[4,3]-max.1.qi[6,3])/max.1.qi[6,3],
      (max.2.qi[4,3]-max.2.qi[6,3])/max.2.qi[6,3],
      (max.3.qi[4,3]-max.3.qi[6,3])/max.3.qi[6,3],
      (max.1.qi[3,3]-max.1.qi[5,3])/max.1.qi[5,3],
      (max.2.qi[3,3]-max.2.qi[5,3])/max.2.qi[5,3],
      (max.3.qi[3,3]-max.3.qi[5,3])/max.3.qi[5,3])

range((mean.1.qi[2,3]-mean.1.qi[4,3])/mean.1.qi[4,3],
      (mean.2.qi[2,3]-mean.2.qi[4,3])/mean.2.qi[4,3],
      (mean.3.qi[2,3]-mean.3.qi[4,3])/mean.3.qi[4,3],
      (mean.1.qi[1,3]-mean.1.qi[3,3])/mean.1.qi[3,3],
      (mean.2.qi[1,3]-mean.2.qi[3,3])/mean.2.qi[3,3],
      (mean.3.qi[1,3]-mean.3.qi[3,3])/mean.3.qi[3,3])

range((mean.1.qi[2,3]-mean.1.qi[6,3])/mean.1.qi[6,3],
      (mean.2.qi[2,3]-mean.2.qi[6,3])/mean.2.qi[6,3],
      (mean.3.qi[2,3]-mean.3.qi[6,3])/mean.3.qi[6,3],
      (mean.1.qi[1,3]-mean.1.qi[5,3])/mean.1.qi[5,3],
      (mean.2.qi[1,3]-mean.2.qi[5,3])/mean.2.qi[5,3],
      (mean.3.qi[1,3]-mean.3.qi[5,3])/mean.3.qi[5,3])

range((mean.1.qi[4,3]-mean.1.qi[6,3])/mean.1.qi[6,3],
      (mean.2.qi[4,3]-mean.2.qi[6,3])/mean.2.qi[6,3],
      (mean.3.qi[4,3]-mean.3.qi[6,3])/mean.3.qi[6,3],
      (mean.1.qi[3,3]-mean.1.qi[5,3])/mean.1.qi[5,3],
      (mean.2.qi[3,3]-mean.2.qi[5,3])/mean.2.qi[5,3],
      (mean.3.qi[3,3]-mean.3.qi[5,3])/mean.3.qi[5,3])

mean.values <- rbind.fill(max.1.qi,
                          max.2.qi,
                          max.3.qi,
                          mean.1.qi,
                          mean.2.qi,
                          mean.3.qi) %>%
  select(sociality,measure.type,.value,.lower,.upper,model)

mean.values[,3:5] <- mean.values[,3:5]/1000

mean.values$species <- revalue(mean.values$sociality,
                             c("Solitary"="Osmia",
                               "Primitively Eusocial"="Bombus",
                               "Highly Eusocial"= "Apis"))

mean.values$species <- factor(mean.values$species,levels=c("Osmia",
                                                       "Bombus",
                                                       "Apis"))

##plot
samples.plots <- rbind(pred.draws.max.1.r,
                       pred.draws.max.2.r,
                       pred.draws.max.3.r,
                       pred.draws.mean.1.r,
                       pred.draws.mean.2.r,
                       pred.draws.mean.3.r)

samples.plots$species <- revalue(samples.plots$sociality,
                             c("Solitary"="Osmia",
                               "Primitively Eusocial"="Bombus",
                               "Highly Eusocial"= "Apis"))

samples.plots$species <- factor(samples.plots$species,levels=c("Osmia",
                                           "Bombus",
                                           "Apis"))


samples.plots$.prediction <- (samples.plots$.prediction)/1000

# reading image

labels <- c(
  Osmia = "<img src='images/osmia2.png'
    width='65' /><br>",
  Bombus = "<img src='images/bumblebee.png'
    width='55' /><br>",
  Apis = "<img src='images/honeybee.png'
    width='80' /><br>"
)

samples.cast <- dcast(samples.plots, model + publication + metric+ species + genus + itd + sociality + .row +
                        .chain + .iteration + .draw ~ measure.type, value.var = ".prediction")

value.cast <- data.table::dcast(setDT(mean.values), model + species + sociality  ~ measure.type, value.var = c(".value", ".lower",".upper"))

samples.cast.typ <- samples.cast%>%
  filter(model%in%c("Typ_1",
                    "Typ_2",
                    "Typ_3"))

values.typ <- mean.values%>%
  filter(model%in%c("Typ_1",
                    "Typ_2",
                    "Typ_3"))

model.cols.typ <- c("Typ_1" = "#757b16",
                "Typ_2" = "#2F638F",
                "Typ_3" = "#490B0A")

pd <- position_dodgev(height = 0.35)

samples.cast.typ$model <- factor(samples.cast.typ$model,
                                 levels = c("Typ_3",
                                            "Typ_2",
                                            "Typ_1"))
samples.cast.typ$Title <- "Typical"

species.plot.typ <- ggplot(samples.cast.typ,aes(x=Potential,
                                           y=species,
                                           col=model,
                                           fill=model))+
  scale_y_discrete(expand=c(0.2,0.1),
    name = NULL,
    labels = labels)+
  geom_vline(xintercept = foragedist(2.9)[2],linetype="dashed",alpha=0.5)+
  coord_cartesian(xlim=c(0,6.5))+
  stat_density_ridges(alpha = 0.5,
                      scale = 0.5,
                      rel_min_height = 0.01,
                      quantile_lines = T,
                      quantiles = 2,show.legend = F,
                      quantile_fun = median,col="black",
                      position = position_nudge(y = 0.2))+
  stat_density_ridges(aes(x=Realised),
                      alpha = 0.5,
                      scale = 0.5,
                      rel_min_height = 0.01,
                      quantile_lines = T,
                      quantiles = 2,show.legend = F,
                      quantile_fun = median,
                      position = position_nudge(y = 0.2), 
                      col="black")+
  geom_errorbarh(data=values.typ,
                 aes(xmin=.lower,
                     xmax=.upper,
                     y=species,
                     col=model,
                     linetype=measure.type),
                 inherit.aes = F,
                 position = pd,
                 height=0.3)+
  geom_point(data=values.typ,aes(x=.value,
                                  y=species,
                                  fill=model,
                                  pch=measure.type),
             size=2.5,col="black",
             position = pd)+
  xlab("Foraging range (km)")+
  ylab("Taxa")+
  labs(name="Metric type",
       fill="Metric type",
       col="Metric type")+
  scale_color_manual(values=model.cols.typ)+
  scale_fill_manual(values=model.cols.typ)+
  scale_shape_manual(values=c(21,22))+
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
        axis.text.y = element_markdown(color = "black", size = 11),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=14,face="bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,2), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  facet_wrap(~Title)

species.plot.typ

###max

samples.cast.max <- samples.cast%>%
  filter(model%in%c("Max_1",
                    "Max_2",
                    "Max_3"))

samples.plots.max <- samples.plots%>%
  filter(model%in%c("Max_1",
                    "Max_2",
                    "Max_3"))

values.max <- mean.values%>%
  filter(model%in%c("Max_1",
                    "Max_2",
                    "Max_3"))

model.cols.max <- c("Max_1" = "#757b16",
                "Max_2" = "#2F638F",
                "Max_3" = "#490B0A")

samples.cast.max$model <- factor(samples.cast.max$model,
                                 levels = c("Max_3",
                                            "Max_2",
                                            "Max_1"))

samples.cast.max$Title <- "Maximum"

species.plot.max <- ggplot(samples.cast.max,aes(x=Potential,
                                                y=species,
                                                col=model,
                                                fill=model))+
  scale_y_discrete(expand=c(0.2,0.1),
                   name = NULL,
                   labels = labels)+
  coord_cartesian(xlim=c(0,16))+
  geom_vline(xintercept = foragedist(2.9)[1],linetype="dashed",alpha=0.5)+
  stat_density_ridges(alpha = 0.5,
                      scale = 0.5,
                      rel_min_height = 0.01,
                      quantile_lines = T,
                      quantiles = 2,
                      show.legend = T,
                      quantile_fun = median,col="black",
                      position = position_nudge(y = 0.2))+
  stat_density_ridges(aes(x=Realised),
                      alpha = 0.5,
                      scale = 0.5,
                      rel_min_height = 0.01,
                      quantile_lines = T,
                      quantiles = 2,show.legend = T,
                      quantile_fun = median,
                      position = position_nudge(y = 0.2), 
                      col="black")+
  geom_errorbarh(data=values.max,
                 aes(xmin=.lower,
                     xmax=.upper,
                     y=species,
                     col=model,
                     linetype=measure.type),
                 show.legend = T,
                 inherit.aes = F,
                 position = pd,
                 height=0.3)+
  geom_point(data=values.max,aes(x=.value,
                                 y=species,
                                 fill=model,
                                 pch=measure.type),
             size=2.5,col="black",
             position = pd)+
  xlab("Foraging range (km)")+
  ylab("Taxa")+
  labs(name="Model",
       fill="Model",
       col="Model")+
  scale_color_manual(values=model.cols.max,
                     labels=c("~(1|Phylogeny)+(1|Species)",
                              "~(1|Phylogeny)",
                              "Fixed effects only"),
                     guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values=model.cols.max,
                    labels=c("~(1|Phylogeny)+(1|Species)",
                             "~(1|Phylogeny)",
                             "Fixed effects only"),
                    guide = guide_legend(reverse = TRUE))+
  scale_shape_manual(values=c(21,22),guide="none")+
  scale_linetype_manual(values=c(1,2),guide="none")+
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
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.title.y = element_blank(),
        strip.text =  element_text(size=14,face="bold"),
        legend.position = c(0.75, 0.15),
        plot.margin = unit(c(0.5,0.5,0.5,2), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))+
  facet_wrap(~Title)

sp.max.p <- ggplotGrob(species.plot.max)
sp.typ.p <- ggplotGrob(species.plot.typ)

ggsave(cbind(sp.typ.p,
             sp.max.p),file="plots/Figure 3.pdf",
       device="pdf",
       width=12,
       height=8,
       units = "in",
       dpi = 300)

ggsave(cbind(sp.typ.p,
             sp.max.p),file="plots/Figure 3.jpg",
       device="jpg",
       width=12,
       height=8,
       units = "in",
       dpi = 300)

