####
#demonstrating predictions
####

limit.cols=c("Potential"="#955F47","Realised"="#A8B9CB")
limit.cols=c("Potential"="#44781E","Realised"="#2C3B75")

example.preds.1 <- data.frame(species=c("Apis_mellifera","Apis_mellifera",
                                        "Apis_mellifera","Apis_mellifera",
                                        "Osmia_cornifrons","Osmia_cornifrons",
                                        "Osmia_cornifrons","Osmia_cornifrons",
                                    "Bombus_bifarius","Bombus_bifarius",
                                    "Bombus_bifarius","Bombus_bifarius"),
                              genus=c("Apis","Apis","Apis","Apis",
                                      "Osmia","Osmia","Osmia","Osmia",
                                      "Bombus","Bombus","Bombus","Bombus"),
                              sociality=c("Highly Eusocial","Highly Eusocial","Highly Eusocial","Highly Eusocial",
                                            "Solitary","Solitary","Solitary","Solitary",
                                            "Primitively Eusocial","Primitively Eusocial","Primitively Eusocial","Primitively Eusocial"),
                              range.4=c("Max-Potential","Max-Realised","Typ-Potential","Typ-Realised",
                                             "Max-Potential","Max-Realised","Typ-Potential","Typ-Realised",
                                             "Max-Potential","Max-Realised","Typ-Potential","Typ-Realised"),
                              publication=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                              metric=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                              wgt10=log10(23))

#bodysize(x=data.frame(IT=2.9), taxa="bee", type="IT")

pred.draws.1.r <- all_social_bm_mod%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|species)+(1|genus)) %>% 
  mutate(.pred=10^.prediction)
                  #    transform=exp)

pred.draws.2.r <- all_social_bm_mod%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = ~(1|genus)) %>% 
  mutate(.pred=10^.prediction)

pred.draws.3.r <- all_social_bm_mod%>%
  add_predicted_draws(newdata = example.preds.1,
                      re_formula = NA) %>% 
  mutate(.pred=10^.prediction)

pred.draws.1.r$model <- "All_1"
pred.draws.2.r$model <- "All_2"
pred.draws.3.r$model <- "All_3"

all.1.qi <- median_hdci(pred.draws.1.r,.value=.pred)%>%
  dplyr::select(sociality,range.4,.value,.lower,.upper)

all.2.qi <- median_hdci(pred.draws.2.r,.value=.pred)%>%
  dplyr::select(sociality,range.4,.value,.lower,.upper)

all.3.qi <- median_hdci(pred.draws.3.r,.value=.pred)%>%
  dplyr::select(sociality,range.4,.value,.lower,.upper)

all.1.qi$model <- "All_1"
all.2.qi$model <- "All_2"
all.3.qi$model <- "All_3"

max.1.qi <- all.1.qi %>% filter(range.4%in%c("Max-Potential","Max-Realised"))
max.2.qi <- all.2.qi %>% filter(range.4%in%c("Max-Potential","Max-Realised"))
max.3.qi <- all.3.qi %>% filter(range.4%in%c("Max-Potential","Max-Realised"))

mean.1.qi <- all.1.qi %>% filter(!range.4%in%c("Max-Potential","Max-Realised"))
mean.2.qi <- all.2.qi %>% filter(!range.4%in%c("Max-Potential","Max-Realised"))
mean.3.qi <- all.3.qi %>% filter(!range.4%in%c("Max-Potential","Max-Realised"))

######
#differences to be re-summarised
######
##differences for apis
(max.1.qi[2,3]-max.3.qi[2,3])/max.3.qi[2,3]
(max.1.qi[1,3]-max.3.qi[1,3])/max.3.qi[1,3]

max.1.qi[2,3]-max.3.qi[2,3]
max.1.qi[2,5]-max.3.qi[2,5]

max.1.qi[1:2,]
max.2.qi[1:2,]
max.3.qi[1:2,]

#difference between funct and physio - depending on random structure

#typical range
#highly eusocial
(mean.1.qi[2,3]-mean.1.qi[1,3])/mean.1.qi[1,3]
(mean.2.qi[2,3]-mean.2.qi[1,3])/mean.2.qi[1,3]
(mean.3.qi[2,3]-mean.3.qi[1,3])/mean.3.qi[1,3]

#primitively eusocial
(mean.1.qi[4,3]-mean.1.qi[3,3])/mean.1.qi[3,3]
(mean.2.qi[4,3]-mean.2.qi[3,3])/mean.2.qi[3,3]
(mean.3.qi[4,3]-mean.3.qi[3,3])/mean.3.qi[3,3]

#solitary
(mean.1.qi[6,3]-mean.1.qi[5,3])/mean.1.qi[5,3]
(mean.2.qi[6,3]-mean.2.qi[5,3])/mean.2.qi[5,3]
(mean.3.qi[6,3]-mean.3.qi[5,3])/mean.3.qi[5,3]

#max range
#highyl eusocial
(max.1.qi[2,3]-max.1.qi[1,3])/max.1.qi[1,3]
(max.2.qi[2,3]-max.2.qi[1,3])/max.2.qi[1,3]
(max.3.qi[2,3]-max.3.qi[1,3])/max.3.qi[1,3]

#primitively eusocial
(max.1.qi[4,3]-max.1.qi[3,3])/max.1.qi[3,3]
(max.2.qi[4,3]-max.2.qi[3,3])/max.2.qi[3,3]
(max.3.qi[4,3]-max.3.qi[3,3])/max.3.qi[3,3]

#solitary
(max.1.qi[6,3]-max.1.qi[5,3])/max.1.qi[5,3]
(max.2.qi[6,3]-max.2.qi[5,3])/max.2.qi[5,3]
(max.3.qi[6,3]-max.3.qi[5,3])/max.3.qi[5,3]

#physio diffgerences

#HE vs. PE
#potential
range((max.1.qi[1,3]-max.1.qi[3,3])/max.1.qi[3,3],
      (max.2.qi[1,3]-max.2.qi[3,3])/max.2.qi[3,3],
      (max.3.qi[1,3]-max.3.qi[3,3])/max.3.qi[3,3])

#realized 
range((max.1.qi[2,3]-max.1.qi[4,3])/max.1.qi[4,3],
      (max.2.qi[2,3]-max.2.qi[4,3])/max.2.qi[4,3],
      (max.3.qi[2,3]-max.3.qi[4,3])/max.3.qi[4,3])

#raw distance difference - wo species effects

#potential PE
(c(as.numeric(mean.3.qi[1,3]-mean.3.qi[3,3]),
   as.numeric(max.3.qi[1,3]-max.3.qi[3,3])))

#potential SO
mean(c(as.numeric(max.3.qi[1,3]-max.3.qi[5,3]),
       as.numeric(mean.3.qi[1,3]-mean.3.qi[5,3])))

mean(c(as.numeric(max.3.qi[2,3]-max.3.qi[4,3]),
       as.numeric(mean.3.qi[2,3]-mean.3.qi[4,3])))

mean(c(as.numeric(max.3.qi[2,3]-max.3.qi[6,3]),
       as.numeric(mean.3.qi[2,3]-mean.3.qi[6,3])))

#realized PE
mean(c(as.numeric(max.3.qi[3,3]-max.3.qi[5,3]),
       as.numeric(mean.3.qi[3,3]-mean.3.qi[5,3])))
mean(c(as.numeric(max.3.qi[4,3]-max.3.qi[6,3]),
       as.numeric(mean.3.qi[4,3]-mean.3.qi[6,3])))

##in text example
(max.3.qi[2,3]-max.3.qi[4,3])/max.3.qi[4,3]
(max.3.qi[2,3]-max.3.qi[6,3])/max.3.qi[6,3]

#HE vs. SO
#potential
range((max.1.qi[1,3]-max.1.qi[5,3])/max.1.qi[5,3],
      (max.2.qi[1,3]-max.2.qi[5,3])/max.2.qi[5,3],
      (max.3.qi[1,3]-max.3.qi[5,3])/max.3.qi[5,3])

#realized
range((max.1.qi[2,3]-max.1.qi[6,3])/max.1.qi[6,3],
      (max.2.qi[2,3]-max.2.qi[6,3])/max.2.qi[6,3],
      (max.3.qi[2,3]-max.3.qi[6,3])/max.3.qi[6,3])


#PE vs SO

#potential
range((max.1.qi[3,3]-max.1.qi[5,3])/max.1.qi[5,3],
      (max.2.qi[3,3]-max.2.qi[5,3])/max.2.qi[5,3],
      (max.3.qi[3,3]-max.3.qi[5,3])/max.3.qi[5,3])

#realized
range((max.1.qi[4,3]-max.1.qi[6,3])/max.1.qi[6,3],
      (max.2.qi[4,3]-max.2.qi[6,3])/max.2.qi[6,3],
      (max.3.qi[4,3]-max.3.qi[6,3])/max.3.qi[6,3])

#typical ranges

#HE vs. PE
#potential
range((mean.1.qi[1,3]-mean.1.qi[3,3])/mean.1.qi[3,3],
      (mean.2.qi[1,3]-mean.2.qi[3,3])/mean.2.qi[3,3],
      (mean.3.qi[1,3]-mean.3.qi[3,3])/mean.3.qi[3,3])

#realized
range((mean.1.qi[2,3]-mean.1.qi[4,3])/mean.1.qi[4,3],
      (mean.2.qi[2,3]-mean.2.qi[4,3])/mean.2.qi[4,3],
      (mean.3.qi[2,3]-mean.3.qi[4,3])/mean.3.qi[4,3])

#HE vs. SO
#potential
range((mean.1.qi[1,3]-mean.1.qi[5,3])/mean.1.qi[5,3],
      (mean.2.qi[1,3]-mean.2.qi[5,3])/mean.2.qi[5,3],
      (mean.3.qi[1,3]-mean.3.qi[5,3])/mean.3.qi[5,3])
#realized
range((mean.1.qi[2,3]-mean.1.qi[6,3])/mean.1.qi[6,3],
      (mean.2.qi[2,3]-mean.2.qi[6,3])/mean.2.qi[6,3],
      (mean.3.qi[2,3]-mean.3.qi[6,3])/mean.3.qi[6,3])

#PE. vs. SO
#potential
range((mean.1.qi[3,3]-mean.1.qi[5,3])/mean.1.qi[5,3],
      (mean.2.qi[3,3]-mean.2.qi[5,3])/mean.2.qi[5,3],
      (mean.3.qi[3,3]-mean.3.qi[5,3])/mean.3.qi[5,3])

#realized
range((mean.1.qi[4,3]-mean.1.qi[6,3])/mean.1.qi[6,3],
      (mean.2.qi[4,3]-mean.2.qi[6,3])/mean.2.qi[6,3],
      (mean.3.qi[4,3]-mean.3.qi[6,3])/mean.3.qi[6,3])

#####
mean.values <- rbind.fill(all.1.qi,
                          all.2.qi,
                          all.3.qi) %>%
  dplyr::select(sociality,range.4,.value,.lower,.upper,model)

mean.values[,3:5] <- mean.values[,3:5]

mean.values$species <- revalue(mean.values$sociality,
                             c("Solitary"="Osmia",
                               "Primitively Eusocial"="Bombus",
                               "Highly Eusocial"= "Apis"))

mean.values$species <- factor(mean.values$species,levels=c("Osmia",
                                                       "Bombus",
                                                       "Apis"))


mean.values$model <- revalue(mean.values$model,
                             c("All_1" = "~(1|Phylogeny)+(1|Species)",
                               "All_2" = "~(1|Phylogeny)",
                               "All_3" = "Fixed effects only"))

# reading image

labels <- c(
  Osmia = "<img src='images/osmia2.png'
    width='65' /><br>",
  Bombus = "<img src='images/bumblebee.png'
    width='60' /><br>", #was 55 
  Apis = "<img src='images/honeybee.png'
    width='80' /><br>"
)

value.cast <- data.table::dcast(setDT(mean.values),
                                model + species + sociality  ~ range.4, 
                                value.var = c(".value", ".lower",".upper"))

value.cast.long <- value.cast %>% 
  pivot_longer(cols=4:15,
               names_to = c("estimate","type", "metric"),
               names_pattern = "(.*)_(.*)-(.*)") %>% 
  pivot_wider(names_from=c("estimate"))

model.cols <- c("~(1|Phylogeny)+(1|Species)" = "#757b16",
                "~(1|Phylogeny)" = "#2F638F",
                "Fixed effects only" = "#490B0A")

value.cast.long$range.4 <- paste(value.cast.long$type,value.cast.long$metric,sep="_")

value.cast.long$range.4 <- revalue(value.cast.long$range.4,
                                   c("Typ_Realised" = "Realized typical",
                                     "Max_Realised" = "Realized maximum",
                                     "Typ_Potential" = "Potential typical",
                                     "Max_Potential" = "Potential maximum"))

value.cast.long$species<- factor(value.cast.long$species,
                                 levels=c("Osmia",
                                          "Bombus",
                                          "Apis"))


pd <- position_dodgev(height = 0.35)


# reading image

labels <- c(
  Apis = "<img src='images/honeybee.png'
    width='80' /><br>",
  Bombus = "<img src='images/bumblebee.png'
    width='60' /><br>", #was 55 
  Osmia = "<img src='images/osmia2.png'
    width='65' /><br>"
)


###
spp_range_labels <- data.frame(range.4=unique(value.cast.long$range.4),
                               x=c(7.5,7.5,3,3),
                               y=c("Apis"))
spp_range_labels$range.4 <- factor(spp_range_labels$range.4,
                                   levels=c("Potential typical","Potential maximum",
                                            "Realized typical","Realized maximum"))
spp_range_labels$model <- NA


#all.cast.long <- rbind.fill(samples.cast.long,value.cast.long)

value.cast.long$range.4 <- factor(value.cast.long$range.4,
                                  levels=c("Potential typical","Potential maximum",
                                           "Realized typical","Realized maximum"))

value.cast.long$model <- factor(value.cast.long$model,
                                  levels=c("~(1|Phylogeny)+(1|Species)",
                                           "~(1|Phylogeny)",
                                           "Fixed effects only"))

gl.dists <- data.frame(gl.dist =c(foragedist(2.9)[2],
                          foragedist(2.9)[1],
                          foragedist(2.9)[2],
                          foragedist(2.9)[1]),
               range.4 =c("Potential typical","Potential maximum",
                                           "Realized typical","Realized maximum"))

gl.dists$range.4 <- factor(gl.dists$range.4,
                                  levels=c("Potential typical","Potential maximum",
                                           "Realized typical","Realized maximum"))


species.plot.all <- ggplot(value.cast.long,aes(x=.value,
                                               y=species,
                                               col=model,
                                               fill=model))+
  geom_vline(data=gl.dists,
             aes(xintercept = gl.dist),
             linetype="dashed",alpha=0.5)+
  facet_wrap(~range.4,scales="free_x",ncol = 2,nrow = 2)+
  scale_y_discrete(expand=c(0.2,0.1),
                   name = NULL,
                   labels = labels)+
  geom_errorbarh(aes(xmin=.lower,
                     xmax=.upper),
                 position = pd,
                 height=0.3)+
  geom_point(size=2.5,col="black",pch=21,
             position = pd)+
  xlab("Foraging range (km)")+
  ylab("Taxa")+
  theme_bw() + 
  labs(name="Model",
       fill="Model",
       col="Model")+
  scale_color_manual(values=model.cols,
                     labels=c("Phylogeny + Species",
                              "Phylogeny",
                              "Fixed effects only"),
                     guide = guide_legend(reverse = F))+
  scale_fill_manual(values=model.cols,
                    labels=c("Phylogeny + Species",
                             "Phylogeny",
                             "Fixed effects only"),
                    guide = guide_legend(reverse = F))+
  theme(plot.title = element_text(hjust = 0,face="bold",size=14),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=11),
        aspect.ratio = 1,
        strip.background = element_blank(),
        text=element_text(family="Times"),
        axis.ticks.length = unit(0,"mm"),
        legend.title.align=0.5,
        axis.text.y = element_markdown(color = "black", size = 11),
        axis.text.x = element_text(size=11),
        axis.title.x = element_text(size=12,face="bold"),
        axis.title.y = element_blank(),
        strip.text =  element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0.5,2,0.5,2), "lines"),
        panel.spacing = unit(0.5,"lines"),
        legend.box = "vertical",
        legend.background = element_blank(),
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 0.4))

species.plot.all

species.plot.all<-species.plot.all+
  facetted_pos_scales(
    x = list(
      range.4 == "Potential typical" ~ scale_x_continuous(limits = c(0,6)),
      range.4 == "Potential maximum" ~ scale_x_continuous(limits = c(0,15)),
      range.4 == "Realized typical" ~ scale_x_continuous(limits = c(0,6)),
      range.4 == "Realized maximum" ~ scale_x_continuous(limits = c(0,15))
    ))

species.plot.all <- species.plot.all+
  geom_text(aes(label = range.4,
                x=x,
                y=y),
            col="black",
            fontface="bold",
            size=5,
            data = spp_range_labels,
            # inherit.aes = F,
            #hjust="left",
            family="Times",
            vjust = -2)

ggsave(species.plot.all,file="plots/Figure S5.pdf",
       device="pdf",
       width=9,
       height=7,
       units = "in",
       dpi = 600)

ggsave(species.plot.all,file="plots/Figure S5.jpg",
       device="jpg",
       width=9,
       height=7,
       units = "in",
       dpi = 600)

