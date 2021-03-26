#brazil

brazil <- read.csv("data/Brazilian_bees_specimens_data.csv")
colnames(brazil)

brazil$Genus.species <- paste(brazil$Genus,brazil$Specific.epithet,sep="_")

brazil$Sex <- factor(brazil$Sex)

brazil_traits <- brazil%>%
  filter(Sex == "Female")%>%
  group_by(Genus.species)%>%
  summarise(mean_ITD=mean(ITD),
            SD_ITD=sd(ITD))

brazil_traits[brazil_traits$Genus.species%in%forage_data_needed[is.na(forage_data_needed$ITD)==TRUE,]$Genus.species,]



