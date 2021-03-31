#visualising phylogeny
library(ggtree)
library(treeio)
library(phyloseq)
library(ggjoy)
library(dplyr)
library(ggtree)


#species from trait dataframe
max.species=unique(as.character(forage.traits.max$Genus_species))
mean.species=unique(as.character(forage.traits.mean$Genus_species))
all.species=unique(as.character(forage.traits$Genus_species))

bee.max.spp.tree <- bee.max.tree
bee.mean.spp.tree <- bee.mean.tree

bee.mcmc


bee.all.tree=drop.tip(bee.mcmc, setdiff(bee.mcmc$tip.label,
                                        unique(forage.traits$Genus)))

## Will's suggestion
bee.all.tree$tip.label<-paste(bee.all.tree$tip.label,"_xyz",sep="")
for(i in 1:length(all.species)){
  bee.all.tree<-add.species.to.genus(bee.all.tree,all.species[i],
                                     where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.all.tree$tip.label)
bee.all.tree<-drop.tip(bee.all.tree,bee.all.tree$tip.label[ii])
bee.all.tree$node.label=NULL


## Will's suggestion
bee.max.spp.tree$tip.label<-paste(bee.max.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(max.species)){
  bee.max.spp.tree<-add.species.to.genus(bee.max.spp.tree,max.species[i],
                                         where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.max.spp.tree$tip.label)
bee.max.spp.tree<-drop.tip(bee.max.spp.tree,bee.max.spp.tree$tip.label[ii])
bee.max.spp.tree$node.label=NULL

#mean
bee.mean.spp.tree$tip.label<-paste(bee.mean.spp.tree$tip.label,"_xyz",sep="")
for(i in 1:length(mean.species)){
  bee.mean.spp.tree<-add.species.to.genus(bee.mean.spp.tree,mean.species[i],
                                          where="root")
}
## prune out these same taxa
ii<-grep("xyz",bee.mean.spp.tree$tip.label)
bee.mean.spp.tree<-drop.tip(bee.mean.spp.tree,bee.mean.spp.tree$tip.label[ii])
bee.mean.spp.tree$node.label=NULL

bee.max.spp.tree
bee.mean.spp.tree

##example
data("GlobalPatterns")
GP <- GlobalPatterns
GP <- prune_taxa(taxa_sums(GP) > 600, GP)
sample_data(GP)$human <- get_variable(GP, "SampleType") %in% 
  c("Feces", "Skin") 

mergedGP <- merge_samples(GP, "SampleType")
mergedGP <- rarefy_even_depth(mergedGP,rngseed=394582)
mergedGP <- tax_glom(mergedGP,"Order") 

melt_simple <- psmelt(mergedGP) %>% 
  filter(Abundance < 120) %>% 
  select(OTU, val=Abundance)

`tax_table<-`()

forage.summaries[,c("Family","Genus_species","social_tree")]

OTU = otu_table(otu_mat, taxa_are_rows = TRUE)
TAX = tax_table(forage.summaries)
samples = sample_data(samples_df)

carbom <- phyloseq(OTU, TAX, samples)
carbom


forage.summaries <- forage.traits[,c("Family","Genus_species","distance_type2","social_tree","ITD","Distance")]%>%
  group_by(Family,Genus_species,distance_type2,social_tree)%>%
  summarise(mean_dist=mean(Distance),
            mean_log_dist=mean(log(Distance)),
            sd_dist=sd(Distance),
            n_dist=n())#%>%
  tibble::column_to_rownames("Genus_species")

forage.summaries.max <- forage.summaries%>%
  filter(distance_type2%in%"Max")%>%
  ungroup()%>%
  select(Genus_species,mean_log_dist)%>%
  tibble::column_to_rownames("Genus_species")%>%
  t()

forage.summaries.mean <- forage.summaries%>%
  filter(distance_type2%in%"Typical")%>%
  ungroup()%>%
  select(Genus_species,mean_log_dist)%>%
  tibble::column_to_rownames("Genus_species")%>%
  t()


names(forage.summaries.max) <- colnames(forage.summaries.max)

names(forage.summaries.mean) <- colnames(forage.summaries.mean)

setdiff(bee.max.spp.tree$tip.label,rownames(forage.summaries.max))
setdiff(colnames(forage.summaries.max),bee.max.spp.tree$tip.label)

contMap(bee.max.spp.tree,x=forage.summaries.max)
contMap(bee.mean.spp.tree,x=forage.summaries.mean)
head(forage.summaries)

boxplot(forage.summaries$mean_dist~forage.summaries$distance_type2)


?geom_joy

ggplot(forage.summaries,aes(x=mean_dist,y=Genus_species,col=distance_type2))+
  geom_point()+
  geom_errorbarh(aes(xmin=mean_dist-(sd_dist/sqrt(n_dist)),
                     xmax=mean_dist+(sd_dist/sqrt(n_dist))))
?contMap

p <- ggtree(mergedGP) + 
  geom_tippoint(aes(color=Phylum), size=1.5)

facet_plot(p, panel="Abundance", data=melt_simple, 
           geom_joy, mapping = aes(x=val,group=label, 
                                   fill=Phylum), 
           color='grey80', lwd=.3)