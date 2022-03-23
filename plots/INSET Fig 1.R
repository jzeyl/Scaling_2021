
#count number of orders present in the full phylogenetic tree
superorderdf<-avgdf %>% count(superorder)


#count number of species with audiograms available
####these values of audio_count added as superscripts manually
audio_count<-avgdf %>%
  filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder) %>% count()

#only one superorder per row
avgdf_super<-avgdf[!duplicated(avgdf$superorder),]#orders only

avgdf_super<-arrange(avgdf_super,superorder)#sort to match order df
avgdf_super$n<-superorderdf$n#attach number of species in order

#prune phylogeny to superorder level
orderPhy<-keep.tip(birdtreels,avgdf_super$Binomial)

#create labels with the numbers of species, and number of audiograms
avgdf_super$full<-paste0(avgdf_super$Order,
                         " (",
                         as.character(avgdf_super$n),
                         ")"                         )
avgdf_super$full

avgdf_super$Order<-as.character(avgdf_super$Order)
avgdf_super$Order
orderPhy$tip.label
orderPhy$tip.label
orderPhy$tip.label<-as.character(orderPhy$tip.label)#######have as character!!!
str(avgdf_super$full)

#circular cladogram
#ppiped <- ggtree(orderPhy, layout = "circular",
#                 branch.length = "none") %<+% avgdf_super +
#  geom_tiplab(aes(label=full, angle = angle), offset = 1) +
#  xlim(NA,30)
#ppiped
#order rectangle
ppiped <- ggtree(orderPhy,  branch.length = "none") %<+% avgdf_super +
  geom_tiplab(aes(label=full), offset = 1) +
  xlim(NA,30)
ppiped

  #geom_text(aes(label = node))+
# geom_cladelabel(51, "Paleognathae", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(50, "Galloanserae(8)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(22, "Apodiformes (1)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(48, "Columbaves (8)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(18, "Gruiformes (5)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(41, "Aequorlitornithes(59)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(9, "Accipitriformes(7)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(35, "Australaves(30)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(8, "Strigiformes (2)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
# geom_cladelabel(38, "Coraciimorphae (5)", offset=7, barsize=2, align = T, angle=0,offset.text=0)#, fontsize=3
#piped
ggtree(orderPhy,  branch.length = "none") %<+% avgdf_super +
  geom_tiplab(aes(label=full), offset = 1) +
  xlim(NA,30)
avgdf %>% count(aud_rel)
avgdf %>% group_by(superorder) %>% count(aud_rel)
?filter
avgdf %>% group_by(superorder) %filter(., superorder == "Yes") %>% count(aud_rel)
avgdf %>% group_by(superorder) %>% filter(., superorder == "Yes") %>% count(aud_rel)
avgdf %>% filter(superorder == "Yes")
avgdf %>% filter(.,superorder == "Yes")
unique(avgdf$superorder)
avgdf %>% filter(aud_rel == "Yes")
avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder) %>% count()
avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder,aud_rel) %>% count()
avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder,aud_rel) %>% count() %>% View()
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder,aud_rel) %>% count()
View(audio_count)
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder) %>% count()
sum(audio_count$n)
#count number of species with audiograms available
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder,aul_rel) %>% count()
#count number of species with audiograms available
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder,aud_rel) %>% count()
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder) %>% count(Order)
audio_count<-avgdf %>% filter(aud_rel == "Yes"| aud_rel =="Congener") %>%
  group_by(superorder) %>% count()
