
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
                         ")")
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
  geom_tiplab(aes(label = full), offset = 1) +
  xlim(NA,30)
ppiped

ggsave("C:/Users/jeffz/Downloads/JZ_rescalingms_JAN/Figs/inset.svg",
       device = "svg",
       width = 4, height = 4)
