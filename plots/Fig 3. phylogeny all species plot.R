#run 'set up data_scl.R up to line 70 before running this script

library(ggplot2)
library(RColorBrewer)
library(ggtree)
library(colorspace)

#count number of orders present in the full phylogenetic tree
orderdf<-avgdf %>% count(Order)

#make a phylogeny with each unique order present
g2<-avgdf[!duplicated(avgdf$Order),]#orders only
g2order<-arrange(g2,Order)#sort to match order df
g2order$n<-orderdf$n#attach number of species in order
str(g2order$Binomial)
g2order$full<-paste0(g2order$Order," (",as.character(g2order$n),")")
g2order$Order<-as.character(g2order$Order)

#fig sampling and grouping
mypal <- colorRampPalette(brewer.pal(6, "Blues"))
mypal2 <- colorRampPalette(brewer.pal(6, "YlOrRd"))


#make clade labels
A<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Passeriformes"])
B<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Charadriiformes"])
C<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Falconiformes"])
D<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Accipitriformes"])
E<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Bucerotiformes"])
F<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Gruiformes"])
G<--findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Anseriformes"])
H<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Galliformes"])
I<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Pelecaniformes"])
J<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Procellariiformes"])
K<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Sphenisciformes"])
L<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Suliformes"])
M<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Psittaciformes"])
N<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Columbiformes"])
O<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Strigiformes"])
P<-findMRCA(birdtreels,avgdf$Binomial[avgdf$Order=="Anseriformes"])


#plot
p<-ggtree(birdtreels, layout = "circular", open.angle = 150) %<+% avgdf + ###########, layout = "circular"
  scale_color_manual(values = c(mypal(5),"black","grey","green","blue"))+
  geom_cladelabel(A, grep("Passeriformes",g2order$full, value = T), offset=25, barsize=2, align = T, angle=0,offset.text=15, fontsize=3)+
  geom_cladelabel(B, grep("Charadriiformes",g2order$full, value = T), offset=25, barsize=2, align = T, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(C, grep("Falconiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(D, grep("Accipitriformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(E, grep("Bucerotiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(F, grep("Gruiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(G, grep("Anseriformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(H, grep("Galliformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(I, grep("Pelecaniformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(J, grep("Procellariiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(K, grep("Sphenisciformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(L, grep("Suliformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(M, grep("Psittaciformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  #geom_cladelabel(119, "Struthioniformes", offset=0, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(N, grep("Columbiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(O, grep("Strigiformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+
  geom_cladelabel(P, grep("Anseriformes",g2order$full, value = T), offset=25, barsize=2, angle=0, offset.text=15,   fontsize=3)+

  #geom_tiplab2(aes(size = 1, label = Binomialsinge), align = TRUE, geom = "text", angle = 0, offset=15, linetype = "dotted")+

  geom_strip("Phaethon_rubricauda", "Phaethon_rubricauda", offset=16, offset.text=15, hjust=0, fontsize=3,
             label="Phaethontiformes")+
  geom_strip(g2order$Binomial[9], g2order$Binomial[9], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[9])+
  geom_strip(g2order$Binomial[20], g2order$Binomial[20], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[20])+
  geom_strip(g2order$Binomial[7], g2order$Binomial[7], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[7])+
  geom_strip(g2order$Binomial[3], g2order$Binomial[3], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[3])+
  geom_strip(g2order$Binomial[15], g2order$Binomial[15], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[15])+
  geom_strip(g2order$Binomial[10], g2order$Binomial[10], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[10])+
  geom_strip(g2order$Binomial[19], g2order$Binomial[19], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[19])+
  geom_strip(g2order$Binomial[25], g2order$Binomial[25], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[25])+
  geom_strip(g2order$Binomial[13], g2order$Binomial[13], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[13])+
  geom_strip(g2order$Binomial[5], g2order$Binomial[5], offset=16, offset.text=15, hjust=0, fontsize=3,
             label=g2order$full[5])
p

#label rownames of dataframe by species name
###########ESSENTIAL step to change row names forheatmap#############
rownames(avgdf)<-avgdf$Binomial

avgdf$aud_rel<-ifelse(is.na(avgdf$aud_rel),"_",avgdf$aud_rel)

#add heatmap to circular phylogeny
avgdf %>% dplyr::select(aud_rel) %>%
  gheatmap(p,., #"Category",
            width = 0.2, offset = 0,
            color = "black",
            #colnames = FALSE,
            colnames_position = "top",
            colnames_angle = 0,
            colnames_offset_x = 0,
            colnames_offset_y = 0)+
  scale_fill_manual(values = c("white","grey","black"), na.value = "#FFFFFF")




#open up the circle
l<-open_tree(k,30)
l

ggsave("D:/Analysis_plots/ecolcircle.pdf", width=10, height=10)

