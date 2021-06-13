#load phylogeny and set up pgls analysis
birdtree<-read.tree("JZ tree Prum merged hackett.tree")
birdtree<-drop.tip(birdtree,c("Alle_alle","Phalacrocorax_auritus"))
birdtree$tip.label<-as.character(birdtree$tip.label)
birdtreels<-birdtree

#make 'distinct df', with only one species per line, and append variables back onto avgdf
distinctdf<-distinct(df, Binomial, .keep_all = TRUE)
distinctdforder<-arrange(distinctdf,Binomial)

#put current correct binomials on the tree, updated from t he birdtree
new<-cbind.data.frame(birdtreels$tip.label,distinctdf$Binomial,gsub(" ","_",distinctdf$Birdtree))
colnames(new)<-c("tiplabel","binomial","birdtree")
#View(new)
match(new$tiplabel,new$birdtree)#get ordered with correct binomials
new$binomialordered<-new$binomial[match(new$tiplabel,new$birdtree)]
str(new)
str(new$binomialordered)
str(birdtreels$tip.label)
birdtreels$tip.label<-new$binomialordered
birdtreels$tip.label<-as.character(birdtreels$tip.label)



#make comparative data object for caper
birdCDO<-comparative.data(phy = birdtreels,data = distinctdf, #avgdf[avgdf$Category!="Terrestrial",]
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped