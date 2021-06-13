library(ggalt)
library(ggrepel)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

df<-read.csv(file.choose())

#display.brewer.all(colorblindFriendly = TRUE)

#Scatterplot function
runit<-function(x,y,group){
  xl<-paste0("log(",x,")")
  yl<-paste0("log(",y,")")
  ggplot(avgdf, aes_string(x = xl, y = yl))+
    geom_point(aes_string(color =group))+
    geom_smooth(aes_string(col = group), method = "lm", se = F)+
    theme_bw() +   theme(legend.position = "none")
    #geom_encircle(aes(fill = group))
  #scale_color_brewer(palette = "Paired")
}

runitlogx<-function(x,y,group){
  xl<-paste0("log(",x,")")
  yl<-y
  ggplot(df, aes_string(x = xl, y = yl))+
    geom_point(aes_string(color =group))+
    geom_smooth(aes_string(col = group), method = "lm", se = F)+
    theme_bw()+
    theme(legend.position = "none")
  #geom_encircle(aes(fill = group))
  #scale_color_brewer(palette = "Paired")
}

#intra
ggarrange(
runit("Columella.length.mm","Columella.volume.mm3","waterbirds"),  
runit("FPtotalarea","Columella.volume.mm3","waterbirds"),
runit("FPtotalarea","Columella.length.mm","waterbirds"),
runit("FPtotalarea","TMtotalarea","waterbirds"),
runit("TMtotalarea","dis_coltip_TMcentroid","waterbirds"),
runit("TMtotalarea","Umbo_distancetoTMplane","waterbirds"),
runit("meanTMangle","TMtotalarea","waterbirds"),
runit("RWtotalarea","FPtotalarea","waterbirds"),
runit("Columella.length.mm","totalEClength","waterbirds"),
runit("TMtotalarea","Columella.volume.mm3","waterbirds"))

#head mass
ggarrange(
runit("Head.mass..g.","TMtotalarea","waterbirds"),
runit("Head.mass..g.","FPtotalarea","waterbirds"),
runit("Head.mass..g.","area_ratio","waterbirds"),
runit("Head.mass..g.","RWtotalarea","waterbirds"),
runit("Head.mass..g.","dis_coltip_TMcentroid","waterbirds"),
runit("Head.mass..g.","Umbo_distancetoTMplane","waterbirds"),
runit("Head.mass..g.","meanTMangle","waterbirds"),
runit("Head.mass..g.","totalEClength","waterbirds"),#EC length
runit("Head.mass..g.","Behind.TM","waterbirds"),#cranial air volume
runit("Head.mass..g.","Columella.volume.mm3","waterbirds"),#colsize
runit("Head.mass..g.","Columella.length.mm","waterbirds")#colsize
)

runit("Head.mass..g.","area_ratio","waterbirds"),


runit("Columella.length.mm","Columella.volume.mm3","waterbirds")
runit("FPtotalarea","Columella.volume.mm3","waterbirds")
runit("Columella.length.mm","totalEClength","waterbirds")

runit("Head.mass..g.","totalECDlength","waterbirds")


#scatterplots
runit("Head.mass..g.","TMtotalarea","superorder")
runit("Head.mass..g.","area_ratio","superorder")
runit("Head.mass..g.","rw_fp","superorder")
runit("Head.mass..g.","EC_CL","superorder")
runit("Head.mass..g.","`FP^(0.5)_collengthratio`","superorder")

runit("Head.mass..g.","TMtotalarea","waterbirds")
runit("Head.mass..g.","area_ratio","waterbirds")
runit("Head.mass..g.","rw_fp","waterbirds")
runit("Head.mass..g.","EC_CL","waterbirds")
runit("Head.mass..g.","`FP^(0.5)_collengthratio`","waterbirds")


runit("Head.mass..g.","totalEClength","superorder")#EC length
runit("Head.mass..g.","Columella.volume.mm3","superorder")#colsize

runit("Head.mass..g.","TMtotalarea","superorder")#TM area
runit("Head.mass..g.","FPtotalarea","waterbirds")#FP area
runit("Head.mass..g.","Behind.TM","superorder")#cranial air volume
RWtotalarea

df$RW_FP<-df$RWtotalarea/df$FPtotalarea
df$percentcart<-df$totalEClength/(df$totalEClength+df$Columella.length.mm)


runit("Head.mass..g.","Behind.TM","superorder")#cranial air volume

runitlogx("Head.mass..g.","percentcart","superorder")#cranial air volume
runitlogx("Head.mass..g.","area_ratio","superorder")#cranial air volume
runitlogx("Head.mass..g.","RW_FP","superorder")#cranial air volume


#INTRA-EAR relationships
runit("FPtotalarea","TMtotalarea","waterbirds")
runit("RWtotalarea","FPtotalarea","superorder")



runit("TMtotalarea","Columella.volume.mm3","waterbirds")
runit("Head.mass..g.","Columella.volume.mm3^(2/3)/TMtotalarea","Order")


runit("area_ratio","dis_coltip_TMcentroid","superorder")
runit("area_ratio","meanTMangle","superorder")



runit("TMtotalarea","coltip_distancetoTMplane","superorder")
runit("TMtotalarea","meanTMangle","superorder")
runit("FPtotalarea","RWtotalarea","superorder")

#columella length
runit("totalcollength","Columella.volume.mm3","superorder")
runit("FPtotalarea","Columella.volume.mm3","superorder")
runit("totalcollength","totalEClength","superorder")
runit("totalcollength","Columella.volume.mm3","superorder")
runit("Head.mass..g.","totalECDlength","superorder")



#audiograms

runit_limits<-function(x,y){
  xl<-paste0("log(",x,")")
  yl<-paste0("log(",y,")")
  ggplot(limits, aes_string(x = xl, y = yl))+
    geom_point()+
    geom_smooth(aes_string(method = "lm", se = F))+
    theme_bw()
}

runit_limits("ES","LowHzlimit","black")
runit_limits("HighHzlimit","LowHzlimit")

ggplot(limits, aes(x = log(HighHzlimit), y = log(LowHzlimit)))+
  geom_smooth()+
  geom_point()

e<-

birdCDO<-comparative.data(phy = birdtreels,data = avgdf[avgdf$waterbirds=="not Aequelornithes",],
                            names.col = Binomial, 
                            vcv = TRUE, na.omit = FALSE, 
                            warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped
