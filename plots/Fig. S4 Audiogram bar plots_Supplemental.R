#NOTE: prior to using this script, create the birdCDO
#dataframe from the 'Set up dat_scl.R' file.

library(ggrepel)
library(ggplot2)
library(ggpubr)
library(flextable)
library(officer)
library(dplyr)
library(PerformanceAnalytics)

#NOTE: a portion of this script is to be re-run to
#save hearing limits first at 35 and then 60 dB
#When first prompted (line 219), return to the top of script
#and run lines 20-31 to save data for cutoff level and update the cutoff

#####SAVE limits 35, modify cutoff to 60 dB, and re-run###
#
limits35<-limits
limitslong35<-limits35[which(!is.na(limits35$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))

###
##
#now re-run the analysis to get the limits at 60 dB, changing the cutoff
rm(list = setdiff(ls(),c("limits35","limitslong35")))

#set cutoff here as 35 or 60 dB
cutoff<- 60
######################################333


####create averaged values for instances where multiple species match a congener with audiogram####
phalacrocoraxavg<-avgdf[grepl('Phalacrocorax', avgdf$Binomial), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)

corvusavg<-avgdf[grepl('Corvus', avgdf$Binomial), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)
names(corvusavg)

# add averaged Corvus and Phalacrocorax values----------------------------------------
cong_avg<-dplyr::bind_rows(avgdf,corvusavg,phalacrocoraxavg)
cong_avg$Binomial[128]<-"Corvus_cornix"
cong_avg$Binomial[129]<-"Phalacrocorax_carbo"

#remove single-species
cong_avg<-cong_avg[-c(grep('Corvus_albus|Corvus_splendens', cong_avg$Binomial)), ]
cong_avg<-cong_avg[-c(grep('Phalacrocorax_capensis|Phalacrocorax_lucidus|Phalacrocorax_neglectus', cong_avg$Binomial)), ]
avgdf<-cong_avg


# load audiograms ---------------------------------------------------------
fig1<-read.csv(paste0(getwd(),"/audiograms.csv"), stringsAsFactors = FALSE)

#check how many reach cutoff
#species not reaching lower Hz limit:
#minsubset<-fig1 %>% group_by(Species) %>% filter(Hz == min(Hz))
#species not reaching upper Hz limit
#maxsubset<-fig1 %>% group_by(Species) %>% filter(Hz == max(Hz))


#option to include only ones with anatomical data
minsubset<-fig1 %>% group_by(Species) %>% filter(Hz == min(Hz), havescan == "Have scan data for species")
maxsubset<-fig1 %>% group_by(Species) %>% filter(Hz == max(Hz), havescan == "Have scan data for species")

#create columns testing whether min & max tested frequencies are above cutoffs
minsubset$reachcutoff<-ifelse(minsubset$Threshold <cutoff, "under cutoff","over cutoff")
minsubset$Species[minsubset$reachcutoff=="under cutoff"]

maxsubset$reachcutoff<-ifelse(maxsubset$Threshold <cutoff, "under cutoff","over cutoff")
maxsubset$Species[maxsubset$reachcutoff=="under cutoff"]

table(minsubset$reachcutoff)#count number
table(maxsubset$reachcutoff)#count number


# get the high and low Hz limits from a cutoff level ----------------------
splt<-split(fig1,fig1$Species)

#create new matrix to populate with data and convert to data audiogramram
limits<-matrix(nrow=length(splt),ncol = 9)

for(i in seq_along(splt)){
  audiogram<-data.frame()#
  df_audiogram<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))#approx function to interpolate audiogram
  df_audiogram
  #NOTE:df_audiogram$y is sound level (dB)
  #and df_audiogram$x is frequency (Hz)
  besthz<-df_audiogram$x[df_audiogram$y==min(df_audiogram$y)]
  bestsensitivity<-df_audiogram$y[df_audiogram$y==min(df_audiogram$y)]

  #calculate low Hz limit by segmenting audiogram into lowerhalf and upper half
  lowflank<-df_audiogram[df_audiogram$y>cutoff & df_audiogram$x <besthz,]#get frequency where audiogram crosses cutoff value
  highflank<-df_audiogram[df_audiogram$y>cutoff & df_audiogram$x >besthz,]#get frequency where audiogram crosses cutoff value

  #if the audiogram does not go above cutoff value, value is NA
  if(nrow(df_audiogram[df_audiogram$y>cutoff & df_audiogram$x <besthz,])==0){
    #lowlimit<-min(df_audiogram$x) #<--other option here to get minimum frequency tested
    lowlimit<-NA
    lowlimitdB<-df_audiogram$y[df_audiogram$x==min(df_audiogram$x)] #get dB at which min frequ occurs

  }
  #when audiogram surpasses cutoff
  else{
    lowlimit<-max(lowflank$x)#lowhz limit
    lowlimitdB<-cutoff
  }

  #high frequency
  #if the audiogram does not go above cutoff value, value is NA
  if(nrow(df_audiogram[df_audiogram$y>cutoff & df_audiogram$x >besthz,])==0){# #if the audiogram does not go above cutoff value
    #highlimit<-max(df_audiogram$x)
    highlimit<-NA
    highlimitdB<-df_audiogram$y[df_audiogram$x==max(df_audiogram$x)] #get dB at which min frequ occurs

  }

  else{
    highlimit<-min(highflank$x)#High hz limit
    highlimitdB<-cutoff
  }

  limits[i,1]<-lowlimit
  limits[i,2]<-highlimit
  limits[i,3]<-splt[[i]]$Species[1]
  limits[i,4]<-splt[[i]]$group[1]
  limits[i,5]<-splt[[i]]$Hz[1]
  limits[i,6]<-besthz
  limits[i,7]<-bestsensitivity
  limits[i,8]<-lowlimitdB
  limits[i,9]<-highlimitdB

}
#View(limits)

#convert to dataframe and give column names
limits<-as.data.frame(limits)
colnames(limits)<-c("LowHzlimit","HighHzlimit","Species","supraorder","Hz",
                    "besthz","bestsensitivity","reallowdBlimit","realhighdBlimit")
limits[,1]<-as.numeric(limits$LowHzlimit)
limits[,2]<-as.numeric(limits$HighHzlimit)
limits$Hz<-as.numeric(limits$Hz)
limits$besthz<-as.numeric(limits$besthz)
limits$bestsensitivity<-as.numeric(limits$bestsensitivity)
limits$reallowdBlimit<-as.numeric(limits$reallowdBlimit)
limits$realhighdBlimit<-as.numeric(limits$realhighdBlimit)

###################add the anatomical data with audiograms###############
limits$binomial<-NA
limits$binomial[limits$Species=="Barn owl"]<-"Tyto_alba"
limits$binomial[limits$Species=="American kestrel"]<-"Falco_rupicolus" #
limits$binomial[limits$Species=="Budgerigar"]<-"Melopsittacus_undulatus"
limits$binomial[limits$Species=="Canary"]<-"Serinus_canaria"
limits$binomial[limits$Species=="Chicken"]<-"Gallus_domesticus"
limits$binomial[limits$Species=="Cockatiel"]<-"Nymphicus_hollandicus"
limits$binomial[limits$Species=="Eurasian eagle owl"]<-"Bubo_africanus"
limits$binomial[limits$Species=="Eurasian sparrowhawk"]<-"Accipiter_melanoleucus"
limits$binomial[limits$Species=="Great cormorant"]<-"Phalacrocorax_carbo"
limits$binomial[limits$Species=="Hooded crow"]<-"Corvus_cornix"
limits$binomial[limits$Species=="Indian peafowl"]<-"Pavo_muticus"
limits$binomial[limits$Species=="Mallard duck"]<-"Anas_georgica_georgica"
limits$binomial[limits$Species=="Rock dove"]<-"Columba_livia"#
limits$binomial[limits$Species=="Zebra finch"]<-"Taeniopygia_guttata"

##################add anatomical data from anatomy df############
limits$TM<-avgdf$TM[match(limits$binomial,avgdf$Binomial)]
limits$RW<-avgdf$RW[match(limits$binomial,avgdf$Binomial)]
limits$FP<-avgdf$FP[match(limits$binomial,avgdf$Binomial)]
limits$Air<-avgdf$Air[match(limits$binomial,avgdf$Binomial)]
limits$TM<-avgdf$TM[match(limits$binomial,avgdf$Binomial)]
limits$HM<-avgdf$HM[match(limits$binomial,avgdf$Binomial)]
limits$BM<-avgdf$BM_lit[match(limits$binomial,avgdf$Binomial)]
limits$ES<-avgdf$ES[match(limits$binomial,avgdf$Binomial)]
limits$TM_FP<-avgdf$TM_FP[match(limits$binomial,avgdf$Binomial)]
limits$TMA<-avgdf$TMA[match(limits$binomial,avgdf$Binomial)]
limits$COffset<-avgdf$COffset[match(limits$binomial,avgdf$Binomial)]
limits$ECD<-avgdf$ECD[match(limits$binomial,avgdf$Binomial)]
limits$CL<-avgdf$CL[match(limits$binomial,avgdf$Binomial)]
limits$CV<-avgdf$CV[match(limits$binomial,avgdf$Binomial)]
limits$UH<-avgdf$UH[match(limits$binomial,avgdf$Binomial)]
limits$spp_aud<-avgdf$aud_spp[match(limits$binomial,avgdf$Binomial)]
limits$aud_rel<-avgdf$aud_rel[match(limits$binomial,avgdf$Binomial)]

#classification for the two species withaverage
limits$aud_rel[limits$binomial=="Corvus_cornix"]<-"Congener"
limits$spp_aud[limits$binomial=="Corvus_cornix"]<-"Corvus_cornix"
limits$aud_rel[limits$binomial=="Phalacrocorax_carbo"]<-"Congener"
limits$spp_aud[limits$binomial=="Phalacrocorax_carbo"]<-"Phalacrocorax_carbo"
limits$aud_rel[limits$binomial=="Corvus_cornix"]<-"Congener"


########The audiogram metrics have now been computed.
########the 'pgls_resid re headmass.R' file can now be used to run the pgls modesl###
########below the models are run without adjusting for head size####


library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)
library(ggrepel)

##plot graph after creating the 'limits' dataframe (see 'Audiograms linked to anatomy' file)

#split into audiograms by species, only including ones with scan data
splt<-fig1 %>% #filter(.,fig1$havescan=="Have scan data for species")%>%
  split(.$Species)

df_audiogrm_lst<-list()

for(i in seq_along(splt)){
  df_audiogrm_lst[[i]]<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))
  df_audiogrm_lst[[i]]$Species<- rep(splt[[i]]$Species,length.out = 5000)
  }

#####SAVE limits 35, modify cutoff to 60 dB, and re-run###
#return to top of script to save at 35 dB then adjust cutoff

#save data at 60 dB cutoff
limits60<-limits
limitslong60<-limits60[which(!is.na(limits60$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))

#bind audiograms together
bound<-do.call(rbind.data.frame,df_audiogrm_lst)

#we will append data from the 'limits' df using species as a key
bound$LowHzlimit<-NA
bound$HighHzlimit<-NA
bound$besthz<-NA
bound$bestsensitivity<-NA

#add best hz to interpolated datset for sorting
bound$besthz<-limits$besthz[match(bound$Species,limits$Species)]
bound$bestsensitivity<-limits$bestsensitivity[match(bound$Species,limits$Species)]
#bound$HighHzlimit<-limits$HighHzlimit[match(bound$Species,limits$Species)]
#bound$LowHzlimit<-limits$LowHzlimit[match(bound$Species,limits$Species)]
bound$Hz<-bound$x #give appropriate naming for x
#bound2$Hz<-bound2$x #give appropriate naming for x
bound$Species<-as.factor(bound$Species)
bound$`Threshold (dB)`<-bound$y

#bound$Species = with(bound, reorder(Species, besthz, median))

#Add new rows to end of dataframe with audiogram metrics (best Hz, etc.)
bound2<-bound
levels(bound2$Species)<-c(levels(bound2$Species),"High freq. limit","Best freq.","Low freq. limit")
##bound2[95001,"Species"]<-"Low freq. limit"#add 'low hz as species name, placeholder for bethz
# bound2[95002,"Species"]<-"Best freq."
#bound2[95003,"Species"]<-"High freq. limit"
bound$Species<-as.character(bound$Species)
bound$besthz<-as.numeric(bound$besthz)
bound2$Species = with(bound2, reorder(Species, besthz, median))

library(forcats)
range<-ggplot(bound, aes(x = Hz,
                    y =  forcats::fct_rev(reorder(
                      Species,Species))))+
geom_path(data = bound,aes(col = `Threshold (dB)`), size = 2)+
  geom_point(data = limits, aes(x = besthz, y = Species), shape = 21, size = 2, colour = "black", fill = "white")+
  geom_point(data = filter(limitslong60,limit!="besthz"), aes(x = Hz, y = Species),shape = 21, size = 2, colour = "black", fill = "black")+
  geom_point(data = filter(limitslong35,limit!="besthz"), aes(x = Hz, y = Species),shape = 21, size = 2, colour = "black", fill = "grey")+
  scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  #coord_cartesian(clip = "off", ylim = c(1,22))+
  annotation_logticks(sides = "b", outside = TRUE, colour = "black")+
  ylab("")+
  xlab("Frequency(Hz)")+
  #geom_hline(yintercept = 3.5)+
  theme(axis.title.y = element_text(angle= 0, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle= 0, vjust = -2.5, hjust=0.5))
range



bestsens<-ggplot(limits, aes(x = bestsensitivity, y = 0))+
  geom_boxplot(width = 0.25)+
  geom_point(aes(y = 0,label = Species))+
  geom_text_repel(aes(y = 0,label = Species),
                  #direction = "x",
                  size = 3,
                  min.segment.length = 0,
                  segment.size      = 1)+
                  #nudge_y = 1)+
                  #box.padding = unit(0.35, "lines"),
                #point.padding = unit(1, "lines"))+
  #coord_flip()+
  #geom_text(aes(x = 0, label = Species))+
  theme_classic()+
  ylim(c(-0.5,0.5))+
  xlab("Best sensitivity(dB SPL)")+
  ylab("")+
  theme(#axis.title.y = element_text(angle= 0, vjust = 0.5, hjust=1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())
bestsens

range/bestsens+
  plot_layout(heights = c(2,1))+
  plot_annotation(tag_levels = "A")

ggsave(file=paste0(choose.dir(),"/supplemental_bar S4 oct 24.png"))

