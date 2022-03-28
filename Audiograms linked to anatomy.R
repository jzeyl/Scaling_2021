library(ggrepel)
library(ggplot2)
library(ggpubr)
library(flextable)
library(officer)
library(dplyr)


# add avg Corvus and Phalacrocorax values----------------------------------------
phalacrocoraxavg<-avgdf[grepl('Phalacrocorax', avgdf$Binomial), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)

corvusavg<-avgdf[grepl('Corvus', avgdf$Binomial), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)
names(corvusavg)

cong_avg<-dplyr::bind_rows(avgdf,corvusavg,phalacrocoraxavg)
cong_avg$Binomial[128]<-"Corvus_cornix"
cong_avg$Binomial[129]<-"Phalacrocorax_carbo"
cong_avg<-cong_avg[-c(grep('Corvus_albus|Corvus_splendens', cong_avg$Binomial)), ]
cong_avg<-cong_avg[-c(grep('Phalacrocorax_capensis|Phalacrocorax_lucidus|Phalacrocorax_neglectus', cong_avg$Binomial)), ]
avgdf<-cong_avg

# load audiograms ---------------------------------------------------------

fig1<-read.csv(paste0(getwd(),"/audiograms.csv"), stringsAsFactors = FALSE)



# get the high and low Hz limits from a cutoff level ----------------------
splt<-split(fig1,fig1$Species)

#set cutoff for the high and low Hz limits (35 dB)
cutoff<-35

#create new matrix to populate with data and convert to data audiogramram
limits<-matrix(nrow=length(splt),ncol = 7)


for(i in seq_along(splt)){
  audiogram<-data.frame()#
  df_audiogram<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))#approx function to interpolate audiogram
  df_audiogram
  #df_audiogram$y is sound level (dB)
  #df_audiogram$x is frequency (Hz)
  besthz<-df_audiogram$x[df_audiogram$y==min(df_audiogram$y)]
  bestsensitivity<-df_audiogram$y[df_audiogram$y==min(df_audiogram$y)]

  #calcualte low Hz limit
  if(nrow(df_audiogram[df_audiogram$y>cutoff & df_audiogram$x <besthz,])==0){#if the audiogram does not go above cutoff value, get minimum frequency tested
    lowlimit<-min(df_audiogram$x)
  }

  else{
    lowflank<-df_audiogram[df_audiogram$y>cutoff & df_audiogram$x <besthz,]#get frequency where audiogram crosses cutoff value
    lowlimit<-max(lowflank$x)#lowhz limit
  }

  #calculate high Hz limit
  if(nrow(df_audiogram[df_audiogram$y>cutoff & df_audiogram$x >besthz,])==0){# #if the audiogram does not go above cutoff value, get max frequency tested
    highlimit<-max(df_audiogram$x)

  }

  else{
    highflank<-df_audiogram[df_audiogram$y>35 & df_audiogram$x >besthz,]#get frequency where audiogram crosses cutoff value
    highlimit<-min(highflank$x)#High hz limit
  }

  limits[i,1]<-lowlimit
  limits[i,2]<-highlimit
  limits[i,3]<-splt[[i]]$Species[1]
  limits[i,4]<-splt[[i]]$group[1]
  limits[i,5]<-splt[[i]]$Hz[1]
  limits[i,6]<-besthz
  limits[i,7]<-bestsensitivity
}
#View(limits)

#convert to dataframe and give column names
limits<-as.data.frame(limits)
colnames(limits)<-c("LowHzlimit","HighHzlimit","Species","supraorder","Hz", "besthz","bestsensitivity")
limits[,1]<-as.numeric(as.character(limits$LowHzlimit))
limits[,2]<-as.numeric(as.character(limits$HighHzlimit))
limits$Hz<-as.numeric(as.character(limits$Hz))
limits$besthz<-as.numeric(as.character(limits$besthz))
limits$bestsensitivity<-as.numeric(as.character(limits$bestsensitivity))



###################add species from scan data that correspond with audiograms###############
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

##################add anatomical data from df############

limits$TM<-avgdf$TMtotalarea[match(limits$binomial,avgdf$Binomial)]
limits$RW<-avgdf$RWtotalarea[match(limits$binomial,avgdf$Binomial)]
limits$FP<-avgdf$FPtotalarea[match(limits$binomial,avgdf$Binomial)]
limits$Air<-avgdf$Behind.TM[match(limits$binomial,avgdf$Binomial)]
limits$TM<-avgdf$TMtotalarea[match(limits$binomial,avgdf$Binomial)]
limits$HM<-avgdf$Head.mass..g.[match(limits$binomial,avgdf$Binomial)]
limits$BM<-avgdf$bodymass[match(limits$binomial,avgdf$Binomial)]
limits$ES<-avgdf$totalEClength[match(limits$binomial,avgdf$Binomial)]
limits$TM_FP<-avgdf$area_ratio[match(limits$binomial,avgdf$Binomial)]
limits$TMA<-avgdf$meanTMangle[match(limits$binomial,avgdf$Binomial)]
limits$COff<-avgdf$dis_coltip_TMcentroid[match(limits$binomial,avgdf$Binomial)]
limits$ECD<-avgdf$totalECDlength[match(limits$binomial,avgdf$Binomial)]
limits$CL<-avgdf$Columella.length.mm[match(limits$binomial,avgdf$Binomial)]
limits$CV<-avgdf$Columella.volume.mm3[match(limits$binomial,avgdf$Binomial)]
limits$UH<-avgdf$Umbo_distancetoTMplane[match(limits$binomial,avgdf$Binomial)]

limits$spp_aud<-avgdf$aud_spp[match(limits$binomial,avgdf$Binomial)]
limits$aud_rel<-avgdf$aud_rel[match(limits$binomial,avgdf$Binomial)]


#limits$pPC1<-speciesPCAvalues$PC1[match(limits$binomial,speciesPCAvalues$Binomial)]
#limits$rw_fp<-avgdf$rw_fp[match(limits$binomial,avgdf$Binomial)]
#limits$ec_cl<-avgdf$EC_CL[match(limits$binomial,avgdf$Binomial)]
#limits$rw_fp<-avgdf$rw_fp[match(limits$binomial,avgdf$Binomial)]
#limits$col2.3_TM<-avgdf$col2.3_TM[match(limits$binomial,avgdf$Binomial)]

# cor plots
aud_data<- limits[,c("LowHzlimit","HighHzlimit","besthz","bestsensitivity")]

audlog<-aud_data %>% mutate_at(vars(c("LowHzlimit","HighHzlimit","besthz")),log)

library(PerformanceAnalytics)
chart.Correlation(audlog, histogram = TRUE, method = "pearson")

# p-values from correlation tests
cor.test(aud_data$LowHzlimit, aud_data$HighHzlimit)
cor.test(aud_data$LowHzlimit, aud_data$besthz)
cor.test(aud_data$LowHzlimit, aud_data$bestsensitivity)
cor.test(aud_data$HighHzlimit, aud_data$bestsensitivity)


#summary statistics of audiograms
mean(limits$HighHzlimit)
sd(limits$HighHzlimit)/sqrt(length(limits$HighHzlimit))

mean(limits$LowHzlimit)
sd(limits$LowHzlimit)/sqrt(length(limits$LowHzlimit))

mean(limits$besthz)
sd(limits$besthz)/sqrt(length(limits$besthz))

mean(limits$bestsensitivity)
sd(limits$bestsensitivity)/sqrt(length(limits$bestsensitivity))


###############PGLS MODELS BEST SENSITIVITY####################
modellist_bs<-c(
  "bestsensitivity~log(Air)",
  "bestsensitivity~log(ES)",
  "bestsensitivity~log(TMA)",
  "bestsensitivity~log(UH)",
  "bestsensitivity~log(COff)",
  "bestsensitivity~log(TM_FP)",
  "bestsensitivity~log(ECD)",
  "bestsensitivity~log(TM)",
  "bestsensitivity~log(FP)",
  "bestsensitivity~log(RW)",
  "bestsensitivity~log(HM)",
  "bestsensitivity~log(BM)",
  "bestsensitivity~log(CL)",
  "bestsensitivity~log(CV)")

#####################PGLS MODELS LOW HZ LIMIT#################
modellist_lf<-c(
  "log(LowHzlimit)~log(Air)",
  "log(LowHzlimit)~log(ES)",
  "log(LowHzlimit)~log(TMA)",
  "log(LowHzlimit)~log(UH)",
  "log(LowHzlimit)~log(COff)",
  "log(LowHzlimit)~log(TM_FP)",
  "log(LowHzlimit)~log(ECD)",
  "log(LowHzlimit)~log(TM)",
  "log(LowHzlimit)~log(FP)",
  "log(LowHzlimit)~log(RW)",
  "log(LowHzlimit)~log(HM)",
  "log(LowHzlimit)~log(BM)",
  "log(LowHzlimit)~log(CL)",
  "log(LowHzlimit)~log(CV)")

###################PGLS MODELS HIGH LIMIT##############
modellist_hf<-c(
  "log(HighHzlimit)~log(Air)",
  "log(HighHzlimit)~log(ES)",
  "log(HighHzlimit)~log(TMA)",
  "log(HighHzlimit)~log(UH)",
  "log(HighHzlimit)~log(COff)",
  "log(HighHzlimit)~log(TM_FP)",
  "log(HighHzlimit)~log(ECD)",
  "log(HighHzlimit)~log(TM)",
  "log(HighHzlimit)~log(FP)",
  "log(HighHzlimit)~log(RW)",
  "log(HighHzlimit)~log(HM)",
  "log(HighHzlimit)~log(BM)",
  "log(HighHzlimit)~log(CL)",
  "log(HighHzlimit)~log(CV)")

##################PGLS MODELS besthz###############
modellist_bh<-c(
  "log(besthz)~log(Air)",
  "log(besthz)~log(ES)",
  "log(besthz)~log(TMA)",
  "log(besthz)~log(UH)",
  "log(besthz)~log(COff)",
  "log(besthz)~log(TM_FP)",
  "log(besthz)~log(ECD)",
  "log(besthz)~log(TM)",
  "log(besthz)~log(FP)",
  "log(besthz)~log(RW)",
  "log(besthz)~log(HM)",
  "log(besthz)~log(BM)",
  "log(besthz)~log(CL)",
  "log(besthz)~log(CV)")

categorylist_lf<-c("Stiffness",
                   "Stiffness",
                   "Impedance match",
                   "Impedance match",
                   "Impedance match",
                   "Impedance match",
                   "Auditory endorgan length",
                   "Input/output areas",
                   "Input/output areas",
                   "Input/output areas",
                   "Animal size",
                   "Animal size",
                   "Columella size",
                   "Columella size")

categorylist_bs<-categorylist_lf
categorylist_bh<-categorylist_lf
categorylist_hf<-categorylist_lf

#only select the rows for which anatomical data is available for the corresponding audiograms
limitsanat<-limits[which(!is.na(limits$binomial)),]

birdtreels$tip.label[14]<-"Corvus_cornix"
birdtreels$tip.label[51]<-"Phalacrocorax_carbo"
#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = limitsanat,#[avgdf$Category!="Terrestrial",]
                          names.col =binomial,
                          vcv = TRUE, na.omit = F,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped


###########best sensitivity#################
source("pgls_audiogram_bs.R")

#visualize the table better using the flextable package
flexall<-flextable(audiogrampgls_bs) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall
write.csv(audiogrampgls_bs,"audiogrampgls_bs.csv")
print(toprint,target = "audiogrampgls_bs.docx")

source("pgls_audiogram_lf.R")

#visualize the table better using the flextable package
flexall<-flextable(audiogrampgls_lf) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

write.csv(audiogrampgls_lf,"audiogrampgls_lf.csv")
print(toprint,target = "audiogrampgls_lf.docx")

source("pgls_audiogram_hf.R")

#visualize the table better using the flextable package
flexall<-flextable(audiogrampgls_hf) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

write.csv(audiogrampgls_hf,"audiogrampgls_hf.csv")
print(toprint,target = "audiogrampgls_hf.docx")

source("pgls_audiogram_bh.R")

#visualize the table better using the flextable package
flexall<-flextable(audiogrampgls_bh) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

write.csv(audiogrampgls_bh,"audiogrampgls_bh.csv")
print(toprint,target = "audiogrampgls_bh.docx")


#######plotting metrics on audiogram########
aas<-function(d){
  set<-limits[d,]
  bestsensitivity<-set$bestsensitivity
  bestHz<-set$besthz
  highHz<-set$HighHzlimit
  lowHz<-set$LowHzlimit
  ggplot(fig1[fig1$Species==set$Species,], aes(x = Hz, y = Threshold, factor = Species))+
    scale_x_log10()+
    #geom_vline(xintercept = lowHz, col = "grey", size = 2)+

    #Hz metrics
    geom_segment(aes(x = lowHz, y = -Inf, xend = lowHz, yend = 35), col = "grey", size = 2)+
    geom_segment(aes(x = bestHz, y = -Inf, xend = bestHz, yend = bestsensitivity), col = "grey", size = 2)+
    geom_segment(aes(x = highHz, y = -Inf, xend = highHz, yend = 35), col = "grey", size = 2)+
    geom_line(aes(x = Hz, y = Threshold), size = 2)+

    #geom    geom_line(aes(x = Hz, y = Threshold), size = 2)+
    scale_color_brewer(palette = "Set1")+
    geom_hline(yintercept = bestsensitivity)+
    geom_hline(yintercept = 35, col = "black")+
    theme_bw()+
    #theme(legend.position = "none")+
    coord_cartesian(clip = "off")+
    ylab("Threshold (dB SPL)")+
    xlab("Frequency (Hz)")+
    annotation_logticks(sides = "b", outside = TRUE)+
    ylim(c(0,80))+
    annotate("text",x = 50, y = 35+3, label = "Cutoff level (dB)")+
    annotate("text",x = 50, y = bestsensitivity+3, label = "Best sensitivity (dB)")+
    annotate("text",x = bestHz, y = 5, label = "Best frequency")+
    annotate("text",x = lowHz, y = 5, label = "Low frequency limit")+
    annotate("text",x = highHz, y = 5, label = "High frequency limit")

}
aas(16)+theme()

aas(16)+ xlim(c(0,50000))


#CVs
#CV high Hz
mean(limits$HighHzlimit)/sd(limits$HighHzlimit)
mean(limits$LowHzlimit)/sd(limits$LowHzlimit)
