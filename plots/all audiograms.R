library(ggrepel)
library(ggplot2)
library(ggpubr)
library(flextable)
library(officer)
library(dplyr)
library(PerformanceAnalytics)

cutoff<- 60#set cutoff here as 35 or 60 dB


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

#set cutoff for the high and low Hz limits (35 or 60 dB)


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
    lowlimit<-min(df_audiogram$x) #<--other option here to get minimum frequency tested
    #lowlimit<-NA
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
    highlimit<-max(df_audiogram$x)
    #highlimit<-NA
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

#limits$binomial[limits$Species=="Orange fronted conure"]<-"Aratinga_canicularis"
#limits$binomial[limits$Species=="Lesser scaup"]<-"Aythya_affinis"
#limits$binomial[limits$Species=="Japanese quail"]<-"Coturnix_japonica"
#limits$binomial[limits$Species=="Blue Jay"]<-"Cyanocitta_cristata"
#limits$binomial[limits$Species=="Kea parrot"]<-"Nestor_notabilis"


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





#creat the 'bound' datframe from the 'Audiogram bar plots...' file
library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)
library(ggrepel)

##plot graph after creating the 'limits' dataframe (see 'Audiograms linked to anatomy' file)

#split into audiograms by species, only including ones with scan data
splt<-fig1 %>% filter(.,fig1$havescan=="Have scan data for species")%>%
  split(.$Species)

df_audiogrm_lst<-list()

for(i in seq_along(splt)){
  df_audiogrm_lst[[i]]<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))
  df_audiogrm_lst[[i]]$Species<- rep(splt[[i]]$Species,length.out = 5000)
}

#single y audiogram example
#ggplot(df_audiogram, aes(x = x, y = y), col = y)+
#  geom_point(aes(y = 60, col = y))+
#  scale_color_viridis()

#bind audiograms together
bound<-do.call(rbind.data.frame,df_audiogrm_lst)

#ggplot(bound, aes(x = x, y = Species), col = y)+
#  scale_x_log10()+
#  geom_point()+
#  geom_point(aes(col = y))+
#  scale_color_viridis()

#convert "limits" dataframe to long format

limitslong<-limits[which(!is.na(limits$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))


#we will append data from the 'limits' df using species as a key
bound$LowHzlimit<-NA
bound$HighHzlimit<-NA
bound$besthz<-NA
bound$bestsensitivity<-NA

#add best hz to interpolated datset for sorting
bound$besthz<-limits$besthz[match(bound$Species,limits$Species)]
bound$bestsensitivity<-limits$bestsensitivity[match(bound$Species,limits$Species)]
bound$HighHzlimit<-limits$HighHzlimit[match(bound$Species,limits$Species)]
bound$LowHzlimit<-limits$LowHzlimit[match(bound$Species,limits$Species)]

#
bound$Species<-as.factor(bound$Species)
#reorder 'bound' df by besthz
bound$Hz<-bound$x #give appropriate naming for x

#re-do with limits = 35
limits35<-limits
limitslong35<-limits35[which(!is.na(limits35$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))

#now re-run the analysis to get the limits at 60 dB, changing the cutoff
rm(list = setdiff(ls(),c("limits35","limitslong35")))

#re-do with limits = 60
limits60<-limits
limitslong60<-limits60[which(!is.na(limits60$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))


#limits long is 60

ggplot(bound)+
  geom_line(aes(x = x, y = y, group = Species), col = "grey")+
scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  geom_hline(yintercept = c(60,35))+
  geom_point(data = filter(limitslong,between(reallowdBlimit,35,59),limit=="LowHzlimit"), aes(x = Hz, y = reallowdBlimit),col = "red", size = 2, shape = 0)+
  geom_point(data = filter(limitslong,reallowdBlimit<35,limit=="LowHzlimit"), aes(x = Hz, y = reallowdBlimit),col = "red", size = 2, shape = 2)+
  ylab("Sound pressure level (dB SPL)")+
  xlab("Frequency (Hz)")+

  geom_point(data = filter(limitslong,between(realhighdBlimit,35,59),limit=="HighHzlimit"), aes(x = Hz, y = realhighdBlimit),col = "red", size = 2, shape = 0)+
  geom_point(data = filter(limitslong,realhighdBlimit<35,limit=="HighHzlimit"), aes(x = Hz, y = realhighdBlimit),col = "red", size = 2, shape = 2)+

  geom_point(data = filter(limitslong,reallowdBlimit==60,limit=="LowHzlimit"), aes(x = Hz, y = reallowdBlimit),col = "black", size = 2, shape = 21)+
  geom_point(data = filter(limitslong,realhighdBlimit==60,limit=="HighHzlimit"), aes(x = Hz, y = realhighdBlimit),col = "black", size = 2, shape = 21)+
  geom_point(data = filter(limitslong35,reallowdBlimit==35,limit=="LowHzlimit"), aes(x = Hz, y = reallowdBlimit),col = "black", size = 2, shape = 21)+
  geom_point(data = filter(limitslong35,realhighdBlimit==35,limit=="HighHzlimit"), aes(x = Hz, y = realhighdBlimit),col = "black", size = 2, shape = 21)

ggsave("audiogram.svg")

ifelse(x < reallowdBlimit<35,'red','green')
