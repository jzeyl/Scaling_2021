#run up until line 185, proceed to pgls_resids re headmass
#otherwise unadjusted for head mass proceed past line 185

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


########The audiogram metrics have now been computed.
########the 'pgls_resid re headmass.R' file can now be used to run the pgls modesl###
########below the models are run without adjusting for head size####


#########correlation betweenaudiogram measures#########3333
# correlation plots - summary stats for audiogram metrics
aud_data<- limits[,c("LowHzlimit","HighHzlimit","besthz","bestsensitivity")]
audlog<-aud_data %>% mutate_at(vars(c("LowHzlimit","HighHzlimit","besthz")),log)


chart.Correlation(audlog, histogram = TRUE, method = "pearson")

# p-values from correlation tests
cor.test(aud_data$LowHzlimit, aud_data$HighHzlimit)
cor.test(aud_data$LowHzlimit, aud_data$besthz)
cor.test(aud_data$LowHzlimit, aud_data$bestsensitivity)
cor.test(aud_data$HighHzlimit, aud_data$bestsensitivity)


#summary statistics of audiograms
meanhigh<-mean(limits$HighHzlimit, na.rm = TRUE)
se_high<-sd(limits$HighHzlimit, na.rm = TRUE)/sqrt(length(limits$HighHzlimit))

meanlow<-mean(limits$LowHzlimit, na.rm = TRUE)
se_low<-sd(limits$LowHzlimit, na.rm = TRUE)/sqrt(length(limits$LowHzlimit))

meanbesthz<-mean(limits$besthz)
se_besthz<-sd(limits$besthz)/sqrt(length(limits$besthz))

meanbestsens<-mean(limits$bestsensitivity)
se_bestsens<-sd(limits$bestsensitivity)/sqrt(length(limits$bestsensitivity))

###############____########
##################models unadjusted for ead mass #############
  #PGLS MODELS BEST SENSITIVITY
modellist_bs<-c(
  "bestsensitivity~log(Air)",
  "bestsensitivity~log(ES)",
  "bestsensitivity~log(TMA)",
  "bestsensitivity~log(UH)",
  "bestsensitivity~log(COffset)",
  "bestsensitivity~log(TM_FP)",
  "bestsensitivity~log(ECD)",
  "bestsensitivity~log(TM)",
  "bestsensitivity~log(FP)",
  "bestsensitivity~log(RW)",
  "bestsensitivity~log(HM)",
  "bestsensitivity~log(CL)",
  "bestsensitivity~log(CV)")

#####################PGLS MODELS LOW HZ LIMIT#################
modellist_lf<-c(
  "log(LowHzlimit)~log(Air)",
  "log(LowHzlimit)~log(ES)",
  "log(LowHzlimit)~log(TMA)",
  "log(LowHzlimit)~log(UH)",
  "log(LowHzlimit)~log(COffset)",
  "log(LowHzlimit)~log(TM_FP)",
  "log(LowHzlimit)~log(ECD)",
  "log(LowHzlimit)~log(TM)",
  "log(LowHzlimit)~log(FP)",
  "log(LowHzlimit)~log(RW)",
  "log(LowHzlimit)~log(HM)",
  "log(LowHzlimit)~log(CL)",
  "log(LowHzlimit)~log(CV)")

###################PGLS MODELS HIGH LIMIT##############
modellist_hf<-c(
  "log(HighHzlimit)~log(Air)",
  "log(HighHzlimit)~log(ES)",
  "log(HighHzlimit)~log(TMA)",
  "log(HighHzlimit)~log(UH)",
  "log(HighHzlimit)~log(COffset)",
  "log(HighHzlimit)~log(TM_FP)",
  "log(HighHzlimit)~log(ECD)",
  "log(HighHzlimit)~log(TM)",
  "log(HighHzlimit)~log(FP)",
  "log(HighHzlimit)~log(RW)",
  "log(HighHzlimit)~log(HM)",
  "log(HighHzlimit)~log(CL)",
  "log(HighHzlimit)~log(CV)")

##################PGLS MODELS besthz###############
modellist_bh<-c(
  "log(besthz)~log(Air)",
  "log(besthz)~log(ES)",
  "log(besthz)~log(TMA)",
  "log(besthz)~log(UH)",
  "log(besthz)~log(COffset)",
  "log(besthz)~log(TM_FP)",
  "log(besthz)~log(ECD)",
  "log(besthz)~log(TM)",
  "log(besthz)~log(FP)",
  "log(besthz)~log(RW)",
  "log(besthz)~log(HM)",
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
                   "Head size",
                   "Columella size",
                   "Columella size")

categorylist_bs<-categorylist_lf#category list is the same, regardless of which metric we are predicting
categorylist_bh<-categorylist_lf
categorylist_hf<-categorylist_lf

#only select the rows for which anatomical data is available for the corresponding audiograms
limitsanat<-limits[which(!is.na(limits$binomial)),]

#rename phylogeny tips to matching with the species for which audiogram is available
birdtreels$tip.label[14]<-"Corvus_cornix" #renamed from Corvus_albus
birdtreels$tip.label[51]<-"Phalacrocorax_carbo" #rename "phalacrocorax_lucidus"


#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = limitsanat,#[avgdf$Category!="Terrestrial",]
                          names.col =binomial,
                          vcv = TRUE, na.omit = F,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped


###########best sensitivity#################
source("pgls_audiogram_bs.R")

#results table is saved as 'audiogrampgls-bs'

#visualize the table better using the flextable package
#flexall<-flextable(audiogrampgls_bs) %>% add_header_lines(
#  values = "Table X. Models for selection") %>%
#  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
#  autofit()
#flexall

#define pgls model diagnostics function
#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_audio<-lapply(pgls_models_list_bs, plot)
plots_audio


###print results
#write.csv(audiogrampgls_bs,"audiogrampgls_bs.csv")
#print(toprint,target = "audiogrampgls_bs.docx")


# low frequency limit (Hz) ------------------------------------------------

source("pgls_audiogram_lf.R")

#results table is saved as 'audiogrampgls-lf'
#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_audio<-lapply(pgls_models_list_lf, plot)
plots_audio

#visualize the table better using the flextable package
#flexall<-flextable(audiogrampgls_lf) %>% add_header_lines(
# values = "Table X. Models for selection") %>%
#  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
#  autofit()
#flexall


#print to file
#write.csv(audiogrampgls_lf,"audiogrampgls_lf.csv")
#print(toprint,target = "audiogrampgls_lf.docx")


#high frequency limit (Hz) ------------------------------------------------

source("pgls_audiogram_hf.R")

#results table is saved as 'audiogrampgls-hf'
#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_audio<-lapply(pgls_models_list_hf, plot)
plots_audio

#results table is saved as 'audiogrampgls-hf'


#visualize the table better using the flextable package
#flexall<-flextable(audiogrampgls_hf) %>% add_header_lines(
#  values = "Table X. Models for selection") %>%
#  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
#  autofit()
#flexall


#diagnostics
#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_audio<-lapply(pgls_models_list_hf, plot)
plots_audio
#print to file
#write.csv(audiogrampgls_hf,"audiogrampgls_hf.csv")
#print(toprint,target = "audiogrampgls_hf.docx")


## best frequency ----------------------------------------------------------

source("pgls_audiogram_bh.R")

#results table is saved as 'audiogrampgls-bh'
#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_audio<-lapply(pgls_models_list_bs, plot)
plots_audio

### combine results from regressions for each measure into a single datagrame
audio_pgls_results<-bind_rows(audiogrampgls_bh,
                              audiogrampgls_bs,
                              audiogrampgls_lf,
                              audiogrampgls_hf)
audio_pgls_results$CI95_low<-audio_pgls_results$Estimate-audio_pgls_results$`Std. Error`*1.96
audio_pgls_results$CI95_high<-audio_pgls_results$Estimate+audio_pgls_results$`Std. Error`*1.96

#combine estimate +/- 95 CI into one cell
audio_pgls_results$pglsslope<-paste0(audio_pgls_results$Estimate," (",
                        format(round(audio_pgls_results$CI95_low, 3), nsmall = 3),
                        ",",
                        format(round(audio_pgls_results$CI95_high, 3), nsmall = 3),
                        ")")


#split up model column
spltmodel<-strsplit(audio_pgls_results$Model,"~")
audio_pgls_results$`Audiogram metric`<-unlist(lapply(spltmodel, `[[`, 1))
audio_pgls_results$anattraitx<-unlist(lapply(spltmodel, `[[`, 2))

#only keep significant relationships
audio_pgls_results<-audio_pgls_results %>%
  select(`Audiogram metric`,
          category,
          Coefficients,
          pglsslope,
          Adj_Rsquared,
          P.val,
          Lambda)%>%
  filter(Coefficients!="(Intercept)" &
           P.val <0.05)

#arrange by audiogram metric, category, and R2
audio_pgls_results <-audio_pgls_results %>%
  arrange(factor(audio_pgls_results$`Audiogram metric`),
          factor(category,levels = c("Impedance match",
                                     "Stiffness",
                                     "Input/output areas",
                                     "Auditory endorgan length",
                                     "Columella size")),
          desc(Adj_Rsquared))

# remove the "log" from 'Coefficients'
#audio_pgls_results$xmodel_nolog<-numeric()
for(i in seq_along(audio_pgls_results$Coefficients)){
  audio_pgls_results$Coefficients[i]<-gsub("[\\(\\)]", "", regmatches(audio_pgls_results$Coefficients, gregexpr("\\(.*?\\)", audio_pgls_results$Coefficients))[[i]])
}


#visualize the table better using the flextable package
flexall<-flextable(audio_pgls_results) %>% add_header_lines(
  values = "Table X. ") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
#write.csv(intra,"E:/Analysis_plots/scalingintra feb 17.csv")
print(toprint,target = paste0(choose.dir(),"/pgls_audio 60 cutoff with na no head size adjustment.docx"))

###############____########



#more audiogram descriptive statistics
#CVs
mean(limits$HighHzlimit)/sd(limits$HighHzlimit)
mean(limits$LowHzlimit)/sd(limits$LowHzlimit)
