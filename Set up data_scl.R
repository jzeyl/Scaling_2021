library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(purrr)

################Set up data############
#set working directory and load data
setwd(choose.dir())

#load main dataframe
df<-read.csv("databmadded.csv", stringsAsFactors = F, header = T) #, stringsAsFactors = FALSE

#The pgls model function, which will be applied to list of formulas
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.0001,1)))#####
}


#Load phylogeny and correct names that were different between birdtree.org and the up-to-date species names
source("load phylogeny and make CDO.R")

#Some missing headmass values to be imputed using PGLS of skull width and head mass
#Computed head mass from head mass~skullwidth pgls
df$HM#without imputed values
source("SW_HM_.R")#add phylogeny here
df$HM#with imputed values


#Since PGLS uses one point per species,I make the dataframe to have average values for species with more than one specimen:
#First I make a dataframe with only one species per line
distinctdf<-distinct(df, Binomial, .keep_all = TRUE)
distinctdforder<-arrange(distinctdf,Binomial)#sort by species name

#Next get averages by species for columns with continuous data
avgdf<-df %>% group_by(Binomial) %>%
  summarise_at(vars(Skull.width..mm.:TM_FP),mean, na.rm = TRUE)
avgdf<-as.data.frame(avgdf)

#Columns from the distinctdf dataframe which don't require averaging are added back
avgdf$Species<-distinctdforder$Species
avgdf$Low.Hz<-distinctdforder$Low.Hz
avgdf$Order<-distinctdforder$Order
avgdf$Family<-distinctdforder$Family
avgdf$maxdivedepth<-distinctdforder$max
avgdf$Category<-as.character(distinctdforder$Category)
avgdf$birdtree<-gsub(" ","_",distinctdforder$Birdtree)
avgdf$BM_lit<-distinctdforder$BM_lit
avgdf$aud_spp<-distinctdforder$spp_audio
avgdf$aud_rel<-distinctdforder$audio_relation


#make comparative data frame object
birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial,
                          vcv = TRUE, na.omit = FALSE,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

#go to 'Audiograms linked to anatomy.R' file to audiogram analysis


#########scaling intraear##########
#set up intra-ear analyses
modellist_intra <- c(  "log(TM)~log(FP)",#impedance-related measures
                       "log(COffset)~log(TM)",
                       "log(UH)~log(TM)",
                       "log(TMA)~log(TM)",
                       "log(RW)~log(FP)",

                       "log(ES)~log(CL)",

                       "log(CL)~log(CV)",
                       "log(CL)~log(FP)",#
                       "log(FP)~log(CV)",#

                       "log(TM)~log(CV)")

  geomcoefs_intra<-c(1,
                     0.5,
                     0.5,
                     0,
                     1,

                     1,

                     0.33,
                     0.5,
                     0.67,
                     0.67)

#list of functional categories for table
categorylist_intra<-c(rep("Impedance match",5),
                      "Stiffness",
                      rep("Columella morphology",4))

#run the
source("pgls_intraear.R")

#remove intercept estimates, drop model column,
#only keep significant relationships

#combine estimate +/- 95 CI into one cell
intra$pglsslope<-paste0(intra$Estimate," (",
                        format(round(intra$CI95_low, 3), nsmall = 3),
                        ",",
                        format(round(intra$CI95_high, 3), nsmall = 3),
                        ")")

#split up the model formula to get x and y components
splitmodel<-strsplit(intra$Model,"~")
intra$ymodel<-map(splitmodel,1)#left side of formula

# remove the "log" from each character string
intra$ymodel_nolog<-numeric()
for(i in seq_along(intra$ymodel)){
  intra$ymodel_nolog[i]<-gsub("[\\(\\)]", "", regmatches(intra$ymodel, gregexpr("\\(.*?\\)", intra$ymodel))[[i]])
}

options(scipen = 100, digits = 2)
intra<-intra %>% select(category, ymodel_nolog,Coefficients,
                  geometric_exp, pglsslope,scalingtype,Adj_Rsquared,pval, Lambda) %>%
  filter(Coefficients!="(Intercept)")
# remove the "log" from 'Coefficients'
#intra$xmodel_nolog<-numeric()
for(i in seq_along(intra$Coefficients)){
  intra$Coefficients[i]<-gsub("[\\(\\)]", "", regmatches(intra$Coefficients, gregexpr("\\(.*?\\)", intra$Coefficients))[[i]])
}

#sort table by category and then adjusted R2
intra$category<-as.factor(intra$category)
levels(intra$category)<-c("Impedance match",
intra<-arrange(intra,factor(intra$category, levels = c("Impedance match", "Stiffness", "Columella morphology")),
               desc(Adj_Rsquared))
intra$pval<-format(round(intra$pval, 3), nsmall = 3)



#visualize the table better using the flextable package
flexall<-flextable(intra) %>%
  add_header_lines(values = "Table X. ") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_intra<-lapply(pgls_models_list, plot)
plots_intra

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
#write.csv(intra,"E:/Analysis_plots/scalingintra feb 17.csv")
print(toprint,target = paste0(choose.dir(),"/pgls_intra_scaling all_Apr4 2022.docx"))

####list of pgls models to run (only models with head mass are used)####
pgls_todo_hm<- c("log(TM_FP)~log(HM)",
                        "log(COffset)~log(HM)",
                        "log(UH)~log(HM)",
                        "log(TMA)~log(HM)",
                        "log(ECD)~log(HM)",
                        "log(TM)~log(HM)",#
                        "log(FP)~log(HM)",#
                        "log(RW)~log(HM)",
                        "log(ES)~log(HM)",
                        "log(Air)~log(HM)",#
                        "log(CL)~log(HM)",
                        "log(CV)~log(HM)")

####list of expected geometric coefficients for___###
geomcoefs<-c(0,#impedance-matching
             0.33,
             0.33,
             0,
             0.33,#auditory endorgan
             0.67,#input/output areas
             0.67,
             0.67,
             0.33,#stiffness
             1,
             0.33,#columella size
             1)


#######functional category list#######
categorylist<-c(rep("Impedance matching",4),
                "Auditory endorgan length",
                rep("Input/output areas",3),
                rep("Stiffness",2),
                rep("Columella size",2))

############RUN PGLS############
####scaling vs head mass########
source("pgls_HM.R")#creates list of model outputs 'pgls_model_list'

#dataframe with results 'hm'

#formatting table
#remove intercept estimates, drop model column,
#only keep significant relationships

#combine estimate +/- 95 CI into one cell
hm$pglsslope<-paste0(hm$Estimate," (",
                     format(round(hm$CI95_low, 3), nsmall = 3),
                     ",",
                     format(round(hm$CI95_high, 3), nsmall = 3),
                     ")")

#split up the model formula to get x and y components
splitmodel<-strsplit(hm$Model,"~")
hm$ymodel<-map(splitmodel,1)#left side of formula

# remove the "log" from each character string
hm$ymodel_nolog<-numeric()
for(i in seq_along(hm$ymodel)){
  hm$ymodel_nolog[i]<-gsub("[\\(\\)]", "", regmatches(hm$ymodel, gregexpr("\\(.*?\\)", hm$ymodel))[[i]])
}

options(scipen = 100, digits = 2)
hm<-hm %>% select(category, ymodel_nolog,Coefficients,
                  geometric_exp, pglsslope,scalingtype,Adj_Rsquared,pval, Lambda) %>%
  filter(Coefficients!="(Intercept)")
# remove the "log" from 'Coefficients'
#hm$xmodel_nolog<-numeric()
for(i in seq_along(hm$Coefficients)){
  hm$Coefficients[i]<-gsub("[\\(\\)]", "", regmatches(hm$Coefficients, gregexpr("\\(.*?\\)", hm$Coefficients))[[i]])
}

#sort table by category and then adjusted R2
hm$category<-as.factor(hm$category)
hm<-arrange(hm,factor(hm$category, levels = c(
  "Columella size",
  "Auditory endorgan length",
  "Input/output areas",
  "Stiffness",
  "Impedance match")),desc(Adj_Rsquared)
            )
hm$pval<-format(round(hm$pval, 3), nsmall = 3)


####visualize the table better using the flextable package####
flexall<-flextable(hm) %>%
  add_header_lines(  values = "Table X. Models for selection") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall


#pgls model diagnostics
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plots_hm<-lapply(pgls_models_list, plot)
plots_hm

#write table to word file
#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
#write.csv(intra,"E:/Analysis_plots/scalingintra feb 17.csv")
print(toprint,target = paste0(choose.dir(),"/pgls_hm_scaling all_Apr4 2022.docx"))



#body mass independent
bm_vs_hm<-pgls_models(log(HM)~log(BM_lit))
summary(bm_vs_hm)


