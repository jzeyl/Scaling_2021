library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyr)

################Set up data############
#set working directory and load data
setwd("C:/Users/jeffz/Desktop/scling/Scaling_2021")

#load main dataframe
df<-read.csv("databmadded.csv", stringsAsFactors = F, header = T) #, stringsAsFactors = FALSE

#The pgls model function, which will be applied to list of formulas
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.0001,1)))#####
}

#Some missing headmass values to be imputed using PGLS of skull width and head mass
df$Head.mass..g.

#Load phylogeny and correct names that were different between birdtree.org and the up-to-date species names
source("load phylogeny and make CDO.R")

#Ddd computed head mass from head mass~skullwidth pgls
source("SW_HM_.R")#add phylogeny here
df$Head.mass..g.#with imputed values

#Since PGLS uses one point per species,I make the dataframe to have average values for species with more than one specimen:
#First I make a dataframe with only one species per line
distinctdf<-distinct(df, Binomial, .keep_all = TRUE)
distinctdforder<-arrange(distinctdf,Binomial)#sort by species name

#Next get averages by species for columns with continuous data
avgdf<-df %>% group_by(Binomial) %>% summarise_at(vars(Skull.width..mm.:area_ratio),
                                                           mean, na.rm = TRUE)                         
avgdf<-as.data.frame(avgdf)

#Now we add back columns from the distinctdf dataframe which don't require averaging
avgdf$Species<-distinctdforder$Species
avgdf$Low.Hz<-distinctdforder$Low.Hz
avgdf$Order<-distinctdforder$Order
avgdf$Family<-distinctdforder$Family
avgdf$maxdivedepth<-distinctdforder$max
avgdf$Category<-as.character(distinctdforder$Category)
avgdf$birdtree<-gsub(" ","_",distinctdforder$Birdtree)
avgdf$Behind.TM<-distinctdforder$Behind.TM
avgdf$bodymass<-distinctdforder$bodymass_lit


avgdf$superorder<-avgdf$Order
avgdf$superorder[avgdf$Order=="Passeriformes"|
                   avgdf$Order=="Falconiformes"|
                   avgdf$Order=="Psittaciformes"]<-"Australaves"
avgdf$superorder[avgdf$Order=="Coraciiformes"|
                   avgdf$Order=="Piciformes"|
                   avgdf$Order=="Bucerotiformes"|
                   avgdf$Order=="Coliiformes"]<-"Coraciimorphae"
avgdf$superorder[avgdf$Order=="Suliformes"|
                   avgdf$Order=="Sphenisciformes"|
                   avgdf$Order=="Charadriiformes"|
                   avgdf$Order=="Procellariiformes"|
                   avgdf$Order=="Gaviiformes"|
                   avgdf$Order=="Pelecaniformes"|
                   avgdf$Order=="Phoenicopteriformes"|
                   avgdf$Order=="Phaethontiformes"]<-"Aequorlitornithes"
avgdf$superorder[avgdf$Order=="Columbiformes"|
                   avgdf$Order=="Cuculiformes"|
                   avgdf$Order=="Musophagiformes"]<-"Columbaves"
avgdf$superorder[avgdf$Order=="Galliformes"|
                   avgdf$Order=="Anseriformes"]<-"Galloanserae"
avgdf$superorder[avgdf$Order=="Struthioniformes"|
                   avgdf$Order=="Cassuariiformes"]<-"Paleognathae"
avgdf$waterbirds<-ifelse(avgdf$superorder=="Aequorlitornithes","Aequorlitornithes","not Aequelornithes")


#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

####list of pgls models to run (only models with head mass are used)####
pgls_todo_nogeomet <- c(
                        "log(area_ratio)~log(Skull.width..mm.)",
                        "log(area_ratio)~log(Head.mass..g.)",
                        
                        "log(dis_coltip_TMcentroid)~log(Skull.width..mm.)",
                        "log(dis_coltip_TMcentroid)~log(Head.mass..g.)",
                        
                        "log(Umbo_distancetoTMplane)~log(Skull.width..mm.)",
                        "log(Umbo_distancetoTMplane)~log(Head.mass..g.)",
                        
                        "log(meanTMangle)~log(Skull.width..mm.)",
                        "log(meanTMangle)~log(Head.mass..g.)",
                        
                        "log(totalECDlength)~log(Skull.width..mm.)",
                        "log(totalECDlength)~log(Head.mass..g.)", 
                        

                        "log(TMtotalarea)~log(Skull.width..mm.)",
                        "log(TMtotalarea)~log(Head.mass..g.)",# 
                        
                        "log(FPtotalarea)~log(Skull.width..mm.)",
                        "log(FPtotalarea)~log(Head.mass..g.)",#
                        
                        "log(RWtotalarea)~log(Skull.width..mm.)",
                        "log(RWtotalarea)~log(Head.mass..g.)", 
                        
                         "log(totalEClength)~log(Skull.width..mm.)",
                        "log(totalEClength)~log(Head.mass..g.)",
                        
                        "log(Behind.TM)~log(Skull.width..mm.)",
                        "log(Behind.TM)~log(Head.mass..g.)",#   
                        
                        "log(Columella.length.mm)~log(Skull.width..mm.)",
                        "log(Columella.length.mm)~log(Head.mass..g.)",  
                        
                        "log(Columella.volume.mm3)~log(Skull.width..mm.)",
                        "log(Columella.volume.mm3)~log(Head.mass..g.)",
                        
                        "log(Columella.volume.mm3)~log(Skull.width..mm.)",
                        "log(bodymass)~log(Head.mass..g.)")

#select models with head mass
pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

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
             1,
             
             1)
#######functional category list#######
categorylist<-c(rep("Impedance matching",4),
                "Auditory endorgan length",
                rep("Input/output areas",3),
                rep("Stiffness",2),
                rep("Columella size",2),
                "Body size")

############RUN PGLS############
####scaling vs head mass########
source("pgls_HM.R")#creates dataframe with results 'hm'

####visualize the table better using the flextable package####
flexall<-flextable(hm) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

#write result/table to files
#write.csv(hm,"E:/Analysis_plots/scaling_hm Feb 17.csv")
print(toprint,target = "pgls_hm_all Dec 18 2021.docx")
#print(toprint,target = "E:/Analysis_plots/pgls_hm_scaling waterbirds mar 3.docx")



#########scaling intraear##########
#set up intra-ear analyses
modellist_intra <- c(  "log(TMtotalarea)~log(FPtotalarea)",
                       "log(dis_coltip_TMcentroid)~log(TMtotalarea)",
                       "log(Umbo_distancetoTMplane)~log(TMtotalarea)",
                       "log(meanTMangle)~log(TMtotalarea)",
                       "log(RWtotalarea)~log(FPtotalarea)",
                       
                       "log(totalEClength)~log(Columella.length.mm)",
                       
                       "log(Columella.length.mm)~log(Columella.volume.mm3)",
                       "log(Columella.length.mm)~log(FPtotalarea)",#
                       "log(FPtotalarea)~log(Columella.volume.mm3)",#   
                       
                       "log(TMtotalarea)~log(Columella.volume.mm3)")

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

categorylist_intra<-c(rep("Impedance match",5),
                      "Stiffness",
                      rep("Columella morphology",2),
                      rep("Columella inertia",2))

source("pgls_intraear.R")

#visualize the table better using the flextable package
flexall<-flextable(intra) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
#write.csv(intra,"E:/Analysis_plots/scalingintra feb 17.csv")
print(toprint,target = "pgls_intra_scaling all_Dec 18 2021.docx")
#print(toprint,target = "E:/Analysis_plots/pgls_intra_scaling watermar17.docx")

#########scaling with body mass##########
source("pgls_bm.R")

#visualize the table better using the flextable package
flexall<-flextable(bm) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
#write.csv(intra,"E:/Analysis_plots/scalingintra feb 17.csv")
print(toprint,target = "pgls_bm_scaling all_apr 14.docx")


#go to 'Audiograms linked to anatomy.R' file to get model lists for audiogram pgls

#
#########################non-waterbirds only################################
#birdCDO<-comparative.data(phy = birdtreels,data = avgdf[avgdf$superorder!="Aequorlitornithes",],
#                          names.col = Binomial, 
#                          vcv = TRUE, na.omit = F, 
#                          warn.dropped = TRUE)
#
##check any tips dropped between linking phylogeny and dataframe
#birdCDO$dropped
#
##WATERBIRDS only
#birdCDO<-comparative.data(phy = birdtreels,data = avgdf[avgdf$superorder=="Aequorlitornithes",],
#                          names.col = Binomial, 
#                          vcv = TRUE, na.omit = F, 
#                          warn.dropped = TRUE)
#
##check any tips dropped between linking phylogeny and dataframe
#birdCDO$dropped

