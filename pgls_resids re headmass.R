###Note the dataframe 'limits', created from 'Audiograms linked to anatomy.R'
###is required to run this code


library(patchwork)
library(ggrepel)


#ensure phylogeny matches dataframe
#remake comparative data frame object with averaged congeners
#rename phylogeny tips to matching with the species for which audiogram is available
birdtreels$tip.label[14]<-"Corvus_cornix" #renamed from Corvus_albus
birdtreels$tip.label[51]<-"Phalacrocorax_carbo" #rename "phalacrocorax_lucidus"


birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial,
                          vcv = TRUE, na.omit = FALSE,
                          warn.dropped = TRUE)



#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

############RUN PGLS############
####scaling vs head mass########
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

#creates list of model outputs 'pgls_model_list'
#dataframe with results 'hm'
source("pgls_HM.R")


# list of models predicted by hm are in an object 'pgls_model_list'
pgls_models_list

#list of the original regressions here:
pgls_todo_hm

#########get residuals of a pgls of measure~head mass as a dataframe#####
getresids_as_df<-function(i){
residtest<-as.data.frame(residuals(pgls_models_list[[i]]))
residtest$resid_bname<-row.names(residtest)
resid_measure<-function(){
  paste0("resid_",pgls_todo_hm[i])
}
residtest<-setNames(residtest,c(resid_measure(),"resid_bname"))
}

#############create list of dataframes containing residuals##############
resids_df_list<-list()
for(i in seq_along(pgls_todo_hm)){
  resids_df_list[[i]]<-assign("toadd",getresids_as_df(i))
}

#for(i in seq_along(resids_df_list)){
#limit2<-limits
#limit2<-full_join(limits,resids_df_list[[i]],by = c("spp_aud" = "resid_bname"))
#
#}

#join residual data into single dataframe
joined<-limits %>% full_join(.,resids_df_list[[1]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[2]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[3]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[4]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[5]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[6]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[7]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[8]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[9]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[10]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[11]],by = c("spp_aud" = "resid_bname"))%>%
  full_join(.,resids_df_list[[12]],by = c("spp_aud" = "resid_bname"))

#only keep audiogram species
joined<-joined[which(!is.na(joined$aud_rel)),]

names(joined)

#remove tilda from names to not mess up pgls formulas based on names
joined<-joined %>% rename_with(~ gsub("~", "vs", .x, fixed = TRUE))
residlist<-gsub("[()]","_",names(joined)[27:38])
joined<-setNames(joined,c(names(joined)[1:26],residlist))

names(joined)

#make the comparative data object for pgls point to your new
#residual data
birdCDO<-comparative.data(phy = birdtreels,data = joined,#[avgdf$Category!="Terrestrial",]
                          names.col =binomial,
                          vcv = TRUE, na.omit = F,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped


############get model list#########3
modellist_bs<-paste0("bestsensitivity~",residlist)
modellist_lf<-paste0("log(LowHzlimit)~",residlist)
modellist_hf<-paste0("log(HighHzlimit)~",residlist)
modellist_bh<-paste0("log(besthz)~",residlist)

residlist
#note same object name for category list but different order relative to non-resid pgls
categorylist_lf<-c("Impedance match",
                    "Impedance match",
                    "Impedance match",
                    "Impedance match",
                    "Auditory endorgan length",
                    "Input/output areas",
                    "Input/output areas",
                    "Input/output areas",
                    "Stiffness",
                   "Stiffness",
                   "Columella size",
                   "Columella size")

categorylist_bs<-categorylist_lf
categorylist_bh<-categorylist_lf
categorylist_hf<-categorylist_lf

#pgls_models(modellist_bs[[2]])
###########recreate the pgls forumula list to include the residuals#################
####now the same scripts can be run for analysis for each part of the audiogram####

source("pgls_audiogram_lf.R")

# high frequency limit ----------------------------------------------------

source("pgls_audiogram_hf.R")

source("pgls_audiogram_bh.R")

source("pgls_audiogram_bs.R")


### combine results from regressions for each measure into a single datagrame
audio_pgls_results<-bind_rows(audiogrampgls_bh,
                              audiogrampgls_bs,
                              audiogrampgls_lf,
                              audiogrampgls_hf)
audio_pgls_results$CI95_low<-audio_pgls_results$Estimate-audio_pgls_results$`Std. Error`*1.96
audio_pgls_results$CI95_high<-audio_pgls_results$Estimate+audio_pgls_results$`Std. Error`*1.96

#############formatting table###############
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
print(toprint,target = paste0(choose.dir(),"/pgls_resid_35 db cutoff with na_oct22.docx"))



