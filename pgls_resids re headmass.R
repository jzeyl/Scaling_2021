library(patchwork)
library(ggrepel)
source("pgls_audiogram_bs.R")


#average the congeners for linkage with audiograms
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
cong_avg$aud_spp[128]<-"Corvus_cornix"
cong_avg$aud_spp[129]<-"Phalacrocorax_carbo"
cong_avg$aud_rel[128]<-"Congener"
cong_avg$aud_rel[129]<-"Congener"
cong_avg<-cong_avg[-c(grep('Corvus_albus|Corvus_splendens', cong_avg$Binomial)), ]
cong_avg<-cong_avg[-c(grep('Phalacrocorax_capensis|Phalacrocorax_lucidus|Phalacrocorax_neglectus', cong_avg$Binomial)), ]
avgdf<-cong_avg

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

getresids_as_df<-function(i){
residtest<-as.data.frame(residuals(pgls_models_list[[i]]))
residtest$resid_bname<-row.names(residtest)
resid_measure<-function(){
  paste0("resid_",pgls_todo_hm[i])
}
residtest<-setNames(residtest,c(resid_measure(),"resid_bname"))
}

#create list of dataframes containing residuals
resids_df_list<-list()
for(i in seq_along(pgls_todo_hm)){
  resids_df_list[[i]]<-assign("toadd",getresids_as_df(i))
}

for(i in seq_along(resids_df_list)){
limit2<-limits
limit2<-full_join(limits,resids_df_list[[i]],by = c("spp_aud" = "resid_bname"))

}

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
residlist<-gsub("[()]","_",names(joined)[25:36])
joined<-setNames(joined,c(names(joined)[1:24],residlist))

names(joined)
birdCDO<-comparative.data(phy = birdtreels,data = joined,#[avgdf$Category!="Terrestrial",]
                          names.col =binomial,
                          vcv = TRUE, na.omit = F,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped



modellist_bs<-paste0("bestsensitivity~",residlist)
modellist_lf<-paste0("log(LowHzlimit)~",residlist)
modellist_hf<-paste0("log(HighHzlimit)~",residlist)
modellist_bh<-paste0("log(besthz)~",residlist)


pgls_models(modellist_bs[[1]])
###########best sensitivity#################
source("pgls_audiogram_bs.R")

source("pgls_audiogram_lf.R")

# high frequency limit ----------------------------------------------------

source("pgls_audiogram_hf.R")


