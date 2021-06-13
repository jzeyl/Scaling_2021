####for each ear measure, combine together the different model outputs into single dataframe
grouped_eachmeasure<-list()
for (i in 1:length(tbllist_intra)){
  grouped_eachmeasure[[i]]<-rbind(tbllist_intra[[i]],tbllist_intra_t_wb[[i]])                   #,
}

################
add_AIC<-list()

for (i in 1:length(tbllist_intra)){
  add_AIC[[i]]<-grouped_eachmeasure[[i]][!duplicated(grouped_eachmeasure[[i]]$Model),
                                         c("Model","Adj_Rsquared","Lambda","Fstat","Fstat_numdf","Fstat_dendf",
                                                                                       "AICc")]
  add_AIC[[i]]$ChangeAIC<-add_AIC[[i]]$AICc-min(add_AIC[[i]]$AICc)
  add_AIC[[i]]$relativelikelihood<-exp(-0.5*add_AIC[[i]]$ChangeAIC)
  add_AIC[[i]]$AICweight_calc<-add_AIC[[i]]$relativelikelihood/sum(add_AIC[[i]]$relativelikelihood)
}

#Sort rows by AIC, low to high
add_AIC2<-list()
for (i in 1:length(add_AIC)){
  add_AIC2[[i]]<-arrange(add_AIC[[i]],add_AIC[[i]]$AICc)
}

all<-do.call(rbind.data.frame,add_AIC2)
all$Measurementgroup<-""#add extra column for category

#visualize the table better using the flextable package
flexall<-flextable(all) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  bold(i = ~ ChangeAIC <2.5) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall




##############
####for each ear measure, combine together the different model outputs into single dataframe
grouped_eachmeasure<-list()
for (i in 1:length(tbllist_HM)){
  grouped_eachmeasure[[i]]<-rbind(tbllist_HM[[i]],tbllist_HM_T_W[[i]])                   #,
}

################
add_AIC<-list()

for (i in 1:length(tbllist_HM)){
  add_AIC[[i]]<-grouped_eachmeasure[[i]][!duplicated(grouped_eachmeasure[[i]]$Model),
                                         c("Model","Adj_Rsquared","Lambda","Fstat","Fstat_numdf","Fstat_dendf",
                                           "AICc")]
  add_AIC[[i]]$ChangeAIC<-add_AIC[[i]]$AICc-min(add_AIC[[i]]$AICc)
  add_AIC[[i]]$relativelikelihood<-exp(-0.5*add_AIC[[i]]$ChangeAIC)
  add_AIC[[i]]$AICweight_calc<-add_AIC[[i]]$relativelikelihood/sum(add_AIC[[i]]$relativelikelihood)
}

#Sort rows by AIC, low to high
add_AIC2<-list()
for (i in 1:length(add_AIC)){
  add_AIC2[[i]]<-arrange(add_AIC[[i]],add_AIC[[i]]$AICc)
}

all<-do.call(rbind.data.frame,add_AIC2)
all$Measurementgroup<-""#add extra column for category

#visualize the table better using the flextable package
flexall<-flextable(all) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  bold(i = ~ ChangeAIC <2.5) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

