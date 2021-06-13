library(flextable)
library(officer)

#The pgls model function, which will be applied to list of formulas
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.00001,1)))#####
}

birdCDO$data$waterbirds<-as.factor(birdCDO$data$waterbirds)
modellist_t_wb<-paste0(modellist_intra,"*waterbirds")
pgls_models_listt_wb<-lapply(modellist_t_wb,pgls_models)#run pgls

categorylist_intra<-c("Impedance match",
                      "Impedance match",
                      "Impedance match",
                      "Impedance match",
                      "Impedance match (inner ear)",
                      "Columella",
                      "Columella",
                      "Columella",
                      "Stiffness",
                      "Mass inertia")

#make list of dataframes with the PGLS outputs. 
tbllist_intra_t_wb<-list()
for (i in seq_along(pgls_models_listt_wb)){#change th 'Model' colume in this as appropriate
  
  #put stats outputs into dataframe
  tbllist_intra_t_wb[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_intra_t_wb[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_intra_t_wb[[i]]$Model<-modellist_t_wb[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_intra_t_wb[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_intra_t_wb[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_intra_t_wb[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_intra_t_wb[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_intra_t_wb[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
  
}

#organize the dataframe table (significant digits, remove redundant F stat & R squared)
for(i in seq_along(tbllist_intra_t_wb)){
  tbllist_intra_t_wb[[i]]$Coefficients<-row.names(tbllist_intra_t_wb[[i]])
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_intra_t_wb[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_intra_t_wb[[i]], is.numeric))# Identify numeric columns
  tbllist_intra_t_wb[[i]]<-cbind(tbllist_intra_t_wb[[i]][,which(character_cols)],signif(tbllist_intra_t_wb[[i]][,which(numeric_cols)], digits = 2))
  colnames(tbllist_intra_t_wb[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  row.names(tbllist_intra_t_wb[[i]])<-c()#remove row names
  print(tbllist_intra_t_wb[[i]])
}

intra<-do.call(rbind.data.frame,tbllist_intra)
intra$CI95_low<-intra$Estimate-intra$`Std. Error`*1.96
intra$CI95_high<-intra$Estimate+intra$`Std. Error`*1.96
intra$geometric_exp<-rep(geomcoefs_intra,each =2)

intra$category<-rep(categorylist_intra,each = 2)
intra$scalingtype<-ifelse(intra$CI95_high<intra$geometric_exp,"hypoallometric",
                          "other")
intra$scalingtype<-ifelse(intra$CI95_low>intra$geometric_exp,"hyperallometric",
                          intra$scalingtype)
intra$scalingtype<-ifelse(intra$CI95_high>intra$geometric_exp&intra$CI95_low<intra$geometric_exp,"isometric",
                          intra$scalingtype)

intra = subset(intra, select = c(category,Model,Coefficients,geometric_exp,Estimate, CI95_low,
                                 CI95_high,scalingtype,Adj_Rsquared,Lambda))

#visualize the table better using the flextable package
flexall<-flextable(intra) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  #bold(i = ~ ChangeAIC < 0.01) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall
