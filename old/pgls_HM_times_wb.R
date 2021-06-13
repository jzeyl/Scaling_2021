options(digits = 3)

library(flextable)
library(officer)
#pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#Head mass only
modellist<-paste(pgls_todo_hm,'*waterbirds')
pgls_models_list<-lapply(modellist,pgls_models)#run pgls

categorylist<-c("Middle ear", "Middle ear", "Impedance match","Impedance match","Impedance match",
                "Impedance match", "Stiffness","Inner ear opening","Inner ear opening", "Stiffness",
                "Columella","Columella")

#make list of dataframes with the PGLS outputs. 
tbllist_HM_T_W<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_HM_T_W[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_HM_T_W[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_HM_T_W[[i]]$Model<-modellist[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_HM_T_W[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_HM_T_W[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_HM_T_W[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_HM_T_W[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_HM_T_W[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
  tbllist_HM_T_W[[i]]$T_1<-(coef(pgls_models_list[[i]])[2]-1)/pgls_models_list[[i]]$sterr[2]
  tbllist_HM_T_W[[i]]$T_0<-(coef(pgls_models_list[[i]])[2]-0)/pgls_models_list[[i]]$sterr[2]
  tbllist_HM_T_W[[i]]$p_slope_re_1<- 2*pt(abs(tbllist_HM_T_W[[i]]$T_1), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  tbllist_HM_T_W[[i]]$p_slope_re_0<- 2*pt(abs(tbllist_HM_T_W[[i]]$T_0), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  
}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_HM_T_W)){
  tbllist_HM_T_W[[i]]$Coefficients<-row.names(tbllist_HM_T_W[[i]])
  tbllist_HM_T_W[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_HM_T_W[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_HM_T_W[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_HM_T_W[[i]], is.numeric))# Identify numeric columns
  tbllist_HM_T_W[[i]]<-cbind(tbllist_HM_T_W[[i]][,which(character_cols)],signif(tbllist_HM_T_W[[i]][,which(numeric_cols)], digits = 2))
  #tbllist_HM_T_W[[i]] <- tbllist_HM_T_W[[i]][, c(6,11,8:10,7,5,1:4)]#change order of columns
  #dplyr::select_if(tbllist_HM_T_W[[i]], is.numeric)#select only numeric data
  colnames(tbllist_HM_T_W[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  row.names(tbllist_HM_T_W[[i]])<-c()#remove row names
  print(tbllist_HM_T_W[[i]])
}
