#pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#Head mass only
#modellist<-pgls_todo_hm
pgls_models_list<-lapply(modellist,pgls_models)#run pgls

#make list of dataframes with the PGLS outputs. 
tbllist_audiogram<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_audiogram[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_audiogram[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_audiogram[[i]]$Model<-modellist[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_audiogram[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_audiogram[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_audiogram[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_audiogram[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_audiogram[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
  tbllist_audiogram[[i]]$T_1<-(coef(pgls_models_list[[i]])[2]-1)/pgls_models_list[[i]]$sterr[2]
  tbllist_audiogram[[i]]$T_0<-(coef(pgls_models_list[[i]])[2]-0)/pgls_models_list[[i]]$sterr[2]
  tbllist_audiogram[[i]]$p_slope_re_1<- 2*pt(abs(tbllist_audiogram[[i]]$T_1), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  tbllist_audiogram[[i]]$p_slope_re_0<- 2*pt(abs(tbllist_audiogram[[i]]$T_0), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  
}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_audiogram)){
  tbllist_audiogram[[i]]$Coefficients<-row.names(tbllist_audiogram[[i]])
  tbllist_audiogram[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_audiogram[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_audiogram[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_audiogram[[i]], is.numeric))# Identify numeric columns
  tbllist_audiogram[[i]]<-cbind(tbllist_audiogram[[i]][,which(character_cols)],signif(tbllist_audiogram[[i]][,which(numeric_cols)], digits = 2))
  #tbllist_audiogram[[i]] <- tbllist_audiogram[[i]][, c(6,11,8:10,7,5,1:4)]#change order of columns
  #dplyr::select_if(tbllist_audiogram[[i]], is.numeric)#select only numeric data
  colnames(tbllist_audiogram[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  #tbllist_audiogram[[i]]$Fstat[2:nrow(tbllist_audiogram[[i]])]<-""
  #tbllist_audiogram[[i]]$Fstat_numdf[2:nrow(tbllist_audiogram[[i]])]<-""
  #tbllist_audiogram[[i]]$Fstat_dendf[2:nrow(tbllist_audiogram[[i]])]<-" "
  ##tbllist_audiogram[[i]]$Model[2:nrow(tbllist_audiogram[[i]])]<-""
  #tbllist_audiogram[[i]]$Lambda[2:nrow(tbllist_audiogram[[i]])]<-""
  #tbllist_audiogram[[i]]$Adj_Rsquared[2:nrow(tbllist_audiogram[[i]])]<-""
  #tbllist_audiogram[[i]]$AICc[2:nrow(tbllist_audiogram[[i]])]<-""
  row.names(tbllist_audiogram[[i]])<-c()#remove row names
  print(tbllist_audiogram[[i]])
}

audiogrampgls<-do.call(rbind.data.frame,tbllist_audiogram)
audiogrampgls$CI_lower<-audiogrampgls$Estimate-audiogrampgls$`Std. Error`*1.96
audiogrampgls$CI_upper<-audiogrampgls$Estimate+audiogrampgls$`Std. Error`*1.96


Model, geometric exponent



write.csv(audiogrampgls,"E:/Analysis_plots/audiogrambestsensitivity.csv")