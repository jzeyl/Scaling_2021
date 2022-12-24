
#options(scipen = 100)
library(flextable)
library(officer)
#pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#Head mass only
modellist<-pgls_todo_hm
pgls_models_list<-lapply(modellist,pgls_models)#run pgls


#make list of dataframes with the PGLS outputs.
tbllist_HM<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_HM[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_HM[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_HM[[i]]$Model<-modellist[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_HM[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_HM[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_HM[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_HM[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_HM[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
  tbllist_HM[[i]]$T_1<-(coef(pgls_models_list[[i]])[2]-1)/pgls_models_list[[i]]$sterr[2]
  tbllist_HM[[i]]$T_0<-(coef(pgls_models_list[[i]])[2]-0)/pgls_models_list[[i]]$sterr[2]
  tbllist_HM[[i]]$p_slope_re_1<- 2*pt(abs(tbllist_HM[[i]]$T_1), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  tbllist_HM[[i]]$p_slope_re_0<- 2*pt(abs(tbllist_HM[[i]]$T_0), pgls_models_list[[i]]$n-2, lower.tail = FALSE)

}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_HM)){
  tbllist_HM[[i]]$Coefficients<-row.names(tbllist_HM[[i]])
  tbllist_HM[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_HM[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function
  character_cols<-unlist(lapply(tbllist_HM[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_HM[[i]], is.numeric))# Identify numeric columns
  tbllist_HM[[i]]<-cbind(tbllist_HM[[i]][,which(character_cols)],signif(tbllist_HM[[i]][,which(numeric_cols)], digits = 2))
  #tbllist_HM[[i]] <- tbllist_HM[[i]][, c(6,11,8:10,7,5,1:4)]#change order of columns
  #dplyr::select_if(tbllist_HM[[i]], is.numeric)#select only numeric data
  colnames(tbllist_HM[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
 row.names(tbllist_HM[[i]])<-c()#remove row names
  print(tbllist_HM[[i]])
}


hm<-do.call(rbind.data.frame,tbllist_HM)
hm$CI95_low<-hm$Estimate-hm$`Std. Error`*1.96
hm$CI95_high<-hm$Estimate+hm$`Std. Error`*1.96
hm$geometric_exp<-rep(geomcoefs,each =2)

hm$category<-rep(categorylist,each = 2)
hm$scalingtype<-ifelse(hm$CI95_high<hm$geometric_exp,"Hypo",
                       "other")
hm$scalingtype<-ifelse(hm$CI95_low>hm$geometric_exp,"Hyper",
                       hm$scalingtype)
hm$scalingtype<-ifelse(hm$CI95_high>hm$geometric_exp&hm$CI95_low<hm$geometric_exp,"Iso",
                       hm$scalingtype)

hm$tval<-(hm$Estimate-hm$geometric_exp)/hm$`Std. Error`#t-value of differnce between estimate and isometric slope
hm$pval<-2*pt(abs(hm$tval),df=hm$Fstat_dendf, lower.tail = FALSE)#two tailed p-val
#hm$pval<-round(hm$pval,digits = 5)


hm <- subset(hm, select = c(category,Model,Coefficients,
                            geometric_exp,Estimate, CI95_low,
                      CI95_high,scalingtype,pval,Adj_Rsquared,Lambda))

#visualize the table better using the flextable package
flexall<-flextable(hm) %>%
  add_header_lines(  values = "Table X. Models for selection") %>%
  bold(i = ~ pval < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

#print to file


