
#select models with head mass
pgls_todo_bm<-gsub("Head.mass..g.","bodymass",pgls_todo_hm)
pgls_todo_bm[13]<-"log(Head.mass..g.)~log(bodymass)"

options(digits = 5)

library(flextable)
library(officer)
#pgls_todo_bm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#Head mass only

pgls_models_list<-lapply(pgls_todo_bm,pgls_models)#run pgls

#make list of dataframes with the PGLS outputs.
tbllist_bm<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_bm[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_bm[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_bm[[i]]$Model<-pgls_todo_bm[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_bm[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_bm[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_bm[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_bm[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_bm[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
  tbllist_bm[[i]]$T_1<-(coef(pgls_models_list[[i]])[2]-1)/pgls_models_list[[i]]$sterr[2]
  tbllist_bm[[i]]$T_0<-(coef(pgls_models_list[[i]])[2]-0)/pgls_models_list[[i]]$sterr[2]
  tbllist_bm[[i]]$p_slope_re_1<- 2*pt(abs(tbllist_bm[[i]]$T_1), pgls_models_list[[i]]$n-2, lower.tail = FALSE)
  tbllist_bm[[i]]$p_slope_re_0<- 2*pt(abs(tbllist_bm[[i]]$T_0), pgls_models_list[[i]]$n-2, lower.tail = FALSE)

}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_bm)){
  tbllist_bm[[i]]$Coefficients<-row.names(tbllist_bm[[i]])
  tbllist_bm[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_bm[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function
  character_cols<-unlist(lapply(tbllist_bm[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_bm[[i]], is.numeric))# Identify numeric columns
  tbllist_bm[[i]]<-cbind(tbllist_bm[[i]][,which(character_cols)],signif(tbllist_bm[[i]][,which(numeric_cols)], digits = 2))
  #tbllist_bm[[i]] <- tbllist_bm[[i]][, c(6,11,8:10,7,5,1:4)]#change order of columns
  #dplyr::select_if(tbllist_bm[[i]], is.numeric)#select only numeric data
  colnames(tbllist_bm[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  #tbllist_bm[[i]]$Fstat[2:nrow(tbllist_bm[[i]])]<-""
  #tbllist_bm[[i]]$Fstat_numdf[2:nrow(tbllist_bm[[i]])]<-""
  #tbllist_bm[[i]]$Fstat_dendf[2:nrow(tbllist_bm[[i]])]<-" "
  ##tbllist_bm[[i]]$Model[2:nrow(tbllist_bm[[i]])]<-""
  #tbllist_bm[[i]]$Lambda[2:nrow(tbllist_bm[[i]])]<-""
  #tbllist_bm[[i]]$Adj_Rsquared[2:nrow(tbllist_bm[[i]])]<-""
  #tbllist_bm[[i]]$AICc[2:nrow(tbllist_bm[[i]])]<-""
  row.names(tbllist_bm[[i]])<-c()#remove row names
  print(tbllist_bm[[i]])
}

bm<-do.call(rbind.data.frame,tbllist_bm)
bm$CI95_low<-bm$Estimate-bm$`Std. Error`*1.96
bm$CI95_high<-bm$Estimate+bm$`Std. Error`*1.96
bm$geometric_exp<-rep(geomcoefs,each =2)

bm$category<-rep(categorylist,each = 2)
bm$scalingtype<-ifelse(bm$CI95_high<bm$geometric_exp,"hypoallometric",
                       "other")
bm$scalingtype<-ifelse(bm$CI95_low>bm$geometric_exp,"hyperallometric",
                       bm$scalingtype)
bm$scalingtype<-ifelse(bm$CI95_high>bm$geometric_exp&bm$CI95_low<bm$geometric_exp,"isometric",
                       bm$scalingtype)

bm = subset(bm, select = c(category,Model,Coefficients,geometric_exp,Estimate, CI95_low,
                           CI95_high,scalingtype,Adj_Rsquared,Lambda))

#visualize the table better using the flextable package
flexall<-flextable(bm) %>%
  add_header_lines(  values = "Table X. Models for selection") %>%
  bold(i = ~ scalingtype == "isometric") %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

#print to file
#print(toprint,target = "E:/Analysis_plots/pgls_bm_allmar 3.docx")
