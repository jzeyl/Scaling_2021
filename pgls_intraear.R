library(flextable)
library(officer)

#The pgls model function, which will be applied to list of formulas
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.001,1)))#####
}

pgls_models_list<-lapply(modellist_intra,pgls_models)#run pgls

#make list of dataframes with the PGLS outputs.
tbllist_intra<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate

  #put stats outputs into dataframe
  tbllist_intra[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_intra[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_intra[[i]]$Model<-modellist_intra[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_intra[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_intra[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_intra[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_intra[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_intra[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
    }

#organize the dataframe table (significant digits, remove redundant F stat & R squared)
for(i in seq_along(tbllist_intra)){
  tbllist_intra[[i]]$Coefficients<-row.names(tbllist_intra[[i]])
  #identify numeric cols and character cols to apply the significant digits function
  character_cols<-unlist(lapply(tbllist_intra[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_intra[[i]], is.numeric))# Identify numeric columns
  tbllist_intra[[i]]<-cbind(tbllist_intra[[i]][,which(character_cols)],signif(tbllist_intra[[i]][,which(numeric_cols)], digits = 2))
  colnames(tbllist_intra[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  row.names(tbllist_intra[[i]])<-c()#remove row names
  print(tbllist_intra[[i]])
}

intra<-do.call(rbind.data.frame,tbllist_intra)
intra$CI95_low<-intra$Estimate-intra$`Std. Error`*1.96
intra$CI95_high<-intra$Estimate+intra$`Std. Error`*1.96
intra$geometric_exp<-rep(geomcoefs_intra,each =2)

intra$category<-rep(categorylist_intra,each = 2)
intra$scalingtype<-ifelse(intra$CI95_high<intra$geometric_exp,"Hypo",
                       "other")
intra$scalingtype<-ifelse(intra$CI95_low>intra$geometric_exp,"Hyper",
                       intra$scalingtype)
intra$scalingtype<-ifelse(intra$CI95_high>intra$geometric_exp&intra$CI95_low<intra$geometric_exp,"Iso",
                       intra$scalingtype)
intra$tval<-(intra$Estimate-intra$geometric_exp)/intra$`Std. Error`#t-value of differnce between estimate and isometric slope
intra$pval<-2*pt(abs(intra$tval),df=intra$Fstat_dendf, lower.tail = FALSE)#two tailed p-val


#subset the columns to present concise paper
intra <- subset(intra, select = c(category,Model,Coefficients,geometric_exp,Estimate, CI95_low,
                           CI95_high,scalingtype,tval,pval,Adj_Rsquared,Lambda))

#visualize the table better using the flextable package
flexall<-flextable(intra) %>% add_header_lines(
values = "Table X. Models for selection") %>%
  bold(i = ~ pval < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

#print to file
