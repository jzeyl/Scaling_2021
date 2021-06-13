#pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = limits,#[avgdf$Category!="Terrestrial",]
                          names.col =binomial, 
                          vcv = TRUE, na.omit = F, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped


#Head mass only
#modellist<-pgls_todo_hm
pgls_models_list<-lapply(modellist_audiom,pgls_models)#run pgls


#make list of dataframes with the PGLS outputs. 
tbllist_audiogram<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_audiogram[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_audiogram[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_audiogram[[i]]$Model<-modellist_audiom[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_audiogram[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_audiogram[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_audiogram[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_audiogram[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  
}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_audiogram)){
  tbllist_audiogram[[i]]$Coefficients<-row.names(tbllist_audiogram[[i]])
  tbllist_audiogram[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_audiogram[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_audiogram[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_audiogram[[i]], is.numeric))# Identify numeric columns
  tbllist_audiogram[[i]]<-cbind(tbllist_audiogram[[i]][,which(character_cols)],signif(tbllist_audiogram[[i]][,which(numeric_cols)], digits = 2))
  colnames(tbllist_audiogram[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
   tbllist_audiogram[[i]]$Model[2:nrow(tbllist_audiogram[[i]])]<-""
  row.names(tbllist_audiogram[[i]])<-c()#remove row names
  print(tbllist_audiogram[[i]])
}

audiogrampgls_audiom<-do.call(rbind.data.frame,tbllist_audiogram)

audiogrampgls_audiom <- subset(audiogrampgls_audiom, select = c(Model,Coefficients,Estimate, `Std. Error`,P.val,Adj_Rsquared,Lambda))

#visualize the table better using the flextable package
flexall<-flextable(audiogrampgls_audiom) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

#write table to word file
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)
