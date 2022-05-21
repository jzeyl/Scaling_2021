library(patchwork)
library(ggrepel)
source("pgls_audiogram_bs.R")

pgls_todo_hm

modellist_bs
modellist_lf
modellist_bh
modellist_hf

i<-5

plotlist<-list()

for(i in seq_along(pgls_todo_hm)){

residtest<-as.data.frame(residuals(pgls_models_list[[i]]))
residtest$residname<-row.names(residtest)

# add avg Corvus and Phalacrocorax values----------------------------------------
phalacrocoraxavg<-residtest[grepl('Phalacrocorax', residtest$residname), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)

corvusavg<-residtest[grepl('Corvus', residtest$residname), ] %>%
  dplyr::select(where(is.numeric)) %>%
  summarise_all(mean, na.rm=T)
names(corvusavg)

cong_avg<-dplyr::bind_rows(residtest,corvusavg,phalacrocoraxavg)
cong_avg$residname[nrow(residtest)+1]<-"Corvus_cornix"
cong_avg$residname[nrow(residtest)+2]<-"Phalacrocorax_carbo"
cong_avg<-cong_avg[-c(grep('Corvus_albus|Corvus_splendens', cong_avg$residname)), ]
cong_avg<-cong_avg[-c(grep('Phalacrocorax_capensis|Phalacrocorax_lucidus|Phalacrocorax_neglectus', cong_avg$residname)), ]
residtest<-cong_avg


joined<-left_join(residtest,limits,by = c("residname" = "spp_aud"))
joined<-joined[which(!is.na(joined$aud_rel)),] #only keep audiogram species

plotit<-function(meas){
  p<-ggplot(joined, aes_string(x = meas, y = "V1"))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("residual")+
    geom_text_repel(aes(label = residname))
  p
}

plotlist[[i]]<-plotit("log(LowHzlimit)")+
plotit("log(HighHzlimit)")+
plotit("log(besthz)")+
plotit("bestsensitivity")+
  plot_annotation(title = paste0("resids of ",pgls_todo_hm[i]))
}

#print to pdf
pdf(paste0(choose.dir(),"/allplots.pdf"),onefile = TRUE)
for(i in seq_along(plotlist)){
  print(plotlist[[i]])
}
dev.off()


lowhz_lm<-lm("log(LowHzlimit)~V1", data = joined)
highz_lm<-lm("log(HighHzlimit)~V1", data = joined)
bestdb_lm<-lm("bestsensitivity~V1", data = joined)
besthz_lm<-lm("log(besthz)~V1", data = joined)
summary(lowhz_lm)
summary(highz_lm)
summary(bestdb_lm)
summary(besthz_lm)


joined %>% filter()
