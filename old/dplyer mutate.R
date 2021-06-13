library(dplyr)


###########
modellist_intra 
spleet<-strsplit(modellist_intra,"~")


vecty<-numeric()
for (i in seq_along(spleet)){
  vecty[i]<-spleet[[i]][1]
}
vecty

vectx<-numeric()
for (i in seq_along(spleet)){
  vectx[i]<-spleet[[i]][2]
}
vectx

# remove variable from within log
vectxsimple<-numeric()
for(i in seq_along(vectx)){
  vectxsimple[i]<-gsub("[\\(\\)]", "", regmatches(vectx, gregexpr("\\(.*?\\)", vectx))[[i]])
}

vectysimple<-numeric()
for(i in seq_along(vecty)){
  vectysimple[i]<-gsub("[\\(\\)]", "", regmatches(vecty, gregexpr("\\(.*?\\)", vecty))[[i]])
}

pgls_models_list_intra<-lapply(modellist_intra,pgls_models)#run pgls


o<-avgdf%>% mutate_at(c(

"TMtotalarea",
"FPtotalarea",

"area_ratio",
"dis_coltip_TMcentroid",

"Umbo_distancetoTMplane",
"meanTMangle",

"RWtotalarea",
"totalECDlength",

"totalEClength",
"CAtotalarea",

"Behind.TM",
"Columella.length.mm",

"Columella.volume.mm3",
"bodymass"
),log)

oselect<-o[,c("waterbirds",
  
  "TMtotalarea",
  "FPtotalarea",
  
  "area_ratio",
  "dis_coltip_TMcentroid",
  
  "Umbo_distancetoTMplane",
  "meanTMangle",
  
  "RWtotalarea",
  "totalECDlength",
  
  "totalEClength",
  "CAtotalarea",
  
  "Behind.TM",
  "Columella.length.mm",
  
  "Columella.volume.mm3",
  "bodymass"
)]


#plot intra
for(i in seq_along(vecty)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           oselect[,vectxsimple[i]]*pgls_models_list_intra[i][[1]]$model$coef[2])
  assign(paste0("slplineiso_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           oselect[,vectxsimple[i]]*geomcoefs_intra[i])
}

runplotpglsintra<-function(e,grp){
  p<-ggplot(oselect, 
            aes_string(x = vectxsimple[e], y = vectysimple[e], shape = grp))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes_string(shape = grp), size = 2)+
    geom_line(aes_string(x = vectxsimple[e],
                         y = paste0("slplineiso_",as.character(e))),
              col = "green", size = 2)+
    geom_line(aes_string(x = vectxsimple[e],
                  y = paste0("slpline_",as.character(e))), 
              col = "black", size = 2)
  p
}

ggarrange(runplotpglsintra(1,grp = "waterbirds"),
runplotpglsintra(2,grp = "waterbirds"),
runplotpglsintra(3,grp = "waterbirds"),
runplotpglsintra(4,grp = "waterbirds"),
runplotpglsintra(5,grp = "waterbirds"),
runplotpglsintra(6,grp = "waterbirds"),
runplotpglsintra(7,grp = "waterbirds"),
runplotpglsintra(8,grp = "waterbirds"),
runplotpglsintra(9,grp = "waterbirds"),
runplotpglsintra(10,grp = "waterbirds"))


#repeat this############
slpline1<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  oselect[,vectxsimple[1]]*pgls_models_list_intra[1][[1]]$model$coef[2]
slpline1iso<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*1





