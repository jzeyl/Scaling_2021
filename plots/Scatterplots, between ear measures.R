library(dplyr)
library(patchwork)


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

codeintray<-c("TM",
             "COff",
             "UH",
             "TMA",
             "RW",
             "ES",
             "CL",
             "CL",
             "FP",
             "TM")

codeintrax<-c("FP",
              "TM",
              "TM",
              "TM",
              "FP",
              "CL",
              "CV",
              "FP",
              "CV",
              "CV")

#plot intra
for(i in seq_along(vecty)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           oselect[,vectxsimple[i]]*pgls_models_list_intra[i][[1]]$model$coef[2])
  assign(paste0("slplineiso_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           oselect[,vectxsimple[i]]*geomcoefs_intra[i])
}

runplotpglsintra<-function(e){
  pval<-summary(pgls_models_list_intra[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(" ",paste0(" R2 = ",signif(summary(pgls_models_list_intra[[e]])$r.squared,2))),

    hjust = c(-0.1,-0.5),
    vjust = c(-0.5,-0.5)
  )
  p<-ggplot(oselect,
            aes_string(x = vectxsimple[e], y = vectysimple[e]))+
    theme_classic()+
    theme(legend.position = "none")+{
      if(intra$scalingtype[e*2] == "isometric")
        geom_point(aes(shape = waterbirds), size = 2, col = "grey")
      else if(intra$scalingtype[e*2] == "hypoallometric")
        geom_point(aes(shape = waterbirds), size = 2, col = "blue")
      else  if(intra$scalingtype[e*2] == "hyperallometric")
        geom_point(aes(shape = waterbirds), size = 2, col = "red")
    }  +
    geom_line(aes_string(x = vectxsimple[e],
                         y = paste0("slplineiso_",as.character(e))),
              col = "grey", size = 2)+
    geom_line(aes_string(x = vectxsimple[e],
                  y = paste0("slpline_",as.character(e))),
              col = "black", size = 2)+
    geom_text(data = lbl,aes(x = xpos, y = ypos, label = annotateText,
                             hjust = hjust, vjust = vjust))+
    ggtitle(categorylist_intra[e])+
    ylab(paste0("log(",codeintray[e],")"))+
    xlab(paste0("log(",codeintrax[e],")"))
  p
}

runplotpglsintra(2)

runplotpglsintra(1)+
runplotpglsintra(2)+
runplotpglsintra(3)+
runplotpglsintra(4)+
runplotpglsintra(5)+
runplotpglsintra(6)+
runplotpglsintra(7)+
runplotpglsintra(8)+
runplotpglsintra(9)+
runplotpglsintra(10)+plot_annotation(tag_levels = "A")
