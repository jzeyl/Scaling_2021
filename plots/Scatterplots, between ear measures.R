library(dplyr)
library(patchwork)


###########get order of plots here based on intra df
modellist_intra<-paste0("log(",intra$ymodel_nolog,")~log(",
                        intra$Coefficients,")")

#split up the model formula to get x and y components
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

# remove the "log" from each character string
vectysimple<-numeric()
for(i in seq_along(vecty)){
  vectysimple[i]<-gsub("[\\(\\)]", "", regmatches(vecty, gregexpr("\\(.*?\\)", vecty))[[i]])
}

vectxsimple<-numeric()
for(i in seq_along(vectx)){
  vectxsimple[i]<-gsub("[\\(\\)]", "", regmatches(vectx, gregexpr("\\(.*?\\)", vectx))[[i]])
}

#re-run the pgls models between ear measures
pgls_models_list_intra<-lapply(modellist_intra,pgls_models)#run pgls

#logtransform measures of interest
o<-avgdf%>% mutate_at(c(

"TM",
"FP",

"TM_FP",
"COffset",

"UH",
"TMA",

"RW",
"ECD",

"ES",

"Air",
"CL",

"CV",
"BM_lit"
),log)


#extract slope lines from pgls models
for(i in seq_along(vecty)){
  assign(paste0("slpline","_",as.character(i)),
         pgls_models_list_intra[i][[1]]$model$coef[1]+
           o[,vectxsimple[i]]*pgls_models_list_intra[i][[1]]$model$coef[2])
  assign(paste0("slplineiso_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           o[,vectxsimple[i]]*geomcoefs_intra[i])
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
  p<-ggplot(o,
            aes_string(x = vectxsimple[e], y = vectysimple[e]))+
    theme_classic()+
    theme(legend.position = "none")+{
      if(intra$scalingtype[e] == "Iso")
        geom_point(aes(), size = 2, col = "grey")
      else if(intra$scalingtype[e] == "Hypo")
        geom_point(aes(), size = 2, col = "blue")
      else  if(intra$scalingtype[e] == "Hyper")
        geom_point(aes(), size = 2, col = "red")
    }  +
    #geom_line(aes_string(x = vectxsimple[e],#isometric slope line
    #                     y = paste0("slplineiso_",as.character(e))),
    #          col = "grey", size = 2)+
    geom_line(aes_string(x = vectxsimple[e],
                  y = paste0("slpline_",as.character(e))),
              col = "black", size = 2)+
    #geom_text(data = lbl,aes(x = xpos, y = ypos, label = annotateText,
    #                         hjust = hjust, vjust = vjust))+
    annotate(geom = 'text', x = Inf, y = -Inf, label = paste('R^2 == ',signif(summary(pgls_models_list_intra[[e]])$r.squared,2)), hjust = "inward", vjust = -0.5, parse = TRUE)+

    ggtitle(categorylist_intra[e])+
    ylab(paste0("log(",vectysimple[e],")"))+
    xlab(paste0("log(",vectxsimple[e],")"))
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
runplotpglsintra(10)+plot_annotation(tag_levels = "A")+
  plot_layout(design = layt)

layt<-"
12345
6####
789A#
"

ggsave(file=paste0(choose.dir(),"/scatter_intra apr 4.svg"), width=10, height=8)
