library(patchwork)
library(dplyr)
########SETUP############
#modellist_intra

#classification for the two species withaverage
limits$aud_rel[limits$binomial=="Corvus_cornix"]<-"Congener"
limits$aud_rel[limits$binomial=="Phalacrocorax_carbo"]<-"Congener"

modellist_bs
modellist_lf
modellist_bh
modellist_hf

splt_modellist_bs<-strsplit(modellist_bs,"~")
splt_modellist_lf<-strsplit(modellist_lf,"~")
splt_modellist_bh<-strsplit(modellist_bh,"~")
splt_modellist_hf<-strsplit(modellist_hf,"~")

#different y
vecty_modellist_lf<-numeric()
for (i in seq_along(splt_modellist_lf)){
  vecty_modellist_lf[i]<-splt_modellist_lf[[i]][1]
}
vecty_modellist_lf

vecty_modellist_bh<-numeric()
for (i in seq_along(splt_modellist_bh)){
  vecty_modellist_bh[i]<-splt_modellist_bh[[i]][1]
}
vecty_modellist_bh

vecty_modellist_hf<-numeric()
for (i in seq_along(splt_modellist_hf)){
  vecty_modellist_hf[i]<-splt_modellist_hf[[i]][1]
}
vecty_modellist_hf

#all x variables
vectx_modellist_lf<-numeric()
for (i in seq_along(modellist_bs)){
  vectx_modellist_lf[i]<-splt_modellist_bs[[i]][2]
}
vectx_modellist_lf

# remove variable from within log
vectxsimple_lf<-numeric()
for(i in seq_along(vectx_modellist_lf)){
  vectxsimple_lf[i]<-gsub("[\\(\\)]", "", regmatches(vectx_modellist_lf, gregexpr("\\(.*?\\)", vectx_modellist_lf))[[i]])
}

#log transform anatomy data for the slope line
ok<-limits%>% mutate_at(vectxsimple_lf,log)
okselect<-ok[,vectxsimple_lf]

categorylist_aud<-c(rep("Stiffness",2),
                    rep("Impedance-matching", 4),
                    rep("Auditory endorgan length",1),
                    rep("Input/output areas",3),
                    rep("Animal size",2),
                    rep("Columella size",2))


##########best Hz##############
for(i in seq_along(vectxsimple_lf)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_bh[i][[1]]$model$coef[1]+
           ok[,vectxsimple_lf[i]]*pgls_models_list_bh[i][[1]]$model$coef[2])
}

runplotpgls_aud_bh<-function(e){
  pval<-summary(pgls_models_list_bh[[e]])$coefficients[,4][[2]]

  p<-ggplot(limits,
            aes_string(x = vectx_modellist_lf[e], y = "log(besthz)"))+
    theme_classic()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank())+
    geom_point(aes_string(shape="aud_rel"), size = 2)+{
      if (pval<0.05)  geom_point(aes_string(shape="aud_rel"), size = 2, col = "black")
      else geom_point(aes_string(shape="aud_rel"), size = 2, col = "black", alpha = 0.4)
    } + {
      if (pval<0.05)  geom_line(aes_string(x = vectx_modellist_lf[e],
                                           y = paste0("slpline_",as.character(e))),
                                col = "black", size = 2)
    }+
    #geom_text(data = lbl,aes(x = xpos, y = ypos, label = annotateText,
    #                         hjust = hjust, vjust = vjust))+
    #scale_shape_manual(values=c(17, 16))+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' R^2 == ',signif(summary(pgls_models_list_bh[[e]])$r.squared,2)), hjust = -0.05, vjust = -0.1, parse = TRUE)+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' P == ',signif(summary(pgls_models_list_bh[[e]])$coefficients[,4][[2]],2)), hjust = -0.05, vjust = -2.1, parse = TRUE)

    ggtitle(categorylist_aud[e])
  p
}

runplotpgls_aud_bh(10)+  geom_text(aes(label=binomial), angle = 30)

#PLOT ALL BEST FREQUENCY
runplotpgls_aud_bh(1)+
  runplotpgls_aud_bh(2)+
  runplotpgls_aud_bh(3)+
  runplotpgls_aud_bh(4)+
  runplotpgls_aud_bh(5)+
  runplotpgls_aud_bh(6)+
  runplotpgls_aud_bh(7)+
  runplotpgls_aud_bh(8)+
  runplotpgls_aud_bh(9)+
  runplotpgls_aud_bh(10)+
  runplotpgls_aud_bh(11)+
  runplotpgls_aud_bh(12)+
  runplotpgls_aud_bh(13)+
  runplotpgls_aud_bh(14)+plot_annotation(tag_levels = "A")


################signif bh##################
runplotpgls_aud_bh(8)+
runplotpgls_aud_bh(10)+
runplotpgls_aud_bh(11)+plot_annotation(tag_levels = "A")


##############best sensitivity##############
for(i in seq_along(vectxsimple_lf)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_bs[i][[1]]$model$coef[1]+
           ok[,vectxsimple_lf[i]]*pgls_models_list_bs[i][[1]]$model$coef[2])
}

runplotpgls_aud_bs<-function(e){
  pval<-summary(pgls_models_list_bs[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(paste0("P = ",
                            signif(summary(pgls_models_list_bs[[e]])$coefficients[,4][[2]],2),
                            ",",
                            " R2 = ",signif(summary(pgls_models_list_bs[[e]])$r.squared,2))
                     ,""),
    hjust = c(-0.1,-0),
    vjust = c(-1,-0)
                     )

  p<-ggplot(limits,
            aes_string(x = vectx_modellist_lf[e], y = "bestsensitivity"))+
    theme_classic()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank())+{
      if (pval<0.05)  geom_point(aes_string(shape="aud_rel"), size = 2, col = "black")
       else geom_point(aes_string(shape="aud_rel"), size = 2, col = "black", alpha = 0.4)
    } + {
      if (pval<0.05)  geom_line(aes_string(x = vectx_modellist_lf[e],
                         y = paste0("slpline_",as.character(e))),
              col = "black", size = 2)
    }+
    scale_shape_manual(values=c(17, 16))+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' R^2 == ',signif(summary(pgls_models_list_bs[[e]])$r.squared,2)), hjust = -0.05, vjust = -0.1, parse = TRUE)+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' P == ',signif(summary(pgls_models_list_bs[[e]])$coefficients[,4][[2]],2)), hjust = -0.05, vjust = -2.1, parse = TRUE)
  ggtitle(categorylist_aud[e])
  p
}

runplotpgls_aud_bs(1)

runplotpgls_aud_bs(1)+
  runplotpgls_aud_bs(2)+
  runplotpgls_aud_bs(3)+
  runplotpgls_aud_bs(4)+
  runplotpgls_aud_bs(5)+
  runplotpgls_aud_bs(6)+
  runplotpgls_aud_bs(7)+
  runplotpgls_aud_bs(8)+
  runplotpgls_aud_bs(9)+
  runplotpgls_aud_bs(10)+
  runplotpgls_aud_bs(11)+
  runplotpgls_aud_bs(12)+
  runplotpgls_aud_bs(13)+
  runplotpgls_aud_bs(14)+plot_annotation(tag_levels = "A")

####signif bs####
(runplotpgls_aud_bs(1)+
runplotpgls_aud_bs(2)+
runplotpgls_aud_bs(3)+
runplotpgls_aud_bs(4)+
runplotpgls_aud_bs(5))/
(runplotpgls_aud_bs(7)+
runplotpgls_aud_bs(8)+
runplotpgls_aud_bs(9)+
runplotpgls_aud_bs(10)+
runplotpgls_aud_bs(13)+
runplotpgls_aud_bs(14))+plot_annotation(tag_levels = "A")

##############low frequency limit##############
for(i in seq_along(vectxsimple_lf)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_lf[i][[1]]$model$coef[1]+
           ok[,vectxsimple_lf[i]]*pgls_models_list_lf[i][[1]]$model$coef[2])
}

runplotpgls_aud_lf<-function(e){
  pval<-summary(pgls_models_list_lf[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(paste0("P = ",
                            signif(summary(pgls_models_list_lf[[e]])$coefficients[,4][[2]],2),
                            ",",
                            " R2 = ",signif(summary(pgls_models_list_lf[[e]])$r.squared,2))
                     ,""),
    hjust = c(-0.1,-0),
    vjust = c(-1,-0)
  )
  p<-ggplot(limits,
            aes_string(x = vectx_modellist_lf[e], y = "log(LowHzlimit)"))+
    theme_classic()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank())+{
      if (pval<0.05)  geom_point(aes_string(shape="aud_rel"), size = 2, col = "black")
      # geom_line(aes_string(x = vectx_modellist_lf[e],
      #                      y = paste0("slpline_",as.character(e))),
      #           col = "red", size = 2)
      else geom_point(aes_string(shape="aud_rel"), size = 2, col = "black", alpha = 0.5)
    } +{
      if (pval<0.05)  geom_line(aes_string(x = vectx_modellist_lf[e],
                                           y = paste0("slpline_",as.character(e))),
                                col = "black", size = 2)
    }+
    scale_shape_manual(values=c(17, 16))+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' R^2 == ',signif(summary(pgls_models_list_lf[[e]])$r.squared,2)), hjust = -0.05, vjust = -0.1, parse = TRUE)+
    annotate(geom = 'text', x = -Inf, y = -Inf, label = paste(' ',' P == ',signif(summary(pgls_models_list_lf[[e]])$coefficients[,4][[2]],2)), hjust = -0.05, vjust = -2.1, parse = TRUE)
  ggtitle(categorylist_aud[e])
  #geom_line(aes_string(x = vectxsimple[e],
  #                     y = paste0("slpline_",as.character(e))),
  #          col = "black", size = 2)
  p
}

runplotpgls_aud_lf(1)


runplotpgls_aud_lf(1)+
runplotpgls_aud_lf(2)+
runplotpgls_aud_lf(3)+
runplotpgls_aud_lf(4)+
runplotpgls_aud_lf(5)+
runplotpgls_aud_lf(6)+
runplotpgls_aud_lf(7)+
runplotpgls_aud_lf(8)+
runplotpgls_aud_lf(9)+
runplotpgls_aud_lf(10)+
runplotpgls_aud_lf(11)+
runplotpgls_aud_lf(12)+
runplotpgls_aud_lf(13)+
  runplotpgls_aud_lf(14)+plot_annotation(tag_levels = "A")
#############signif################
runplotpgls_aud_lf(1)+
runplotpgls_aud_lf(2)+
runplotpgls_aud_lf(4)+
plot_annotation(tag_levels = "A")


##############high frequency limit##############
for(i in seq_along(vectxsimple_lf)){
  assign(paste0("slpline","_",as.character(i)),pgls_models_list_hf[i][[1]]$model$coef[1]+
           ok[,vectxsimple_lf[i]]*pgls_models_list_hf[i][[1]]$model$coef[2])
}

runplotpgls_aud_hf<-function(e){
  pval<-summary(pgls_models_list_hf[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(paste0("P = ",
                            signif(summary(pgls_models_list_hf[[e]])$coefficients[,4][[2]],2),
                            ",",
                            " R2 = ",signif(summary(pgls_models_list_hf[[e]])$r.squared,2))
                     ,""),
    hjust = c(-0.1,-0),
    vjust = c(-1,-0)
  )
  p<-ggplot(limits,
            aes_string(x = vectx_modellist_lf[e], y = "log(HighHzlimit)"))+
    theme_classic()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank())+{
      if (pval<0.05)  geom_point(aes_string(shape="aud_rel"), size = 2, col = "black")
      # geom_line(aes_string(x = vectx_modellist_lf[e],
      #                      y = paste0("slpline_",as.character(e))),
      #           col = "red", size = 2)
      else geom_point(aes_string(shape="aud_rel"), size = 2, col = "black", alpha = 0.5)
    } +{
      if (pval<0.05)  geom_line(aes_string(x = vectx_modellist_lf[e],
                                           y = paste0("slpline_",as.character(e))),
                                col = "black", size = 2)
    }+
    scale_shape_manual(values=c(17, 16))+
    annotate(geom = 'text', x = Inf, y = -Inf, label = paste(' ',' R^2 == ',signif(summary(pgls_models_list_hf[[e]])$r.squared,2)), hjust = 0.99, vjust = -0.1, parse = TRUE)+
    annotate(geom = 'text', x = Inf, y = -Inf, label = paste(' ',' P == ',signif(summary(pgls_models_list_hf[[e]])$coefficients[,4][[2]],2)), hjust = 0.99, vjust = -2.1, parse = TRUE)
  ggtitle(categorylist_aud[e])
  p
}

runplotpgls_aud_hf(1)

runplotpgls_aud_hf(1)+
  runplotpgls_aud_hf(2)+
  runplotpgls_aud_hf(3)+
  runplotpgls_aud_hf(4)+
  runplotpgls_aud_hf(5)+
  runplotpgls_aud_hf(6)+
  runplotpgls_aud_hf(7)+
  runplotpgls_aud_hf(8)+
  runplotpgls_aud_hf(9)+
  runplotpgls_aud_hf(10)+
  runplotpgls_aud_hf(11)+
  runplotpgls_aud_hf(12)+
  runplotpgls_aud_hf(13)+
  runplotpgls_aud_hf(14)+plot_annotation(tag_levels = "A")


##########signif###############
runplotpgls_aud_hf(3)+
runplotpgls_aud_hf(4)+
runplotpgls_aud_hf(5)+
runplotpgls_aud_hf(7)+plot_annotation(tag_levels = "A")

