library(patchwork)
library(dplyr)
########SETUP############

modellist_bs
modellist_lf
modellist_bh
modellist_hf

#get model list from the results table (only significant results)
modellist_sig<-paste0(audio_pgls_results$`Audiogram metric`,"~log(",
                      audio_pgls_results$Coefficients,")")

pgls_models_sig<-lapply(modellist_sig,pgls_models)#run pgls


#split up model column
spltmodel<-strsplit(modellist_sig,"~")
traity<-unlist(lapply(spltmodel, `[[`, 1))
anattraitx<-unlist(lapply(spltmodel, `[[`, 2))

# remove variable from within log
anattrait_simple<-numeric()
for(i in seq_along(anattraitx)){
  anattrait_simple[i]<-gsub("[\\(\\)]", "", regmatches(anattraitx, gregexpr("\\(.*?\\)", anattraitx))[[i]])
}

#only keep significant relationships
audio_pgls_plt<-audio_pgls_plt %>% select(Model, Coefficients, P.val)%>%
  filter(Coefficients!="(Intercept)" &
           P.val <0.05)



#log transform anatomy data for the slope line
logged<-limits%>% mutate_at(audio_pgls_results$Coefficients,log)
loggedselect<-ok[,audio_pgls_results$Coefficients]

categorylist_aud<-audio_pgls_results$category



##########best Hz##############
for(i in seq_along(anattrait_simple)){
  assign(paste0("slpline","_",as.character(i)),
         pgls_models_sig[i][[1]]$model$coef[1]+
           logged[,anattrait_simple[i]]*pgls_models_sig[i][[1]]$model$coef[2])
}

runplot_audio<-function(e){
  p<-ggplot(limits,
            aes_string(x = spltmodel[[e]][2], y = spltmodel[[e]][1]))+
    theme_classic()+
    theme(legend.position = "none")+
    #      axis.text.y = element_blank(),
    #      axis.title.y = element_blank())+
    geom_point(aes_string(shape="aud_rel"), size = 2)+
    geom_line(aes_string(x = anattraitx[e],
                                          y = paste0("slpline_",as.character(e))),
                                col = "black", size = 2)

  p
}

runplot_audio(1)

#PLOT ALL BEST FREQUENCY
runplot_audio(1)+
  runplot_audio(2)+
  runplot_audio(3)+
  runplot_audio(4)+
  runplot_audio(5)+
  runplot_audio(6)+
  runplot_audio(7)+
  runplot_audio(8)+
  runplot_audio(9)+
  runplot_audio(10)+
  runplot_audio(11)+
  runplot_audio(12)+
  runplot_audio(13)+
  runplot_audio(14)+
  runplot_audio(15)+
  runplot_audio(16)+
  runplot_audio(17)+
  runplot_audio(18)+
  plot_annotation(tag_levels = "A")


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

