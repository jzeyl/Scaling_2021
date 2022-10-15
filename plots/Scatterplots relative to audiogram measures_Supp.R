library(patchwork)
library(dplyr)
########SETUP############

#limits<- limits %>% rename(., `Best Sensitivity\n (dB)` = bestsensitivity)


modellist_bs
modellist_lf
modellist_bh
modellist_hf

#include the significant relationship with head mass
#modellist_bh<-append(modellist, "log(besthz)~(HM)")
### combine results from regressions for each measure into a single datagrame
audio_pgls_results<-bind_rows(audiogrampgls_bh,
                              audiogrampgls_bs,
                              audiogrampgls_lf,
                              audiogrampgls_hf)
audio_pgls_results$CI95_low<-audio_pgls_results$Estimate-audio_pgls_results$`Std. Error`*1.96
audio_pgls_results$CI95_high<-audio_pgls_results$Estimate+audio_pgls_results$`Std. Error`*1.96

#combine estimate +/- 95 CI into one cell
audio_pgls_results$pglsslope<-paste0(audio_pgls_results$Estimate," (",
                                     format(round(audio_pgls_results$CI95_low, 3), nsmall = 3),
                                     ",",
                                     format(round(audio_pgls_results$CI95_high, 3), nsmall = 3),
                                     ")")

#split up model column
spltmodel<-strsplit(audio_pgls_results$Model,"~")
#spltmodel<-gsub('bestsensitivity', 'Best Sensitivity (dB)',spltmodel)
audio_pgls_results$`Audiogram metric`<-unlist(lapply(spltmodel, `[[`, 1))
audio_pgls_results$anattraitx<-unlist(lapply(spltmodel, `[[`, 2))


#arrange by audiogram metric, category, and R2
audio_pgls_results <-audio_pgls_results %>%
  arrange(factor(audio_pgls_results$`Audiogram metric`),
          factor(category,levels = c("Impedance match",
                                     "Stiffness",
                                     "Input/output areas",
                                     "Auditory endorgan length",
                                     "Columella size")),
          desc(Adj_Rsquared))

#split up model column
#spltmodel<-strsplit(audio_pgls_results$Model,"~")
#audio_pgls_results$`Audiogram metric`<-unlist(lapply(spltmodel, `[[`, 1))
#audio_pgls_results$anattraitx<-unlist(lapply(spltmodel, `[[`, 2))
#
##only keep significant relationships
audio_pgls_results<-audio_pgls_results %>%
  filter(Coefficients!="(Intercept)" &
           P.val <0.05)


#get model list from the results table (only significant results)
modellist_sig<-paste0(audio_pgls_results$`Audiogram metric`,"~",
                      audio_pgls_results$Coefficients)

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
#audio_pgls_plt<-audio_pgls_plt %>% select(Model, Coefficients, P.val)%>%
# filter(Coefficients!="(Intercept)" &
#          P.val <0.05)



#log transform anatomy data for the slope line
#logged<-limits%>% mutate_at(vars(TM:UH),log)


##########best Hz##############
for(i in seq_along(anattraitx)){
  assign(paste0("slpline","_",as.character(i)),
         pgls_models_sig[i][[1]]$model$coef[1]+
           joined[,anattraitx[i]]*pgls_models_sig[i][[1]]$model$coef[2])
}

anattraitx2<-gsub("resid_log_", "Resid. ", anattraitx)
anattraitx3<-gsub("_vslog_HM_", "",anattraitx2)

runplot_audio<-function(e){
  p<-ggplot(joined,
            aes_string(x = spltmodel[[e]][2], y = spltmodel[[e]][1]))+
    theme_classic()+
    #theme(legend.position = "none")+
    #      axis.text.y = element_blank(),
    #      axis.title.y = element_blank())+
    geom_point(aes_string(shape="aud_rel"), size = 2)+
    geom_line(aes_string(x = anattraitx[e],
                                          y = paste0("slpline_",as.character(e))),
                                col = "black", size = 2)+
    xlab(anattraitx3[e])+{

if(e<11)
  ylab("Best Sensitivity\n (dB)")
 #else if(e >10 & e < 13)
 #  ylab("Best Frequency\n (Hz)")
else if(traity[e] == "log(LowHzlimit)")
  ylab("Low Frequency\n Limit (Hz)")
else if(traity[e] == "log(HighHzlimit)")
 ylab("High Frequency\n Limit (Hz)")
                                }
  p
}

runplot_audio(1)

#design<-"
#ABCKL
#DEFMN
#GHIOP
#J##QR
#"
design<-"
ABCDE
FGHIJ
KLMN#
O####
"
xs = c(log(min(limits$HM, na.rm = T)), log(max(limits$HM, na.rm = T)))
beta = c(8.4, -0.26)
ys = cbind(1, xs) %*% beta

hmplot<-ggplot(limits,aes(x = log(HM), y = log(besthz)))+
  theme_classic()+
  geom_point(aes_string(shape="aud_rel"), size = 2)+
  #geom_abline(intercept = 8.4, slope = -0.26,
   #         col = "black", size = 2, fullrange = T)
geom_segment(x = log(min(limits$HM, na.rm = T)), xend = log(max(limits$HM, na.rm = T)),
             y = ys[1],
             yend = ys[2], size = 2)+
  ylab("Best Frequency\n(Hz)")
  #slope = -0.26, intercept = 8.4
hmplot

#PLOT ALL
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
  #runplot_audio(15)+
  #runplot_audio(16)+
  #runplot_audio(17)+
  #runplot_audio(18)+
  #runplot_audio(19)+
  #runplot_audio(20)+
  #runplot_audio(21)+
  hmplot+
  plot_annotation(tag_levels = list(c(
    "A","","","","",
    "","","","","",
    "B","","","","C")))+

    #,"",
    #"","","","","",
    #"C","D","","","",
    #"D","","","",""+
  plot_layout(design = design, guides = "collect")




ggsave(file=paste0(choose.dir(),"/audiogramscatter_supp_RESID jun 17.svg"), width=10, height=10)
