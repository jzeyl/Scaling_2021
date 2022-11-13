#this script puts the data in long format so that a list of ggplots can be made using a loop


library(ggplot2)
library(ggrepel)
library(cowplot)
library(gridGraphics)
library(patchwork)
library(png)

#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial,
                          vcv = TRUE, na.omit = FALSE,
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

#order of plots is set here, based on the hm dataframe
pgls_todo_hm<-paste0("log(",hm$ymodel_nolog,")~log(HM)")
pgls_models_list1<-lapply(pgls_todo_hm,pgls_models)#run pgls

#put in long format
longdfplotting<-gather(avgdf,key = "earmeasures", value = "earmeasureval",
                       -c(Binomial, Skull.width..mm.,HM, Order,
                          Family))#
longdfplotting$earmeasures<-as.factor(longdfplotting$earmeasures)
longdfplotting$earmeasureval<-as.numeric(longdfplotting$earmeasureval)

pgls_todo_hm

splt_hm<-strsplit(pgls_todo_hm,"~")

splt_hm_anat<-numeric()
for (i in seq_along(splt_hm)){
  splt_hm_anat[i]<-splt_hm[[i]][1]
}
splt_hm_anat

# remove variable from within log
splt_hm_anatsimple<-numeric()
for(i in seq_along(splt_hm_anat)){
  splt_hm_anatsimple[i]<-gsub("[\\(\\)]", "", regmatches(splt_hm_anat, gregexpr("\\(.*?\\)", splt_hm_anat))[[i]])
}
splt_hm_anatsimple

#make list of ear measures to plot
yvarnames<-splt_hm_anatsimple

codes<-hm$ymodel_nolog

#Plotting functions for interaction model. takes the index of the 'yvarnames' list as an argument

options(scipen = 999)
#input geomcoff
runplot_HM_only<-function(e){
  slopeline<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$HM)*
    (pgls_models_list1[e][[1]]$model$coef[2])
  isoline<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$HM)*
   geomcoefs[e]

  pval<-summary(pgls_models_list1[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(" ",paste('R^2 == ',signif(summary(pgls_models_list1[[e]])$r.squared,2))),

    hjust = c(-0.1,-0.5),
    vjust = c(-0.5,-0.5)
  )
  rsq_label <- paste(' R^2 == ', '3')


  ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
            aes(x = log(HM), y = log(earmeasureval), label = Binomial),
            factor = as.factor(waterbirds))+
       theme_classic()+
    theme(legend.position = "none")+{
          #axis.text.x = element_blank(),
          #axis.title.x = element_blank())+{
    if(hm$scalingtype[e] == "Iso")
    geom_point(aes(), size = 2, col = "grey")
    else if(hm$scalingtype[e] == "Hypo")
      geom_point(aes(), size = 2, col = "blue")
    else  if(hm$scalingtype[e] == "Hyper")
      geom_point(aes(), size = 2, col = "red")
      }  +
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(HM),y = slopeline), col = "black", size = 2)+
    #isometric line
    #geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
    #          aes(x = log(HM),y = isoline), col = "grey", size = 2)+
  ylab(paste0("log(",codes[e],")"))+
    #geom_text(data = lbl,aes(x = xpos, y = ypos, label = annotateText,
    #                         hjust = hjust, vjust = vjust, paste = TRUE))+
    annotate(geom = 'text', x = Inf, y = -Inf, 
             label = paste('R^2 == ',signif(summary(pgls_models_list1[[e]])$r.squared,2)), hjust = "inward", vjust = -0.5, parse = TRUE)
    #ggtitle(hm$category[e])#+
}
runplot_HM_only(3)

layout_<-"
ABC
DEF
GHI
JKL"

#export multipanel plot
runplot_HM_only(1)+
runplot_HM_only(2)+
runplot_HM_only(3)+
runplot_HM_only(4)+
runplot_HM_only(5)+
runplot_HM_only(6)+
runplot_HM_only(7)+
runplot_HM_only(8)+
runplot_HM_only(9)+
runplot_HM_only(10)+
runplot_HM_only(11)+
runplot_HM_only(12) +plot_annotation(tag_levels = "A")+
  plot_layout(design = layout_)


ggsave(file=paste0(choose.dir(),"/scatterheadmass jun 6.svg"), 
       width=6, height=10)

