#this script puts the data in long format so that a list of ggplots can be made using a loop

library(ggplot2)
library(ggrepel)
library(cowplot)
library(gridGraphics)
library(patchwork)
library(png)

#earimg<-readPNG("C:/Users/jeffz/Desktop/ear.png", native = T)

#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

pgls_todo_hm
pgls_models_list1<-lapply(pgls_todo_hm,pgls_models)#run pgls

#pgls_todo_hm_w<- paste(pgls_todo_hm,"+waterbirds")
#pgls_models_list2<-lapply(pgls_todo_hm_w,pgls_models)#run pgls
#
pgls_todo_bm<- gsub("Head.mass..g.","bodymass",pgls_todo_hm)
pgls_models_list3<-lapply(pgls_todo_bm,pgls_models)#run pgls
#
#modellist_intra


#put in long format
longdfplotting<-gather(avgdf,key = "earmeasures", value = "earmeasureval", 
                       -c(Binomial, Skull.width..mm.,Head.mass..g.,waterbirds, Order,
                          Family))#
longdfplotting$earmeasures<-as.factor(longdfplotting$earmeasures)
longdfplotting$earmeasureval<-as.numeric(longdfplotting$earmeasureval)
#longdfplotting$Orderpdonly<-ifelse(longdfplotting$Category== "Pursuit diving", longdfplotting$Binomial, "")
#longdfplotting$SW1<-ifelse(longdfplotting$Binomial == "Ardenna gravis", "Ardenna gravis", "")
#longdfplotting$SW2<-ifelse(longdfplotting$Binomial == "Ardenna_grisea", longdfplotting$Binomial, "")

#textdf <- longdfplotting[rbind(longdfplotting$Binomial == "Ardenna gravis",longdfplotting$Binomial,"")
#                               longdfplotting$Binomial =="Ardenna_grisea"), ]


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

codes<-c(
  "AR",
  "Columella offset",
  "Umbo height",
  "TM angle",
  "ECD length",
  "TM area",
  "FP area",
  "RW area",
  "ES length",
  "Cranial air",
  "Columella length",
  "Columella volume",
  "Body mass"
)
#Plotting functions for interaction model. takes the index of the 'yvarnames' list as an argument

options(scipen = 999)
#input geomcoff
runplot_HM_only<-function(e){
  slopeline<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list1[e][[1]]$model$coef[2])
  isoline<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
   geomcoefs[e]
  
  pval<-summary(pgls_models_list1[[e]])$coefficients[,4][[2]]
  lbl<-data.frame(
    xpos = c(-Inf,-Inf),
    ypos = c(-Inf,-Inf),
    annotateText = c(" ",paste0(" R2 = ",signif(summary(pgls_models_list1[[e]])$r.squared,2))),
                     
    hjust = c(-0.1,-0.5),
    vjust = c(-0.5,-0.5)
  )
  
  
  ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(waterbirds))+
       theme_classic()+
    theme(legend.position = "none",
          #axis.text.x = element_blank(),
          axis.title.x = element_blank())+{
    if(hm$scalingtype[e*2] == "isometric")
    geom_point(aes(shape = waterbirds), size = 2, col = "grey")
    else if(hm$scalingtype[e*2] == "hypoallometric")
      geom_point(aes(shape = waterbirds), size = 2, col = "blue")
    else  if(hm$scalingtype[e*2] == "hyperallometric") 
      geom_point(aes(shape = waterbirds), size = 2, col = "red")
      }  +
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = slopeline), col = "black", size = 2)+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = isoline), col = "grey", size = 2)+
  ylab(paste0("log(",codes[e],")"))+
    geom_text(data = lbl,aes(x = xpos, y = ypos, label = annotateText,
                             hjust = hjust, vjust = vjust))+
    ggtitle(categorylist[e])#+
    #inset_element(p = earimg,
    #              left = 0.8,
    #              bottom = 0.1,
    #              right = 1,
    #              top = 0.2)
  
}
runplot_HM_only(3)

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
runplot_HM_only(12)+
  runplot_HM_only(13) +plot_annotation(tag_levels = "A")

#BM
runplot_BM_only<-function(e){
  parampd<-pgls_models_list3[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$bodymass)*
    (pgls_models_list3[e][[1]]$model$coef[2])
  
  
  ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
         aes(x = log(bodymass), y = log(earmeasureval), label = Binomial), 
         factor = as.factor(waterbirds))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = waterbirds), size = 2)+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(bodymass),y = parampd), col = "black")+
    
    ylab(yvarnames[e])
}
runplot_BM_only(3)

ggarrange(
  runplot_BM_only(1),
  runplot_BM_only(2),
  runplot_BM_only(3),
  runplot_BM_only(4),
  runplot_BM_only(5),
  runplot_BM_only(6),
  runplot_BM_only(7),
  runplot_BM_only(8),
  runplot_BM_only(9),
  runplot_BM_only(10),
  runplot_BM_only(11),
  runplot_BM_only(12)
)


runplot_waterbirds_main<-function(e){
  parampd<-pgls_models_list_W[e][[1]]$model$coef[1]+#intercept
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial")$Head.mass..g.)*#heamass times slope
    (pgls_models_list_W[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list_W[e][[1]]$model$coef[1]+pgls_models_list_W[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface")$Head.mass..g.)*#heamass times slope
    (pgls_models_list_W[e][[1]]$model$coef[2])
    p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(waterbirds))+
    theme_classic()+
    theme(legend.position = c(0.8,0.2))+
    geom_point(aes(color = waterbirds))+
    scale_color_manual(values=c("black","red","blue","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial"),
              aes(x = log(Head.mass..g.),y = parampd), col = "black")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface"),
              aes(x = log(Head.mass..g.),y = paramsf), col = "red")+
     #geom_text_repel(aes(label = Orderpdonly))+
    ylab(paste0("log(",yvarnames[e],")"))
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_waterbirds_main(9)


runplot_category_main<-function(e){
  parampd<-pgls_models_list2[e][[1]]$model$coef[1]+#intercept
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial")$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list2[e][[1]]$model$coef[1]+pgls_models_list2[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface")$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  paramsT<-(pgls_models_list2[e][[1]]$model$coef[1]+pgls_models_list2[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit")$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","darkgrey","blue","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial"),
              aes(x = log(Head.mass..g.),y = parampd), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface"),
              aes(x = log(Head.mass..g.),y = paramsf), col = "darkgrey")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit"),
              aes(x = log(Head.mass..g.),y = paramsT), col = "blue")+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(paste0("log(",yvarnames[e],")"))
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_category_main(2)
runplot_category_main(6)#TM angle
runplot_category_main(8)#RW

runplot_plungedistinct_main<-function(e){
  parampd<-pgls_models_list4[e][[1]]$model$coef[1]+#intercept
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial")$Head.mass..g.)*#heamass times slope
    (pgls_models_list4[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list4[e][[1]]$model$coef[1]+pgls_models_list4[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Plunging")$Head.mass..g.)*#heamass times slope
    (pgls_models_list4[e][[1]]$model$coef[2])
  paramsT<-(pgls_models_list4[e][[1]]$model$coef[1]+pgls_models_list4[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface")$Head.mass..g.)*#heamass times slope
    (pgls_models_list4[e][[1]]$model$coef[2])
  paramslast<-(pgls_models_list4[e][[1]]$model$coef[1]+pgls_models_list4[e][[1]]$model$coef[5])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit")$Head.mass..g.)*#heamass times slope
    (pgls_models_list4[e][[1]]$model$coef[2])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(plungedistinct))+
    theme_classic()+
    #geom_encircle()+
    theme(legend.position = "none")+
    geom_point(aes(color = plungedistinct))+
    scale_color_manual(values=c("green","black","darkgrey","blue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial"),
              aes(x = log(Head.mass..g.),y = parampd), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Plunging"),
              aes(x = log(Head.mass..g.),y = paramsf), col = "black")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface"),
              aes(x = log(Head.mass..g.),y = paramsT), col = "darkgrey")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit"),
              aes(x = log(Head.mass..g.),y = paramslast), col = "blue")+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(paste0("log(",yvarnames[e],")"))
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_plungedistinct_main(4)#col offset
runplot_plungedistinct_main(5)#umbo height
runplot_plungedistinct_main(7)#Ec length

runplot_plungedistinct_main(10)#air
ggsave("D:/Analysis_plots/air.pdf", width=4, height=4)


ggarrange(runplot_plungedistinct_main(4),#col offset
          runplot_plungedistinct_main(5),#umbo height
          runplot_category_main(6),#TM angle
          runplot_plungedistinct_main(7),#Ec length
          runplot_category_main(8),#RW
          nrow = 1)
ggsave("D:/Analysis_plots/sw Z2.pdf", width=8, height=4)

e<-5
runplot_plungedistinct_int<-function(e){
  parampd<-pgls_models_list5[e][[1]]$model$coef[1]+#terr
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial")$Head.mass..g.)*
    (pgls_models_list5[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list5[e][[1]]$model$coef[1]+pgls_models_list5[e][[1]]$model$coef[4])+#plunging
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface")$Head.mass..g.)*
    (pgls_models_list5[e][[1]]$model$coef[2]+pgls_models_list5[e][[1]]$model$coef[7])
  paramsT<-(pgls_models_list5[e][[1]]$model$coef[1]+pgls_models_list5[e][[1]]$model$coef[5])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit")$Head.mass..g.)*
    (pgls_models_list5[e][[1]]$model$coef[2]+pgls_models_list5[e][[1]]$model$coef[8])
  paramsplg<-(pgls_models_list5[e][[1]]$model$coef[1]+pgls_models_list5[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Plunging")$Head.mass..g.)*
    (pgls_models_list5[e][[1]]$model$coef[2]+pgls_models_list5[e][[1]]$model$coef[6])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(plungedistinct))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = plungedistinct))+
    scale_color_manual(values=c("green","black","darkgrey","blue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Terrestrial"),
              aes(x = log(Head.mass..g.),y = parampd), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Surface"),
              aes(x = log(Head.mass..g.),
                  y = paramsf), col = "black")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Underwater pursuit"),
              aes(x = log(Head.mass..g.),y = paramsT), col = "blue")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]&longdfplotting$plungedistinct=="Plunging"),
              aes(x = log(Head.mass..g.),y = paramsplg), col = "black")+
    #scale_x_log10()+
    #scale_y_log10()+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(paste0("log(",yvarnames[e],")"))
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_plungedistinct_int(3)
runplot_plungedistinct_int(11)
runplot_plungedistinct_int(2)
runplot_plungedistinct_int(3)




ggarrange(runplot_plungedistinct_int(1),
          runplot_category_main(2),
          runplot_plungedistinct_int(3), nrow = 1)
ggsave("D:/Analysis_plots/sw Z1.pdf", width=8, height=4)


ggarrange(runplot_waterbirds_main(9),
runplot_plungedistinct_int(11))
ggsave("D:/Analysis_plots/CA_collengt.pdf", width=8, height=4)

ggarrange(runplot_category_main(9),
          runplot_plungedistinct_int(11))
ggsave("D:/Analysis_plots/CA_collengt2.pdf", width=8, height=4)


runplot_int_cat<-function(e){
  parampd<-pgls_models_list3[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list3[e][[1]]$model$coef[1]+pgls_models_list3[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2]+pgls_models_list3[e][[1]]$model$coef[5])
  paramsT<-(pgls_models_list3[e][[1]]$model$coef[1]+pgls_models_list3[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2]+pgls_models_list3[e][[1]]$model$coef[6])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = parampd), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = paramsf), col = "blue")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = paramsT), col = "darkgrey")+
    #scale_x_log10()+
    #scale_y_log10()+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(yvarnames[e])
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_int_cat(12)

ggarrange(runplot_plungedistinct_int(11),runplot_int_cat(12), labels = c("A","B"))
ggsave("D:/Analysis_plots/columella lebgth oct 5b.pdf", width=4, height=4)

amphipyrinae_clade <- findMRCA(birdtreels, c("Alca_torda","Columba_livia"))



#####################log scale plots
runplot_HM_only<-function(e){
  parampd<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list1[e][[1]]$model$coef[2])
   p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = Head.mass..g., y = earmeasureval, label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(parampd)), col = "black")+
    scale_x_log10()+
    scale_y_log10()+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(yvarnames[e])
  ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_HM_only(1)
runplot_category_main<-function(e){
  parampd<-pgls_models_list2[e][[1]]$model$coef[1]+#intercept
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list2[e][[1]]$model$coef[1]+pgls_models_list2[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  paramsT<-(pgls_models_list2[e][[1]]$model$coef[1]+pgls_models_list2[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*#heamass times slope
    (pgls_models_list2[e][[1]]$model$coef[2])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = Head.mass..g., y = earmeasureval, label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(parampd)), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsf)), col = "blue")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsT)), col = "darkgrey")+
    scale_x_log10()+
    scale_y_log10()+annotation_logticks(sides = "lr")+
    geom_text_repel(aes(label = Binomial))+
    ylab(yvarnames[e])
  ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_category_main(1)+annotation_logticks(sides = "lr")
runplot_category_main(10)
runplot_int<-function(e){
  parampd<-pgls_models_list3[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list3[e][[1]]$model$coef[1]+pgls_models_list3[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2]+pgls_models_list3[e][[1]]$model$coef[5])
  paramsT<-(pgls_models_list3[e][[1]]$model$coef[1]+pgls_models_list3[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list3[e][[1]]$model$coef[2]+pgls_models_list3[e][[1]]$model$coef[6])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = Head.mass..g., y = earmeasureval, label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(parampd)), col = "green")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsf)), col = "blue")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsT)), col = "darkgrey")+
    scale_x_log10()+
    scale_y_log10()+
    #geom_text_repel(aes(label = Orderpdonly))+
    ylab(yvarnames[e])
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_int(2)
######################3333


1"Columella.length.mm"-interation
2"Columella.volume.mm3"    -interaction
3"totalEClength"           -main effect
4"meanTMangle"    -main effect   
5"Air.volume"           -main
6"FPtotalarea" -interaction            
7"TMtotalarea" -interaction    
8"RWtotalarea" -main effect      
9"CAtotalarea" -HM only            
10"area_ratio"   -main effect
11"Umbo_distancetoTMplane" -main effect
"coltip_distancetoTMplane"
"angle_FP_TM"            
"angle_Col_EC"           
"totalECDlength"          
16"dis_coltip_TMcentroid" -main effect



#make list of ggplots, for each variable
scatterlist1<-list()
for(i in 1:16){
  scatterlist1[[i]]<-runplot_HM_only(i)#as_grob
}

scatterlist2<-list()
for(i in 1:16){
  scatterlist2[[i]]<-runplot_category_main(i)#as_grob
}

scatterlist3<-list()
for(i in 1:16){
  scatterlist3[[i]]<-runplot_int(i)#as_grob
}

#scatterplots
collength<-scatterlist3[[1]]
colvol<-scatterlist3[[2]]
ESlength<-scatterlist2[[3]]
TMangle<-scatterlist2[[4]]
Air<-scatterlist2[[5]]
FP<-scatterlist3[[6]]
TMarea<-scatterlist3[[7]]
RW<-scatterlist2[[8]]
CA<-scatterlist1[[9]]
ARatio<-scatterlist2[[10]]
umboheight<-scatterlist2[[11]]
offset<-scatterlist2[[16]]




#scatterplot highlighting low frequency grouping
runplotLF<-function(e){
  parampd<-pgls_models_list[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list[e][[1]]$model$coef[2])
  paramsf<-(pgls_models_list[e][[1]]$model$coef[1]+pgls_models_list[e][[1]]$model$coef[3])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list[e][[1]]$model$coef[2]+pgls_models_list[e][[1]]$model$coef[5])
  paramsT<-(pgls_models_list[e][[1]]$model$coef[1]+pgls_models_list[e][[1]]$model$coef[4])+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list[e][[1]]$model$coef[2]+pgls_models_list[e][[1]]$model$coef[6])
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = Head.mass..g., y = earmeasureval, label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    #theme(legend.position = "none")+
    geom_point(aes(color = Low.Hz))+
    scale_color_manual(values=c("black","darkgrey","red","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(parampd)), col = "blue")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsf)), col = "grey")+
    geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = Head.mass..g.,y = exp(paramsT)), col = "green")+
    scale_x_log10()+
    scale_y_log10()+
    ylab(yvarnames[e])
  ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}




#test
#runplot(2)
#runplotLF(3)
runplotnolines(7)

#runplotboxplot

bx<-ggplot(aqonly, aes(x = divescore, y = ))


#make list of ggplots, for each variable
ggplotlist<-list()
for(i in 1:16){
  ggplotlist[[i]]<-runplotnolines(i)#as_grob
}

#test
ggplotlist[[1]]

library(flextable)
library(officer)

toprint<-read_docx() #create word doc object

#add tables to word document
for (i in seq_along(1:16)){
  # body_add_flextable(toprint,myftlist[[i]])#add pgls output table
  #body_add_flextable(toprint,SW_MAIN_effect_tbl[[i]])#add pgls output table
  #body_add_flextable(toprint,HM_MAIN_effect_tbl[[i]])#add pgls output table
  body_add_gg(toprint,ggplotlist[[i]])#ggplot and contmap combined in other file . , height = 4, width = 6 in inches
  #body_add_break(toprint) 
  #body_end_section_landscape(toprint)
}
#body_add_gg(toprint,pcanoair)
#write word document
print(toprint,target = "D:/Analysis_plots/Aug 17 scatters coloured order.docx")