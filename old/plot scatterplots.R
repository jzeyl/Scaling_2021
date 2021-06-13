#this script puts the data in long format so that a list of ggplots can be made using a loop

library(ggplot2)
library(ggrepel)
library(cowplot)
library(gridGraphics)

pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]
modellist<-pgls_todo_hm

pgls_models_list1<-lapply(pgls_todo_hm,pgls_models)#run pgls

pgls_todo_category_hm_main<- paste(pgls_todo_hm,"+Category")
pgls_models_list2<-lapply(pgls_todo_category_hm_main,pgls_models)##run pgls

pgls_todo_category_hm_int<- paste(pgls_todo_hm,"*Category")
pgls_models_list3<-lapply(pgls_todo_category_hm_int,pgls_models)#run pgls

modellist<- paste(pgls_todo_hm,"+plungedistinct")
pgls_models_list4<-lapply(modellist,pgls_models)#run pgls

modellist<- paste(pgls_todo_hm,"*plungedistinct")
pgls_models_list5<-lapply(modellist,pgls_models)#run pgls

modellist<- paste(pgls_todo_hm,"+waterbirds")
pgls_models_list_W<-lapply(modellist,pgls_models)#run pgls


#put in long format
longdfplotting<-gather(avgdf,key = "earmeasures", value = "earmeasureval", 
                       -c(Binomial, Skull.width..mm.,Head.mass..g.,Category, Order,
                          Family,Low.Hz,plungedistinct,))#
longdfplotting$earmeasures<-as.factor(longdfplotting$earmeasures)
longdfplotting$earmeasureval<-as.numeric(longdfplotting$earmeasureval)
#longdfplotting$Orderpdonly<-ifelse(longdfplotting$Category== "Pursuit diving", longdfplotting$Binomial, "")
#longdfplotting$SW1<-ifelse(longdfplotting$Binomial == "Ardenna gravis", "Ardenna gravis", "")
#longdfplotting$SW2<-ifelse(longdfplotting$Binomial == "Ardenna_grisea", longdfplotting$Binomial, "")

#textdf <- longdfplotting[rbind(longdfplotting$Binomial == "Ardenna gravis",longdfplotting$Binomial,"")
#                               longdfplotting$Binomial =="Ardenna_grisea"), ]

#make list of ear measures to plot
yvarnames<-c("TMtotalarea",
             "FPtotalarea",
             "area_ratio",
             "dis_coltip_TMcentroid",
             "Umbo_distancetoTMplane",
             "meanTMangle",
             "totalEClength",
             "RWtotalarea",
             "CAtotalarea",
             "Behind.TM",
             "Columella.length.mm",
             "Columella.volume.mm3")

[1] "log(TMtotalarea)~log(Head.mass..g.) *plungedistinct"           
[2] "log(FPtotalarea)~log(Head.mass..g.) *plungedistinct"           
[3] "log(area_ratio)~log(Head.mass..g.) *plungedistinct"            
[4] "log(dis_coltip_TMcentroid)~log(Head.mass..g.) *plungedistinct" 
[5] "log(Umbo_distancetoTMplane)~log(Head.mass..g.) *plungedistinct"
[6] "log(meanTMangle)~log(Head.mass..g.) *plungedistinct"           
[7] "log(totalEClength)~log(Head.mass..g.) *plungedistinct"         
[8] "log(RWtotalarea)~log(Head.mass..g.) *plungedistinct"           
[9] "log(CAtotalarea)~log(Head.mass..g.) *plungedistinct"           
[10] "log(Behind.TM)~log(Head.mass..g.) *plungedistinct"             
[11] "log(Columella.length.mm)~log(Head.mass..g.) *plungedistinct"   
[12] "log(Columella.volume.mm3)~log(Head.mass..g.) *plungedistinct"  


#scatterplot
runplotnolines<-function(e,grp){
  p<-ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes_string(x = "Head.mass..g.", y = "earmeasureval", col = grp))+
    theme_classic()+
    
    #theme(legend.position = "none")+
    geom_point(aes_string(col = grp), size = 4)+
    geom_smooth(aes_string(col = grp), method = "lm", se = F)+
    
    #scale_color_gradientn(colours = rainbow(4))+
    scale_x_log10()+
    scale_y_log10()+
    #geom_text(aes(label = Binomial))+
    ylab(yvarnames[e])
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplotnolines(1,"waterbirds")

#Plotting functions for interaction model. takes the index of the 'yvarnames' list as an argument

runplot_HM_only<-function(e){
  parampd<-pgls_models_list1[e][[1]]$model$coef[1]+
    log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$Head.mass..g.)*
    (pgls_models_list1[e][[1]]$model$coef[2])
ggplot(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]), 
            aes(x = log(Head.mass..g.), y = log(earmeasureval), label = Binomial), 
            factor = as.factor(Category))+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(aes(color = Category))+
    scale_color_manual(values=c("green","blue","darkgrey","l2ghtblue","green","darkgray","darkgreen","corns2lk4","blue"))+
   geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
              aes(x = log(Head.mass..g.),y = parampd), col = "black")

    #geom_text_repel(aes(label = Orderpdonly))+
    #ylab(yvarnames[e])
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplot_HM_only(3)

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