

source("pgls_intraear.R")

pgls_models_list_intra<-lapply(modellist_intra,pgls_models)#run pgls


#visualize the table better using the flextable package
flexall<-flextable(intra) %>% 
  add_header_lines(  values = "Table X. Models for selection") %>%
  #bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall



#scatterplot
runplotnolines<-function(i1,i2,grp){
  p<-ggplot(avgdf, 
            aes_string(x = i1, y = i2, col = grp))+
    theme_classic()+
    #theme(legend.position = "none")+
    geom_point(aes_string(col = grp), size = 2)+
    geom_smooth(aes_string(col = grp), method = "lm", se = F)+
    
    #scale_color_gradientn(colours = rainbow(4))+
    scale_x_log10()+
    scale_y_log10()#
    #geom_text(aes(label = Binomial))+
    #ylab(yvarnames[e])
  p
  #ggExtra::ggMarginal(p, type = "boxplot", groupColour = TRUE, margins = "y")#add marginal plot to 'p' object
}
runplotnolines(i1 = yvarnames[1],i2 = yvarnames[2],grp = "waterbirds")
runplotnolines(i1 = yvarnames[13],i2 = yvarnames[2],grp = "waterbirds")


modellist_intra 
spleet<-strsplit(modellist_intra,"~")


vecty<-numeric()
for (i in seq_along(spleet)){
  vecty[i]<-spleet[[i]][1]
}

vectx<-numeric()
for (i in seq_along(spleet)){
  vectx[i]<-spleet[[i]][2]
}
  

  parampd<-pgls_models_list_intra[e][[1]]$model$coef[1]+#intercept
    log($Head.mass..g.)*
    (pgls_models_list_intra[e][[1]]$model$coef[2])
  

gsub("log(","log(avgdf\\$",vecty)
paste0("log(avgdf$",vecty)
gsub

e<-1
runplotpglsintra<-function(e,grp){
    p<-ggplot(avgdf, 
              aes_string(x = vectx[e], y = vecty[e], shape = grp))+
      theme_classic()+
      theme(legend.position = "none")+
      geom_point(aes_string(shape = grp), size = 2)
    p
  }

#repeat this############
slpline1<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*pgls_models_list_intra[e][[1]]$model$coef[2]
slpline1iso<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*1

runplotpglsintra(e = 1,grp = "waterbirds")+
  geom_line(aes(x = log(FPtotalarea),
                y = slpline1iso), col = "green", size = 2)+
  geom_line(aes(x = log(FPtotalarea),
            y = slpline), col = "black", size = 2)



  

#intercept is off
runplotpglsintra(i1 = vectx[1],i2 = vecty[1],grp = "waterbirds")+
  geom_hline(yintercept = pgls_models_list_intra[e][[1]]$model$coef[1], col = "black", size = 2)
#
#
#runplotpglsintra(i1 = vectx[1],i2 = vecty[1],grp = "waterbirds")+
#  geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
#              slope = 1, col = "green", size = 2)+
#  geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
#              slope = pgls_models_list_intra[e][[1]]$model$coef[2], col = "black", size = 2)

runplotpglsintra(i1 = vectx[1],i2 = vecty[1],grp = "waterbirds")+
  geom_line(avgdf,x = log(FPtotalarea),
            y = 
              = pgls_models_list_intra[e][[1]]$model$coef[1],
            slope = log(avgdf$FPtotalarea)*pgls_models_list_intra[e][[1]]$model$coef[2], col = "black", size = 2)



runany<-function(e){
  runplotpglsintra(i1 = vectx[e],i2 = vecty[e],grp = "waterbirds")+
  geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
              slope = geomcoefs_intra[e], col = "green", size = 2)+
  geom_line(avgdf,intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
              slope = log(avgdf$FPtotalarea)*pgls_models_list_intra[e][[1]]$model$coef[2], col = "black", size = 2)
}
runany(10)

parampd<-pgls_models_list3[e][[1]]$model$coef[1]+
  log(subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e])$bodymass)*
  (pgls_models_list3[e][[1]]$model$coef[2])

geom_line(data = subset(longdfplotting,longdfplotting$earmeasures==yvarnames[e]),
          aes(x = log(bodymass),y = parampd), col = "black")+

avgdf_lg<-log(avgdf)

e<-10
runany<-function(e){
  
  pgls_models_intra<-pgls_models(modellist_intra[e])#run pgls
  
  slopeline<-pgls_models_intra$model$coef[1]+
    log(avgdf$names(avgdf)[10])*(pgls_models_list_intra[e][[1]]$model$coef[2])
  
    runplotpglsintra(i1 = vectx[e],i2 = vecty[e],grp = "waterbirds")+
      geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
                  slope = geomcoefs_intra[e], col = "green", size = 2)+
      geom_line(data = avgdf,
                aes(x = vectx[e], y = slopeline,
                col = "black", size = 2)
  }
runany(1)

for(i in seq_along(avgdf)){
  print(is.numeric(avgdf[i]))
}

ggarrange(runany(1,1),
  runany(2,0.5),
runany(3,0.5),
runany(4,0),
runany(5,1),
runany(6,0.33),
runany(7,0.5),
runany(8,0.67),
runany(9,1),
runany(10,0.67))

e<-2
runplotpglsintra(e = 2,i1 = vectx[1],i2 = vecty[1],grp = "waterbirds")+
geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
            slope = 1, col = "green", size = 2)+
  geom_abline(intercept = pgls_models_list_intra[e][[1]]$model$coef[1],
              slope = pgls_models_list_intra[e][[1]]$model$coef[2], col = "black", size = 2)




runplotpglsintra(i1 = vecty[2],i2 = vectx[2],grp = "waterbirds")



modellist_intra[1]
vectx[1]
vecty[1]
pgls_models_list_intra[10]