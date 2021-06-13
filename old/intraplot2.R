#repeat this############
slpline1<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*pgls_models_list_intra[1][[1]]$model$coef[2]
slpline1iso<-pgls_models_list_intra[1][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*1

i<-1
for(i in seq_along(vecty)){
assign(paste0("slpline","_",as.character(i)),pgls_models_list_intra[i][[1]]$model$coef[1]+
           log(avgdf$FPtotalarea)*pgls_models_list_intra[i][[1]]$model$coef[2])
assign(paste0("slplineiso_",as.character(i)),pgls_models_list_intra[1][[1]]$model$coef[1]+
    log(avgdf$FPtotalarea)*1)
}

runplotpglsintra(e = 1,grp = "waterbirds")+
  geom_line(aes(x = log(FPtotalarea),
                y = slpline_1), col = "green", size = 2)+
  geom_line(aes(x = log(FPtotalarea),
                y = slplineiso_1), col = "black", size = 2)


runplotpglsintra(1,"waterbirds")+
  geom_line(aes_string(x = vectxsimple[1],
                       y = paste0("slplineiso_",as.character(1))),
                       col = "green", size = 2)

runplotpglsintra(3,"waterbirds")+
  geom_line(aes_string(x = vectx[3],
                       y = paste0("slplineiso_",as.character(3))),
            col = "green", size = 2)


runplotpglsintra(3,"waterbirds")
runplotpglsintra(4,"waterbirds")
runplotpglsintra(5,"waterbirds")
runplotpglsintra(6,"waterbirds")
runplotpglsintra(7,"waterbirds")
runplotpglsintra(8,"waterbirds")
runplotpglsintra(9,"waterbirds")

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



slpline1<-pgls_models_list_intra[e][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*pgls_models_list_intra[e][[1]]$model$coef[2]
slpline1iso<-pgls_models_list_intra[e][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*1

slpline2<-pgls_models_list_intra[2][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*pgls_models_list_intra[2][[1]]$model$coef[2]
slpline2iso<-pgls_models_list_intra[2][[1]]$model$coef[1]+
  log(avgdf$FPtotalarea)*1
runplotpglsintra(e = 1,grp = "waterbirds")+
  geom_line(aes(x = log(FPtotalarea),
                y = slpline1iso), col = "green", size = 2)+
  geom_line(aes(x = log(FPtotalarea),
                y = slpline), col = "black", size = 2)

