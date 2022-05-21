summary(pgls_models(log(LowHzlimit)~V1))

plotit<-function(meas){
  p<-ggplot(joined, aes_string(x = meas, y = "V1"))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("residual")+
    geom_text_repel(aes(label = residname))
  p
}



plotlist[[i]]<-plotit("log(LowHzlimit)")+
  plotit("log(HighHzlimit)")+
  plotit("log(besthz)")+
  plotit("bestsensitivity")+
  plot_annotation(title = paste0("resids of ",pgls_todo_hm[i]))
}

#print to pdf
pdf(paste0(choose.dir(),"/allplots.pdf"),onefile = TRUE)
for(i in seq_along(plotlist)){
  print(plotlist[[i]])
}
dev.off()


lowhz_lm<-lm("log(LowHzlimit)~V1", data = joined)
highz_lm<-lm("log(HighHzlimit)~V1", data = joined)
bestdb_lm<-lm("bestsensitivity~V1", data = joined)
besthz_lm<-lm("log(besthz)~V1", data = joined)
summary(lowhz_lm)
summary(highz_lm)
summary(bestdb_lm)
summary(besthz_lm)
