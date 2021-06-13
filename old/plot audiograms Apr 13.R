library(RColorBrewer)
library(viridis)
library(ggrepel)
ggplot(df_audiogram, aes(x = x, y = y), col = y)+
  geom_point(aes(y = 60, col = y))+
  scale_color_viridis()

display.brewer.pal(n, name)
display.brewer.all(2)
          
#limits<-matrix(nrow=length(splt),ncol = 7)

df_audiogrm_lst<-list()

for(i in seq_along(splt)){
  df_audiogrm_lst[[i]]<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))
  df_audiogrm_lst[[i]]$Species<- rep(splt[[i]]$Species,length.out = 5000)
  }


bound<-do.call(rbind.data.frame,df_audiogrm_lst)

ggplot(bound, aes(x = x, y = Species), col = y)+
  scale_x_log10()+
  geom_point()+
  geom_point(aes(col = y))+
  scale_color_viridis()

range<-ggplot(bound, aes(x = x, y = 
                           Species), col = y)+   #, col= supraorder
  geom_path(aes(col = y), size = 3)+
  scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  coord_cartesian(clip = "off", ylim = c(-6,22))+
  #scale_y_continuous(breaks = c(-6,0,10))+
  annotation_logticks(sides = "b", outside = TRUE)+
  #geom_vline(xintercept = median(limits$LowHzlimit))+
  #geom_vline(xintercept = min(limits$LowHzlimit))+
  #geom_vline(xintercept = max(limits$LowHzlimit))+
  #geom_hline(yintercept = 5)+
  ylab("Species")+
  xlab("Frequency(Hz)")+
  geom_boxplot(data = limits,aes(x = LowHzlimit, y = -1), width = 2)+
  geom_boxplot(data = limits,aes(x =  besthz, y = -3), width = 2)+
  geom_boxplot(data = limits,aes(x = HighHzlimit, y = -5), width = 2)

  #theme(legend.position = "none")
  #ylim(c(-2,20))+
 range


 
 bestsens<-ggplot(limits, aes(y = bestsensitivity))+
   geom_boxplot()+
   geom_point(aes(x = 0,label = Species))+
   geom_text_repel(aes(x = 0,label = Species))+
   #coord_flip()+
   theme_classic()+
   ylab("Best sensitivity(dB SPL)")
 bestsens

 range+bestsens+plot_layout(widths = c(3,1))+
   plot_annotation(tag_levels = "A")
 
 scale_x_log10(breaks = c(1,2,5,10,15,20,22,25^1.0,
                          25^(1+1*0.09166667),
                          25^(1+2*0.09166667),
                          25^(1+3*0.09166667),60), 
               labels = c("1","2","5","10","15","20","","FG","GF","F","G",""))+ 
   
 
 
 library(dplyr)
 bs<-bound %>% group_by(Species) %>% summarise(min_ = min(y))
 
bsfactorder<-as.factor(bs$Species[order(bs$min_)])

reorder(bound$Species,bs$Species[order(bs$min_)])

bound$Species<-as.factor(bound$Species, levels = bsfactorder)
reorder(bound$Species,bsfactorder)
 