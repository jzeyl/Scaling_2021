library(RColorBrewer)
library(viridis)
library(patchwork)
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


 range<-ggplot(bound, aes(x = x, y = Species), col = y)+   #, col= supraorder
  geom_path(aes(col = y), size = 2)+
  scale_color_viridis()+
  scale_x_log10()+
  theme_minimal()+
  coord_cartesian(clip = "off", ylim = c(-0,22))+
  annotation_logticks(sides = "b", outside = TRUE)+
  #geom_vline(xintercept = median(limits$LowHzlimit))+
  #geom_vline(xintercept = min(limits$LowHzlimit))+
  #geom_vline(xintercept = max(limits$LowHzlimit))+
  #geom_hline(yintercept = 5)+
  ylab("Species")+
  xlab("Frequency(Hz)")
  #theme(legend.position = "none")
  #ylim(c(-2,20))+
 range
 
 lowmidhigh<-gather(limits, key = "range", value = "Hzvalue",LowHzlimit,      HighHzlimit ,    Species,
                    besthz)
 View(head(lowmidhigh))
 
 low<-ggplot(limits, aes(x = LowHzlimit))+
   geom_boxplot()+
   coord_flip()+
   theme_minimal()
 low
 
 high<-ggplot(limits, aes(x = HighHzlimit))+
   geom_boxplot()+
   coord_flip()+
   theme_minimal()
 high
 
 besthz<-ggplot(limits, aes(x = besthz))+
   geom_boxplot()+
   coord_flip()+
   theme_minimal()
 besthz
 
 
 bestsens<-ggplot(limits, aes(x = bestsensitivity))+
   geom_boxplot()+
   coord_flip()+
   theme_minimal()
 bestsens
 bestsensitivity
 
range/low+besthz+high+bestsens+plot_layout(nrow = 1)
 

 library(dplyr)
 bound %>% group_by(Species) %>% summarise(rangey = max(y)-min(y))
 
 bymin<-bound %>% group_by(Species) %>% summarise(min = min(y))
 
 sort(bymin$min)
 
 bymin$Species[bymin$min== sort(bymin$min)]
 
 match(bymin$Species