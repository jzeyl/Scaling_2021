library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)


display.brewer.pal(n, name)
display.brewer.all(2)

df_audiogrm_lst<-list()

for(i in seq_along(splt)){
  df_audiogrm_lst[[i]]<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))
  df_audiogrm_lst[[i]]$Species<- rep(splt[[i]]$Species,length.out = 5000)
  }
#single y example
ggplot(df_audiogram, aes(x = x, y = y), col = y)+
  geom_point(aes(y = 60, col = y))+
  scale_color_viridis()

#using data
bound<-do.call(rbind.data.frame,df_audiogrm_lst)

ggplot(bound, aes(x = x, y = Species), col = y)+
  scale_x_log10()+
  geom_point()+
  geom_point(aes(col = y))+
  scale_color_viridis()

limits$Hz<-limits$besthz


tolong<-limits %>% select(Species,LowHzlimit,HighHzlimit,besthz)
limitslong<-tolong %>% gather(key = "limit", value = "Hz", -Species)

bound$LowHzlimit<-NA
bound$HighHzlimit<-NA
bound$besthz<-NA
bound$bestsensitivity<-NA

#add best hz to interpolated datset for sorting
bound$besthz<-limits$besthz[match(bound$Species,limits$Species)]

boundorderbybesthz<-bound[order(bound$besthz),]


library(forcats)
bound$Species<-as.character(bound$Species)
bound$besthz<-as.numeric(bound$besthz)

#reorder 'bound' df by besthz
bound$Hz<-bound$x
bound$Species = with(bound, reorder(Species, besthz, median))

range<-ggplot(bound, aes(x = Hz, y = Species))+   #, col= supraorder
  geom_path(aes(col = y), size = 2)+
  geom_point(data = limitslong, col = "black")+
  scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  coord_cartesian(clip = "off", ylim = c(-6,22))+
  annotation_logticks(sides = "b", outside = TRUE)+
  geom_point(data = limits, col = "white")+
  #geom_vline(xintercept = min(limits$LowHzlimit))+
  #geom_vline(xintercept = max(limits$LowHzlimit))+
  #geom_hline(yintercept = 5)+
  ylab("Species")+
  xlab("Frequency(Hz)")+
  #theme(legend.position = "none")
  #ylim(c(-2,20))+
  geom_boxplot(data = limits,aes(x = LowHzlimit, y = -1), width = 2)+
  geom_boxplot(data = limits,aes(x =  besthz, y = -3), width = 2)+
  geom_boxplot(data = limits,aes(x = HighHzlimit, y = -5), width = 2)

range


range+bestsens+plot_layout(widths = c(3,1))+
  plot_annotation(tag_levels = "A")

bestsens<-ggplot(limits, aes(y = bestsensitivity))+
  geom_boxplot()+
  geom_point(aes(x = 0,label = Species))+
  geom_text_repel(aes(x = 0,label = Species))+
  #coord_flip()+
  theme_classic()+
  ylab("Best sensitivity(dB SPL)")
bestsens


scale_x_log10(breaks = c(1,2,5,10,15,20,22,25^1.0,
                         25^(1+1*0.09166667),
                         25^(1+2*0.09166667),
                         25^(1+3*0.09166667),60),
              labels = c("1","2","5","10","15","20","","FG","GF","F","G",""))+


lowmidhigh<-gather(limits, key = "range", value = "Hzvalue",LowHzlimit,      HighHzlimit ,    Species,
                    besthz)
 View(head(lowmidhigh))

 low<-ggplot(limitslong, aes(x = reorder(limit,Hz,median), y = Hz))+
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

range/low+besthz+high+bestsens
+plot_layout(nrow = 1)


 library(dplyr)
 bound %>% group_by(Species) %>% summarise(rangey = max(y)-min(y))

 bymin<-bound %>% group_by(Species) %>% summarise(min = min(y))

 sort(bymin$min)

 bymin$Species[bymin$min== sort(bymin$min)]

 match(bymin$Species
