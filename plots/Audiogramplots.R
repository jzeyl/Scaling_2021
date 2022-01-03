library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)
library(ggrepel)



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


bound2<-bound
bound2[nrow(bound) + 1, ] <- new_row

# Add new row
bound2<-bound
levels(bound2$Species)<-c(levels(bound2$Species),"High freq. limit","Best freq.","Low freq. limit")
newrow<-c(5,5,as.factor("test"), 5,5)
newrow1<-c(5,5,as.factor("test1"), 5,5)
newrow2<-c(5,5,as.factor("test2"), 5,5)

bound2<-rbind(bound2,newrow,newrow1,newrow2)

bound2[95001,c("Species","besthz")]<-c("High freq. limit",10)
 bound2[95002,c("Species","besthz")]<-c("Best freq.", 10)
bound2[95003,c("Species","besthz")]<-c("Low freq. limit" ,10)
#bound$Species<-as.character(bound$Species)
#bound$besthz<-as.numeric(bound$besthz)

#reorder 'bound' df by besthz
bound$Hz<-bound$x
bound$Species = with(bound, reorder(Species, besthz, median))
bound2$Species = with(bound2, reorder(Species, besthz, median))


range<-ggplot(bound2, aes(x = Hz, y = Species))+   #, col= supraorder
  geom_path(aes(col = y), size = 2)+
  geom_point(data = limitslong, col = "black", size = 2)+
  scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  coord_cartesian(clip = "off", ylim = c(1,22))+
  annotation_logticks(sides = "b", outside = TRUE)+
  geom_point(data = limits, shape = 21, size = 2, colour = "black", fill = "white")+
  #geom_vline(xintercept = min(limits$LowHzlimit))+
  #geom_vline(xintercept = max(limits$LowHzlimit))+
  #geom_hline(yintercept = 5)+
  ylab("Species")+
  xlab("Frequency(Hz)")+
  #theme(legend.position = "none")
  #ylim(c(-2,20))+
  geom_boxplot(data = limits,aes(x = LowHzlimit, y = 3), width = 1)+
  geom_boxplot(data = limits,aes(x =  besthz, y = 2), width = 1)+
  geom_boxplot(data = limits,aes(x = HighHzlimit, y = 1), width = 1)

range

bestsens<-ggplot(limits, aes(x = 0,y = bestsensitivity))+
  geom_boxplot()+
  geom_point(aes(x = 0,label = Species))+
  geom_label_repel(aes(x = 0,label = Species),
                  #direction = "x",
                  size = 3,
                  #nudge_x = 1,
                  box.padding = unit(0.35, "lines"),
                point.padding = unit(1, "lines"))+
  coord_flip()+
  #geom_text(aes(x = 0, label = Species))+
  theme_classic()+
  xlim(c(-4,4))+
  ylab("Best sensitivity(dB SPL)")
bestsens

range/bestsens+
  #plot_layout(widths = c(1,1))+
  plot_annotation(tag_levels = "A")


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
