library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)
library(ggrepel)

##plot graph after creating the 'limits' dataframe (see 'Audiograms linked to anatomy' file)


df_audiogrm_lst<-list()

for(i in seq_along(splt)){
  df_audiogrm_lst[[i]]<-as.data.frame(approx(splt[[i]]$Hz,splt[[i]]$Threshold,n = 5000))
  df_audiogrm_lst[[i]]$Species<- rep(splt[[i]]$Species,length.out = 5000)
  }

#single y audiogram example
#ggplot(df_audiogram, aes(x = x, y = y), col = y)+
#  geom_point(aes(y = 60, col = y))+
#  scale_color_viridis()

#bind audiograms together
bound<-do.call(rbind.data.frame,df_audiogrm_lst)

#ggplot(bound, aes(x = x, y = Species), col = y)+
#  scale_x_log10()+
#  geom_point()+
#  geom_point(aes(col = y))+
#  scale_color_viridis()

#convert "limits" dataframe to long format
tolong<-limits %>% select(Species,LowHzlimit,HighHzlimit,besthz)
limitslong<-tolong %>% gather(key = "limit", value = "Hz", -Species)

#we will append data from the 'limits' df using species as a key
bound$LowHzlimit<-NA
bound$HighHzlimit<-NA
bound$besthz<-NA
bound$bestsensitivity<-NA

#add best hz to interpolated datset for sorting
bound$besthz<-limits$besthz[match(bound$Species,limits$Species)]
bound$bestsensitivity<-limits$bestsensitivity[match(bound$Species,limits$Species)]
bound$HighHzlimit<-limits$HighHzlimit[match(bound$Species,limits$Species)]
bound$LowHzlimit<-limits$LowHzlimit[match(bound$Species,limits$Species)]



# Add new rows with audiogram metrics (best Hz, etc.)
bound$Species<-as.factor(bound$Species)

bound2<-bound
levels(bound2$Species)<-c(levels(bound2$Species),"High freq. limit","Best freq.","Low freq. limit")
bound2[95001,c("Species","besthz")]<-c("Low freq. limit",10)
 bound2[95002,c("Species","besthz")]<-c("Best freq.", 10)
bound2[95003,c("Species","besthz")]<-c("High freq. limit" ,10)
#bound$Species<-as.character(bound$Species)
#bound$besthz<-as.numeric(bound$besthz)

#reorder 'bound' df by besthz
bound$Hz<-bound$x #give appropriate naming for x
bound$Species = with(bound, reorder(Species, besthz, median))
bound2$Species = with(bound2, reorder(Species, besthz, median))
bound2$`Threshold (dB)`<-bound2$y

#plot audiograms as bars, per species
range<-ggplot(bound, aes(x = Hz, y = Species))+   #, col= supraorder
  geom_path(data = bound2[c(seq(1, 95000, 10),95001,95002,95003), ],aes(col = `Threshold (dB)`), size = 2)+
  geom_point(data = limitslong, aes(x = Hz, y = Species),col = "black", size = 2)+
  scale_color_viridis()+
  scale_x_log10()+
  theme_classic()+
  coord_cartesian(clip = "off", ylim = c(1,22))+
  annotation_logticks(sides = "b", outside = TRUE, colour = "black")+
  geom_point(data = limits, aes(x = besthz, y = Species), shape = 21, size = 2, colour = "black", fill = "white")+
  #geom_vline(xintercept = min(limits$LowHzlimit))+
  #geom_vline(xintercept = max(limits$LowHzlimit))+
  #geom_hline(yintercept = 5)+
  ylab("")+
  xlab("Frequency(Hz)")+
  #theme(legend.position = "none")
  #ylim(c(-2,20))+
  geom_boxplot(data = limits,aes(x = LowHzlimit, y = 1), width = 0.8)+
  geom_boxplot(data = limits,aes(x =  besthz, y = 2), width = 0.8)+
  geom_boxplot(data = limits,aes(x = HighHzlimit, y = 3), width = 0.8)+
  geom_hline(yintercept = 3.5)+
  theme(axis.title.y = element_text(angle= 0, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle= 0, vjust = -2.5, hjust=0.5))
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
  ylab("Best sensitivity(dB SPL)")+
  xlab("")+
  theme(axis.title.y = element_text(angle= 0, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
bestsens

range/bestsens+
  plot_layout(heights = c(2,1))+
  plot_annotation(tag_levels = "A")

ggsave(file=paste0(choose.dir(),"/supplemental_bar.svg"), width=10, height=10)

