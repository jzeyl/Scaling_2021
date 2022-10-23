library(RColorBrewer)
library(viridis)
library(patchwork)
library(tidyr)
library(ggrepel)

##plot graph after creating the 'limits' dataframe (see 'Audiograms linked to anatomy' file)

#split into audiograms by species, only including ones with scan data
splt<-fig1 %>% filter(.,fig1$havescan=="Have scan data for species")%>%
  split(.$Species)

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

limitslong<-limits[which(!is.na(limits$aud_rel)),] %>%#only select the rows for which anatomical data is available for the corresponding audiograms
  select(Species,LowHzlimit,HighHzlimit,besthz,reallowdBlimit,realhighdBlimit) %>%
  gather(key = "limit", value = "Hz", -c(Species,reallowdBlimit,realhighdBlimit))


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

#
bound$Species<-as.factor(bound$Species)
#reorder 'bound' df by besthz
bound$Hz<-bound$x #give appropriate naming for x

bound %>% arrange(Species)

#bound$Species = with(bound, reorder(Species, besthz, median))

#Add new rows to end of dataframe with audiogram metrics (best Hz, etc.)
bound2<-bound
levels(bound2$Species)<-c(levels(bound2$Species),"High freq. limit","Best freq.","Low freq. limit")
##bound2[95001,"Species"]<-"Low freq. limit"#add 'low hz as species name, placeholder for bethz
# bound2[95002,"Species"]<-"Best freq."
#bound2[95003,"Species"]<-"High freq. limit"
#bound$Species<-as.character(bound$Species)
#bound$besthz<-as.numeric(bound$besthz)

bound2$Hz<-bound2$x #give appropriate naming for x
bound2$Species = with(bound2, reorder(Species, besthz, median))
bound2$`Threshold (dB)`<-bound2$y

library(forcats)
bound %>%
  mutate(spp = fct_reorder(Species, Species))%>%
ggplot(bound, aes(x = Hz, y = Species))+   #, col= supraorder
         geom_path(data = bound2,aes(col = `Threshold (dB)`), size = 2)+
         geom_point(data = limitslong, aes(x = Hz, y = Species),col = "black", size = 2)+
         scale_color_viridis()
#plot audiograms as bars, per species
range<-ggplot(bound, aes(x = Hz, y = reorder(Species))+   #, col= supraorder
  geom_path(data = bound2,aes(col = `Threshold (dB)`), size = 2)+
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
  #geom_boxplot(data = limits,aes(x = LowHzlimit, y = 1), width = 0.8)+
  #geom_boxplot(data = limits,aes(x =  besthz, y = 2), width = 0.8)+
  #geom_boxplot(data = limits,aes(x = HighHzlimit, y = 3), width = 0.8)+
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
  #coord_flip()+
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

