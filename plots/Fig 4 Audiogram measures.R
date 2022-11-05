# to plot, the 'fig1' and 'limits' 
#dataframe comes from the 'Audiograms linked to anatomy.R' script

#######plotting metrics on audiogram########
aas<-function(d){
  set<-limits[d,]
  bestsensitivity<-set$bestsensitivity
  bestHz<-set$besthz
  highHz<-set$HighHzlimit
  lowHz<-set$LowHzlimit
  ggplot(fig1[fig1$Species==set$Species,], aes(x = Hz, y = Threshold, factor = Species))+
    scale_x_log10()+
    #geom_vline(xintercept = lowHz, col = "grey", size = 2)+
    
    #Hz metrics
    geom_segment(aes(x = lowHz, y = -Inf, xend = lowHz, yend = 35), col = "grey", size = 2)+
    geom_segment(aes(x = bestHz, y = -Inf, xend = bestHz, yend = bestsensitivity), col = "grey", size = 2)+
    geom_segment(aes(x = highHz, y = -Inf, xend = highHz, yend = 35), col = "grey", size = 2)+
    geom_line(aes(x = Hz, y = Threshold), size = 2)+
    
    #geom    geom_line(aes(x = Hz, y = Threshold), size = 2)+
    scale_color_brewer(palette = "Set1")+
    geom_hline(yintercept = bestsensitivity)+
    geom_hline(yintercept = 35, col = "black")+
    theme_bw()+
    #theme(legend.position = "none")+
    coord_cartesian(clip = "off")+
    ylab("Threshold (dB SPL)")+
    xlab("Frequency (Hz)")+
    annotation_logticks(sides = "b", outside = TRUE)+
    ylim(c(0,80))+
    annotate("text",x = 50, y = 35+3, label = "Cutoff level (dB)")+
    annotate("text",x = 50, y = bestsensitivity+3, label = "Best sensitivity (dB)")+
    annotate("text",x = bestHz, y = 5, label = "Best frequency")+
    annotate("text",x = lowHz, y = 5, label = "Low frequency limit")+
    annotate("text",x = highHz, y = 5, label = "High frequency limit")
  
}
aas(16)+theme()

#aas(16)+ xlim(c(0,50000))