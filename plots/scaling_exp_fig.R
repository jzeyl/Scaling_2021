
library(patchwork)
library(ggplot2)
library(ggtext)
library(geomtextpath)

#plot different isometric slopes
multidimensions<-ggplot()+
  geom_richtext(aes(x =c(10.5), y = c(10)),
            label = "mm ~ mm, <br> mm<sup>2</sup> ~ mm<sup>2</sup>,<br> mm<sup>3</sup> ~ mm<sup>3</sup> or g", hjust = 0)+
  geom_textpath(aes(x = c(0,10), y = c(0,10)),
                label = "b = 1",
                hjust = 0.6, vjust = -0.2)+
  #0.6
 geom_richtext(aes(x = c(10.5), y = c(6.6)),
            label = "mm<sup>2</sup> ~ mm<sup>3</sup> or g", hjust = 0)+
 geom_textpath(aes(x = c(0,10), y = c(0,6.6)),
                label = "b = 0.67",
                hjust = 0.6, vjust = -0.2)+
  #0.5
  geom_richtext(aes(x = c(10.5), y = c(5)),
            label = "mm ~ mm<sup>2</sup>", hjust = 0)+
 geom_textpath(aes(x = c(0,10), y =  c(0,5)),
                label = "b = 0.5",
                hjust = 0.6, vjust = -0.2)+
#3.3
  geom_richtext(aes(x = c(10.5), y =  c(3.3)),
            label = "mm ~ mm<sup>3</sup> or g" , hjust = 0)+
geom_path(aes(x = c(0,10), y =  c(0,3.3)))+
  #0
  geom_textpath(aes(x = c(0,10), y = c(0,3.3)),
                label = "b = 0.33",
                hjust = 0.6, vjust = -0.2)+
#angle vs 3D
geom_path(aes(x = c(0,10), y = c(0,0)))+
  geom_textpath(aes(x = c(0,10), y = c(0,0)),
                label = "b = 0.0",
                hjust = 0.6, vjust = -0.2)+
  geom_richtext(aes(x = c(10.5), y = c(0)),
                label = "degree ~ mm<sup>3</sup> or g" , hjust = 0)+

#themes, scales
  theme_minimal()+
  #theme(plot.margin = margin(1,5,1,1,"in"))+
  ylab("log(y)")+
  xlab("log(x)")+
  scale_x_continuous(limits = c(0,10.5), breaks = seq(0,20, by = 2.5))+
  scale_y_continuous(limits = c(0,10))+
  coord_fixed(ratio = 1, clip = "off")+
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_text(hjust = 0.25),
        plot.margin = unit(c(1,2,1,1), "cm"))
multidimensions


# definition of hypo-hyper-iso ----------------------------------------------------------
types<-ggplot()+
  geom_textpath(aes(x = c(0,10), y = c(0,10)), label = "Isometric (b = 1)",
                hjust = 0.5, vjust = -0.2)+
  geom_textpath(aes(x = c(0,5), y = c(0,10)), col = "black",  label = "Hyperallometric (b > 1)",
                hjust = 0.6, vjust = -0.2)+
  geom_textpath(aes(x = c(0,10), y = c(0,5)), col = "black",  label = "Hypoallometric (b < 1)",
                hjust = 0.6, vjust = -0.2)+
  theme_minimal()+
ylab("log(y)")+
  xlab("log(x)")+
  scale_x_continuous(limits = c(0,10))+
  scale_y_continuous(limits = c(0,10))+
  coord_fixed(ratio = 1, clip = "off")+
  theme(axis.text = element_text(colour = "black"))
types



# combine plots -------------------------------------------------------


types+multidimensions+#plot_layout(widths = c(1,2))+
  plot_annotation(tag_levels = 'A')

ggsave(file=paste0(choose.dir(),"/scalingtypes_jun 16.svg"),
       width=10.5, height=5)
