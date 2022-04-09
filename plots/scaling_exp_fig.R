#isometry example
library(patchwork)
library(ggplot2)
library(ggtext)
library(geomtextpath)


#bm<-seq(1,100000,50)
#three_d<-seq(1,100000,50)



multidimensions<-ggplot()+
  geom_richtext(aes(x =c(10), y = c(10)),
            label = "**b: 1** (mm~mm,mm<sup>2</sup>~mm<sup>2</sup>,mm<sup>3</sup>~mm<sup>3</sup>,g)", hjust = 0)+
  geom_textpath(aes(x = c(0,10), y = c(0,10)),
                label = "b:1",
                hjust = 0.6, vjust = -0.2)+
  #0.6
 geom_richtext(aes(x = c(10), y = c(6.6)),
            label = "**b: 0.66** (mm<sup>2</sup>~mm<sup>3</sup>,g)", hjust = 0)+
 geom_textpath(aes(x = c(0,10), y = c(0,6.6)),
                label = "b:0.66",
                hjust = 0.6, vjust = -0.2)+
  #0.5
  geom_richtext(aes(x = c(10), y = c(5)),
            label = "**b: 0.5** (mm~mm<sup>2</sup>))", hjust = 0)+
 geom_textpath(aes(x = c(0,10), y =  c(0,5)),
                label = "b:0.5",
                hjust = 0.6, vjust = -0.2)+
#3.3
  geom_richtext(aes(x = c(10), y =  c(3.3)),
            label = "**b: 0.33** (mm~mm<sup>3</sup>,g)" , hjust = 0)+
geom_path(aes(x = c(0,10), y =  c(0,3.3)))+
  #0
  geom_textpath(aes(x = c(0,10), y = c(0,3.3)),
                label = "b:0.33",
                hjust = 0.6, vjust = -0.2)+
#angle vs 3D
geom_path(aes(x = c(0,10), y = c(0,0)))+
  geom_textpath(aes(x = c(0,10), y = c(0,0)),
                label = "b:0.0",
                hjust = 0.6, vjust = -0.2)+
  geom_richtext(aes(x = c(10), y = c(0)),
                label = "**b: 0.0** (degree~mm<sup>3</sup>,g)" , hjust = 0)+

#themes, scales
  theme_minimal()+
  ylab("log(y)")+
  xlab("log(x)")+
  scale_x_continuous(limits = c(0,20), breaks = seq(0,10, by = 2.5))+
  scale_y_continuous(limits = c(0,10))+
  coord_fixed(ratio = 1, clip = "off")+
  theme(axis.text = element_text(colour = "black"))
multidimensions
scale_x_continuous(breaks = seq(50, 350, by = 2.5))
#geom_textabline(slope = 15, intercept = -100, label = "partition line",
#                color = "green4", hjust = 0.6, vjust = -0.2)



# hypo-hyper-iso ----------------------------------------------------------
types<-ggplot()+
  geom_textpath(aes(x = c(0,10), y = c(0,10)), label = "Isometric",
                hjust = 0.6, vjust = -0.2)+
  geom_textpath(aes(x = c(0,5), y = c(0,10)), col = "black",  label = "Hyperallometric",
                hjust = 0.6, vjust = -0.2)+
  geom_textpath(aes(x = c(0,10), y = c(0,5)), col = "black",  label = "Hypoallometric",
                hjust = 0.6, vjust = -0.2)+
  theme_minimal()+
ylab("log(y)")+
  xlab("log(x)")+
  scale_x_continuous(limits = c(0,10))+
  scale_y_continuous(limits = c(0,10))+
  coord_fixed(ratio = 1, clip = "off")+
  theme(axis.text = element_text(colour = "black"))
types



# insert inset plot -------------------------------------------------------
#combo<-lineplot+ inset_element(inset, 0, 0.4, 0.5, 1)
#combo$patches$layout$widths  <- 1
#combo$patches$layout$heights <- 1
#combo

types+multidimensions+plot_layout(widths = c(1,2))+
  plot_annotation(tag_levels = 'A')




#table

slopes<-c(1,0.66,0.5,0.3,0)
dimensions<-c("1D:1:D,2D:2D")
intra_rel<-c("ESL~CL, TM~FP,RW~FP",
             "TM~CV,FP~CV",
             "COff~TM,CL~FP,UH~TM",
             "CL~CV",
             "TMA~TM")
tabl<-as.data.frame(cbind(slopes,intra_rel))

library(gt)
plottab<-gt(tabl)
plottab

library(patchwork)
lineplot+plottab

ggsave(file=paste0(choose.dir(),"/scalingtypes_apr 8.svg"),
       width=10, height=5)
