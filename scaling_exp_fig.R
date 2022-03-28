#isometry example
library(patchwork)
library(ggplot2)
library(ggtext)
library(geomtextpath)


bm<-seq(1,10000,5)

three_d<-seq(1,10000,5)
three_d_hyper<-three_d*1.1
three_d_hypo<-three_d*0.9


#0 is angle vs 3D



lineplot<-ggplot()+
  geom_richtext(aes(x = max(log(three_d))+0.1, y = max(log(three_d))),
            label = "**b: 1** (CV~HM)", hjust = 0)+
  geom_path(aes(x = log(bm), y = log(three_d)))+

 geom_richtext(aes(x = max(log(three_d)+0.1), y = max(log(three_d)*0.66)),
            label = "**b: 0.66** (TM~CV,FP~CV,TM~HM)", hjust = 0)+
  geom_path(aes(x = log(bm), y = log(three_d)*0.66))+
  #geom_text(aes(x = max(log(three_d)+3), y = max(log(three_d)*0.66)),
  #          label = "TM~CV,FP~CV", hjust = 0)+

  geom_richtext(aes(x = max(log(three_d)+0.1), y = max(log(three_d)*0.5)),
            label = "**b: 0.5** (UH~TM, COff~TM))", hjust = 0)+
  geom_path(aes(x = log(bm), y = log(three_d)*0.5))+

  geom_richtext(aes(x = max(log(three_d)+0.1), y = max(log(three_d)*0.33)),
            label = "**b: 0.33** (mm~g)", hjust = 0)+
geom_path(aes(x = log(bm), y = log(three_d)*0.33))+

#angle vs 3D
geom_path(aes(x = log(bm), y = log(three_d)*0))+
  geom_richtext(aes(x = max(log(three_d)+0.1), y = max(log(three_d)*0)),
            label = "**b: 0.0** (degrees~g)", hjust = 0)+

geom_textabline(slope = 1, intercept = 0, label = "b:1",
                hjust = 0.6, vjust = -0.2)+
  geom_textabline(slope = 0.66, intercept = 0, label = "b:0.66",
                  hjust = 0.6, vjust = -0.2)+
  geom_textabline(slope = 0.5, intercept = 0, label = "b:0.5",
                  hjust = 0.6, vjust = -0.2)+
  geom_textabline(slope = 0.33, intercept = 0, label = "b:0.33",
                  hjust = 0.6, vjust = -0.2)+
  geom_textabline(slope = 0, intercept = 0, label = "b:0",
                  hjust = 0.6, vjust = -0.2)+
#themes, scales
ylab("log(measure)")+
  xlab("log(measure)")+
scale_x_continuous(limits = c(0,20), breaks = seq(0,10,2))+
scale_y_continuous(limits = c(-2,11))+

  theme_minimal()+
theme(axis.text = element_blank(),
      panel.grid = element_blank())
lineplot


#geom_textabline(slope = 15, intercept = -100, label = "partition line",
#                color = "green4", hjust = 0.6, vjust = -0.2)



# hypo-hyper-iso ----------------------------------------------------------

inset<-ggplot()+
 geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.25),
                  ymax=log(three_d)),
              fill="red", alpha = 0.5)+
  geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*1.75),
                  ymax=log(three_d)),
              fill="red", alpha = 0.5) +
  geom_textpath(aes(x = log(bm), y = log(three_d)), label = "Isometric")+
  geom_textpath(aes(x = log(bm), y = log(three_d)*1.75), col = "black",  label = "Hyperallometric")+
  geom_textpath(aes(x = log(bm), y = log(three_d)*0.25), col = "black",  label = "Hypoallometric")+

  geom_segment(aes(x = max(log(bm)), #hyperallometric
                 y = max(log(three_d)),
                 xend = max(log(bm)),
                 yend = max(log(three_d))*1.75),
                arrow = arrow(type = "closed"))+
  geom_segment(aes(x = max(log(bm)), #hyperallometric
                   y = max(log(three_d)),
                   xend = max(log(bm)),
                   yend = max(log(three_d))*0.25),
               arrow = arrow(type = "closed"))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        panel.grid = element_blank())+
ylab(" ")+
  xlab("")+
  scale_x_continuous(limits = c(0,20), breaks = seq(0,10,2))+
  scale_y_continuous(limits = c(-2,20))
  #geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d))),
  #          label = "Isometric", hjust = 0)+
  #geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d)*1.75)),
  #          label = "Hyperallometric", hjust = 0)+
  #geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d)*0.25)),
  #          label = "Hypoallometric", hjust = 0)
  #coord_cartesian(clip = off)
inset

# insert inset plot -------------------------------------------------------
lineplot+ inset_element(inset, 0, 0.4, 0.5, 1)

inset+lineplot+plot_layout(widths = c(1,2))+
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
