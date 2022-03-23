#isometry example

bm<-seq(1,10000,5)

three_d<-seq(1,10000,5)
three_d_hyper<-three_d*1.1
three_d_hypo<-three_d*0.9


#0 is angle vs 3D



lineplot<-ggplot()+
  #3d vs 3D
#geom_path(aes(x = log(bm), y = log(three_d)*1.1), col = "red", alpha = 0.5)+
#geom_path(aes(x = log(bm), y = log(three_d)*0.9), col = "red", alpha = 0.5)+
##geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.9),
#                ymax=log(three_d)),
#            fill="red", alpha=0.5)+
#geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*1.1),
#                  ymax=log(three_d)),
#              fill="red", alpha=0.5) +
  geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d))),
            label = "3D~3D (slope: 1)")+
  geom_path(aes(x = log(bm), y = log(three_d)))+

#three_three
#3D vs area
#ggplot()+
#  geom_path(aes(x = log(bm), y = log(three_d)*0.76), col = "red", alpha = 0.5)+
#  geom_path(aes(x = log(bm), y = log(three_d)*0.56), col = "red", alpha = 0.5)+
# geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.76),
#                 ymax=log(three_d)*0.66),
#             fill="red", alpha=0.5)+
# geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.56),
#                 ymax=log(three_d)*0.66),
#             fill="red", alpha=0.5) +
 geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d)*0.66)),
            label = "2D~3D (slope: 0.66)")+
  geom_path(aes(x = log(bm), y = log(three_d)*0.66))+

#are vs
#  geom_path(aes(x = log(bm), y = log(three_d)*0.43), col = "red", alpha = 0.5)+
#  geom_path(aes(x = log(bm), y = log(three_d)*0.23), col = "red", alpha = 0.5)+
# geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.43),
#                 ymax=log(three_d)*0.23),
#             fill="red", alpha=0.5)+
# geom_ribbon(aes(x = log(bm),ymin=(log(three_d)*0.23),
#                 ymax=log(three_d)*0.43),
#             fill="red", alpha=0.5) +
  geom_text(aes(x = max(log(three_d)+1), y = max(log(three_d)*0.33)),
            label = "1D~3D (slope: 0.33)")+
geom_path(aes(x = log(bm), y = log(three_d)*0.33))+

#angle vs 3D
geom_path(aes(x = log(bm), y = log(three_d)*0))+

#themes, scales
theme_minimal()+
scale_x_continuous(limits = c(0,11), breaks = seq(0,10,2))+
scale_y_continuous(limits = c(-2,10))

lineplot

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

library(patchwork)
lineplot+plottab





