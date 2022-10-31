###Script for creating graphs to showcase how we think the risk index varaibles are related to susceptibility


x1<-seq(1,20)
v1<-seq(1,20)
v2<-seq(20,1)
x2<-c(1,2,3,4,5)
v3<-c(20,20,15,10,5)
v4<-c(0,0,1,1,2,2,3,3)
x3<-c(0,1,1,2,2,3,3,4)

v5<-c(3,2,1,1,2,3)
x4<-c(1,2,3,4,5,6)

plot(x,v1)

library(ggplot2)


#change name of y axis ticks to low and high and remove the other ticks
#change name of x axis ticks to high and low or dry and wer etc. and remove other ticks 

#Volume of spruce
p1<-ggplot()+
  geom_line(aes(x1,v1))+
  xlab("Volume of spruce")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15))+                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

+
  scale_x_discrete(breaks = c(2,18), labels=c("Low","High")) #why is this not working??

#Volume of birch
p2<-ggplot()+
  geom_line(aes(x1,v2))+
  xlab("Volume of birch")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15)) +                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


#Soil moisture
p3<-ggplot()+
  geom_line(aes(x2,v3))+
  xlab("Soil moisture")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15)) +                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


#Temperature sum
p4<-ggplot()+
  geom_line(aes(x3,v4))+
  xlab("Temperature sum")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15)) 

#Stand density
y4<--(x4^2)+x4+25
ggplot()+
  geom_line(aes(x4,y4))

p5<-ggplot()+
  geom_line(aes(x4,v5))+
  xlab("Stand density")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15))+
  ylim(0.5,3)+                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 



#Storm
x5<-c(0,1,1,2)
v6<-c(0,0,1,1)
p6<-ggplot()+
  geom_line(aes(x5,v6))+
  xlab("Storm occurence")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15))+                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


#Age structure 
p7<-ggplot()+
  geom_line(aes(x1,v2))+
  xlab("Age structure")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15))+                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 


#Diameter
p8<-ggplot()+
  geom_line(aes(x5,v6))+
  xlab("Diameter")+
  ylab("SBB susceptibility")+
  theme_classic()+
  theme(text = element_text(size = 15))+                                                           
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

library(ggpubr)

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8)

ggarrange(p1,p2,p3,p5,p7,p8)
