library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(openxlsx)

# variables
M3color="#FF6666"
M4color="#00CCFF"
M4negative="#004ACC"
linesize=2
g_start =0
g_end=1600
g_seq=365
d60=60/365

sortid=c("#215","#236","#255","#241","#224","#223",
         "#218","#207","#238","#201","#229","#234","#245","#225","#163","#221","#153")
sortid1=c("215R","236J","255C","241J","224J","223C",
          "229R","234J","245J","225J","163R","221J","153R","201R","207R","218J","238J")

#-#-#-#-#-#-#-#
# data import #
#-#-#-#-#-#-#-#
df = read.xlsx("modulation_effect.xlsx",1) %>%
  pivot_longer(cols=c(start.y,end.y),names_to = "StartEnd",values_to = "year") %>%
  mutate(moid=factor(moid,levels=sortid))

#data frame of positive results
dfp = df %>%
  dplyr::filter(effect=="positive")

#data frame of 234 distinguished results
dfn = df %>%
  dplyr::filter(effect=="negative")

#functional assessments
petdate=read.csv(file="df_petdate.csv")      #dcz-pet
behavdate=read.csv(file="df_behavdate.csv")  #behavior
fdgdate=read.csv(file="df_fdgdate.csv")      #fdg-pet
histdate=read.csv(file="df_histdate.csv")    #perfusion

#plot for hM3Dq
gM3 <- ggplot(subset(dfp,dreadd=="hM3Dq"), mapping=aes(x=moid,y=year))+
  geom_hline(yintercept = c(d60,seq(0,3,1)),size=0.25,linetype="dashed",color="#DDDDDD")+
  geom_line(mapping=aes(group=group),linewidth=linesize,color=M3color)+
  geom_text(
    data=subset(subset(dfp,dreadd=="hM3Dq"),StartEnd=="end.y"),
    aes(x=moid,y=year,label=activation),,size=2.5,
    position=position_nudge(x=0.4,y=-0.05)
  )+
  #DCZ-PET timing
  geom_segment(data=subset(petdate,dreadd=="hM3Dq"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(1.5,'mm')),
               lwd=0.3,color="#FF0000")+
  #perfusion timing
  geom_segment(data=subset(histdate,dreadd=="hM3Dq"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=30,length=unit(1,'mm')),
               lwd=0.5,color="#FF0000")+
  #behavior timing
  geom_segment(data=subset(behavdate,dreadd=="hM3Dq"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(0.5,'mm')),
               lwd=0.2,color="#000000")+
  #FDG-PET timing
  geom_segment(data=subset(fdgdate,dreadd=="hM3Dq"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(0.5,'mm')),
               lwd=0.2,color="#000000")+
  #theme settings
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=8),
        legend.position = "none",
        plot.title = element_text(color=M3color,face="bold",size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8,color="black"),
        axis.title=element_text(size=10))+
  labs(title="hM3Dq",
       y="Years after viral vector injection",
       x="Monkey ID",
       color="DREADD")+
  #axis settings
  scale_x_discrete(expand=c(0,1))+
  scale_y_continuous(breaks=c(d60,seq(1,5,1)),limits=c(0,4.2),expand=c(0,0),labels = c("60 days",seq(1,5,1)))+
  coord_flip()

#plot for hM4Di
gM4 <- ggplot(subset(dfp,dreadd=="hM4Di"), mapping=aes(x=moid,y=year))+
  geom_hline(yintercept = c(d60,seq(0,3,1)),size=0.25,linetype="dashed",color="#DDDDDD")+
  geom_line(mapping=aes(group=group),linewidth=linesize,color=M4color)+
  geom_line(data=dfn,mapping=aes(x=moid,y=year),color=M4negative,size=linesize)+
  geom_text(
    data=subset(subset(dfp,dreadd=="hM4Di"),StartEnd=="end.y"),
    aes(x=moid,y=year,label=activation),,size=2.5,
    position=position_nudge(x=0.4,y=-0.05)
  )+
  #DCZ-PET timing
  geom_segment(data=subset(petdate,dreadd=="hM4Di"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(1.5,'mm')),
               lwd=0.3,color="#FF0000")+
  #perfusion timing
  geom_segment(data=subset(histdate,dreadd=="hM4Di"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=30,length=unit(1,'mm')),
               lwd=0.5,color="#FF0000")+
  #behavior timing
  geom_segment(data=subset(behavdate,dreadd=="hM4Di"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(0.5,'mm')),
               lwd=0.2,color="#000000")+
  #FDG-PET timing
  geom_segment(data=subset(fdgdate,dreadd=="hM4Di"),
               aes(x=moid,y=years,xend=moid,yend=years),
               arrow=arrow(angle=90,length=unit(0.5,'mm')),
               lwd=0.2,color="#000000")+
  labs(title="hM4Di",
       y="",
       x="Monkey ID",
       color="DREADD")+
  #theme settings
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size=8),
        legend.position = "none",
        plot.title = element_text(color=M4color,face="bold",size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8,color="black"),
        axis.title=element_text(size=10))+
  #axis settings
  scale_x_discrete(expand=c(0,1))+
  scale_y_continuous(breaks=c(d60,seq(1,5,1)),limits=c(0,4.2),expand=c(0,0),labels = c("60 days",seq(1,5,1)))+
  coord_flip()

#figure layout
p=gM4/gM3+plot_layout(height=c(9,5))+plot_annotation(tag_levels = "A")
p
