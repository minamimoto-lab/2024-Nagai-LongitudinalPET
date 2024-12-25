library(tidyverse)
library(ggplot2)
library(openxlsx)
library(patchwork)

# variables
set.seed(123)
sortHemisphere = c("Ipsi","Contra")
sortTiming = c("positive","extinguished")
sortCondition = c("Ctrl","Cold")

HandContraColor="#EE2D2A"
HandIpsiColor="#2471B4"
FootColdColor="#EC008C"
FootCtrlColor="#1FA7DD"

mytheme = theme(
  strip.background = element_blank(),
  legend.position = "none",
  panel.border = element_blank(),
  strip.text = element_text(size=12),
  axis.line.y.left = element_line(color="black",linewidth=1),
  axis.ticks.y = element_line(linewidth=1),
  axis.line.x=element_blank(),
  axis.ticks.x = element_blank(),
  axis.text = element_text(color="black"),
  panel.grid = element_blank()
)

#import data of fine grasping
dfHand=read.xlsx("fig4.xlsx",1) %>%
  mutate(hemisphere=factor(hemisphere,levels=sortHemisphere)) %>%
  mutate(timing=factor(timing,levels=sortTiming)) %>%
  mutate(hemi_numeric=as.numeric(as.factor(hemisphere)))%>%
  mutate(jittered_x=jitter(hemi_numeric,amount=0.1))
  
#import data of foot cold
dfFoot=read.xlsx("fig4.xlsx",2) %>%
  mutate(hemisphere=factor(hemisphere,levels=sortHemisphere)) %>%
  mutate(timing=factor(timing,levels=sortTiming)) %>%
  mutate(condition=factor(condition,levels=sortCondition))%>%
  mutate(cond_numeric=as.numeric(as.factor(condition)))%>%
  mutate(jittered_x=jitter(cond_numeric,amount=0.1))

#make figure for Fine grasping
#positive results
g_hand_p = ggplot()+
  geom_errorbar(data=subset(dfHandSummary,timing=="positive"),aes(x=hemisphere,ymin=mean-se,ymax=mean+se),width=0)+
  geom_col(data=subset(dfHandSummary,timing=="positive"),aes(x=hemisphere,y=mean,fill=hemisphere),width=0.5)+
  geom_hline(yintercept = 0,linewidth=1)+
  geom_point(data=subset(dfHand,timing=="positive"),
             aes(x=jittered_x,y=value,group=session),
             color="#BBBBBB")+
  geom_line(data=subset(dfHand,timing=="positive"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  scale_fill_manual(values=c(HandIpsiColor,HandContraColor))+
  scale_y_continuous(limit=c(-20,50),expand=c(0,0),breaks=seq(-20,50,70))+
  labs(title="",x="")+
  ylab(expression(atop(Delta ~ "duration for","fine grasping (%)"))) +
  theme_bw()+
  mytheme+
  theme(
    axis.text.x=element_text(color=ifelse(dfHandSummary$hemisphere=="Ipsi",HandIpsiColor,HandContraColor))
  )

#extinguished results
g_hand_e = ggplot()+
  geom_errorbar(data=subset(dfHandSummary,timing=="extinguished"),aes(x=hemisphere,ymin=mean-se,ymax=mean+se),width=0)+
  geom_col(data=subset(dfHandSummary,timing=="extinguished"),aes(x=hemisphere,y=mean,fill=hemisphere),width=0.5)+
  geom_hline(yintercept = 0,linewidth=1)+
  geom_point(data=subset(dfHand,timing=="extinguished"),
             aes(x=jittered_x,y=value,group=session),
             color="#BBBBBB")+
  geom_path(data=subset(dfHand,timing=="extinguished"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  scale_fill_manual(values=c("Ipsi"=HandIpsiColor,"Contra"=HandContraColor))+
  scale_y_continuous(limit=c(-20,50),expand=c(0,0),breaks=seq(-20,50,70))+
  labs(title="", x="")+
  ylab(expression(atop(Delta ~ "duration for","fine grasping (%)"))) +
  theme_bw()+
  mytheme+
  theme(
    axis.title.y = element_blank(),
    axis.text.x=element_text(color=ifelse(dfHandSummary$hemisphere=="Ipsi",HandIpsiColor,HandContraColor))
  )

#make figure for foot cold
#positive results
g_foot_p = ggplot()+
  geom_errorbar(data=subset(dfFootSummary,timing=="positive"),aes(x=condition,ymin=mean-se,ymax=mean+se),width=0)+
  geom_col(data=subset(dfFootSummary,timing=="positive"),aes(x=condition,y=mean,fill=condition),width=0.5)+
  geom_hline(yintercept = 0,linewidth=1)+
  geom_point(data=subset(dfFoot,timing=="positive"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  geom_path(data=subset(dfFoot,timing=="positive"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  scale_fill_manual(values=c("Cold"=FootColdColor,"Ctrl"=FootCtrlColor))+
  facet_wrap(~hemisphere)+
  scale_y_continuous(limit=c(-100,100),expand=c(0,0),breaks=seq(-100,100,200))+
  labs(title="",
       x="",
       y=expression(paste(Delta,"withdrawal latency (%)")))+
  theme_bw()+
  mytheme+
  theme(
    axis.text.x=element_text(color=ifelse(dfFootSummary$condition=="Ctrl",FootCtrlColor,FootColdColor))
  )

#extinguished results
g_foot_e = ggplot()+
  geom_errorbar(data=subset(dfFootSummary,timing=="extinguished"),aes(x=condition,ymin=mean-se,ymax=mean+se),width=0)+
  geom_col(data=subset(dfFootSummary,timing=="extinguished"),aes(x=condition,y=mean,fill=condition),width=0.5)+
  geom_hline(yintercept = 0,linewidth=1)+
  geom_point(data=subset(dfFoot,timing=="extinguished"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  geom_path(data=subset(dfFoot,timing=="extinguished"),aes(x=jittered_x,y=value,group=session),color="#BBBBBB")+
  scale_fill_manual(values=c("Cold"=FootColdColor,"Ctrl"=FootCtrlColor))+
  facet_wrap(~hemisphere)+
  scale_y_continuous(limit=c(-100,100),expand=c(0,0),breaks=seq(-100,100,200))+
  labs(title="",x="",
       y=expression(paste(Delta,"withdrawal latency (%)")))+
  theme_bw()+
  mytheme+
  theme(
    axis.title.y = element_blank(),
    axis.text.x=element_text(color=ifelse(dfFootSummary$condition=="Ctrl",FootCtrlColor,FootColdColor))
  )

#figure layout
g=(plot_spacer()+g_hand_p+g_hand_e+plot_spacer()+plot_layout(widths=c(2,3,3,2)))/(g_foot_p|g_foot_e)
g
