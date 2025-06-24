library(tidyverse)
library(patchwork)
library(ggrepel)
library(openxlsx)
#library(ggpmisc)

rm(list=ls())

#-#-#-#-#-#-#-#-#
#  data import  #
#-#-#-#-#-#-#-#-#
df=read.csv(file="fig2_rev.csv")

df_m3=df %>%
  dplyr::filter(dreadd=="hM3Dq") %>%
  mutate(year2=if_else(day<81,60/365.25,round(year*2)/2))

df_m4=df %>%
  dplyr::filter(dreadd=="hM4Di") %>%
  mutate(year2=if_else(day<81,60/365.25,round(year*2)/2))

# for label
df_m3_label = df_m3 %>%
  group_by(idxregion) %>%
  mutate(last=max(day)) %>%
  dplyr::filter(day==last)

df_m4_label = df_m4 %>%
  group_by(idxregion) %>%
  mutate(last=max(day)) %>%
  dplyr::filter(day==last)

# averaging
df_m3_mean=df_m3 %>%
  group_by(year2) %>%
  summarise(avg=mean(ratio))

df_m4_mean=df_m4 %>%
  group_by(year2) %>%
  summarise(avg=mean(ratio))

#-#-#-#-#-#-#
# variables #
#-#-#-#-#-#-#
xlab2=expression(paste("Years after viral vector injection"))
x1=40/365.25
x2=80/365.25
m3figratio=c(5.3,1)
m3shapeidx=c(23,21,22,24,25)
m4shapeidx=c(21,22)
M3color="#FF6666"
M4color="#00CCFF"
M4negative="#004ACC"

#-#-#-#-#-#-#-#-#
# make figures  #
#-#-#-#-#-#-#-#-#
g_m3_ratio = ggplot(df_m3,aes(x=year,y=ratio))+
  geom_vline(xintercept=c(x1,x2),linetype="dotted",color="#FF6666",linewidth=0.25)+
  geom_line(aes(color=vector,group=idxregion),linewidth=0.25)+
  geom_point(size=1,aes(color=vector,group=idxregion))+
  geom_line(data = df_m3_mean,aes(x=year2,y=avg),linewidth=0.25,linetype="dashed")+
  geom_text_repel(
    data=df_m3_label,
    aes(label=symbol,color=vector),
    size=2
  )+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size=8),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size=7),
    legend.text = element_text(size=6),
    legend.key.height = unit(3,"mm"), 
    plot.title = element_text(color=M3color,face="bold",size=8),
    panel.grid.major.x= element_line(color="#AAAAAA",linetype="dotted",linewidth=0.25),
    panel.grid.major.y= element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=7,color="black"),
    axis.title=element_text(size=8)
  )+
  scale_y_continuous(limits=c(0,135),breaks=seq(0,150,25))+
  scale_x_continuous(limits=c(0,3.1),breaks=seq(0,5,1))+
  scale_shape_manual(values=m3shapeidx)+
  labs(
    title="hM3Dq",
    x=xlab2,
    y=expression(paste("Percent of peak ",Delta,{BP[ND]})),
    color="Vector (hM3Dq)"
  )

g_m3_ratio

g_m4_ratio = ggplot(df_m4,aes(x=year,y=ratio))+
  geom_vline(xintercept=c(x1,x2),linetype="dotted",color="#FF6666",linewidth=0.25)+
  geom_line(aes(color=vector,group=idxregion),linewidth=0.25)+
  geom_point(size=1,aes(color=vector,group=idxregion))+
  geom_line(data = df_m4_mean,aes(x=year2,y=avg),size=0.25,linetype="dashed")+
  geom_text_repel(
    max.overlaps = 20,
    data=df_m4_label,
    aes(label=symbol,color=vector),
    size=2
  )+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size=8),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size=7),
    legend.text = element_text(size=6),
    legend.key.height = unit(3,"mm"), 
    plot.title = element_text(color=M4color,face="bold",size=9),
    panel.grid.major.x= element_line(color="#AAAAAA",linetype="dotted",linewidth=0.25),
    panel.grid.major.y= element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=7,color="black"),
    axis.title=element_text(size=8)
  )+
  scale_y_continuous(limits=c(0,135),breaks=seq(0,150,25))+
  scale_x_continuous(limits=c(0,4),breaks=seq(0,5,1))+
  scale_shape_manual(values=m4shapeidx)+
  labs(
    title="hM4Di",
    x=xlab2,
    y=expression(paste("Percent of peak ",Delta,{BP[ND]})),
    color="Vector (hM4Di)"
  )

g_m4_ratio

gfig2_ratio=g_m4_ratio/(g_m3_ratio+plot_spacer()+plot_layout(guides="collect",widths=m3figratio))+
  plot_annotation(tag_levels = list(c("B","C")),theme=theme(plot.tag = element_text(size=9)))
gfig2_ratio

#end of note