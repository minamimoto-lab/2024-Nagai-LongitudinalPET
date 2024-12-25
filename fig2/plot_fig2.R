library(tidyverse)
library(ggplot2)
library(patchwork)

#variables
xlab2=expression(paste("Years after viral vector injection"))
x1=40/365.25
x2=80/365.25
m3figratio=c(3.7,1)
m3shapeidx=c(23,21,22,24,25)
m4shapeidx=c(21,22)
M3color="#FF6666"
M4color="#00CCFF"
M4negative="#004ACC"

#data import
df=read.csv(file="fig2_df.csv") 

df_m3=df %>%
  dplyr::filter(dreadd=="hM3Dq")

df_m4=df %>%
  dplyr::filter(dreadd=="hM4Di")

# make figures
g_m3_ratio = ggplot(df_m3,aes(x=year,y=ratio,group=idxregion))+
  geom_vline(xintercept=c(x1,x2),linetype="dotted",color="#FF6666")+
  geom_line(aes(color=vector))+
  geom_point(size=2,aes(shape=serotype2,fill = vector))+
  #テーマの設定
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size=16),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size=15),
    legend.text=element_text(size=14),
    legend.key.height = unit(4,"mm"), 
    plot.title = element_text(color=M3color,face="bold",size=18),
    panel.grid.major.x = element_line(color="#AAAAAA",linetype="dotted"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=14,color="black"),
    axis.title=element_text(size=16)
    )+
  scale_y_continuous(limits=c(0,150),breaks=seq(0,150,25),expand=c(0,NA))+
  scale_x_continuous(limits=c(0,3.1),breaks=seq(0,5,1),expand=c(0,0))+
  scale_shape_manual(values=m3shapeidx)+
  labs(
    title="hM3Dq",
    x=xlab2,
    y=expression(paste("Ratio to peak ",Delta,{BP[ND]},"(%)")),
    color="Vector (hM3Dq)",
    fill="Vector (hM3Dq)",
    shape="Serotype"
  )

g_m4_ratio = ggplot(df_m4,aes(x=year,y=ratio,group=idxregion))+
  geom_vline(xintercept=c(x1,x2),linetype="dotted",color="#FF6666")+
  geom_line(aes(color=vector))+
  geom_point(size=2,aes(shape=serotype2,fill = vector))+
  #テーマの設定
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size=16),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size=15),
    legend.text=element_text(size=14),
    legend.key.height = unit(4,"mm"), 
    plot.title = element_text(color=M4color,face="bold",size=18),
    panel.grid.major.x= element_line(color="#AAAAAA",linetype="dotted"),
    panel.grid.major.y= element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text=element_text(size=14,color="black"),
    axis.title=element_text(size=16)
    )+
  scale_y_continuous(limits=c(0,125),breaks=seq(0,150,25),expand=c(0,NA))+
  scale_x_continuous(limits=c(0,4),breaks=seq(0,5,1),expand=c(0,NA))+
  scale_shape_manual(values=m4shapeidx)+
  labs(
    title="hM4Di",
    x=xlab2,
    y=expression(paste("Ratio to peak ",Delta,{BP[ND]},"(%)")),
    color="Vector (hM4Di)",
    fill="Vector (hM4Di)",
    shape="Serotype"
  )

gfig2_ratio=g_m4_ratio/(g_m3_ratio+plot_spacer()+plot_layout(guides="collect",widths=m3figratio))+
  plot_annotation(tag_levels = list(c("B","C")),theme=theme(plot.tag = element_text(size=18)))

gfig2_ratio
