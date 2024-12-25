library(tidyverse)
library(ggplot2)
library(patchwork)

#variables
g_start=0
g_end=150
g_sep=20

#data import
dfFig1=read.csv("df_fig1.csv")

#sigmoid fitting
fm<-nls(deltaBP~a*(1-exp(-(day/b)^c)),dfFig1,start=c(a=1.1,b=20,c=1.5),trace=FALSE)
a1=coef(fm)[1]
b1=coef(fm)[2]
c1=coef(fm)[3]

# make figures
g_fig1 <- ggplot(dfFig1,mapping=aes(x=day,y=deltaBP))+
  stat_function(fun=function(x) a1*(1-exp(-(x/b1)^c1)),color="black",linewidth=1.5)+
  geom_path(linewidth=1,mapping=aes(group=idxregion,color=vector),alpha=0.5)+
  geom_point(size=2,color="#555555")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.box = "vertical",
    legend.key.height = unit(4,"mm"),
    legend.text = element_text(size=12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="#AAAAAA",linetype="dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    # axis.title.y = element_text(vjust=0.5),
    axis.text=element_text(size=14,color="black"),
    axis.title=element_text(size=16)
    )+
  labs(title="",
       x="Days from viral vector injection",
       y="DREADD expression level",
       color="Viral Vector")+
  scale_x_continuous(breaks=seq(g_start,g_end,g_sep),limits=c(g_start,g_end), expand=c(0,NA))+
  scale_y_continuous(limits=c(0,3),expand=c(0,NA))

g_fig1
