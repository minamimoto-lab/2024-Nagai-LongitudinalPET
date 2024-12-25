library(tidyverse)
library(ggplot2)
library(patchwork)
library(MASS)
library(car)

regions=c('Amyg','Cd','rmCD','Put','VPL','dlPFC','S1','OFC')

# data import
dat=read.csv("fig5_df.csv")

#stats
fit <- lm(deltaBP ~ titer * dreadd * promoter * tag * serotype,data=dat)
resultAIC=MASS::stepAIC(fit)
resultAIC$anova
bestfit <- lm(deltaBP ~ titer + dreadd + promoter + tag + titer:dreadd + titer:promoter + dreadd:promoter, data=dat)
bestfit %>% rstatix::anova_test()
bestfit %>% car::vif(type="predictor")

# make graphs
g_titer = ggplot(data=dat,mapping=aes(x=titer,y=deltaBP))+
  geom_point(size=3,alpha=1,aes(color=dreadd))+
  geom_smooth(method = "lm",se=FALSE,color="black")+
  geom_smooth(method = "lm",se=FALSE,aes(color=dreadd))+
  theme_classic()+
  theme(axis.text=element_text(color="black"),
        strip.background = element_blank(),
        legend.title = element_text(size=10),
        legend.text=element_text(size=9),
        legend.key.height = unit(4,"mm"), 
        axis.title=element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)
  )+
  labs(title="",x=expression(paste("Titer (",{x10^13},")")),y=expression(paste("Peak ",Delta,{BP[ND]})),color="DREADD")+
  scale_color_manual(values=c("hM3Dq"="#FF6666","hM4Di"="#00CCFF"))+
  scale_x_continuous(limits = c(0,5.25),expand=c(0,NA))+
  scale_y_continuous(breaks=seq(0,4,0.5))

g_tag = ggplot(data=dat,mapping=aes(x=tag,y=deltaBP))+
  geom_boxplot(outliers=FALSE)+
  geom_point(position=position_jitter(width=0.1,seed=1),size=2,alpha=1,aes(color=dreadd))+
  theme_classic()+
  theme(axis.text=element_text(color="black"),
        legend.position = "none",
        strip.background = element_blank(),
        axis.title=element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)
  )+
  labs(title="",x="Tag",y=expression(paste("Peak ",Delta,{BP[ND]})),color="DREADD")+
  scale_color_manual(values=c("hM3Dq"="#FF6666","hM4Di"="#00CCFF"))+
  scale_y_continuous(breaks=seq(0,4,0.5))+
  scale_x_discrete(label=c("5'-HA","IRES-AcGFP","None"))

#figure layout
gfig5=(g_titer|g_tag)+plot_layout(width=c(7,6))+plot_annotation(tag_levels = "A")
gfig5
