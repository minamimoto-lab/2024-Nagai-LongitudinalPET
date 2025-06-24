#titerが高すぎるoutliersを外した解析
#20250605

library(tidyverse)
library(patchwork)
library(ggrepel)
library(rstatix)
library(openxlsx)

rm(list=ls())

#領域の順番設定
arr=c("Caudate","Putamen","Thalamus","Amygdala","dlPFC","OFC","S1")

# symbol
dat <- read.csv("fig5_rev.csv") %>%
  mutate(region3=factor(region3,levels=arr)) %>%
  dplyr::filter(injvolume<10) %>%  #dlPFCおよびOFCの除外
  dplyr::filter(titer<4.6)         #outliersの除外

#------------------
# check multicolinearity
lm(deltaBP ~ titer + dreadd + promoter + tag + serotype + injvolume, data = dat) %>%
  car::vif(type = "predictor")

#------------------
# model
fm <- lm(deltaBP ~ titer * dreadd * promoter * tag * serotype * injvolume, data = dat)
MASS::stepAIC(fm, direction = "both")

best_fm <-
  lm(formula = deltaBP ~ titer + dreadd + promoter + tag + serotype + 
       injvolume + titer:promoter + dreadd:promoter + titer:tag + 
       titer:injvolume + dreadd:injvolume, data = dat)
best_fm %>% rstatix::anova_test()

#------------------
# make graph

dat = dat %>%
  mutate(tag_numeric = as.numeric(as.factor(tag))) %>%
  mutate(jittered_x_t=jitter(tag_numeric,amount=0.2)) %>%
  mutate(promo_numeric=as.numeric(as.factor(promoter)))%>%
  mutate(jittered_x_p=jitter(promo_numeric,amount=0.2))

g_tag = ggplot(data=dat,mapping=aes(x=tag,y=deltaBP))+
  geom_boxplot(outliers=FALSE)+
  geom_point(size=2,alpha=1,aes(x=jittered_x_t))+
  geom_text_repel(aes(label=symbol,x=jittered_x_t),size=3)+
  # geom_text_repel(aes(label=idxregion,x=jittered_x,color=dreadd),size=3)+
  theme_classic()+
  theme(axis.text=element_text(color="black"),
        # legend.position = "none",
        legend.position.inside = c(0.15,0.8),
        strip.background = element_blank(),
        axis.title=element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)
  )+
  labs(title="",x="Tag",y=expression(paste("Peak ",Delta,{BP[ND]})),color="DREADD")+
  # scale_color_manual(values=c("hM3Dq"="#FF6666","hM4Di"="#00CCFF"))+
  scale_y_continuous(limits = c(0,3.5),breaks=seq(0,4,0.5),expand=c(0,NA))+
  scale_x_discrete(label=c("5'-HA","IRES-AcGFP","None"))

g_tag

g_promoter=ggplot(data=dat,mapping=aes(x=promoter,y=deltaBP))+
  geom_boxplot(outliers=FALSE)+
  geom_point(size=2,alpha=1,aes(x=jittered_x_p,color=dreadd))+
  geom_text_repel(aes(label=symbol,x=jittered_x_p,color=dreadd),size=3)+
  theme_classic()+
  theme(axis.text=element_text(color="black"),
        legend.position = c(0.9,0.8),
        strip.background = element_blank(),
        strip.text = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)
  )+
  labs(title="",x="Promoter",y=expression(paste("Peak ",Delta,{BP[ND]})),color="DREADD")+
  facet_wrap(~dreadd)+
  scale_color_manual(values=c("hM3Dq"="#FF6666","hM4Di"="#00CCFF"))+
  scale_y_continuous(limits = c(0,3.5),breaks=seq(0,4,0.5),expand=c(0,NA))

g_promoter

#end of note