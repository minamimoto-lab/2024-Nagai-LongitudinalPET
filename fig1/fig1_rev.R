library(tidyverse)
library(minpack.lm)
library(patchwork)
library(openxlsx)
library(ggrepel)

rm(list=ls())

#variables
g_start=0
g_end=150
g_sep=20


#data import
df_fig1rev=read.csv("fig1_rev.csv")

dflast=dfFig1rev %>%
  group_by(idxregion) %>%
  dplyr::filter(day==max(day)) %>%
  ungroup()

# estimation
dat_for_nls <-
  dfFig1rev %>%
  mutate(y = deltaBP,
         t = day) %>%
  select(y, t)

# double logistic
model_dl <-
  minpack.lm::nlsLM(
    y ~ a + b / (1 + exp(-c * (t - d))) - e / (1 + exp(-g * (t - h))),
    data = dat_for_nls,
    start = list(
      a = 0.5,     # 最低値付近
      b = 1.5,    # 上昇量（0→ピーク付近）
      c = 0.01,    # 緩やかな立ち上がり
      d = 0.3,     # 上昇の中心（t = 0〜50あたり）
      e = 1,    # 下降量（ピーク後の安定値への移行）
      g = 0.05,   # 緩やかな下降（または 0.01〜0.1）
      h = 75      # 下降の中心
    ),
    control = minpack.lm::nls.lm.control(maxiter = 500),
    trace = TRUE
  )

coef(model_dl)

dat_pred_dl <-
  tibble(
    t = seq(0, 150, by =0.1),
    y = model_dl %>% predict(data.frame(t = seq(0, 150, by = 0.1)))
  )

# simple sigmoid
model_sig <-
  minpack.lm::nlsLM(
    y ~ a*(1-exp(-(t/b)^c)),
    data = dat_for_nls,
    start = list(
      a = 1.1,
      b = 20,
      c = 1.5
    ),
    control = minpack.lm::nls.lm.control(maxiter = 500),
    trace = TRUE
  )

coef(model_sig)

dat_pred_sig <-
  tibble(
    t = seq(0, 150, by =0.1),
    y = model_sig %>% predict(data.frame(t = seq(0, 150, by = 0.1)))
  )

# BIC
model_dl %>% BIC()
model_sig %>% BIC()

#plot
g1rev = dat_for_nls %>%
  ggplot() +
  aes(x = t, y = y) +
  geom_path(data = dat_pred_sig,color="black",linewidth = 1.5)+
  geom_path(data=df_fig1rev,aes(x=day,y=deltaBP,color=vector,group=idxregion,linetype=dreadd),linewidth=1,alpha=0.5) +
  geom_point(data=df_fig1rev,aes(x=day,y=deltaBP,fill = vector,group=idxregion),size=2,shape=21,color="#555555")+
  geom_text_repel(data=dflast,aes(x=day,y=deltaBP,label=symbol,color=vector))+
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
    axis.text=element_text(size=14,color="black"),
    axis.title=element_text(size=16)
  )+
  labs(title="",
       x="Days from viral vector injection",
       y=expression(paste("DREADD expression level (",Delta,{BP[ND]},")")),
       color="Viral Vector",
       linetypw="DREADD")+
  scale_x_continuous(breaks=seq(g_start,g_end,g_sep),limits=c(g_start,g_end), expand=c(0,NA))+
  scale_y_continuous(limits=c(0,3),expand=c(0,NA))

g1rev

# end of note
