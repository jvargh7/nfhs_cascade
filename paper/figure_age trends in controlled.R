nfhs5_women <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS"))  %>%
  dplyr::filter(hv024 %in% v024_nfhs5_14states) %>% 
  mutate(hv025 = factor(hv025,levels=c(1,2),labels=c("Urban","Rural")),
         phase = factor(phase,levels=c(1,2),labels=c("Phase 1","Phase 2")))

nfhs5_men <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS"))  %>%
  dplyr::filter(hv024 %in% v024_nfhs5_14states) %>% 
  mutate(hv025 = factor(hv025,levels=c(1,2),labels=c("Urban","Rural")),
         phase = factor(phase,levels=c(1,2),labels=c("Phase 1","Phase 2")))

require(splines)
require(ggpubr)

figA <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Urban") %>% 
  ggplot(data=,aes(x=f_age,y=f_dm_controlled*100,group=phase,col = phase)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue")) +
  theme(legend.text = element_text(size = 14))

figB <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Rural") %>% 
  ggplot(data=,aes(x=f_age,y=f_dm_controlled*100,group=phase,col = phase)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue")) +
  theme(legend.text = element_text(size = 14))

figC <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Urban") %>% 
  ggplot(data=,aes(x=f_age,y=f_htn_controlled*100,group=phase,col = phase)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue")) +
  theme(legend.text = element_text(size = 14))

figD <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Rural") %>% 
  ggplot(data=,aes(x=f_age,y=f_htn_controlled*100,group=phase,col = phase)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4),fill="grey80") +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("red","darkblue")) +
  theme(legend.text = element_text(size = 14))

ggarrange(figA,figB,figC,figD,
          labels = LETTERS[1:4],ncol = 2,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/age trends controlled by urban rural.png"))
