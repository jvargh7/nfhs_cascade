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
  dplyr::select(f_age,f_dm_disease,f_dm_diagnosed,f_dm_treated,f_dm_controlled) %>% 
  pivot_longer(cols=-f_age,names_to="cascade",values_to="status") %>% 
  mutate(cascade = str_replace(cascade,"f_dm_","") %>% str_to_title(.)) %>% 
  mutate(cascade = factor(cascade,levels=c("Disease","Diagnosed","Treated","Controlled"),ordered=TRUE)) %>% 
  ggplot(data=,aes(x=f_age,y=status*100,group=cascade,col=cascade)) +
  coord_cartesian(ylim =c(0,25)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4)) +
 theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("black","red" ,"blue","darkgreen" )) +
  theme(legend.text = element_text(size = 14))

figB <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Rural") %>% 
  dplyr::select(f_age,f_dm_disease,f_dm_diagnosed,f_dm_treated,f_dm_controlled) %>% 
  pivot_longer(cols=-f_age,names_to="cascade",values_to="status") %>% 
  mutate(cascade = str_replace(cascade,"f_dm_","") %>% str_to_title(.)) %>% 
  mutate(cascade = factor(cascade,levels=c("Disease","Diagnosed","Treated","Controlled"),ordered=TRUE)) %>% 
  ggplot(data=,aes(x=f_age,y=status*100,group=cascade,col=cascade)) +
  coord_cartesian(ylim =c(0,25)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4)) +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("black","red" ,"blue","darkgreen" )) +
  theme(legend.text = element_text(size = 14))

figC <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Urban") %>% 
  dplyr::select(f_age,f_htn_disease,f_htn_diagnosed,f_htn_treated,f_htn_controlled) %>% 
  pivot_longer(cols=-f_age,names_to="cascade",values_to="status") %>% 
  mutate(cascade = str_replace(cascade,"f_htn_","") %>% str_to_title(.)) %>% 
  mutate(cascade = factor(cascade,levels=c("Disease","Diagnosed","Treated","Controlled"),ordered=TRUE)) %>% 
  ggplot(data=,aes(x=f_age,y=status*100,group=cascade,col=cascade)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4)) +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("black","red" ,"blue","darkgreen" )) +
  theme(legend.text = element_text(size = 14))

figD <- nfhs5_women %>% 
  dplyr::filter(hv025 == "Rural") %>% 
  dplyr::select(f_age,f_htn_disease,f_htn_diagnosed,f_htn_treated,f_htn_controlled) %>% 
  pivot_longer(cols=-f_age,names_to="cascade",values_to="status") %>% 
  mutate(cascade = str_replace(cascade,"f_htn_","") %>% str_to_title(.)) %>% 
  mutate(cascade = factor(cascade,levels=c("Disease","Diagnosed","Treated","Controlled"),ordered=TRUE)) %>% 
  ggplot(data=,aes(x=f_age,y=status*100,group=cascade,col=cascade)) +
  coord_cartesian(ylim =c(0,100)) +
  geom_smooth(method="lm",formula=y ~ ns(x,4)) +
  theme_bw() +
  xlab("Age in years") +
  ylab("Prevalence (%)") +
  scale_color_manual(name="",values=c("black","red" ,"blue","darkgreen" )) +
  theme(legend.text = element_text(size = 14))

ggarrange(figA,figB,figC,figD,
          labels = LETTERS[1:4],ncol = 2,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/age trends consolidated by urban rural.png"),width = 10,height=7)
