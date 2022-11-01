
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre03_nfhs5 total svydesign.R")


fig_fasting <- nfhs5_df %>% 
  mutate(lastate_categories = case_when(lastate %in% c(0:2) ~ 1,
                                     lastate %in% c(3:7) ~ 2,
                                     TRUE ~ 3),
         lastdrank_categories = case_when(lastdrank %in% c(0:2) ~ 1,
                                       lastdrank %in% c(3:7) ~ 2,
                                     TRUE ~ 3)
         ) %>% 
  mutate_at(vars(lastate_categories,lastdrank_categories),function(x) factor(x,levels=c(1:3),labels=c("0 to 2h","3 to 7h",">=8h"))) %>% 
  group_by(lastate_categories,lastdrank_categories) %>% 
  tally() %>%
  ungroup() %>% 
  dplyr::mutate(prop = n/sum(n)) %>% 
  ggplot(data=.,aes(x=lastate_categories,y=lastdrank_categories,fill=prop*100,label=round(prop*100,1))) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2("Crude (%)",low="darkgreen",mid="yellow",high="red",midpoint = 50,limits=c(0,100)) +
  theme_bw() +
  xlab("Last Ate (hours)") +
  ylab("Last Drank (hours)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.ticks.y = element_blank(),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")


ggsave(fig_fasting,filename = paste0(path_cascade_folder,"/figures/figure_fasting times.png"),width=8,height=8)
