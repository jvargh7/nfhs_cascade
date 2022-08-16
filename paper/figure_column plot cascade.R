unmet_cascade <- read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
  dplyr::filter(is.na(stratification)) %>% 
  mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
  mutate(variable = factor(variable,levels=c("Unscreened","Undiagnosed","Untreated","Uncontrolled")))

fig_uc <- unmet_cascade %>% 
  ggplot(data=.,aes(x = n5_state,y = estimate,
                    group=interaction(residence,n5_state),
                    fill=residence)) +
  geom_col(position=position_dodge(width=0.9)) +
  theme_bw() + 
  coord_flip() +
  facet_grid(zone~variable,scales="free_y",space="free_y") +
  scale_fill_manual(name="",values=c("darkblue","red")) +
  scale_shape_discrete(name="") +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.text = element_text(size=12),
    legend.position = "bottom") +
  # scale_y_continuous(limits=c(0,50)) +
  ylab("Prevalence (%)") +
  xlab("") 


fig_uc %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_column cascade.png"),width=15,height=8)
