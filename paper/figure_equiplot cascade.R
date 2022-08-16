unmet_cascade <- read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
  dplyr::filter(is.na(stratification)) %>% 
  mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
  mutate(variable = factor(variable,levels=c("Unscreened","Undiagnosed","Untreated","Uncontrolled")))

fig_uc <- unmet_cascade %>% 
  ggplot(data=.,aes(x = n5_state,y = estimate,
                    group=interaction(residence,n5_state),
                    col=variable,
                    shape=residence)) +
  geom_point(position = position_dodge(width=0.9),size=3) +
  geom_path(position = position_dodge(width=0.9),col="black") +
  theme_bw() + 
  coord_flip() +
  facet_wrap(zone~.,scales="free_y") +
  scale_color_manual(name="",values=c("darkblue","red","green","orange")) +
  scale_shape_discrete(name="") +
  theme(
        legend.text = element_text(size=16),
        axis.text = element_text(size = 14),
        legend.position = "bottom") +
  # scale_y_continuous(limits=c(0,50)) +
  ylab("Prevalence (%)") +
  xlab("") 


fig_uc %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_equiplot cascade.png"),width=15,height=8)
