source(".Rprofile")

absolute_df <- bind_rows(
  read_csv(file = "age_standardized/ncz01_total age standardized national care cascade.csv"),
  read_csv(file = "age_standardized/ncz01_age standardized national care cascade.csv")) %>% 
  dplyr::mutate(residence = case_when(is.na(residence) ~ "Total",
                                      TRUE ~ residence)) %>% 
  dplyr::filter(is.na(strata),variable != "dm_screened") %>% 
  mutate(variable = factor(variable,
                           levels=c("dm_disease","dm_diagnosed","dm_treated","dm_controlled"),
                           labels=c("Diabetes","Diagnosed","Treated","Controlled")),
         residence = factor(residence,
                            levels=c("Total","Urban","Rural"))) %>% 
  arrange(residence,variable) %>% 
  group_by(residence) %>% 
  mutate(denominator = case_when(variable == "Diabetes" ~ estimate,
                                 TRUE ~ NA_real_)) %>% 
  mutate(denominator = zoo::na.locf(denominator)) %>% 
  mutate(drop = (1-(estimate*100/denominator)))

fig_absolute <- absolute_df %>% 
  ggplot(data=.,aes(x=residence,y=estimate,ymin=lci,ymax=uci,fill=variable,group=variable)) +
  geom_col(position=position_dodge(width=0.9),width=0.6,col="black") +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=c("#375a66","#698994","#cad8de","#eff3f5")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(color="grey80"),
        axis.text = element_text(size=16)
        ) +
  scale_y_continuous(breaks=seq(0,12,by=2),limits=c(0,12))


ggsave(fig_absolute,filename = paste0(path_cascade_folder,"/figures/nested and absolute diabetes cascade.png"),width=17.53,height=6.27)  
