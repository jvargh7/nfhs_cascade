unmet_cascade <- bind_rows(read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                           read_csv(file="analysis/nca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Diabetes")
) %>% 
  # dplyr::filter(n >= 50) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Undiagnosed","Untreated","Uncontrolled"),
                           labels=c("Diabetes","Undiagnosed \namong Diabetes","Untreated \namong Diagnosed","Uncontrolled \namong Diagnosed"))) %>% 
  dplyr::filter(!is.na(variable)) %>% 
  mutate(estimate_label = round(estimate,1),
         estimate_fig = case_when(variable == "Diabetes" ~ 120,
                                  TRUE ~ estimate))


figure_urban <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban") %>% 
  ggplot(data=.,aes(x=variable,y=n5_state,fill=estimate_fig,label=estimate_label)) +
  geom_tile() +
  geom_text() +
  facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_fill_gradient2("Estimate (%)",low="white",mid="lightblue",high="darkblue",midpoint = 50,limits=c(0,100),na.value = "white") +
  theme_bw() +
  scale_x_discrete(position = "top") + 
  xlab("") +
  ylab("")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 10),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

  
figure_rural <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural") %>% 
  ggplot(data=.,aes(x=variable,y=n5_state,fill=estimate_fig,label=estimate_label)) +
  geom_tile() +
  geom_text() +
  facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_fill_gradient2("Estimate (%)",low="white",mid="lightblue",high="darkblue",midpoint = 50,limits=c(0,100),na.value = "white") +
  theme_bw() +
  scale_x_discrete(position = "top") + 
  xlab("") +
  ylab("")  +
  theme(
    legend.text = element_text(size=12),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_blank(),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

require(ggpubr)
  ggarrange(figure_urban,figure_rural,nrow=1,ncol=2,
            labels = c("A","B"),
            common.legend = TRUE,legend="bottom",
            widths = c(2.2,1.5)) %>% 
    ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_heatmap cascade.png"),width=15,height=8)

  
ggarrange(figure_urban,figure_rural,nrow=1,ncol=2,
            labels = c("A","B"),
            common.legend = TRUE,legend="bottom",
            widths = c(2.2,1.5)) %>% 
    ggsave(.,filename = paste0(path_cascade_folder,"/writing/JAMA Int Med R2/Figure 2.jpg"),width=15,height=8)
  
  
ggarrange(figure_urban,figure_rural,nrow=1,ncol=2,
          labels = c("A","B"),
          common.legend = TRUE,legend="bottom",
          widths = c(2.2,1.5)) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/writing/JAMA Int Med R2/Figure 2.pdf"),width=15,height=8)  
  
