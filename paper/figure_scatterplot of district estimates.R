district_met <- bind_rows(read_csv(file = "analysis/nca08_district met need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                           read_csv(file="analysis/nca04_district2018 level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Diabetes")
) %>% 
  dplyr::filter(!variable %in% c("Screened","Highbp","Invhighbp")) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Diagnosed","Treated","Controlled"))) %>% 
  mutate(kl_comparison = case_when(n5_state == "Kerala" & REGNAME %in% c("Kottayam","Ernakulam") ~ "Central Kerala",
                                       n5_state == "Kerala" & REGNAME %in% c("Wayanad","Kasaragod") ~ "North Kerala",
                                       TRUE ~ "Other"),
         
         ka_comparison = case_when(n5_state == "Karnataka" & REGNAME %in% c("Mysore","Bangalore","Bangalore Rural") ~ "Bengaluru-Mysuru",
                                   n5_state == "Karnataka" & REGNAME %in% c("Gulbarga","Raichur") ~ "Gulbarga-Raichur",
                                   n5_state == "Karnataka" & REGNAME %in% c("Davanagere") ~ "Davanagere",
                                   TRUE ~ "Other")) %>% 
  left_join(readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="map2020_v024") %>% 
              dplyr::select(n5_state,zone) %>% 
              distinct(n5_state,.keep_all=TRUE),
            by=c("n5_state"))

figA = district_met %>%
  dplyr::filter(variable %in% c("Diabetes","Diagnosed")) %>% 
  dplyr::select(n5_state,REGNAME,kl_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Diabetes,y=Diagnosed,col=kl_comparison))  +
  geom_point() +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Central Kerala"="red","North Kerala"="darkgreen","Other"="grey80")) +
  theme_bw() +
  xlab("Diabetes (%)") +
  ylab("Diagnosed (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")


figB = district_met %>%
  dplyr::filter(variable %in% c("Diabetes","Treated")) %>% 
  dplyr::select(n5_state,REGNAME,ka_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Diabetes,y=Treated,col=ka_comparison))  +
  geom_point() +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Bengaluru-Mysuru"="red","Gulbarga-Raichur"="darkgreen","Davanagere" = "darkblue","Other"="grey80")) +
  theme_bw() +
  xlab("Diabetes (%)") +
  ylab("Treated (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

figC = district_met %>%
  dplyr::filter(variable %in% c("Diabetes","Controlled")) %>% 
  dplyr::select(n5_state,REGNAME,ka_comparison,estimate,variable) %>% 
  pivot_wider(names_from=variable,values_from=estimate) %>% 
  ggplot(data=,aes(x=Diabetes,y=Controlled,col=ka_comparison))  +
  geom_point() +
  # geom_text() +
  # facet_grid(zone~.,scales="free_y",space="free_y") +
  scale_color_manual("Estimate (%)",values=c("Bengaluru-Mysuru"="red","Gulbarga-Raichur"="darkgreen","Davanagere" = "darkblue","Other"="grey80")) +
  theme_bw() +
  xlab("Diabetes (%)") +
  ylab("Controlled (%)")  +
  theme(
    legend.text = element_text(size=12),
    axis.text = element_text(size = 12),
    strip.background.y = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position = "bottom")

require(ggpubr)
ggarrange(figA,figB,figC,nrow=3,ncol=1,
          labels = c("A","B","C"),
          common.legend = FALSE) %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/figure_scatterplot of district estimates.png"),width=12,height=8)

