national_cascade <- read_csv(file = "age_standardized/ncz01_age standardized national care cascade.csv") %>% 
  mutate(cascade = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
  mutate(cascade = factor(cascade,levels=c("Screened","Disease","Diagnosed","Treated","Controlled"),
                          labels=c("Screened","Diabetes","Diagnosed","Taking Medication","Under Control"))) %>% 
  mutate(group = case_when(is.na(strata) ~ paste0(residence,"\nTotal"),
                           TRUE ~ paste0(residence,"\n",strata))) %>% 
  dplyr::filter(!is.na(residence))

source("functions/cascade_plot.R")

figA <- national_cascade %>% 
  dplyr::filter(is.na(stratification)|stratification == "sex") %>% 
  cascade_plot(.,limits_y = c(0,28))
figB <- national_cascade %>% 
  dplyr::filter(stratification == "age_category") %>% 
  cascade_plot(.,limits_y = c(0,28))

require(ggpubr)
ggarrange(figA,
          figB,
          labels = LETTERS[1:2],ncol = 1,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/national care cascade.png"),width=10,height=6)

ggarrange(figA,
          figB,
          labels = LETTERS[1:2],ncol = 1,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/writing/JAMA Int Med/Figure 1.jpg"),width=10,height=6)
