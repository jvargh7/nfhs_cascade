cascade_summary <- read_csv("analysis/cascade summary.csv") %>% 
  mutate(hv025 = factor(hv025,levels=c(1,2),labels=c("Urban","Rural")),
         sex = str_to_title(sex),
         disease = case_when(str_detect(variable,"dm") ~ "Diabetes",
                             str_detect(variable,"htn") ~ "Hypertension",
                             TRUE ~ NA_character_),
         phase = factor(phase,levels=c(1,2),labels=c("Phase 1","Phase 2")),
         cascade = case_when(str_detect(variable,"disease") ~ 1,
                             str_detect(variable,"diagnosed") ~ 2,
                             str_detect(variable,"treated") ~ 3,
                             str_detect(variable,"controlled") ~ 4,
                             TRUE ~ NA_real_
                             )) %>% 
  mutate(cascade = factor(cascade,levels=c(1:4),labels=c("Disease","Diagnosed","Treated","Controlled")))

cascade_plot <- function(df,limits_y = c(0,40)){
  
  fig <- ggplot(data=df,aes(x = cascade,y=est*100,ymax = uci*100,ymin = lci*100,fill=phase)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_errorbar(width = 0.1, position = position_dodge(width = 0.9)) +
    theme_bw() +
    xlab("") +
    ylab("Prevalence (%)") +
    scale_fill_discrete(name = "") +
    scale_y_continuous(limits = limits_y)
  
  return(fig)
}


figA <- cascade_summary %>% 
  dplyr::filter(sex == "Female",disease == "Diabetes",hv025 == "Urban") %>% 
  cascade_plot(.)

figB <- cascade_summary %>% 
  dplyr::filter(sex == "Male",disease == "Diabetes",hv025 == "Urban") %>% 
  cascade_plot(.)

figC <- cascade_summary %>% 
  dplyr::filter(sex == "Female",disease == "Diabetes",hv025 == "Rural") %>% 
  cascade_plot(.)

figD <- cascade_summary %>% 
  dplyr::filter(sex == "Male",disease == "Diabetes",hv025 == "Rural") %>% 
  cascade_plot(.)

require(ggpubr)
ggarrange(figA,figB,figC,figD,
          labels = LETTERS[1:4],ncol = 2,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/cascade summary diabetes.png"))



figA2 <- cascade_summary %>% 
  dplyr::filter(sex == "Female",disease == "Hypertension",hv025 == "Urban") %>% 
  cascade_plot(.)

figB2 <- cascade_summary %>% 
  dplyr::filter(sex == "Male",disease == "Hypertension",hv025 == "Urban") %>% 
  cascade_plot(.)

figC2 <- cascade_summary %>% 
  dplyr::filter(sex == "Female",disease == "Hypertension",hv025 == "Rural") %>% 
  cascade_plot(.)

figD2 <- cascade_summary %>% 
  dplyr::filter(sex == "Male",disease == "Hypertension",hv025 == "Rural") %>% 
  cascade_plot(.)

require(ggpubr)
ggarrange(figA2,figB2,figC2,figD2,
          labels = LETTERS[1:4],ncol = 2,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/cascade summary hypertension.png"))