urbanicity <- read_csv("analysis/urbanicity summary.csv") %>% 
  mutate(hv025 = factor(hv025,levels=c(1,2),labels=c("Urban","Rural")),
         sex = str_to_title(sex),
         disease = case_when(str_detect(variable,("dm|diabetes")) ~ "Diabetes",
                             str_detect(variable,"(htn|hypertension)") ~ "Hypertension",
                             TRUE ~ NA_character_),
         cascade = case_when(str_detect(variable,"screened") ~ 1,
                             str_detect(variable,"pre") ~ 2,
                             str_detect(variable,"disease") ~ 3,
                             str_detect(variable,"diagnosed") ~ 4,
                             str_detect(variable,"treated") ~ 5,
                             str_detect(variable,"controlled") ~ 6,
                             TRUE ~ NA_real_
                             )) %>% 
  mutate(cascade = factor(cascade,levels=c(1:6),labels=c("Screened","Pre-clinical","Disease","Diagnosed","Taking Medication","Under Control"))) %>% 
  mutate(group = case_when(hv025 == "Urban" & sex == "Female" ~ "Urban Women",
                           hv025 == "Urban" & sex == "Male" ~ "Urban Men",
                           hv025 == "Rural" & sex == "Female" ~ "Rural Women",
                           hv025 == "Rural" & sex == "Male" ~ "Rural Men",
                           TRUE ~ NA_character_
  ))

age <- read_csv("analysis/age_category summary.csv") %>% 
  mutate(age_category = factor(age_category,levels=c("[18,39]","(39,64]","(64,98]"),labels=c("18-39","40-64","65+")),
         sex = str_to_title(sex),
         disease = case_when(str_detect(variable,("dm|diabetes")) ~ "Diabetes",
                             str_detect(variable,"(htn|hypertension)") ~ "Hypertension",
                             TRUE ~ NA_character_),
         cascade = case_when(str_detect(variable,"screened") ~ 1,
                             str_detect(variable,"pre") ~ 2,
                             str_detect(variable,"disease") ~ 3,
                             str_detect(variable,"diagnosed") ~ 4,
                             str_detect(variable,"treated") ~ 5,
                             str_detect(variable,"controlled") ~ 6,
                             TRUE ~ NA_real_
         )) %>% 
  mutate(cascade = factor(cascade,levels=c(1:6),labels=c("Screened","Pre-clinical","Disease","Diagnosed","Taking Medication","Under Control"))) %>% 
  mutate(group = case_when(age_category == "18-39" & sex == "Female" ~ "18-39 \nWomen",
                           age_category == "18-39" & sex == "Male" ~ "18-39 \nMen",
                           age_category == "40-64" & sex == "Female" ~ "40-64 \nWomen",
                           age_category == "40-64" & sex == "Male" ~ "40-64 \nMen",
                           age_category == "65+" & sex == "Female" ~ "65+ \nWomen",
                           age_category == "65+" & sex == "Male" ~ "65+ \nMen",
                           TRUE ~ NA_character_
  ))


cascade_plot <- function(df,limits_y = c(0,65),exclude = "Screened"){
  df2 <- df %>% 
    dplyr::filter(!cascade %in% exclude)
  
    fill_values = c("purple","orange","red","lightblue","darkgreen","lightgreen")
    
  if(exclude == "Screened"){
    fill_values = c("orange","red","lightblue","darkgreen","lightgreen")
  }
  
  fig <- df2 %>% 
    ggplot(data=.,aes(x = group,y=est*100,ymax = uci*100,ymin = lci*100,fill=cascade)) +
    geom_col(position = position_dodge(width=0.9)) +
    geom_text(aes(y=uci*100 + 2,label=round(est*100,1)),
              position = position_dodge(width = 0.9),size=2) +
    geom_errorbar(width = 0.1, position = position_dodge(width = 0.9)) +
    theme_bw() +
    xlab("") +
    ylab("Prevalence (%)") +
    scale_y_continuous(limits = limits_y) + 
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual(name = "",values = fill_values) 
  
  return(fig)
}


figA <- urbanicity %>% 
  dplyr::filter(disease == "Diabetes") %>% 
  cascade_plot(.,limits_y = c(0,25))

figB <- urbanicity %>% 
  dplyr::filter(disease == "Hypertension") %>% 
  cascade_plot(.)

figC <- age %>% 
  dplyr::filter(disease == "Diabetes") %>% 
  cascade_plot(.,limits_y = c(0,25))

figD <- age %>% 
  dplyr::filter(disease == "Hypertension") %>% 
  cascade_plot(.)

require(ggpubr)
ggarrange(figA,figB,figC,figD,
          labels = LETTERS[1:4],ncol = 2,nrow=2,common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_cascade_folder,"/figures/cascade summary diabetes.png"),width=10,height=6)


