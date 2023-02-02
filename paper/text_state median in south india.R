state_dm <- read_csv(file="analysis/nca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Diabetes") 

state_dm %>% 
  group_by(zone=="South"|n5_state == "Goa",residence) %>% 
  summarize(median = median(estimate))
