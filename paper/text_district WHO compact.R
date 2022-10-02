district_met <- read_csv("analysis/nca08_district met need care cascade.csv",guess_max = 4000)

district_met %>% 
  dplyr::filter(is.na(stratification), n > 50) %>% 
  group_by(variable) %>% 
  summarize(count = sum(estimate > 80),
            n = n())
