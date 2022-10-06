# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
population <- read_csv("age_standardized/ncz01_age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("dm_disease"))

nested <- read_csv(file = "age_standardized/ncz03_national met need care cascade.csv") %>% 
  dplyr::filter(variable %in% c("dm_diagnosed","dm_treated"))

conditional <- read_csv(file = "age_standardized/ncz08_national conditional cascade.csv") %>% 
  dplyr::filter(variable %in% c("dm_controlled"))

bind_rows(population,
          nested,
          conditional) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_conditional care cascade.csv")
