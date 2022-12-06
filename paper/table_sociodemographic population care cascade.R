# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
population <- read_csv("age_standardized/ncz01_age standardized national care cascade.csv") %>% 
  dplyr::filter(variable %in% c("dm_screened","dm_disease")) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence))

nested <- read_csv(file = "age_standardized/ncz03_national met need care cascade.csv") %>% 
  dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled"))  %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence))


bind_rows(population,
          nested) %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_sociodemographic population care cascade.csv")
