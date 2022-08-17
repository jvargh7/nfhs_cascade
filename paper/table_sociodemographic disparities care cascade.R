# nfhs5_svysummary <- read_csv("analysis/nca02_national level care cascade.csv")
nfhs5_svysummary <- read_csv("analysis/nca08_age standardized care cascade.csv")

nfhs5_svysummary %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_sociodemographic disparities care cascade.csv")
