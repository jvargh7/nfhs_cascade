source(".Rprofile")

absolute_df <- bind_rows(
  read_csv(file = "analysis/nca02_total national care cascade.csv"),
  read_csv(file = "analysis/nca02_national level care cascade.csv")) %>% 
  dplyr::mutate(residence = case_when(is.na(residence) ~ "Total",
                                      TRUE ~ residence)) %>% 
  dplyr::filter(is.na(strata),variable != "dm_screened") %>% 
  mutate(variable = factor(variable,
                           levels=c("dm_disease","dm_diagnosed","dm_treated","dm_controlled"),
                           labels=c("Diabetes","Diagnosed","Treated","Controlled")),
         residence = factor(residence,
                            levels=c("Total","Urban","Rural"))) %>% 
  arrange(residence,variable) %>% 
  group_by(residence) %>% 
  mutate(denominator = case_when(variable == "Diabetes" ~ estimate,
                                 TRUE ~ NA_real_)) %>% 
  mutate(denominator = zoo::na.locf(denominator)) %>% 
  mutate(drop = (1-(estimate*100/denominator)))

