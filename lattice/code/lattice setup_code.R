rm(list=ls());gc();source(".Rprofile")
# The csv versions of the below files are in lattice/dashboard original >>
# I used the RDS versions because this is what the dashboard is calling
state_nested <- readRDS(file="diabetes_cascade/data/state_nested.RDS")
statez_nested <- readRDS(file="diabetes_cascade/data/statez_nested.RDS")
district_nested <- readRDS(file="diabetes_cascade/data/district_nested.RDS")
districtz_nested <- readRDS(file="diabetes_cascade/data/districtz_nested.RDS")

bind_rows(state_nested %>% mutate(Level1 = "Crude"),
          statez_nested %>% mutate(Level1 = "Age-standardized")) %>% 
  mutate(stratification = case_when(stratification == "" | is.na(stratification) ~ "Total",
                                    stratification == "sex" ~ "Sex",
                                    stratification == "age_category" ~ "Age Category",
                                    stratification == "education" ~ "Schooling",
                                    stratification == "swealthq_ur" ~ "State Wealth Quintile",
                                    TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(stratification)) %>% 
  dplyr::filter(!variable %in% c("Screened")) %>% 
  rename(Level2 = residence,
         Level3 = variable,
         Level4 = stratification,
         Level5 = strata,
         nfhs5_statecode = state,
         nfhs5_statename = n5_state,
         ) %>%
  dplyr::select(Level1,Level2,Level3,Level4,Level5,
                ST_NM,estimate,lci,uci,est_ci,
                nfhs5_statecode,nfhs5_statename) %>% 
  write_csv("lattice/state comparison.csv")


bind_rows(district_nested %>% mutate(Level1 = "Crude"),
          districtz_nested %>% mutate(Level1 = "Age-standardized"))  %>% 
  dplyr::filter(!variable %in% c("Screened")) %>% 
  rename(Level2 = variable,
         Level3 = strata,
         nfhs5_statecode = v024,
         nfhs5_statename = n5_state,
  ) %>%
  dplyr::select(Level1,Level2,Level3,
                REGNAME,REGCODE,estimate,lci,uci,est_ci,
                nfhs5_statecode,nfhs5_statename) %>% 
  write_csv("lattice/district comparison.csv")
