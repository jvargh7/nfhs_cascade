state_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/smapsmaster_sp.RDS"))
saveRDS(state_shp,file="diabetes_cascade/data/state_shp.RDS")


district_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/dnfhs5_sp.RDS"))
saveRDS(district_shp,file="diabetes_cascade/data/district_shp.RDS")

# nca02_national -------
nca02_national <- read_csv(file = "analysis/nca02_national level care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(nca02_national,file="diabetes_cascade/data/nca02_national.RDS")

ncz01_national <- read_csv(file = "age_standardized/ncz01_age standardized national care cascade.csv")  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(ncz01_national,file="diabetes_cascade/data/ncz01_national.RDS")

# national_nested -----
national_nested <- bind_rows(
  read_csv("analysis/nca02_national level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "analysis/nca09_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))
saveRDS(national_nested,file="diabetes_cascade/data/national_nested.RDS")

nationalz_nested <- bind_rows(
  read_csv("age_standardized/ncz01_age standardized national care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "age_standardized/ncz03_national met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))


saveRDS(nationalz_nested,file="diabetes_cascade/data/nationalz_nested.RDS")



# nca03_state ----------
nca03_state <- read_csv("analysis/nca03_state level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 
saveRDS(nca03_state,file="diabetes_cascade/data/nca03_state.RDS")


ncz02_state <- read_csv("age_standardized/ncz02_age standardized state cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 
saveRDS(ncz02_state,file="diabetes_cascade/data/ncz02_state.RDS")

# nca05_state - Unmet need ----------
nca05_state <- bind_rows(read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                         read_csv(file="analysis/nca03_state level care cascade.csv") %>% 
                           dplyr::filter(is.na(stratification)) %>% 
                           mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                           dplyr::filter(variable == "Disease") %>% 
                           mutate(variable = "Diabetes")
) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) 
saveRDS(nca05_state,file="diabetes_cascade/data/nca05_state_unmet.RDS")

# state_nested --------
state_nested <- bind_rows(
  read_csv("analysis/nca03_state level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "analysis/nca05_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 


saveRDS(state_nested,file="diabetes_cascade/data/state_nested.RDS")

# statez_nested: Age standardized --------
statez_nested <- bind_rows(
  read_csv("age_standardized/ncz02_age standardized state cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "age_standardized/ncz04_state met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(state,n5_state,residence,variable,estimate,lci,uci,stratification,strata,est_ci) %>% 
  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                           n5_state == "Nct Of Delhi" ~ "Delhi",
                           TRUE ~ n5_state)) 


saveRDS(statez_nested,file="diabetes_cascade/data/statez_nested.RDS")

# nca04_district --------
nca04_district <- read_csv("analysis/nca04_district2018 level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata))  %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)
saveRDS(nca04_district,file="diabetes_cascade/data/nca04_district.RDS")

# nca08_district - Unmet need --------

nca08_district <- bind_rows(read_csv(file = "analysis/nca08_district unmet need care cascade.csv") %>% 
                           mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                         read_csv(file="analysis/nca04_district2018 level care cascade.csv") %>% 
                           mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                           dplyr::filter(variable == "Disease") %>% 
                           mutate(variable = "Diabetes")
) %>% 
  mutate(strata = case_when(is.na(strata) ~ "Total",
         TRUE ~ strata)) %>% 
  # dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Unscreened","Undiagnosed","Untreated","Uncontrolled"))) %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)

saveRDS(nca08_district,file="diabetes_cascade/data/nca08_district_unmet.RDS")

# district_nested ---------

district_nested <- bind_rows(
  read_csv("analysis/nca04_district2018 level care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "analysis/nca08_district met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification))  %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)


saveRDS(district_nested,file="diabetes_cascade/data/district_nested.RDS")


# statez_nested: Age standardized --------
districtz_nested <- bind_rows(
  read_csv("age_standardized/ncz06_age standardized district cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_screened","dm_disease")),
  read_csv(file = "age_standardized/ncz05_district met need care cascade.csv") %>% 
    dplyr::filter(variable %in% c("dm_diagnosed","dm_treated","dm_controlled")))  %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) & is.na(stratification) ~ "Total",
                            is.na(strata) ~ "Missing",
                            stratification == "swealthq_ur" & strata == 1 ~ "Wealth: Lowest",
                            stratification == "swealthq_ur" & strata == 2 ~ "Wealth: Low",
                            stratification == "swealthq_ur" & strata == 3 ~ "Wealth: Medium",
                            stratification == "swealthq_ur" & strata == 4 ~ "Wealth: High",
                            stratification == "swealthq_ur" & strata == 5 ~ "Wealth: Highest",
                            TRUE ~ strata),
         stratification = case_when(is.na(stratification) ~ "",
                                    TRUE ~ stratification)) %>% 
  dplyr::select(REGCODE,REGNAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)


saveRDS(districtz_nested,file="diabetes_cascade/data/districtz_nested.RDS")

# Copying functions --------

file.copy("functions/cascade_plot.R",to = "diabetes_cascade/code/cascade_plot.R")

write_csv(nca02_national,file="lattice/dashboard original/nca02_national.csv")
write_csv(ncz01_national,file="lattice/dashboard original/ncz01_national.csv")
write_csv(national_nested,file="lattice/dashboard original/national_nested.csv")
write_csv(nationalz_nested,file="lattice/dashboard original/nationalz_nested.csv")
write_csv(nca03_state,file="lattice/dashboard original/nca03_state.csv")
write_csv(ncz02_state,file="lattice/dashboard original/ncz02_state.csv")
write_csv(nca05_state,file="lattice/dashboard original/nca05_state_unmet.csv")
write_csv(state_nested,file="lattice/dashboard original/state_nested.csv")
write_csv(statez_nested,file="lattice/dashboard original/statez_nested.csv")
write_csv(nca04_district,file="lattice/dashboard original/nca04_district.csv")
write_csv(nca08_district,file="lattice/dashboard original/nca08_district_unmet.csv")
write_csv(district_nested,file="lattice/dashboard original/district_nested.csv")
write_csv(districtz_nested,file="diabetes_cascade/data/districtz_nested.csv")
