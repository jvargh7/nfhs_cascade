path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
state_shp <- rgdal::readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")
saveRDS(state_shp,file="diabetes_cascade/data/state_shp.RDS")


path_district_files <- "C:/Cloud/OneDrive - Emory University/data/India Shapefiles/INDIA_2018_DISTRICTS-master"
district_shp <- rgdal::readOGR(paste0(path_district_files),"DISTRICTS_2018")
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

# nca04_district --------
nca04_district <- read_csv("analysis/nca04_district2018 level care cascade.csv",guess_max = 6000) %>% 
  mutate(variable = str_replace(variable,"dm_","")) %>% 
  mutate(variable = factor(variable,levels=c("screened","disease","diagnosed","treated","controlled"),
                           labels=c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
         strata = case_when(is.na(strata) ~ "Total",
                            TRUE ~ strata))  %>% 
  dplyr::select(D_CODE,D_NAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)
saveRDS(nca04_district,file="diabetes_cascade/data/nca04_district.RDS")


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
  dplyr::select(D_CODE,D_NAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)

saveRDS(nca08_district,file="diabetes_cascade/data/nca08_district_unmet.RDS")

# Copying functions --------

file.copy("functions/cascade_plot.R",to = "diabetes_cascade/code/cascade_plot.R")
