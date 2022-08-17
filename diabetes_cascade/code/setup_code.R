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
                            TRUE ~ strata)) %>% 
  rename(D_CODE = district_df) %>% 
  # There are missing values in D_CODE from subsetting on map
  dplyr::filter(!is.na(D_CODE)) %>% 
  left_join(readxl::read_excel("diabetes_cascade/data/maps.xlsx","map2018_sdist") %>% 
              dplyr::select(D_CODE,n5_state,v024,D_NAME) %>% 
              mutate(D_CODE = sprintf("%03d",as.numeric(D_CODE))),
            by=c("D_CODE")) %>% 
  dplyr::select(D_CODE,D_NAME,n5_state,v024,variable,estimate,lci,uci,strata,est_ci)
saveRDS(nca04_district,file="diabetes_cascade/data/nca04_district.RDS")





