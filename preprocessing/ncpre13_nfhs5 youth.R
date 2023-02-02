
nfhs5_youth <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(dm_free)|!is.na(htn_free),age %in% c(15:30)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(age_category_youth = case_when(age %in% c(15:19) ~ "15-19",
                                        age %in% c(20:24) ~ "20-24",
                                        age >= 25 ~ "25 and above"))

nfhs5y_svydesign <- nfhs5_youth %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


# pop_age_youth <- nfhs5y_svydesign %>% 
#   group_by(age_category_youth) %>% 
#   survey_tally()
# 
# write_csv(pop_age_youth,"abstract/population for age standardization youth.csv")
