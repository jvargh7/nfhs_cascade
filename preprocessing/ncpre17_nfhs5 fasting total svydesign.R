
nfhs5fast_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(dm_free)) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(htn_disease_cat = case_when(is.na(htn_disease) ~ "Missing",
                                     htn_disease == 1 ~ "Yes",
                                     htn_disease == 0 ~ "No")) %>% 
  dplyr::filter(fasting == 1)

# with(nfhs5_df,table(age <= 49, !is.na(bmi)))

nfhs5fast_svydesign <- nfhs5fast_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

