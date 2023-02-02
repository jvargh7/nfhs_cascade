
nfhs5diag_youth <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                           mutate(sex = "Female"),
                         readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                           mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(dm_free),age %in% c(15:30)) %>% 
  group_by(cluster,hhid) %>% 
  mutate(familyhistory = sum(dm_diagnosed,na.rm=TRUE)-dm_diagnosed) %>% 
  dplyr::filter(dm_diagnosed == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  mutate(age_category_youth = case_when(age %in% c(15:19) ~ "15-19",
                                        age %in% c(20:24) ~ "20-24",
                                        age >= 25 ~ "25 and above"),
         hscompletion = case_when(education %in% c("Secondary","Higher") ~ 1,
                                  TRUE ~ 0),
         anyhistory = case_when(familyhistory > 0 ~ 1,
                                TRUE ~ 0))

nfhs5ydiag_svydesign <- nfhs5diag_youth %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


