
nfhs5dmlt2h_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                              mutate(sex = "Female"),
                            readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                              mutate(sex = "Male")) %>%
  dplyr::filter(dm_disease == 1) %>% 
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         dm_unscreened = 1 - dm_screened,
         dm_undiagnosed = 1 - dm_diagnosed) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) %>% 
  dplyr::filter(!(lastate>=2 & lastdrank>=2))

nfhs5dmlt2h_svydesign <- nfhs5dmlt2h_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")
