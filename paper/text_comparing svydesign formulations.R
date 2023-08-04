nfhs5_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
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
                                     htn_disease == 0 ~ "No"))

source("C:/code/external/functions/survey/svysummary.R")
proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")

nfhs5_svydesign_used <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

## Svydesign for Total: National ----------
nfhs5_svydesign_orig <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = strata,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


n5_sy_used <- svysummary(nfhs5_svydesign_used,
                         # c_vars = continuous_vars,
                         p_vars = proportion_vars,
                         # g_vars = grouped_vars,
                         # id_vars = id_vars) %>%
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

n5_sy_orig <- svysummary(nfhs5_svydesign_orig,
                         # c_vars = continuous_vars,
                         p_vars = proportion_vars,
                         # g_vars = grouped_vars,
                         # id_vars = id_vars) %>%
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

bind_rows(n5_sy_orig %>% mutate(design = "Original"),
          n5_sy_used %>% mutate(design = "Used")) %>% 
  write_csv(.,"paper/comparing svydesign formulations for national total.csv")

## Svydesign for Diabetes: National -------------

nfhs5dm_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
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
  rename(district_df = REGCODE)

nfhs5dm_svydesign_used <- nfhs5dm_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

nfhs5dm_svydesign_orig <- nfhs5dm_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = strata,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

n5dm_sy_used <- svysummary(nfhs5dm_svydesign_used,
                         # c_vars = continuous_vars,
                         p_vars = proportion_vars,
                         # g_vars = grouped_vars,
                         # id_vars = id_vars) %>%
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

n5dm_sy_orig <- svysummary(nfhs5dm_svydesign_orig,
                         # c_vars = continuous_vars,
                         p_vars = proportion_vars,
                         # g_vars = grouped_vars,
                         # id_vars = id_vars) %>%
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


n5dm_sy_used <- svysummary(nfhs5dm_svydesign_used,
                           # c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           # g_vars = grouped_vars,
                           # id_vars = id_vars) %>%
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

bind_rows(n5dm_sy_orig %>% mutate(design = "Original"),
          n5dm_sy_used %>% mutate(design = "Used")) %>% 
  write_csv(.,"paper/comparing svydesign formulations for national diabetes.csv")

## Svydesign for Diabetes: State x Urban-Rural -------------

n5dm_stateur_sy_orig <- svysummary(nfhs5dm_svydesign_orig,
                           # c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           # g_vars = grouped_vars,
                           id_vars = c("state","residence")) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


n5dm_stateur_sy_used <- svysummary(nfhs5dm_svydesign_used,
                           # c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           # g_vars = grouped_vars,
                           id_vars = c("state","residence")) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

bind_rows(n5dm_stateur_sy_orig %>% mutate(design = "Original"),
          n5dm_stateur_sy_used %>% mutate(design = "Used")) %>% 
  write_csv(.,"paper/comparing svydesign formulations for state ur diabetes.csv")
