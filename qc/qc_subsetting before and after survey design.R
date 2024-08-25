

nfhs5_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                              mutate(sex = "Female"),
                            readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                              mutate(sex = "Male")) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural"),
         dm_untreated = 1 - dm_treated,
         dm_uncontrolled = 1 - dm_controlled) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by= district_matching) %>% 
  rename(district_df = REGCODE) 


nfhs5dmdiag_df = nfhs5_df %>% 
  dplyr::filter(dm_diagnosed == 1) 

# BEFORE ------------
proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")
source("C:/code/external/functions/survey/svysummary.R")

nfhs5dmdiag_svydesign_before <- nfhs5dmdiag_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

n5_sy_before <- svysummary(nfhs5dmdiag_svydesign_before,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


n5_sy_before_state <- svysummary(nfhs5dmdiag_svydesign_before,
                           # c_vars = continuous_vars,
                           p_vars = proportion_vars[4:5],
                           # g_vars = grouped_vars,
                           id_vars = "state"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

# AFTER -------------------
nfhs5dmdiag_svydesign_after <- nfhs5_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer") %>% 
  subset(dm_diagnosed == 1)



n5_sy_after <- svysummary(nfhs5dmdiag_svydesign_after,
                           # c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           # g_vars = grouped_vars,
                           # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


n5_sy_after_state <- svysummary(nfhs5dmdiag_svydesign_after,
                                 # c_vars = continuous_vars,
                                 p_vars = proportion_vars[4:5],
                                 # g_vars = grouped_vars,
                                id_vars = "state"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


# COMPARISON -------
bind_rows(n5_sy_before %>% 
            dplyr::select(variable,est_ci) %>% 
            mutate(subset = "before_svydesign",
                   state = 0),
          n5_sy_before_state %>% 
            dplyr::select(variable,state,est_ci) %>% 
            mutate(subset = "before_svydesign"),
          n5_sy_after %>% 
            dplyr::select(variable,est_ci) %>% 
            mutate(subset = "after_svydesign",
                   state = 0),
          n5_sy_after_state %>% 
            dplyr::select(variable,state,est_ci) %>% 
            mutate(subset = "after_svydesign")) %>% 
  pivot_wider(names_from=subset,values_from=est_ci) %>% View()
  write_csv(.,path = "qc/qc_subsetting before and after survey design.csv")



