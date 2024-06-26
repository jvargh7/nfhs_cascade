
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")
proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")


lasipop_df <- bind_rows(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>% 
                        mutate(sex = "Female"),
                      readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>% 
                        mutate(sex = "Male")) %>%
  dplyr::filter(!is.na(dm_free),age >= 45) %>%
  mutate(residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,D_CODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = D_CODE) %>% 
  mutate(htn_disease_cat = case_when(is.na(htn_disease) ~ "Missing",
                                     htn_disease == 1 ~ "Yes",
                                     htn_disease == 0 ~ "No"))

lasipop_design <- lasipop_df %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

lasipop_dm_design <- lasipop_df %>% 
  dplyr::filter(dm_disease == 1) %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


lasi_national <- map_dfr(group_vars,
                         function(g_v){
                           id_vars = c(g_v);
                           p();
                           n5_sy <- svysummary(lasipop_design,
                                               # c_vars = continuous_vars,
                                               p_vars = proportion_vars,
                                               # g_vars = grouped_vars,
                                               id_vars = id_vars
                           ) %>% 
                             mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                             mutate(est_ci = paste0(estimate," (",
                                                    lci,", ",uci,")"));
                           
                           # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                           n5_ct <- lasipop_df %>% 
                             group_by_at(vars(one_of(id_vars))) %>% 
                             summarize_at(vars(one_of(c(
                               # continuous_vars,
                               proportion_vars
                               # grouped_vars
                             ))),
                             list(n = ~sum(!is.na(.)))) %>% 
                             pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                             mutate(variable = str_replace(variable,"_n$",""));
                           
                           n5_out <- left_join(n5_sy,
                                               n5_ct,
                                               by=c(id_vars[id_vars!=""],"variable")) %>% 
                             
                             # Restrict to those cells with more than 100 observations
                             dplyr::filter(n > 100) %>% 
                             mutate(stratification = g_v) %>% 
                             rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                             mutate_at(vars(one_of("strata")),~as.character(.));
                           
                           return(n5_out)
                           
                         })

lasi_regional <- map_dfr(group_vars,
                                   function(g_v){
                                     id_vars = c("residence",g_v);
                                     p();
                                     n5_sy <- svysummary(lasipop_design,
                                                         # c_vars = continuous_vars,
                                                         p_vars = proportion_vars,
                                                         # g_vars = grouped_vars,
                                                         id_vars = id_vars
                                     ) %>% 
                                       mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                       mutate(est_ci = paste0(estimate," (",
                                                              lci,", ",uci,")"));
                                     
                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     n5_ct <- lasipop_df %>% 
                                       group_by_at(vars(one_of(id_vars))) %>% 
                                       summarize_at(vars(one_of(c(
                                         # continuous_vars,
                                         proportion_vars
                                         # grouped_vars
                                       ))),
                                       list(n = ~sum(!is.na(.)))) %>% 
                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                       mutate(variable = str_replace(variable,"_n$",""));
                                     
                                     n5_out <- left_join(n5_sy,
                                                         n5_ct,
                                                         by=c(id_vars[id_vars!=""],"variable")) %>% 
                                       
                                       # Restrict to those cells with more than 100 observations
                                       dplyr::filter(n > 100) %>% 
                                       mutate(stratification = g_v) %>% 
                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                     
                                     return(n5_out)
                                     
                                   })

lasi_dm_national <- map_dfr(group_vars,
                            function(g_v){
                              id_vars = c(g_v);
                              p();
                              n5_sy <- svysummary(lasipop_dm_design,
                                                  # c_vars = continuous_vars,
                                                  p_vars = proportion_vars[3:5],
                                                  # g_vars = grouped_vars,
                                                  id_vars = id_vars
                              ) %>% 
                                mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                mutate(est_ci = paste0(estimate," (",
                                                       lci,", ",uci,")"));
                              
                              # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                              n5_ct <- lasipop_df %>% 
                                dplyr::filter(dm_disease == 1) %>% 
                                group_by_at(vars(one_of(id_vars))) %>% 
                                summarize_at(vars(one_of(c(
                                  # continuous_vars,
                                  proportion_vars[3:5]
                                  # grouped_vars
                                ))),
                                list(n = ~sum(!is.na(.)))) %>% 
                                pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                mutate(variable = str_replace(variable,"_n$",""));
                              
                              n5_out <- left_join(n5_sy,
                                                  n5_ct,
                                                  by=c(id_vars[id_vars!=""],"variable")) %>% 
                                
                                # Restrict to those cells with more than 100 observations
                                dplyr::filter(n > 100) %>% 
                                mutate(stratification = g_v) %>% 
                                rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                mutate_at(vars(one_of("strata")),~as.character(.));
                              
                              return(n5_out)
                              
                            })

lasi_dm_regional <- map_dfr(group_vars,
                                function(g_v){
                                  id_vars = c("residence",g_v);
                                  p();
                                  n5_sy <- svysummary(lasipop_dm_design,
                                                      # c_vars = continuous_vars,
                                                      p_vars = proportion_vars[3:5],
                                                      # g_vars = grouped_vars,
                                                      id_vars = id_vars
                                  ) %>% 
                                    mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                    mutate(est_ci = paste0(estimate," (",
                                                           lci,", ",uci,")"));
                                  
                                  # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                  n5_ct <- lasipop_df %>% 
                                    dplyr::filter(dm_disease == 1) %>% 
                                    group_by_at(vars(one_of(id_vars))) %>% 
                                    summarize_at(vars(one_of(c(
                                      # continuous_vars,
                                      proportion_vars[3:5]
                                      # grouped_vars
                                    ))),
                                    list(n = ~sum(!is.na(.)))) %>% 
                                    pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                    mutate(variable = str_replace(variable,"_n$",""));
                                  
                                  n5_out <- left_join(n5_sy,
                                                      n5_ct,
                                                      by=c(id_vars[id_vars!=""],"variable")) %>% 
                                    
                                    # Restrict to those cells with more than 100 observations
                                    dplyr::filter(n > 100) %>% 
                                    mutate(stratification = g_v) %>% 
                                    rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                    mutate_at(vars(one_of("strata")),~as.character(.));
                                  
                                  return(n5_out)
                                  
                                })


write_csv(lasi_national,"qc/lasi national equivalent estimates.csv")
write_csv(lasi_regional,"qc/lasi regional equivalent estimates.csv")
write_csv(lasi_dm_national,"qc/lasi national equivalent nested estimates.csv")
write_csv(lasi_dm_regional,"qc/lasi regional equivalent nested estimates.csv")
