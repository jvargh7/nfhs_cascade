
group_vars = c("","sex","age_category","education",
               "caste","religion","wealthq_ur")

source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre12_nfhs5 undiagnosed svydesign.R")

proportion_vars <- c("dm_screened","dm_unscreened")

pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5dmundiagz_svydesign = svystandardize(nfhs5dmundiag_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                          population = pop_age)


unmet_svysummary_dmundiag <- future_map_dfr(group_vars,
                                            function(g_v){
                                              id_vars = c("residence",g_v);
                                              print(g_v);
                                              n5_sy_dmundiag <- svysummary(nfhs5dmundiagz_svydesign,
                                                                           # c_vars = continuous_vars,
                                                                           p_vars = proportion_vars,
                                                                           # g_vars = grouped_vars,
                                                                           id_vars = id_vars
                                              ) %>% 
                                                mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                                mutate(est_ci = paste0(estimate," (",
                                                                       lci,", ",uci,")"));
                                              
                                              # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                              n5_ct_dmundiag <- nfhs5dmundiag_df %>% 
                                                group_by_at(vars(one_of(id_vars))) %>% 
                                                summarize_at(vars(one_of(c(
                                                  # continuous_vars,
                                                  proportion_vars
                                                  # grouped_vars
                                                ))),
                                                list(n = ~sum(!is.na(.)))) %>% 
                                                pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                                mutate(variable = str_replace(variable,"_n$",""));
                                              
                                              n5_out_dmundiag <- left_join(n5_sy_dmundiag,
                                                                           n5_ct_dmundiag,
                                                                           by=c(id_vars[id_vars!=""],"variable")) %>% 
                                                
                                                # Restrict to those cells with more than 100 observations
                                                dplyr::filter(n > 100) %>%
                                                mutate(stratification = g_v) %>% 
                                                rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                                mutate_at(vars(one_of("strata")),~as.character(.));
                                              gc();
                                              return(n5_out_dmundiag)
                                              
                                            })


unmet_svysummary_dmundiag %>% 
  write_csv(.,file = "age_standardized/ncz09_screening among undiagnosed.csv")

