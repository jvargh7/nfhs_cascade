group_vars <- c("","sex","age_category","education",
                "caste","religion","wealthq_ur")


source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre23_nfhs5 total svydesign with rpg200.R")

proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

id_vars = c("residence",group_vars[4]);

nfhs5_svystdz <- svystandardize(nfhs5_svydesign,by=~age_category,over = ~education + caste + religion + wealthq_ur,
                                population = pop_age)
rm(nfhs5_svydesign);gc();

n5_sy <- svysummary(nfhs5_svystdz,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

n5_sy %>% 
  write_csv(.,file = "cutoff200/ncc200z01_total age standardized national care cascade.csv")

# NATIONAL ----------
nfhs5_svysummary_national <- map_dfr(group_vars[-1],
                            function(g_v){
                              id_vars = c(g_v);
                              n5_sy <- svysummary(nfhs5_svystdz,
                                                  # c_vars = continuous_vars,
                                                  p_vars = proportion_vars,
                                                  # g_vars = grouped_vars,
                                                  id_vars = id_vars
                              ) %>% 
                                mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                mutate(est_ci = paste0(estimate," (",
                                                       lci,", ",uci,")"));
                              
                              # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                              n5_ct <- nfhs5_df %>% 
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


# REGIONAL --------
nfhs5_svysummary <- map_dfr(group_vars,
                                     function(g_v){
                                       id_vars = c("residence",g_v);
                                       n5_sy <- svysummary(nfhs5_svystdz,
                                                           # c_vars = continuous_vars,
                                                           p_vars = proportion_vars,
                                                           # g_vars = grouped_vars,
                                                           id_vars = id_vars
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                       
                                       # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                       n5_ct <- nfhs5_df %>% 
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


bind_rows(nfhs5_svysummary,
          nfhs5_svysummary_national) %>% 

write_csv(.,path = "cutoff200/ncc200z01_age standardized national care cascade.csv")



# svysummary(nfhs5_svystdz,
#            # c_vars = continuous_vars,
#            p_vars = proportion_vars,
#            # g_vars = grouped_vars,
#            id_vars = id_vars
# ) %>% 
#   mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
#   mutate(est_ci = paste0(estimate," (",
#                          lci,", ",uci,")")) 

