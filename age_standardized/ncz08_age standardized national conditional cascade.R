
group_vars = c("","sex","age_category","education",
               "caste","religion","wealthq_ur")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/ncpre10_nfhs5 treated svydesign.R")

proportion_vars <- c("dm_controlled","dm_uncontrolled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

# Dropped education in age standardization 
nfhs5dmtreatz_svydesign = svystandardize(nfhs5dmtreat_svydesign,by=~age_category,over = ~caste + religion + wealthq_ur,
                                        population = pop_age)

# National ---------

n5_sy <- svysummary(nfhs5dmtreatz_svydesign,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

n5_sy %>% 
  write_csv(.,file = "age_standardized/ncz08_total age standardized national conditional cascade.csv")


# Regional -----------
unmet_svysummary_dmtreat <- map_dfr(group_vars,
                                          function(g_v){
                                            id_vars = c("residence",g_v);
                                            print(g_v);
                                            n5_sy_dmtreat <- svysummary(nfhs5dmtreatz_svydesign,
                                                                       # c_vars = continuous_vars,
                                                                       p_vars = proportion_vars,
                                                                       # g_vars = grouped_vars,
                                                                       id_vars = id_vars
                                            ) %>% 
                                              mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                              mutate(est_ci = paste0(estimate," (",
                                                                     lci,", ",uci,")"));
                                            
                                            # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                            n5_ct_dmtreat <- nfhs5dmtreat_df %>% 
                                              group_by_at(vars(one_of(id_vars))) %>% 
                                              summarize_at(vars(one_of(c(
                                                # continuous_vars,
                                                proportion_vars
                                                # grouped_vars
                                              ))),
                                              list(n = ~sum(!is.na(.)))) %>% 
                                              pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                              mutate(variable = str_replace(variable,"_n$",""));
                                            
                                            n5_out_dmtreat <- left_join(n5_sy_dmtreat,
                                                                       n5_ct_dmtreat,
                                                                       by=c(id_vars[id_vars!=""],"variable")) %>% 
                                              
                                              # Restrict to those cells with more than 100 observations
                                              dplyr::filter(n > 100) %>%
                                              mutate(stratification = g_v) %>% 
                                              rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                              mutate_at(vars(one_of("strata")),~as.character(.));
                                            gc();
                                            return(n5_out_dmtreat)
                                            
                                          })


unmet_svysummary_dmtreat %>% 
  dplyr::filter(str_detect(variable,"dm_un")) %>% 
  write_csv(.,file = "age_standardized/ncz08_national conditional cascade.csv")

unmet_svysummary_dmtreat %>% 
  dplyr::filter(!str_detect(variable,"dm_un")) %>% 
  write_csv(.,file = "age_standardized/ncz08_national conditional cascade.csv")

