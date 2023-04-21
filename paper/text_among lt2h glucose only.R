
group_vars = c("","sex")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/ncpre20_nfhs5 2h total svydesign.R")
source("preprocessing/ncpre21_nfhs5 2h diabetes svydesign.R")
source("preprocessing/ncpre22_nfhs5 2h diagnosed svydesign.R")

proportion_vars <- c("","dm_screened","dm_diagnosed","dm_unscreened","dm_undiagnosed",
                     "dm_treated","dm_controlled","dm_untreated","dm_uncontrolled")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5z_svydesign = svystandardize(nfhs5lt2h_svydesign,by=~age_category,over = ~caste,
                                  population = pop_age)

svysummary(nfhs5z_svydesign,
           # c_vars = continuous_vars,
           p_vars = c("dm_disease")
           # g_vars = grouped_vars,
           # id_vars = id_vars
)

nfhs5dmz_svydesign = svystandardize(nfhs5dmlt2h_svydesign,by=~age_category,over = ~caste,
                                    population = pop_age)
nfhs5dmdiagz_svydesign = svystandardize(nfhs5dmdiaglt2h_svydesign,by=~age_category,over = ~caste,
                                        population = pop_age)

unmet_svysummary_dm <- map_dfr(group_vars,
                               function(g_v){
                                 id_vars = c(g_v);
                                 print(g_v);
                                 n5_sy_dm <- svysummary(nfhs5dmz_svydesign,
                                                        # c_vars = continuous_vars,
                                                        p_vars = proportion_vars[1:4],
                                                        # g_vars = grouped_vars,
                                                        id_vars = id_vars
                                 ) %>% 
                                   mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                   mutate(est_ci = paste0(estimate," (",
                                                          lci,", ",uci,")"));
                                 
                                 # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                 n5_ct_dm <- nfhs5dmlt2h_df %>% 
                                   group_by_at(vars(one_of(id_vars))) %>% 
                                   summarize_at(vars(one_of(c(
                                     # continuous_vars,
                                     proportion_vars[1:4]
                                     # grouped_vars
                                   ))),
                                   list(n = ~sum(!is.na(.)))) %>% 
                                   pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                   mutate(variable = str_replace(variable,"_n$",""));
                                 
                                 n5_out_dm <- left_join(n5_sy_dm,
                                                        n5_ct_dm,
                                                        by=c(id_vars[id_vars!=""],"variable")) %>% 
                                   
                                   # Restrict to those cells with more than 100 observations
                                   dplyr::filter(n > 100) %>%
                                   mutate(stratification = g_v) %>% 
                                   rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                   mutate_at(vars(one_of("strata")),~as.character(.));
                                 gc();
                                 return(n5_out_dm)
                                 
                               })


unmet_svysummary_dmdiag <- map_dfr(group_vars,
                                   function(g_v){
                                     id_vars = c(g_v);
                                     print(g_v);
                                     n5_sy_dmdiag <- svysummary(nfhs5dmdiagz_svydesign,
                                                                # c_vars = continuous_vars,
                                                                p_vars = proportion_vars[5:8],
                                                                # g_vars = grouped_vars,
                                                                id_vars = id_vars
                                     ) %>% 
                                       mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                       mutate(est_ci = paste0(estimate," (",
                                                              lci,", ",uci,")"));
                                     
                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     n5_ct_dmdiag <- nfhs5dmdiaglt2h_df %>% 
                                       group_by_at(vars(one_of(id_vars))) %>% 
                                       summarize_at(vars(one_of(c(
                                         # continuous_vars,
                                         proportion_vars[5:8]
                                         # grouped_vars
                                       ))),
                                       list(n = ~sum(!is.na(.)))) %>% 
                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                       mutate(variable = str_replace(variable,"_n$",""));
                                     
                                     n5_out_dmdiag <- left_join(n5_sy_dmdiag,
                                                                n5_ct_dmdiag,
                                                                by=c(id_vars[id_vars!=""],"variable")) %>% 
                                       
                                       # Restrict to those cells with more than 100 observations
                                       dplyr::filter(n > 100) %>%
                                       mutate(stratification = g_v) %>% 
                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                     gc();
                                     return(n5_out_dmdiag)
                                     
                                   })


bind_rows(unmet_svysummary_dm,
          unmet_svysummary_dmdiag) %>% 
  write_csv(.,file = "paper/text_among lt2h glucose only.csv")

