group_vars <- c("","sex")


source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre03_nfhs5 total svydesign.R")

proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")

require(furrr)
options(future.globals.maxSize= (4*1024*1024)^2) #4GB
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
plan(multisession, workers = 2)
district_svysummary <- future_map_dfr(group_vars,
                                   function(g_v){
                                     id_vars = c("district_df",g_v);
                                     print(g_v);
                                     n5_sy <- svysummary(nfhs5_svydesign,
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
                                       # dplyr::filter(n > 100) %>% 
                                       mutate(stratification = g_v) %>% 
                                       rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.));
                                     
                                     return(n5_out)
                                     
                                   })

write_csv(district_svysummary,file = "analysis/nca04_district2018 level care cascade.csv")
