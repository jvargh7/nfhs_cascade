
group_vars = c("","sex","age_category","education",
               "caste","religion","wealthq_ur")

source("C:/code/external/functions/survey/svysummary.R")


source("preprocessing/ncpre11_nfhs5 controlled svydesign.R")

proportion_vars <- c("dm_treated","dm_untreated")


pop_age <- read_csv("data/population for age standardization.csv") %>% 
  dplyr::select(n) %>% 
  pull()

# Dropped education in age standardization 
nfhs5dmcontrolz_svydesign = svystandardize(nfhs5dmcontrol_svydesign,by=~age_category,over = ~caste + religion + wealthq_ur,
                                         population = pop_age)


national_control <- svysummary(nfhs5dmcontrolz_svydesign,
                             # c_vars = continuous_vars,
                             p_vars = proportion_vars,
                             # g_vars = grouped_vars,
                             id_vars = c("")
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

# NATIONAL ----------
unmet_svysummary_dmcontrol_national <- map_dfr(group_vars[-1],
                                             function(g_v){
                                               id_vars = c(g_v);
                                               n5_sy <- svysummary(nfhs5dmcontrolz_svydesign,
                                                                   # c_vars = continuous_vars,
                                                                   p_vars = proportion_vars,
                                                                   # g_vars = grouped_vars,
                                                                   id_vars = id_vars
                                               ) %>% 
                                                 mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                                 mutate(est_ci = paste0(estimate," (",
                                                                        lci,", ",uci,")"));
                                               
                                               # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                               n5_ct <- nfhs5dmcontrol_df %>% 
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

# REGION -----------
unmet_svysummary_dmcontrol_region <- map_dfr(group_vars,
                                    function(g_v){
                                      id_vars = c("residence",g_v);
                                      print(g_v);
                                      n5_sy_dmcontrol <- svysummary(nfhs5dmcontrolz_svydesign,
                                                                  # c_vars = continuous_vars,
                                                                  p_vars = proportion_vars,
                                                                  # g_vars = grouped_vars,
                                                                  id_vars = id_vars
                                      ) %>% 
                                        mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                        mutate(est_ci = paste0(estimate," (",
                                                               lci,", ",uci,")"));
                                      
                                      # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                      n5_ct_dmcontrol <- nfhs5dmcontrol_df %>% 
                                        group_by_at(vars(one_of(id_vars))) %>% 
                                        summarize_at(vars(one_of(c(
                                          # continuous_vars,
                                          proportion_vars
                                          # grouped_vars
                                        ))),
                                        list(n = ~sum(!is.na(.)))) %>% 
                                        pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
                                        mutate(variable = str_replace(variable,"_n$",""));
                                      
                                      n5_out_dmcontrol <- left_join(n5_sy_dmcontrol,
                                                                  n5_ct_dmcontrol,
                                                                  by=c(id_vars[id_vars!=""],"variable")) %>% 
                                        
                                        # Restrict to those cells with more than 100 observations
                                        dplyr::filter(n > 100) %>%
                                        mutate(stratification = g_v) %>% 
                                        rename_at(vars(one_of(g_v)),~c("strata")) %>% 
                                        mutate_at(vars(one_of("strata")),~as.character(.));
                                      gc();
                                      return(n5_out_dmcontrol)
                                      
                                    })



bind_rows(national_control, 
          unmet_svysummary_dmcontrol_national,
          unmet_svysummary_dmcontrol_region) %>% 
  write_csv(.,file = "paper/text_controlled blood glucose.csv")


bind_rows(national_control, 
          unmet_svysummary_dmcontrol_national,
          unmet_svysummary_dmcontrol_region)  %>% 
  dplyr::filter(variable %in% c("dm_treated"))  %>% 
  dplyr::select(stratification,strata,residence,variable,est_ci) %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  arrange(residence) %>% 
  pivot_wider(names_from=c(residence,variable),values_from=est_ci) %>% 
  write_csv(.,"paper/table_treated among controlled blood glucose.csv")
