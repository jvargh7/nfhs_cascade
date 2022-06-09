require(haven)
require(tidyverse)
require(lubridate)

iapr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables")$iapr7a_women %>% na.omit(.)


nfhs5_women <- read_dta(paste0(path_dhs_data,"/IA/IAPR7ADT/IAPR7AFL.dta"),col_select = iapr7a_variables) %>%
  dplyr::filter(!is.na(ha1),ha1 >=18,ha54 %in% c(0,9)) %>% 
  mutate(sampleweight = hv005/(10^6),
         hv016 = case_when(is.na(hv016) | hv016 == 98 ~ 15,
                          hv006 == 2 & hv006 %in% c(2008,2012,2016,2020) & hv016 > 29 ~ 29,
                          hv006 == 2 & hv016 > 28 ~ 28,
                          hv006 %in% c(4,6,9,11) & hv016 > 30 ~ 30,
                          TRUE ~ as.numeric(hv016))) %>% 
  mutate(f_interview = as_date(paste0(hv007,"-",hv006,"-",hv016)),
         phase = case_when(f_interview <= "2020-03-23" ~ 1,
                           f_interview > "2020-03-23" ~ 2,
                           TRUE ~ NA_real_)
         ) %>% 
  # Blood pressure cleaning -------
mutate_at(vars(shb18s,shb18d,
               shb25s,shb25d,
               shb29s,shb29d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  
  mutate(f_self_glucosecheck = case_when(shb55 == 1 ~ 1,
                                         TRUE ~ 0),
         f_self_diabetes = case_when(shb56 == 1 ~ 1,
                                     TRUE ~ 0),
         f_self_currdiabmed = case_when(shb57 == 1 ~ 1,
                                        TRUE ~ 0),
         
         
         f_self_bpcheck = case_when(shb19 == 1 ~ 1,
                                         TRUE ~ 0),
         f_self_hypertension = case_when(shb20 == 1 ~ 1,
                                         TRUE ~ 0),
         f_self_currhtnmed = case_when(shb21 == 1 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  mutate(f_bmi_underweight = case_when(ha40 > 6000 ~ NA_real_,
                                       ha40 < 1850 ~ 1,
                                       ha40 >= 1850 ~ 0,
                                       TRUE ~ NA_real_),
         
         
         f_bmi_overweight = case_when(ha40 > 6000 ~ NA_real_,
                                      ha40 >= 2500 & ha40 < 3000 ~ 1,
                                      ha40 < 2500 | ha40 >= 3000 ~ 0,
                                      TRUE ~ NA_real_),
         
         
         f_bmi_obese = case_when(ha40 > 6000 ~ NA_real_,
                                 ha40 >= 3000 ~ 1,
                                 ha40 < 3000 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  
  mutate(f_fasting = case_when(shb53 > 94 | shb54 > 94 ~ NA_real_,
                               shb53 > 8 & shb54 > 8 ~ 1,
                               shb53 <=8 | shb54 <= 8 ~ 0,
                               TRUE ~ NA_real_),

         f_dm = case_when(f_self_diabetes == 1 ~ 1,
                          is.na(shb74) | shb74 > 498 ~ NA_real_,
                          f_fasting == 1 & shb74 >= 126 ~ 1,
                          f_fasting == 0 & shb74 >= 200 ~ 1,
                          is.na(f_fasting) & shb74 >= 200 ~ 1,
                          f_fasting == 1 & shb74 < 126 ~ 0,
                          f_fasting == 0 & shb74 < 200 ~ 0,
                          is.na(f_fasting) & shb74 < 200 ~ 0,
                          TRUE  ~ NA_real_),
         f_highglucose = case_when(
                                   is.na(shb74) | shb74 > 498 ~ NA_real_,
                                   f_fasting == 1 & shb74 >= 126 ~ 1,
                                   f_fasting == 0 & shb74 >= 200 ~ 1,
                                   is.na(f_fasting) & shb74 >= 200 ~ 1,
                                   f_fasting == 1 & shb74 < 126 ~ 0,
                                   f_fasting == 0 & shb74 < 200 ~ 0,
                                   is.na(f_fasting) & shb74 < 200 ~ 0,
                                   TRUE  ~ NA_real_),

         f_diagdm = case_when(
           is.na(shb74) | shb74 > 498 ~ NA_real_,
           f_self_diabetes == 1 & f_fasting == 1 & shb74 >= 126 ~ 1,
           f_self_diabetes == 1 & f_fasting == 0 & shb74 >= 200 ~ 1,
           f_self_diabetes == 1 & is.na(f_fasting) & shb74 >= 200 ~ 1,
           f_self_diabetes == 1 & f_fasting == 1 & shb74 < 126 ~ 0,
           f_self_diabetes == 1 & f_fasting == 0 & shb74 < 200 ~ 0,
           f_self_diabetes == 1 & is.na(f_fasting) & shb74 < 200 ~ 0,
           TRUE  ~ NA_real_
         )
         
         
         ) %>% 
  mutate(f_sbp = rowMeans(.[,c("shb18s","shb25s","shb29s")],na.rm=TRUE),

         # "sb18d" has 108 everywhere
         f_dbp = rowMeans(.[,c("shb25d","shb29d")],na.rm=TRUE),
         f_htn = case_when(f_self_hypertension == 1 ~ 1,
                           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                           f_sbp >= 140 ~ 1,
                           f_dbp >= 90 ~ 1,
                           f_sbp < 140 ~ 0,
                           f_dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         f_highbp = case_when(
                           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                           f_sbp >= 140 ~ 1,
                           f_dbp >= 90 ~ 1,
                           f_sbp < 140 ~ 0,
                           f_dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         f_diaghtn = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           f_self_hypertension == 1 & f_sbp >= 140 ~ 1,
           f_self_hypertension == 1 & f_dbp >= 90 ~ 1,
           f_self_hypertension == 1 & f_sbp < 140 ~ 0,
           f_self_hypertension == 1 & f_dbp < 90 ~ 0,
           TRUE ~ NA_real_),
         ) %>% 
  
  # Diabetes cascade -----
# From cp01_creating couples data.R --------
mutate(f_dm_sample = case_when(is.na(shb74) | shb74 > 498 ~ 0, # | is.na(s728a) removed
                               TRUE ~ 1),
       # Diagnosis: No/DK, Blood sugar: in range
       f_dm_free = case_when(
                             is.na(f_dm) ~ NA_real_,
                             f_dm == 1 ~ 0,
                             f_dm == 0 ~ 1,
                             TRUE ~ NA_real_),
       f_dm_unscreened  = case_when(f_dm == 0 ~ NA_real_,
                                    f_self_glucosecheck == 1 ~ 0,
                                    f_self_glucosecheck == 0 ~ 1,
                                    TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood sugar: diabetes
       f_dm_screened_undiag = case_when(f_self_glucosecheck == 0 | is.na(f_self_glucosecheck) ~ NA_real_,
                                        f_self_diabetes == 1 ~ 0,
                                        f_self_diabetes == 0 ~ 1,
                                        TRUE ~ NA_real_),
       
       f_dm_undiag_uncontr = case_when(f_self_diabetes == 1 | is.na(f_self_diabetes) ~ NA_real_,
                                       f_dm == 1 ~ 1,
                                       f_dm == 0 ~ 0,
                                       TRUE ~ NA_real_),
       # Diagnosis: Yes + Treated: No, Blood sugar: <NA>
       f_dm_diag_untreat = case_when(f_self_diabetes == 1 & f_self_currdiabmed == 1 ~ 0,
                                     f_self_diabetes == 1 & f_self_currdiabmed == 0 ~ 1,
                                     TRUE ~ NA_real_),
       
       # Dignosis: Yes, Treated: Yes, Blood sugar: out of range
       f_dm_treat_uncontr = case_when(f_self_currdiabmed == 0 | is.na(f_self_currdiabmed)  ~ NA_real_,
                                      f_self_diabetes == 1 & f_self_currdiabmed == 1 & f_diagdm == 1 ~ 1,
                                      f_self_diabetes == 1 & f_self_currdiabmed == 1 & f_diagdm == 0 ~ 0,
                                      TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood sugar: in range
       f_dm_treat_contr = 1 - f_dm_treat_uncontr
       
) %>% 
  
  # Hypertension cascade -----
mutate(f_htn_sample = case_when(!is.na(f_sbp)|!is.na(f_dbp) ~ 1,
                                is.na(f_sbp) & is.na(f_dbp) ~ 0,
                                TRUE ~ 1),
       # Diagnosis: No/DK, Blood pressure: in range
       f_htn_free = case_when(
         is.na(f_htn) ~ NA_real_,
         f_htn == 1 ~ 0,
         f_htn == 0 ~ 1,
         TRUE ~ NA_real_),
       f_htn_unscreened  = case_when(f_htn == 0 ~ NA_real_,
                                    f_self_bpcheck == 1 ~ 0,
                                    f_self_bpcheck == 0 ~ 1,
                                    TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood pressure: hypertension
       f_htn_screened_undiag = case_when(f_self_bpcheck == 0 | is.na(f_self_bpcheck) ~ NA_real_,
                                         f_self_hypertension == 1 ~ 0,
                                         f_self_hypertension == 0 ~ 1,
                                         TRUE ~ NA_real_),
       
       f_htn_undiag_uncontr = case_when(f_self_hypertension == 1 | is.na(f_self_hypertension) ~ NA_real_,
                                       f_htn == 1 ~ 1,
                                       f_htn == 0 ~ 0,
                                       TRUE ~ NA_real_),
       
       # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
       f_htn_diag_untreat = case_when(f_self_hypertension == 1 & f_self_currhtnmed == 1 ~ 0,
                                      f_self_hypertension == 1 & f_self_currhtnmed == 0 ~ 1,
                                      TRUE ~ NA_real_),
       
       # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
       f_htn_treat_uncontr = case_when(f_self_currhtnmed == 0 | is.na(f_self_currhtnmed)  ~ NA_real_,
                                       f_self_hypertension == 1 & f_self_currhtnmed == 1 & f_diaghtn == 1 ~ 1,
                                       f_self_hypertension == 1 & f_self_currhtnmed == 1 & f_diaghtn == 0 ~ 0,
                                       TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood pressure: in range
       f_htn_treat_contr = 1 - f_htn_treat_uncontr
       
) %>%
  # HB
  mutate_at(vars(ha56), function(x) case_when(x >= 250 | x < 3 ~ NA_real_,
                                                   TRUE ~ as.numeric(x))) %>% 
  # BMI
  mutate_at(vars(ha40),function(x) case_when(x > 6000 ~ NA_real_,
                                                  TRUE ~ as.numeric(x))) %>%
  # Circumferences
  mutate_at(vars(sh305,sh306),function(x) case_when(x > 240 ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  # Glucose
  mutate_at(vars(shb74), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  # Caste
  mutate_at(vars(sh49),function(x) case_when(x == 1 ~ "Schedule Caste",
                                                   x == 2 ~ "Schedule Tribe",
                                                   x == 3 ~ "OBC",
                                                   x == 4 ~ "General",
                                                   x == 8 ~ "General",
                                                   TRUE ~ NA_character_)) %>% 
  # Education
  mutate_at(vars(ha66),function(x) case_when(x == 0 ~ "No education",
                                                   x == 1 ~ "Primary",
                                                   x == 2 ~ "Secondary",
                                                   x == 3 ~ "Higher",
                                                   x == 9 ~ NA_character_,
                                                   TRUE ~ NA_character_)) %>% 
  # Religion
  mutate_at(vars(sh47),function(x) case_when(x == 1 ~ "Hindu",
                                                   x == 2 ~ "Muslim",
                                                   TRUE ~ "Other")) %>% 
  # insurance, alcohol
  mutate_at(vars(
    sh26,sh71), function(x) case_when(x == 0 ~ 0,
                                                  x == 1 ~ 1,
                                                  TRUE ~ NA_real_)) %>% 
  # Ever married
  mutate_at(vars(ha60), function(x) case_when(x %in% c(2) ~ 0,
                                                    x %in% c(1) ~ 1,
                                                    TRUE ~ NA_real_)) %>% 
  # Smoking
  mutate_at(vars(sh25), function(x) case_when(x == 1 ~ 1,
                                                      x == 0 ~ 0,
                                                      TRUE ~ NA_real_)) %>% 
  
  mutate(
         ha67 = case_when(ha66 == "No education" ~ 0,
                          TRUE ~ as.numeric(ha67))
  ) %>% 
  rename(
    f_wealth = hv270,
    f_evermarried = ha60,
    f_caste = sh49,
    f_religion = sh47,
    
    f_age = ha1,
    f_edulvl = ha67,
    f_eduyr = ha66,
    f_insurance = sh71,

    f_alcohol = sh26,
    f_smoke = sh25,
    
    f_bmi = ha40,
    f_hb = ha56,
    f_glucose = shb74,
    f_wc = sh305,
    f_hc = sh306
    
  ) %>% 
  # From cp03_care cascade datasets.R --------
  mutate(f_dm_disease = case_when(is.na(f_dm_free) ~ NA_real_,
                                     f_dm_free == 1 ~ 0,
                                     f_dm_undiag_uncontr == 1 ~ 1,
                                     f_dm_diag_untreat == 1 ~ 1,
                                     f_dm_treat_uncontr == 1 ~ 1,
                                     f_dm_treat_contr == 1 ~ 1,
                                     TRUE ~ 0),
         f_dm_screened = case_when(
                                   f_self_glucosecheck == 1 ~ 1,
                                   f_dm_undiag_uncontr == 1 ~ 0,
                                   f_dm_diag_untreat == 1 ~ 1,
                                   f_dm_treat_uncontr == 1 ~ 1,
                                   f_dm_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
         
         f_dm_diagnosed = case_when(is.na(f_dm_free) ~ NA_real_,
                                       f_dm_free == 1 ~ 0,
                                       f_dm_undiag_uncontr == 1 ~ 0,
                                       f_dm_diag_untreat == 1 ~ 1,
                                       f_dm_treat_uncontr == 1 ~ 1,
                                       f_dm_treat_contr == 1 ~ 1,
                                       TRUE ~ 0
         ),
         f_dm_treated = case_when(is.na(f_dm_free) ~ NA_real_,
                                     f_dm_free == 1 ~ 0,
                                     f_dm_undiag_uncontr == 1 ~ 0,
                                     f_dm_diag_untreat == 1 ~ 0,
                                     f_dm_treat_uncontr == 1 ~ 1,
                                     f_dm_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
         ),
         f_dm_controlled = case_when(is.na(f_dm_free) ~ NA_real_,
                                        f_dm_free == 1 ~ 0,
                                        f_dm_undiag_uncontr == 1 ~ 0,
                                        f_dm_diag_untreat == 1 ~ 0,
                                        f_dm_treat_uncontr == 1 ~ 0,
                                        f_dm_treat_contr == 1 ~ 1,
                                        TRUE ~ 0
         ),
         f_dm_screened_in_dis = case_when(
                                          is.na(f_dm_free) ~ NA_real_,
                                          f_dm_free == 1 ~ NA_real_,
                                          f_self_glucosecheck == 1 ~ 1,
                                          f_dm_undiag_uncontr == 1 ~ 0,
                                          f_dm_diag_untreat == 1 ~ 1,
                                          f_dm_treat_uncontr == 1 ~ 1,
                                          f_dm_treat_contr == 1 ~ 1,
                                          TRUE ~ 0),
         f_dm_diagnosed_in_dis = case_when(is.na(f_dm_free) ~ NA_real_,
                                              f_dm_free == 1 ~ NA_real_,
                                              f_dm_undiag_uncontr == 1 ~ 0,
                                              f_dm_diag_untreat == 1 ~ 1,
                                              f_dm_treat_uncontr == 1 ~ 1,
                                              f_dm_treat_contr == 1 ~ 1,
                                              TRUE ~ 0
         ),
         f_dm_treated_in_dis = case_when(is.na(f_dm_free) ~ NA_real_,
                                            f_dm_free == 1 ~ NA_real_,
                                            f_dm_undiag_uncontr == 1 ~ 0,
                                            f_dm_diag_untreat == 1 ~ 0,
                                            f_dm_treat_uncontr == 1 ~ 1,
                                            f_dm_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
         ),
         f_dm_controlled_in_dis = case_when(is.na(f_dm_free) ~ NA_real_,
                                               f_dm_free == 1 ~ NA_real_,
                                               f_dm_undiag_uncontr == 1 ~ 0,
                                               f_dm_diag_untreat == 1 ~ 0,
                                               f_dm_treat_uncontr == 1 ~ 0,
                                               f_dm_treat_contr == 1 ~ 1,
                                               TRUE ~ 0
         )) %>% 
  mutate(f_htn_disease = case_when(is.na(f_htn_free) ~ NA_real_,
                                  f_htn_free == 1 ~ 0,
                                  f_htn_undiag_uncontr == 1 ~ 1,
                                  f_htn_diag_untreat == 1 ~ 1,
                                  f_htn_treat_uncontr == 1 ~ 1,
                                  f_htn_treat_contr == 1 ~ 1,
                                  TRUE ~ 0),
         f_htn_screened = case_when(
                                   f_self_bpcheck == 1 ~ 1,
                                   f_htn_undiag_uncontr == 1 ~ 0,
                                   f_htn_diag_untreat == 1 ~ 1,
                                   f_htn_treat_uncontr == 1 ~ 1,
                                   f_htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
         
         f_htn_diagnosed = case_when(is.na(f_htn_free) ~ NA_real_,
                                    f_htn_free == 1 ~ 0,
                                    f_htn_undiag_uncontr == 1 ~ 0,
                                    f_htn_diag_untreat == 1 ~ 1,
                                    f_htn_treat_uncontr == 1 ~ 1,
                                    f_htn_treat_contr == 1 ~ 1,
                                    TRUE ~ 0
         ),
         f_htn_treated = case_when(is.na(f_htn_free) ~ NA_real_,
                                  f_htn_free == 1 ~ 0,
                                  f_htn_undiag_uncontr == 1 ~ 0,
                                  f_htn_diag_untreat == 1 ~ 0,
                                  f_htn_treat_uncontr == 1 ~ 1,
                                  f_htn_treat_contr == 1 ~ 1,
                                  TRUE ~ 0
         ),
         f_htn_controlled = case_when(is.na(f_htn_free) ~ NA_real_,
                                     f_htn_free == 1 ~ 0,
                                     f_htn_undiag_uncontr == 1 ~ 0,
                                     f_htn_diag_untreat == 1 ~ 0,
                                     f_htn_treat_uncontr == 1 ~ 0,
                                     f_htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
         ),
         f_htn_screened_in_dis = case_when(
                                          is.na(f_htn_free) ~ NA_real_,
                                          f_htn_free == 1 ~ NA_real_,
                                          f_self_bpcheck == 1 ~ 1,
                                          f_htn_undiag_uncontr == 1 ~ 0,
                                          f_htn_diag_untreat == 1 ~ 1,
                                          f_htn_treat_uncontr == 1 ~ 1,
                                          f_htn_treat_contr == 1 ~ 1,
                                          TRUE ~ 0),
         f_htn_diagnosed_in_dis = case_when(is.na(f_htn_free) ~ NA_real_,
                                           f_htn_free == 1 ~ NA_real_,
                                           f_htn_undiag_uncontr == 1 ~ 0,
                                           f_htn_diag_untreat == 1 ~ 1,
                                           f_htn_treat_uncontr == 1 ~ 1,
                                           f_htn_treat_contr == 1 ~ 1,
                                           TRUE ~ 0
         ),
         f_htn_treated_in_dis = case_when(is.na(f_htn_free) ~ NA_real_,
                                         f_htn_free == 1 ~ NA_real_,
                                         f_htn_undiag_uncontr == 1 ~ 0,
                                         f_htn_diag_untreat == 1 ~ 0,
                                         f_htn_treat_uncontr == 1 ~ 1,
                                         f_htn_treat_contr == 1 ~ 1,
                                         TRUE ~ 0
         ),
         f_htn_controlled_in_dis = case_when(is.na(f_htn_free) ~ NA_real_,
                                            f_htn_free == 1 ~ NA_real_,
                                            f_htn_undiag_uncontr == 1 ~ 0,
                                            f_htn_diag_untreat == 1 ~ 0,
                                            f_htn_treat_uncontr == 1 ~ 0,
                                            f_htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
         ))

saveRDS(nfhs5_women,paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS"))

  