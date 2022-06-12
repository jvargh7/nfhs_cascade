require(haven)
require(tidyverse)
require(lubridate)

iapr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables")$iapr7a_men %>% na.omit(.)


nfhs5_men <- read_dta(paste0(path_dhs_data,"/IA/IAPR7ADT/IAPR7AFL.dta"),col_select = iapr7a_variables) %>%
  dplyr::filter(!is.na(hb1),hb1 >=18) %>% 
  mutate(sampleweight = hv005/(10^6),
         hv016 = case_when(is.na(hv016) | hv016 == 98 ~ 15,
                           hv006 == 2 & hv006 %in% c(2008,2012,2016,2020) & hv016 > 29 ~ 29,
                           hv006 == 2 & hv016 > 28 ~ 28,
                           hv006 %in% c(4,6,9,11) & hv016 > 30 ~ 30,
                           TRUE ~ as.numeric(hv016))) %>% 
  mutate(interview = as_date(paste0(hv007,"-",hv006,"-",hv016)),
         phase = case_when(interview <= "2020-03-23" ~ 1,
                           interview > "2020-03-23" ~ 2,
                           TRUE ~ NA_real_)
  ) %>% 
# Blood pressure cleaning -------
mutate_at(vars(shb18s,shb18d,
               shb25s,shb25d,
               shb29s,shb29d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
  
  mutate(selfglucosecheck = case_when(shb55 == 1 ~ 1,
                                         TRUE ~ 0),
         selfdiabetes = case_when(shb56 == 1 ~ 1,
                                     TRUE ~ 0),
         selfcurrdiabmed = case_when(shb57 == 1 ~ 1,
                                        TRUE ~ 0),
         
         
         selfbpcheck = case_when(shb19 == 1 ~ 1,
                                    TRUE ~ 0),
         selfhypertension = case_when(shb20 == 1 ~ 1,
                                         TRUE ~ 0),
         selfcurrhtnmed = case_when(shb21 == 1 ~ 1,
                                       TRUE ~ 0)) %>% 
  
  mutate(bmi_underweight = case_when(hb40 > 6000 ~ NA_real_,
                                       hb40 < 1850 ~ 1,
                                       hb40 >= 1850 ~ 0,
                                       TRUE ~ NA_real_),
         
         
         bmi_overweight = case_when(hb40 > 6000 ~ NA_real_,
                                      hb40 >= 2500 & hb40 < 3000 ~ 1,
                                      hb40 < 2500 | hb40 >= 3000 ~ 0,
                                      TRUE ~ NA_real_),
         
         
         bmi_obese = case_when(hb40 > 6000 ~ NA_real_,
                                 hb40 >= 3000 ~ 1,
                                 hb40 < 3000 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  
  mutate(fasting = case_when(shb53 > 94 | shb54 > 94 ~ NA_real_,
                               shb53 > 8 & shb54 > 8 ~ 1,
                               shb53 <=8 | shb54 <= 8 ~ 0,
                               TRUE ~ NA_real_),
         
         dm = case_when(selfdiabetes == 1 ~ 1,
                          is.na(shb74) | shb74 > 498 ~ NA_real_,
                          fasting == 1 & shb74 >= 126 ~ 1,
                          fasting == 0 & shb74 >= 200 ~ 1,
                          is.na(fasting) & shb74 >= 200 ~ 1,
                          fasting == 1 & shb74 < 126 ~ 0,
                          fasting == 0 & shb74 < 200 ~ 0,
                          is.na(fasting) & shb74 < 200 ~ 0,
                          TRUE  ~ NA_real_),
         highglucose = case_when(
           is.na(shb74) | shb74 > 498 ~ NA_real_,
           fasting == 1 & shb74 >= 126 ~ 1,
           fasting == 0 & shb74 >= 200 ~ 1,
           is.na(fasting) & shb74 >= 200 ~ 1,
           fasting == 1 & shb74 < 126 ~ 0,
           fasting == 0 & shb74 < 200 ~ 0,
           is.na(fasting) & shb74 < 200 ~ 0,
           TRUE  ~ NA_real_),
         
         diagdm = case_when(
           is.na(shb74) | shb74 > 498 ~ NA_real_,
           selfdiabetes == 1 & fasting == 1 & shb74 >= 126 ~ 1,
           selfdiabetes == 1 & fasting == 0 & shb74 >= 200 ~ 1,
           selfdiabetes == 1 & is.na(fasting) & shb74 >= 200 ~ 1,
           selfdiabetes == 1 & fasting == 1 & shb74 < 126 ~ 0,
           selfdiabetes == 1 & fasting == 0 & shb74 < 200 ~ 0,
           selfdiabetes == 1 & is.na(fasting) & shb74 < 200 ~ 0,
           TRUE  ~ NA_real_
         )
         
         
  ) %>% 
  mutate(sbp = rowMeans(.[,c("shb18s","shb25s","shb29s")],na.rm=TRUE),
         
         # "sb18d" has 108 everywhere
         dbp = rowMeans(.[,c("shb25d","shb29d")],na.rm=TRUE),
         htn = case_when(selfhypertension == 1 ~ 1,
                           is.na(sbp) | is.na(dbp) ~ NA_real_,
                           sbp >= 140 ~ 1,
                           dbp >= 90 ~ 1,
                           sbp < 140 ~ 0,
                           dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         highbp = case_when(
           is.na(sbp) | is.na(dbp) ~ NA_real_,
           sbp >= 140 ~ 1,
           dbp >= 90 ~ 1,
           sbp < 140 ~ 0,
           dbp < 90 ~ 0,
           TRUE ~ NA_real_),
         diaghtn = case_when(
           is.na(sbp) | is.na(dbp) ~ NA_real_,
           selfhypertension == 1 & sbp >= 140 ~ 1,
           selfhypertension == 1 & dbp >= 90 ~ 1,
           selfhypertension == 1 & sbp < 140 ~ 0,
           selfhypertension == 1 & dbp < 90 ~ 0,
           TRUE ~ NA_real_),
  ) %>% 
  
  # Diabetes cascade -----
# Diabetes cascade -----
mutate(dm_sample = case_when(is.na(shb74) | shb74 > 498 ~ 0, # | is.na(s728a) removed
                               TRUE ~ 1),
       # Diagnosis: No/DK, Blood sugar: in range
       dm_free = case_when(
         is.na(dm) ~ NA_real_,
         dm == 1 ~ 0,
         dm == 0 ~ 1,
         TRUE ~ NA_real_),
       dm_unscreened  = case_when(dm == 0 ~ NA_real_,
                                    selfglucosecheck == 1 ~ 0,
                                    selfglucosecheck == 0 ~ 1,
                                    TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood sugar: diabetes
       dm_screened_undiag = case_when(selfglucosecheck == 0 | is.na(selfglucosecheck) ~ NA_real_,
                                        selfdiabetes == 1 ~ 0,
                                        selfdiabetes == 0 ~ 1,
                                        TRUE ~ NA_real_),
       
       dm_undiag_uncontr = case_when(selfdiabetes == 1 | is.na(selfdiabetes) ~ NA_real_,
                                       dm == 1 ~ 1,
                                       dm == 0 ~ 0,
                                       TRUE ~ NA_real_),
       # Diagnosis: Yes + Treated: No, Blood sugar: <NA>
       dm_diag_untreat = case_when(selfdiabetes == 1 & selfcurrdiabmed == 1 ~ 0,
                                     selfdiabetes == 1 & selfcurrdiabmed == 0 ~ 1,
                                     TRUE ~ NA_real_),
       
       # Dignosis: Yes, Treated: Yes, Blood sugar: out of range
       dm_treat_uncontr = case_when(selfcurrdiabmed == 0 | is.na(selfcurrdiabmed)  ~ NA_real_,
                                      selfdiabetes == 1 & selfcurrdiabmed == 1 & diagdm == 1 ~ 1,
                                      selfdiabetes == 1 & selfcurrdiabmed == 1 & diagdm == 0 ~ 0,
                                      TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood sugar: in range
       dm_treat_contr = 1 - dm_treat_uncontr,
       
       # Dignosis: Yes, Treated: No, Blood sugar: out of range
       dm_diag_uncontr = case_when(selfdiabetes == 0 | is.na(selfdiabetes)  ~ NA_real_,
                                   selfdiabetes == 1 & diagdm == 1 ~ 1,
                                   selfdiabetes == 1 & diagdm == 0 ~ 0,
                                   TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: No, Blood sugar: in range
       dm_diag_contr = 1 - dm_diag_uncontr
       
) %>% 
  
  # Hypertension cascade -----
mutate(htn_sample = case_when(!is.na(sbp)|!is.na(dbp) ~ 1,
                                is.na(sbp) & is.na(dbp) ~ 0,
                                TRUE ~ 1),
       # Diagnosis: No/DK, Blood pressure: in range
       htn_free = case_when(
         is.na(htn) ~ NA_real_,
         htn == 1 ~ 0,
         htn == 0 ~ 1,
         TRUE ~ NA_real_),
       htn_unscreened  = case_when(htn == 0 ~ NA_real_,
                                     selfbpcheck == 1 ~ 0,
                                     selfbpcheck == 0 ~ 1,
                                     TRUE ~ NA_real_),
       
       # Diagnosis: No/DK + Blood pressure: hypertension
       htn_screened_undiag = case_when(selfbpcheck == 0 | is.na(selfbpcheck) ~ NA_real_,
                                         selfhypertension == 1 ~ 0,
                                         selfhypertension == 0 ~ 1,
                                         TRUE ~ NA_real_),
       
       htn_undiag_uncontr = case_when(selfhypertension == 1 | is.na(selfhypertension) ~ NA_real_,
                                       htn == 1 ~ 1,
                                       htn == 0 ~ 0,
                                       TRUE ~ NA_real_),
       
       # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
       htn_diag_untreat = case_when(selfhypertension == 1 & selfcurrhtnmed == 1 ~ 0,
                                      selfhypertension == 1 & selfcurrhtnmed == 0 ~ 1,
                                      TRUE ~ NA_real_),
       
       # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
       htn_treat_uncontr = case_when(selfcurrhtnmed == 0 | is.na(selfcurrhtnmed)  ~ NA_real_,
                                       selfhypertension == 1 & selfcurrhtnmed == 1 & diaghtn == 1 ~ 1,
                                       selfhypertension == 1 & selfcurrhtnmed == 1 & diaghtn == 0 ~ 0,
                                       TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood pressure: in range
       htn_treat_contr = 1 - htn_treat_uncontr,
       
       
       # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
       htn_diag_uncontr = case_when(selfhypertension == 0 | is.na(selfhypertension)  ~ NA_real_,
                                    selfhypertension == 1 &  diaghtn == 1 ~ 1,
                                    selfhypertension == 1 &  diaghtn == 0 ~ 0,
                                    TRUE ~ NA_real_),
       # Dignosis: Yes, Treated: Yes, Blood pressure: in range
       htn_diag_contr = 1 - htn_diag_uncontr
       
) %>% 
  # Prediabetes and Prehypertension ------
mutate(prediabetes = case_when(selfdiabetes == 1 ~ NA_real_,
                               is.na(shb74) | shb74 > 498 ~ NA_real_,
                               dm == 1 ~ 0,
                               fasting == 1 & shb74 >= fpgpre_cutoff & shb74 < fpg_cutoff ~ 1,
                               fasting == 0 & shb74 >= rpgpre_cutoff & shb74 < rpg_cutoff~ 1,
                               is.na(fasting) & shb74 >= rpgpre_cutoff & shb74 < rpg_cutoff ~ 1,
                               fasting == 1 & shb74 < fpgpre_cutoff ~ 0,
                               fasting == 0 & shb74 < rpgpre_cutoff ~ 0,
                               is.na(fasting) & shb74 < rpgpre_cutoff ~ 0,
                               TRUE  ~ NA_real_),
       prehypertension = case_when(selfhypertension == 1 ~ NA_real_,
                                   is.na(sbp) | is.na(dbp) ~ NA_real_,
                                   htn == 1 ~ 0,
                                   sbp >= sbppre_cutoff & sbp < sbp_cutoff ~ 1,
                                   dbp >= dbppre_cutoff & dbp < dbp_cutoff~ 1,
                                   sbp < sbppre_cutoff ~ 0,
                                   dbp < dbppre_cutoff ~ 0,
                                   TRUE ~ NA_real_)
) %>% 
  
  
  # HB
  mutate_at(vars(hb56), function(x) case_when(x >= 250 | x < 3 ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>% 
  # BMI
  mutate_at(vars(hb40),function(x) case_when(x > 6000 ~ NA_real_,
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
  mutate_at(vars(hb66),function(x) case_when(x == 0 ~ "No education",
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
  mutate_at(vars(hb60), function(x) case_when(x %in% c(2) ~ 0,
                                              x %in% c(1) ~ 1,
                                              TRUE ~ NA_real_)) %>% 
  # Smoking
  mutate_at(vars(sh25), function(x) case_when(x == 1 ~ 1,
                                              x == 0 ~ 0,
                                              TRUE ~ NA_real_)) %>% 
  
  mutate(
    hb67 = case_when(hb66 == "No education" ~ 0,
                     TRUE ~ as.numeric(hb67))
  ) %>% 
  rename(
    wealth = hv270,
    evermarried = hb60,
    caste = sh49,
    religion = sh47,
    
    age = hb1,
    edulvl = hb67,
    eduyr = hb66,
    insurance = sh71,
    
    alcohol = sh26,
    smoke = sh25,
    
    bmi = hb40,
    hb = hb56,
    glucose = shb74,
    wc = sh305,
    hc = sh306
    
  ) %>% 
  # From cp03_care cascade datasets.R --------
mutate(dm_disease = case_when(is.na(dm_free) ~ NA_real_,
                                dm_free == 1 ~ 0,
                                dm_undiag_uncontr == 1 ~ 1,
                                dm_diag_untreat == 1 ~ 1,
                                dm_treat_uncontr == 1 ~ 1,
                                dm_treat_contr == 1 ~ 1,
                                TRUE ~ 0),
       dm_screened = case_when(
                                 selfglucosecheck == 1 ~ 1,
                                 dm_undiag_uncontr == 1 ~ 0,
                                 dm_diag_untreat == 1 ~ 1,
                                 dm_treat_uncontr == 1 ~ 1,
                                 dm_treat_contr == 1 ~ 1,
                                 TRUE ~ 0),
       
       dm_diagnosed = case_when(is.na(dm_free) ~ NA_real_,
                                  dm_free == 1 ~ 0,
                                  dm_undiag_uncontr == 1 ~ 0,
                                  dm_diag_untreat == 1 ~ 1,
                                  dm_treat_uncontr == 1 ~ 1,
                                  dm_treat_contr == 1 ~ 1,
                                  TRUE ~ 0
       ),
       dm_treated = case_when(is.na(dm_free) ~ NA_real_,
                                dm_free == 1 ~ 0,
                                dm_undiag_uncontr == 1 ~ 0,
                                dm_diag_untreat == 1 ~ 0,
                                dm_treat_uncontr == 1 ~ 1,
                                dm_treat_contr == 1 ~ 1,
                                TRUE ~ 0
       ),
       dm_controlled = case_when(is.na(dm_free) ~ NA_real_,
                                   dm_free == 1 ~ 0,
                                   dm_undiag_uncontr == 1 ~ 0,
                                   dm_diag_untreat == 1 ~ 0,
                                   dm_treat_uncontr == 1 ~ 0,
                                   dm_treat_contr == 1 ~ 1,
                                   TRUE ~ 0
       ),
       dm_screened_in_dis = case_when(
                                        is.na(dm_free) ~ NA_real_,
                                        dm_free == 1 ~ NA_real_,
                                        selfglucosecheck == 1 ~ 1,
                                        dm_undiag_uncontr == 1 ~ 0,
                                        dm_diag_untreat == 1 ~ 1,
                                        dm_treat_uncontr == 1 ~ 1,
                                        dm_treat_contr == 1 ~ 1,
                                        TRUE ~ 0),
       dm_diagnosed_in_dis = case_when(is.na(dm_free) ~ NA_real_,
                                         dm_free == 1 ~ NA_real_,
                                         dm_undiag_uncontr == 1 ~ 0,
                                         dm_diag_untreat == 1 ~ 1,
                                         dm_treat_uncontr == 1 ~ 1,
                                         dm_treat_contr == 1 ~ 1,
                                         TRUE ~ 0
       ),
       dm_treated_in_dis = case_when(is.na(dm_free) ~ NA_real_,
                                       dm_free == 1 ~ NA_real_,
                                       dm_undiag_uncontr == 1 ~ 0,
                                       dm_diag_untreat == 1 ~ 0,
                                       dm_treat_uncontr == 1 ~ 1,
                                       dm_treat_contr == 1 ~ 1,
                                       TRUE ~ 0
       ),
       dm_controlled_in_dis = case_when(is.na(dm_free) ~ NA_real_,
                                          dm_free == 1 ~ NA_real_,
                                          dm_undiag_uncontr == 1 ~ 0,
                                          dm_diag_untreat == 1 ~ 0,
                                          dm_treat_uncontr == 1 ~ 0,
                                          dm_treat_contr == 1 ~ 1,
                                          TRUE ~ 0
       )) %>% 
  mutate(htn_disease = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_uncontr == 1 ~ 1,
                                   htn_diag_untreat == 1 ~ 1,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
         htn_screened = case_when(
                                    selfbpcheck == 1 ~ 1,
                                    htn_undiag_uncontr == 1 ~ 0,
                                    htn_diag_untreat == 1 ~ 1,
                                    htn_treat_uncontr == 1 ~ 1,
                                    htn_treat_contr == 1 ~ 1,
                                    TRUE ~ 0),
         
         htn_diagnosed = case_when(is.na(htn_free) ~ NA_real_,
                                     htn_free == 1 ~ 0,
                                     htn_undiag_uncontr == 1 ~ 0,
                                     htn_diag_untreat == 1 ~ 1,
                                     htn_treat_uncontr == 1 ~ 1,
                                     htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
         ),
         htn_treated = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_uncontr == 1 ~ 0,
                                   htn_diag_untreat == 1 ~ 0,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0
         ),
         htn_controlled = case_when(is.na(htn_free) ~ NA_real_,
                                      htn_free == 1 ~ 0,
                                      htn_undiag_uncontr == 1 ~ 0,
                                      htn_diag_untreat == 1 ~ 0,
                                      htn_treat_uncontr == 1 ~ 0,
                                      htn_treat_contr == 1 ~ 1,
                                      TRUE ~ 0
         ),
         htn_screened_in_dis = case_when(
                                           is.na(htn_free) ~ NA_real_,
                                           htn_free == 1 ~ NA_real_,
                                           selfbpcheck == 1 ~ 1,
                                           htn_undiag_uncontr == 1 ~ 0,
                                           htn_diag_untreat == 1 ~ 1,
                                           htn_treat_uncontr == 1 ~ 1,
                                           htn_treat_contr == 1 ~ 1,
                                           TRUE ~ 0),
         htn_diagnosed_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                            htn_free == 1 ~ NA_real_,
                                            htn_undiag_uncontr == 1 ~ 0,
                                            htn_diag_untreat == 1 ~ 1,
                                            htn_treat_uncontr == 1 ~ 1,
                                            htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
         ),
         htn_treated_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                          htn_free == 1 ~ NA_real_,
                                          htn_undiag_uncontr == 1 ~ 0,
                                          htn_diag_untreat == 1 ~ 0,
                                          htn_treat_uncontr == 1 ~ 1,
                                          htn_treat_contr == 1 ~ 1,
                                          TRUE ~ 0
         ),
         htn_controlled_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                             htn_free == 1 ~ NA_real_,
                                             htn_undiag_uncontr == 1 ~ 0,
                                             htn_diag_untreat == 1 ~ 0,
                                             htn_treat_uncontr == 1 ~ 0,
                                             htn_treat_contr == 1 ~ 1,
                                             TRUE ~ 0
         ))

saveRDS(nfhs5_men,paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS"))

