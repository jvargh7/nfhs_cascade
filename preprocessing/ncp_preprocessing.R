ncp_preprocessing <- function(df, sex = "Female"){
  
  df %>% 
    mutate(sampleweight = sampleweight/(10^6),
           day_interview = case_when(is.na(day_interview) | day_interview == 98 ~ 15,
                             month_interview == 2 & year_interview %in% c(2008,2012,2016,2020) & day_interview > 29 ~ 29,
                             month_interview == 2 & day_interview > 28 ~ 28,
                             month_interview %in% c(4,6,9,11) & day_interview > 30 ~ 30,
                             TRUE ~ as.numeric(day_interview))) %>% 
    mutate(interview = as_date(paste0(year_interview,"-",month_interview,"-",day_interview)),
           phase = case_when(interview <= "2020-03-23" ~ 1,
                             interview > "2020-03-23" ~ 2,
                             TRUE ~ NA_real_)
    )  %>% 
    mutate_at(vars(sbp1,dbp1,
                   sbp2,dbp2,
                   sbp3,dbp3),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                    TRUE ~ as.numeric(x))) %>% 
    
    mutate(screened_dm = case_when(screened_dm == 1 ~ 1,
                                   TRUE ~ 0),
           diagnosed_dm = case_when(diagnosed_dm == 1 ~ 1,
                                    TRUE ~ 0),
           medication_dm = case_when(medication_dm == 1 ~ 1,
                                     TRUE ~ 0),
           
           
           screened_bp = case_when(screened_bp == 1 ~ 1,
                                   TRUE ~ 0),
           diagnosed_bp = case_when(diagnosed_bp == 1 ~ 1,
                                    TRUE ~ 0),
           medication_bp = case_when(medication_bp == 1 ~ 1,
                                     TRUE ~ 0)) %>% 
    
    mutate(bmi_underweight = case_when(bmi > bmi_max ~ NA_real_,
                                       bmi < bmi_cutoff[1] ~ 1,
                                       bmi >= bmi_cutoff[1] ~ 0,
                                       TRUE ~ NA_real_),
           
           
           bmi_overweight = case_when(bmi > bmi_max ~ NA_real_,
                                      bmi >= bmi_cutoff[2] & bmi < bmi_cutoff[3] ~ 1,
                                      bmi < bmi_cutoff[2] | bmi >= bmi_cutoff[3] ~ 0,
                                      TRUE ~ NA_real_),
           
           
           bmi_obese = case_when(bmi > bmi_max ~ NA_real_,
                                 bmi >= bmi_cutoff[3] ~ 1,
                                 bmi < bmi_cutoff[3] ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    mutate(fasting = case_when(lastate > 94 | lastdrank > 94 ~ NA_real_,
                               lastate > fasting_time & lastdrank > fasting_time ~ 1,
                               lastate <=fasting_time | lastdrank <= fasting_time ~ 0,
                               TRUE ~ NA_real_),
           
           dm = case_when(diagnosed_dm == 1 ~ 1,
                          is.na(glucose) | glucose > 498 ~ NA_real_,
                          fasting == 1 & glucose >= fpg_cutoff ~ 1,
                          fasting == 0 & glucose >= rpg_cutoff ~ 1,
                          is.na(fasting) & glucose >= rpg_cutoff ~ 1,
                          fasting == 1 & glucose < fpg_cutoff ~ 0,
                          fasting == 0 & glucose < rpg_cutoff ~ 0,
                          is.na(fasting) & glucose < rpg_cutoff ~ 0,
                          TRUE  ~ NA_real_),
           highglucose = case_when(
             is.na(glucose) | glucose > 498 ~ NA_real_,
             fasting == 1 & glucose >= fpg_cutoff ~ 1,
             fasting == 0 & glucose >= rpg_cutoff ~ 1,
             is.na(fasting) & glucose >= rpg_cutoff ~ 1,
             fasting == 1 & glucose < fpg_cutoff ~ 0,
             fasting == 0 & glucose < rpg_cutoff ~ 0,
             is.na(fasting) & glucose < rpg_cutoff ~ 0,
             TRUE  ~ NA_real_),
           
           # Among those diagnosed, indicator of diabetes status
           diagdm = case_when(
             diagnosed_dm == 0 ~ NA_real_,
             is.na(glucose) | glucose > 498 ~ NA_real_,
             diagnosed_dm == 1 & fasting == 1 & glucose >= fpg_cutoff ~ 1,
             diagnosed_dm == 1 & fasting == 0 & glucose >= rpg_cutoff ~ 1,
             diagnosed_dm == 1 & is.na(fasting) & glucose >= rpg_cutoff ~ 1,
             diagnosed_dm == 1 & fasting == 1 & glucose < fpg_cutoff ~ 0,
             diagnosed_dm == 1 & fasting == 0 & glucose < rpg_cutoff ~ 0,
             diagnosed_dm == 1 & is.na(fasting) & glucose < rpg_cutoff ~ 0,
             TRUE  ~ NA_real_
           )
           
           
    ) %>% 
    mutate(sbp = rowMeans(.[,c("sbp1","sbp2","sbp3")],na.rm=TRUE),
           
           # "sb18d" has 108 everywhere
           dbp = rowMeans(.[,c("dbp1","dbp2","dbp3")],na.rm=TRUE),
           htn = case_when(diagnosed_bp == 1 ~ 1,
                           is.na(sbp) | is.na(dbp) ~ NA_real_,
                           sbp >= sbp_cutoff ~ 1,
                           dbp >= dbp_cutoff ~ 1,
                           sbp < sbp_cutoff ~ 0,
                           dbp < dbp_cutoff ~ 0,
                           TRUE ~ NA_real_),
           highbp = case_when(
             is.na(sbp) | is.na(dbp) ~ NA_real_,
             sbp >= sbp_cutoff ~ 1,
             dbp >= dbp_cutoff ~ 1,
             sbp < sbp_cutoff ~ 0,
             dbp < dbp_cutoff ~ 0,
             TRUE ~ NA_real_),
           diaghtn = case_when(
             diagnosed_bp == 0 ~ NA_real_,
             is.na(sbp) | is.na(dbp) ~ NA_real_,
             diagnosed_bp == 1 & sbp >= sbp_cutoff ~ 1,
             diagnosed_bp == 1 & dbp >= dbp_cutoff ~ 1,
             diagnosed_bp == 1 & sbp < sbp_cutoff ~ 0,
             diagnosed_bp == 1 & dbp < dbp_cutoff ~ 0,
             TRUE ~ NA_real_),
    ) %>% 
    
    # Diabetes cascade -----
  # From cp01_creating couples data.R --------
  mutate(
         # Diagnosis: No/DK, Blood sugar: in range
         dm_free = case_when(
           is.na(dm) ~ NA_real_,
           dm == 1 ~ 0,
           dm == 0 ~ 1,
           TRUE ~ NA_real_),
         dm_unscreened  = case_when(dm == 0 ~ NA_real_,
                                    screened_dm == 1 ~ 0,
                                    screened_dm == 0 ~ 1,
                                    TRUE ~ NA_real_),
         
         # Diagnosis: No/DK + Blood sugar: diabetes
         dm_screened_undiag = case_when(screened_dm == 0 | is.na(screened_dm) ~ NA_real_,
                                        # or equivalently screened_dm == 0
                                        diagnosed_dm == 1 ~ 0,
                                        diagnosed_dm == 0 ~ 1,
                                        TRUE ~ NA_real_),
         
         dm_undiag_uncontr = case_when(diagnosed_dm == 1 | is.na(diagnosed_dm) ~ NA_real_,
                                       # or equivalently diagnosed_dm == 0
                                       dm == 1 ~ 1,
                                       dm == 0 ~ 0,
                                       TRUE ~ NA_real_),
         # Diagnosis: Yes + Treated: No, Blood sugar: <NA>
         dm_diag_untreat = case_when(diagnosed_dm == 1 & medication_dm == 1 ~ 0,
                                     diagnosed_dm == 1 & medication_dm == 0 ~ 1,
                                     TRUE ~ NA_real_),
         
         # Dignosis: Yes, Treated: Yes, Blood sugar: out of range
         dm_treat_uncontr = case_when(medication_dm == 0 | is.na(medication_dm)  ~ NA_real_,
                                      medication_dm == 1 & diagdm == 1 ~ 1,
                                      medication_dm == 1 & diagdm == 0 ~ 0,
                                      TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood sugar: in range
         dm_treat_contr = 1 - dm_treat_uncontr,
         
         # Dignosis: Yes, Treated: Yes or No, Blood sugar: out of range
         dm_diag_uncontr = case_when(diagnosed_dm == 0 | is.na(diagnosed_dm)  ~ NA_real_,
                                     diagnosed_dm == 1 & highglucose == 1 ~ 1,
                                     diagnosed_dm == 1 & highglucose == 0 ~ 0,
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
                                     screened_bp == 1 ~ 0,
                                     screened_bp == 0 ~ 1,
                                     TRUE ~ NA_real_),
         
         # Diagnosis: No/DK + Blood pressure: hypertension
         htn_screened_undiag = case_when(screened_bp == 0 | is.na(screened_bp) ~ NA_real_,
                                         diagnosed_bp == 1 ~ 0,
                                         diagnosed_bp == 0 ~ 1,
                                         TRUE ~ NA_real_),
         
         htn_undiag_uncontr = case_when(diagnosed_bp == 1 | is.na(diagnosed_bp) ~ NA_real_,
                                        htn == 1 ~ 1,
                                        htn == 0 ~ 0,
                                        TRUE ~ NA_real_),
         
         # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
         htn_diag_untreat = case_when(diagnosed_bp == 1 & medication_bp == 1 ~ 0,
                                      diagnosed_bp == 1 & medication_bp == 0 ~ 1,
                                      TRUE ~ NA_real_),
         
         # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
         htn_treat_uncontr = case_when(medication_bp == 0 | is.na(medication_bp)  ~ NA_real_,
                                       medication_bp == 1 & diaghtn == 1 ~ 1,
                                       medication_bp == 1 & diaghtn == 0 ~ 0,
                                       TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_treat_contr = 1 - htn_treat_uncontr,
         
         # Dignosis: Yes, Treated: Yes, Blood pressure: out of range
         htn_diag_uncontr = case_when(diagnosed_bp == 0 | is.na(diagnosed_bp)  ~ NA_real_,
                                      diagnosed_bp == 1 &  highbp == 1 ~ 1,
                                      diagnosed_bp == 1 &  highbp == 0 ~ 0,
                                      TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_diag_contr = 1 - htn_diag_uncontr
         
  ) %>%
    
    # Prediabetes and Prehypertension ------
  mutate(prediabetes = case_when(diagnosed_dm == 1 ~ NA_real_,
                                 is.na(glucose) | glucose > 498 ~ NA_real_,
                                 dm == 1 ~ 0,
                                 fasting == 1 & glucose >= fpgpre_cutoff & glucose < fpg_cutoff ~ 1,
                                 fasting == 0 & glucose >= rpgpre_cutoff & glucose < rpg_cutoff~ 1,
                                 is.na(fasting) & glucose >= rpgpre_cutoff & glucose < rpg_cutoff ~ 1,
                                 fasting == 1 & glucose < fpgpre_cutoff ~ 0,
                                 fasting == 0 & glucose < rpgpre_cutoff ~ 0,
                                 is.na(fasting) & glucose < rpgpre_cutoff ~ 0,
                                 TRUE  ~ NA_real_),
         prehypertension = case_when(diagnosed_bp == 1 ~ NA_real_,
                                     is.na(sbp) | is.na(dbp) ~ NA_real_,
                                     htn == 1 ~ 0,
                                     sbp >= sbppre_cutoff & sbp < sbp_cutoff ~ 1,
                                     dbp >= dbppre_cutoff & dbp < dbp_cutoff~ 1,
                                     sbp < sbppre_cutoff ~ 0,
                                     dbp < dbppre_cutoff ~ 0,
                                     TRUE ~ NA_real_)
  ) %>% 
    # BMI
    mutate_at(vars(bmi),function(x) case_when(x > 6000 ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Circumferences
    mutate_at(vars(waistcircumference,hipcircumference),function(x) case_when(x > 240 ~ NA_real_,
                                                                              TRUE ~ as.numeric(x))) %>% 
    
   
    # Glucose
    mutate_at(vars(glucose), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
                                                   TRUE ~ as.numeric(x))) %>% 
    # Caste
    mutate_at(vars(caste),function(x) case_when(x == 1 ~ "Schedule Caste",
                                                x == 2 ~ "Schedule Tribe",
                                                x == 3 ~ "OBC",
                                                x == 4 ~ "General",
                                                x == 8 ~ "General",
                                                TRUE ~ "General")) %>% 
    # Education
    mutate_at(vars(education),function(x) case_when(x == 0 ~ "No education",
                                                    x == 1 ~ "Primary",
                                                    x == 2 ~ "Secondary",
                                                    x == 3 ~ "Higher",
                                                    x == 9 ~ "No education",
                                                    TRUE ~ "No education")) %>% 
    # Religion
    mutate_at(vars(religion),function(x) case_when(x == 1 ~ "Hindu",
                                                      x == 2 ~ "Muslim",
                                                      TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(
      alcohol,insurance), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokecurr), function(x) case_when(x == 1 ~ 1,
                                                     x == 0 ~ 0,
                                                     TRUE ~ NA_real_)) %>% 
    
    mutate(smokecount = case_when(smokecount > 80 ~ NA_real_,
                                  TRUE ~ as.numeric(smokecount))) %>% 
    
    mutate(
      eduyr = case_when(education == "No education" ~ 0,
                        TRUE ~ as.numeric(eduyr))
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
           screened_dm == 1 ~ 1,
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
                                   dm_diag_uncontr == 1 ~ 0,
                                   dm_diag_contr == 1 ~ 1,
                                   TRUE ~ 0
         ),
         dm_screened_in_dis = case_when(
           is.na(dm_free) ~ NA_real_,
           dm_free == 1 ~ NA_real_,
           screened_dm == 1 ~ 1,
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
                                          dm_diag_uncontr == 1 ~ 0,
                                          dm_diag_contr == 1 ~ 1,
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
             screened_bp == 1 ~ 1,
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
                                      htn_diag_uncontr == 1 ~ 0,
                                      htn_diag_contr == 1 ~ 1,
                                      TRUE ~ 0
           ),
           htn_screened_in_dis = case_when(
             is.na(htn_free) ~ NA_real_,
             htn_free == 1 ~ NA_real_,
             screened_bp == 1 ~ 1,
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
                                             htn_diag_uncontr == 1 ~ 0,
                                             htn_diag_contr == 1 ~ 1,
                                             TRUE ~ 0
           )) %>% 
    
    mutate(bmi_category = case_when(bmi > bmi_max ~ NA_real_,
                                    bmi >= bmi_cutoff[3] ~ 4,
                                    bmi >= bmi_cutoff[2] ~ 3,
                                    bmi >= bmi_cutoff[1] ~ 2,
                                    bmi < bmi_cutoff[1] ~ 1,
                                    TRUE ~ NA_real_),
           
           highwc = case_when(sex == "Female" & waistcircumference >= female_wc_cutoff ~ 1,
                              sex == "Female" & waistcircumference < female_wc_cutoff ~ 0,
                              sex == "Male" & waistcircumference >= male_wc_cutoff ~ 1,
                              sex == "Male" & waistcircumference < male_wc_cutoff ~ 0,
                              TRUE ~ NA_real_
                              ),
           waist_hip = case_when(!is.na(hipcircumference) ~ waistcircumference/hipcircumference,
                                 TRUE ~ NA_real_)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese")),
           highwhr = case_when(sex == "Female" & waist_hip >= female_whr_cutoff ~ 1,
                               sex == "Female" & waist_hip < female_whr_cutoff ~ 0,
                               sex == "Male" & waist_hip >= male_whr_cutoff ~ 1,
                               sex == "Male" & waist_hip < male_whr_cutoff ~ 0,
                               TRUE ~ NA_real_),
           age_category = case_when(age %in% c(18:39) ~ 1,
                                    age %in% c(40:64) ~ 2,
                                    age >= 65 ~ 3,
                                    TRUE ~ NA_real_),
           dm_at_risk = case_when(age_category %in% c(2,3) & 
                                    # bmi_category %in% c("Overweight","Obese") &
                                    htn_disease == 1 ~ 1,
                                  TRUE ~ 0)) %>% 
    mutate(age_category = factor(age_category,levels=c(1:3),labels=c("18-39","40-64","65 plus")),
           
           # State wealth quintile - urban/rural
           swealthq_ur = case_when(!is.na(suwealthq) ~ suwealthq,
                                   TRUE ~ srwealthq),
           # State wealth factor score - urban/rural
           swealths_ur = case_when(!is.na(suwealths) ~ suwealths,
                                   TRUE ~ srwealths)
           ) %>% 
    
    return(.)
}

