source("preprocessing/ncpre03_nfhs5 svydesign.R")
source("functions/summary_to_long.R")

# require(splines)
# disease <- svyglm(as.formula(paste0("f_dm_disease",female_covariates)),design = women_svydesign,family = poisson())
# diagnosed <- svyglm(as.formula(paste0("f_dm_diagnosed",female_covariates)),design = women_svydesign,family = poisson())
# treated <- svyglm(as.formula(paste0("f_dm_treated",female_covariates)),design = women_svydesign,family = poisson())
# controlled <- svyglm(as.formula(paste0("f_dm_controlled",female_covariates)),design = women_svydesign,family = poisson())
prefix_list <- c("prediabetes","prehypertension",
                 paste0(rep(c("dm_","htn_"),each=5),rep(c("screened","disease","diagnosed","treated","controlled"),times=2)))

# Urbanicity ------------
women_urbanicity <- women_svydesign %>% 
  group_by(hv025) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))

men_urbanicity <- men_svydesign %>% 
  group_by(hv025) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))


urbanicity <- bind_rows(
  map_dfr(prefix_list,
          ~summary_to_long(women_urbanicity,
                           prefix=.,
                           id_cols = c("hv025"))) %>% 
    mutate(sex = "Female"),
  map_dfr(prefix_list,
          ~summary_to_long(men_urbanicity,
                           prefix=.,
                           id_cols = c("hv025"))) %>% 
    mutate(sex = "Male"))

write_csv(urbanicity,"analysis/urbanicity summary.csv")


# Age category ------------
women_age <- women_svydesign %>% 
  mutate(age_category = cut(age,breaks=c(18,39,64,98),include.lowest=TRUE,right=TRUE)) %>% 
  group_by(age_category) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))

men_age <- men_svydesign %>% 
  mutate(age_category = cut(age,breaks=c(18,39,64,98),include.lowest=TRUE,right=TRUE)) %>% 
  group_by(age_category) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))


age_category <- bind_rows(
  map_dfr(prefix_list,
          ~summary_to_long(women_age,
                           prefix=.,
                           id_cols = c("age_category"))) %>% 
    mutate(sex = "Female"),
  map_dfr(prefix_list,
          ~summary_to_long(men_age,
                           prefix=.,
                           id_cols = c("age_category"))) %>% 
    mutate(sex = "Male"))

write_csv(age_category,"analysis/age_category summary.csv")

# For abstract -----
women_svydesign %>% 
  dplyr::filter(age >= 50) %>%  summarize_at(vars(dm_diagnosed_in_dis,htn_diagnosed_in_dis,
                                                  dm_treated_in_dis,htn_treated_in_dis,
                                                  dm_controlled_in_dis,htn_controlled_in_dis),
                                           .funs = ~tryCatch({survey_mean(.,na.rm=TRUE)},error=function(e){NA})) %>% View()
