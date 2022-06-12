source("preprocessing/ncpre04_nfhs5 total svydesign.R")
source("functions/summary_to_long.R")

# require(splines)
# disease <- svyglm(as.formula(paste0("f_dm_disease",female_covariates)),design = women_svydesign,family = poisson())
# diagnosed <- svyglm(as.formula(paste0("f_dm_diagnosed",female_covariates)),design = women_svydesign,family = poisson())
# treated <- svyglm(as.formula(paste0("f_dm_treated",female_covariates)),design = women_svydesign,family = poisson())
# controlled <- svyglm(as.formula(paste0("f_dm_controlled",female_covariates)),design = women_svydesign,family = poisson())
prefix_list <- c("prediabetes","prehypertension",
                 paste0(rep(c("dm_","htn_"),each=5),rep(c("screened","disease","diagnosed","treated","controlled"),times=2)),
                 paste0(rep(c("dm_","htn_"),each=3),rep(c("diagnosed","treated","controlled"),times=2),"_in_dis")
                 )

# Urbanicity ------------
total_urbanicity <- nfhs5_svydesign %>% 
  group_by(hv025) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled,
                    dm_diagnosed_in_dis,dm_treated_in_dis,dm_controlled_in_dis,
                    htn_diagnosed_in_dis,htn_treated_in_dis,htn_controlled_in_dis),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))
map_dfr(paste0(prefix_list,"$"),
          ~summary_to_long(total_urbanicity,
                           prefix=.,
                           id_cols = c("hv025"))) %>% 
write_csv(.,"analysis/total urbanicity summary.csv")


# Age category ------------
total_age <- nfhs5_svydesign %>% 
  mutate(age_category = cut(age,breaks=c(18,39,64,98),include.lowest=TRUE,right=TRUE)) %>% 
  group_by(age_category) %>% 
  summarize_at(vars(dm_screened,prediabetes,
                    dm_disease,dm_diagnosed,
                    dm_treated,dm_controlled,
                    htn_screened,prehypertension,
                    htn_disease,htn_diagnosed,
                    htn_treated,htn_controlled,
                    dm_diagnosed_in_dis,dm_treated_in_dis,dm_controlled_in_dis,
                    htn_diagnosed_in_dis,htn_treated_in_dis,htn_controlled_in_dis),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))

map_dfr(paste0(prefix_list,"$"),
          ~summary_to_long(total_age,
                           prefix=.,
                           id_cols = c("age_category"))) %>% 
write_csv(.,"analysis/total age_category summary.csv")

# For abstract -----
abstract_summary <- nfhs5_svydesign %>% 
 summarize_at(vars(
                   dm_screened,prediabetes,
                   dm_disease,dm_diagnosed,
                   dm_treated,dm_controlled,
                   htn_screened,prehypertension,
                   htn_disease,htn_diagnosed,
                   htn_treated,htn_controlled,
                   dm_diagnosed_in_dis,dm_treated_in_dis,dm_controlled_in_dis,
                   htn_diagnosed_in_dis,htn_treated_in_dis,htn_controlled_in_dis
                   ),
                                             .funs = ~tryCatch({survey_mean(.,vartype = "ci",na.rm=TRUE)},error=function(e){NA}))

map_dfr(paste0(str_replace(colnames(abstract_summary),"(_low|_upp)","") %>% unique(.),"$"),
        ~summary_to_long(abstract_summary,
                         prefix=.,
                         id_cols = NULL) %>% 
          dplyr::select(-sex)) %>% 
  write_csv(.,"analysis/total overall summary.csv")
