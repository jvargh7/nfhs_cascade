source("preprocessing/ncpre03_nfhs5 svydesign.R")
source("functions/summary_to_long.R")

# require(splines)
# disease <- svyglm(as.formula(paste0("f_dm_disease",female_covariates)),design = women_svydesign,family = poisson())
# diagnosed <- svyglm(as.formula(paste0("f_dm_diagnosed",female_covariates)),design = women_svydesign,family = poisson())
# treated <- svyglm(as.formula(paste0("f_dm_treated",female_covariates)),design = women_svydesign,family = poisson())
# controlled <- svyglm(as.formula(paste0("f_dm_controlled",female_covariates)),design = women_svydesign,family = poisson())


f_summary_wide <- women_svydesign %>% 
  group_by(hv025,phase) %>% 
  summarize_at(vars(f_dm_disease,f_dm_diagnosed,
                    f_dm_treated,f_dm_controlled,
                    f_htn_disease,f_htn_diagnosed,
                    f_htn_treated,f_htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))

m_summary_wide <- men_svydesign %>% 
  group_by(hv025,phase) %>% 
  summarize_at(vars(m_dm_disease,m_dm_diagnosed,
                    m_dm_treated,m_dm_controlled,
                    m_htn_disease,m_htn_diagnosed,
                    m_htn_treated,m_htn_controlled),
               .funs = ~tryCatch({survey_mean(.,vartype="ci",na.rm=TRUE)},
                                 error=function(e){NA}))


f_prefix_list <- paste0("f_",rep(c("dm_","htn_"),each=4),rep(c("disease","diagnosed","treated","controlled"),times=2))
m_prefix_list <- paste0("m_",rep(c("dm_","htn_"),each=4),rep(c("disease","diagnosed","treated","controlled"),times=2))
summary_long <- bind_rows(
  map_dfr(f_prefix_list,
          ~summary_to_long(f_summary_wide,
                           prefix=.,
                           id_cols = c("hv025","phase"))),
  map_dfr(m_prefix_list,
          ~summary_to_long(m_summary_wide,
                           prefix=.,
                           id_cols = c("hv025","phase"))))

write_csv(summary_long,"analysis/cascade summary.csv")


