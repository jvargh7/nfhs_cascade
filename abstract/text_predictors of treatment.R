
source("preprocessing/ncpre14_nfhs5 youth diagnosed svydesign.R")
pop_age <- read_csv("abstract/population for age standardization youth.csv") %>% 
  dplyr::select(n) %>% 
  pull()

nfhs5diag_svystdz <- svystandardize(nfhs5ydiag_svydesign,by=~age_category_youth,over = ~caste + religion + wealthq_ur,
                                population = pop_age)
source("C:/code/external/functions/survey/svysummary.R")

n5d_sy <- svysummary(nfhs5diag_svystdz,
                    # c_vars = continuous_vars,
                    p_vars = c("dm_treated","dm_controlled"),
                    # g_vars = grouped_vars,
                    id_vars = "sex"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

library(survey)

m1 <- svyglm(dm_treated ~ sex + hscompletion + residence + factor(swealthq_ur) + age_category_youth + anyhistory + factor(state),
             design=nfhs5ydiag_svydesign,
             family=poisson())
broom::tidy(m1) %>% 
  mutate(coef = exp(estimate),
         lci = exp(estimate - 1.96*std.error),
         uci = exp(estimate + 1.96*std.error)) %>% 
  mutate(coef_ci = paste0(round(coef,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
  write_csv("abstract/text_predictors of treatment.csv")
