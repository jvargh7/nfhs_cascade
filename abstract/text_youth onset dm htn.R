
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre13_nfhs5 youth.R")

pop_age <- read_csv("abstract/population for age standardization youth.csv") %>% 
  dplyr::select(n) %>% 
  pull()

proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


nfhs5_svystdz <- svystandardize(nfhs5y_svydesign,by=~age_category_youth,over = ~caste + religion + wealthq_ur,
                                population = pop_age)
rm(nfhs5y_svydesign);gc();

n5_sy <- svysummary(nfhs5_svystdz,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    # id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

n5_sy %>% 
  write_csv(.,file = "abstract/text_youth onset dm htn.csv")
