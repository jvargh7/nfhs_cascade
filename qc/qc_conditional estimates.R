
source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre03_nfhs5 total svydesign.R")
source("preprocessing/ncpre05_nfhs5 diagnosed svydesign.R")

proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled")

# Stratified
n5_sy_stratified <- svysummary(nfhs5_svydesign,
                    # c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    # g_vars = grouped_vars,
                    id_vars = "dm_diagnosed"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

n5_sy_restricted <- svysummary(nfhs5dmdiag_svydesign ,
                               # c_vars = continuous_vars,
                               p_vars = proportion_vars,
                               # g_vars = grouped_vars,
                               id_vars = "dm_diagnosed"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


bind_rows(n5_sy_stratified %>% mutate(type = "Stratified"),
          n5_sy_restricted %>% mutate(type = "Restricted")) %>% 
  write_csv(.,"qc/qc_restricted vs stratified care cascade.csv")
