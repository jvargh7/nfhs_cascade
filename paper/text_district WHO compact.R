district_met <- read_csv("analysis/nca08_district met need care cascade.csv",guess_max = 4000) %>% 
  dplyr::filter(is.na(strata))

district_met %>% 
  mutate(n_category = case_when(n >= 50 ~ 1,
                                n %in% c(25:49) ~ 2,
                                n < 25 ~ 3)) %>% 
  mutate(n_category = factor(n_category,labels=c(">=50","25-49","<25"))) %>% 
  group_by(variable,n_category) %>% 
  tally() %>% 
  ggplot(data=.,aes(x = variable,y=n,group=n_category,fill=n_category,label=n)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_text(position = position_dodge(width=0.9)) +
  theme_bw() +
  xlab("cascade level")



district_met %>% 
  dplyr::filter(is.na(stratification), n >= 50) %>%
  group_by(variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            n = n()) 
  
district_met %>% 
  dplyr::filter(is.na(stratification)) %>%
  group_by(variable) %>% 
  summarize(count = sum(estimate > 80),
            prop = sum(estimate > 80)/n(),
            n = n()) 

# Between district variation --------

require(lme4)
m_diag = district_met %>% 
  dplyr::filter(variable == "dm_diagnosed") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_diag = m_diag %>% 
  performance::icc(.)

1- icc_diag$ICC_adjusted

m_treat = district_met %>% 
  dplyr::filter(variable == "dm_treated") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_treat = m_treat %>% 
  performance::icc(.)

1-icc_treat

m_contr = district_met %>% 
  dplyr::filter(variable == "dm_controlled") %>% 
  lmer(estimate ~ 1 + (1|n5_state),data=.) 

icc_contr = m_contr %>% 
  performance::icc(.)
1-icc_contr


246/707
76/707


232/639
58/538
