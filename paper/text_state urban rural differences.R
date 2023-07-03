unmet_cascade <- bind_rows(read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                           read_csv(file="analysis/nca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Diabetes")
) %>% 
  dplyr::filter(n >= 50) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Unscreened","Undiagnosed","Untreated","Uncontrolled")))


unmet_cascade %>% 
  group_by(residence,variable) %>% 
  summarize(m = paste0(median(estimate)," [",
                       quantile(estimate,0.25),", ",
                       quantile(estimate,0.75),"]"))

unmet_cascade %>% 
  dplyr::select(state,residence,variable,estimate) %>% 
  pivot_wider(names_from=residence,values_from=estimate) %>% 
  mutate(u_gt_r = case_when(Urban > Rural ~ 0,
                            Urban <= Rural ~ 1,
                            TRUE ~ NA_real_)) %>% 
  group_by(variable) %>% 
  summarize(tot = sum(u_gt_r,na.rm=TRUE),
            n = sum(!is.na(u_gt_r)))


unmet_cascade %>% 
  dplyr::select(state,residence,variable,estimate,zone) %>% 
  pivot_wider(names_from=residence,values_from=estimate) %>% 
  mutate(u_min_r = Urban - Rural) %>% 
  group_by(variable,zone) %>% 
  summarize(minus = median(u_min_r,na.rm=TRUE),
            n = sum(!is.na(u_min_r))) %>% 
  View()


# Unmet cascade at state-level: extra ----------
unmet_cascade %>% 
  dplyr::filter(variable == "Unscreened",is.na(stratification)) %>% 
  dplyr::select(state,residence,estimate) %>% 
  # Count of states with unscreened >= 20
  mutate(cutoff = case_when(estimate < 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))

unmet_cascade %>% 
  dplyr::filter(variable == "Undiagnosed",is.na(stratification)) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))


unmet_cascade %>% 
  dplyr::filter(variable == "Untreated",is.na(stratification)) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))


unmet_cascade %>% 
  dplyr::filter(variable == "Uncontrolled",is.na(stratification)) %>% 
  dplyr::select(state,residence,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)))



unmet_cascade %>% 
  dplyr::filter(variable == "Untreated",is.na(stratification)) %>% 
  dplyr::select(state,residence,zone,estimate)  %>% 
  # Count of states with undiagnosed >= 20
  mutate(cutoff = case_when(estimate >= 20 ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(residence,zone) %>% 
  summarize(cutoff = sum(cutoff,na.rm = TRUE),
            total = sum(!is.na(estimate)),
            median = median(estimate)) %>% 
  View()
