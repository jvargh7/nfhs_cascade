

physiologic_sbp_ranges <- c(60:300)
physiologic_dbp_ranges <- c(30:300)

bp_processing <- function(df){
  
  df %>% 
    mutate_at(vars(sbp1,dbp1,
                    sbp2,dbp2,
                    sbp3,dbp3),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                     TRUE ~ as.numeric(x))) %>% 
    mutate_at(vars(sbp1,sbp2,sbp3),
              function(x) case_when(x %in% physiologic_sbp_ranges ~ x,
                                    TRUE ~ NA_real_)) %>% 
    mutate_at(vars(dbp1,dbp2,dbp3),
              function(x) case_when(x %in% physiologic_dbp_ranges ~ x,
                                    TRUE ~ NA_real_)) %>% 
    # Assuming it's information bias from data entry -- about 3600 cases in total have dbp > sbp
    mutate(sbp1 = case_when(!is.na(sbp1) ~ pmax(sbp1,dbp1,na.rm = TRUE),
                            TRUE ~ sbp1),
           dbp1 = case_when(!is.na(dbp1) ~ pmin(sbp1,dbp1,na.rm = TRUE),
                            TRUE ~ dbp1),
           sbp2 = case_when(!is.na(sbp2) ~ pmax(sbp2,dbp2,na.rm = TRUE),
                            TRUE ~ sbp2),
           dbp2 = case_when(!is.na(dbp2) ~ pmin(sbp2,dbp2,na.rm = TRUE),
                            TRUE ~ dbp2),
           sbp3 = case_when(!is.na(sbp3) ~ pmax(sbp3,dbp3,na.rm = TRUE),
                            TRUE ~ sbp3),
           dbp3 = case_when(!is.na(dbp3) ~ pmin(sbp3,dbp3,na.rm = TRUE),
                            TRUE ~ dbp3)
    ) %>% 
    return(.)
    
  
}

