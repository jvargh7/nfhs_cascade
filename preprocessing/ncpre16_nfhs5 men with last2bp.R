rm(list=ls());gc();source(".Rprofile")


iapr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables")%>% 
  rename("selected" = iapr7a_men) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

source("preprocessing/ncp_preprocessing2.R")

nfhs5_men <- haven::read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),col_select = iapr7a_variables$selected) %>% 
  rename_with(~ iapr7a_variables$new_var[which(iapr7a_variables$selected == .x)], 
              .cols = iapr7a_variables$selected) %>%
  dplyr::filter(!is.na(age),age >=18) %>% 
  ncp_preprocessing2(.,sex = "Male")

saveRDS(nfhs5_men,paste0(path_cascade_folder,"/working/nfhs5 iapr_men last2bp.RDS"))