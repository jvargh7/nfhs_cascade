rm(list=ls());gc();source(".Rprofile")



# Uses average of last 2 BP measurements
source("preprocessing/ncp_preprocessing3.R")

iapr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iapr7a_women) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))


nfhs5_women <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),col_select = iapr7a_variables$selected) %>% 
  rename_with(~ iapr7a_variables$new_var[which(iapr7a_variables$selected == .x)], 
              .cols = iapr7a_variables$selected) %>%
  dplyr::filter(!is.na(age),age >=18,pregnant %in% c(0,9)) %>% 
  ncp_preprocessing3(.,sex = "Female")


nfhs5_pregnant <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),col_select = iapr7a_variables$selected) %>% 
  rename_with(~ iapr7a_variables$new_var[which(iapr7a_variables$selected == .x)], 
              .cols = iapr7a_variables$selected) %>%
  dplyr::filter(!is.na(age),age >=18,pregnant == 1) %>% 
  ncp_preprocessing3(.,sex = "Female")

saveRDS(nfhs5_women,paste0(path_cascade_folder,"/working/nfhs5 iapr_women pre3.RDS"))
saveRDS(nfhs5_pregnant,paste0(path_cascade_folder,"/working/nfhs5 iapr_pregnant pre3.RDS"))
