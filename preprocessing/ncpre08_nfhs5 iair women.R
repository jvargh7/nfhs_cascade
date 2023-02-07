require(haven)
require(tidyverse)
require(lubridate)


# Based on nfhs5_couples/preprocessing/n5c01_creating couples data.R

source("preprocessing/qc15to49_preprocessing.R")

# Variables ------
iapr_id_vars <- c("cluster","hhid","linenumber")

iair7c_female_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",
                                              sheet = "7a variables") %>% 
  dplyr::filter(!is.na(iair7a))

female_pr_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",
                                          sheet = "7a variables") %>% 
  dplyr::filter((is.na(iair7a) & !is.na(iapr7a_women))|new_var %in% c(iapr_id_vars,"age"))

female <- read_dta(paste0(path_dhs_data,"/IA/IAIR7CDT/IAIR7CFL.dta"),
                   col_select = iair7c_female_variables$iair7a)  %>% 
  rename_with(~ iair7c_female_variables$new_var[which(iair7c_female_variables$iair7a == .x)], 
              .cols = iair7c_female_variables$iair7a)  %>% 
  dplyr::filter(!is.na(age))

female_pr <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),
                      col_select = female_pr_variables$iapr7a_women)  %>% 
  rename_with(~ female_pr_variables$new_var[which(female_pr_variables$iapr7a_women == .x)], 
              .cols = female_pr_variables$iapr7a_women) %>% 
  dplyr::filter(!is.na(age))

female_processed <- female %>% 
  mutate(sex = "Female") %>% 
  left_join(female_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars) %>% 
  qc15to49_preprocessing(.)

saveRDS(female_processed,paste0(path_cascade_folder,"/working/nfhs5 15to49 women.RDS"))
# female_processed <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 15to49 women.RDS")) %>% 
#   dplyr::filter(pregnant == 1)



# IAIR7A ---------------
proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")


# Choose only ID variables from IAIR
iair7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iair7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(new_var %in% c("cluster","hhid","linenumber"))

# Get all remaining variables from IAPR processed using ncp_preprocessing
nfhs5_iair <- read_dta(paste0(path_dhs_data,"/IA/IAIR7CDT/IAIR7CFL.dta"),col_select = iair7a_variables$selected) %>% 
  rename_with(~ iair7a_variables$new_var[which(iair7a_variables$selected == .x)], 
              .cols = iair7a_variables$selected) %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")),
            by = c("cluster","hhid","linenumber")) %>% 
  # Excludes all the pregnant?
  dplyr::filter(!is.na(age)) %>% 
  
  # Join with variables created using qc15to49_preprocessing
  left_join(readRDS(paste0(path_cascade_folder,"/working/nfhs5 15to49 women.RDS")) %>% 
              dplyr::select(cluster,hhid,linenumber, one_of(proportion_vars),contains("current"),contains("soughttx")) %>% 
              rename_at(vars(one_of(proportion_vars)),~paste0("qc",.)),
            by = c("cluster","hhid","linenumber"))


saveRDS(nfhs5_iair,paste0(path_cascade_folder,"/working/nfhs5 iair_women.RDS"))


nfhs5_iair_pregnant <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iair_women.RDS")) %>%
  dplyr::filter(pregnant == 1)


