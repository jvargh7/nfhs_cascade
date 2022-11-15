require(haven)
require(tidyverse)
require(lubridate)

# Based on nfhs5_couples/preprocessing/n5c01_creating couples data.R

source("preprocessing/qc15to49_preprocessing.R")
iapr_id_vars <- c("cluster","hhid","linenumber")

iamr7c_male_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",
                                            sheet = "7a variables") %>% 
  dplyr::filter(!is.na(iamr7a)) 

male_pr_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",
                                        sheet = "7a variables") %>% 
  dplyr::filter((is.na(iamr7a) & !is.na(iapr7a_men))|new_var %in% c(iapr_id_vars,"age")) 

# Data ---------

male <- read_dta(paste0(path_dhs_data,"/IA/IAMR7CDT/IAMR7CFL.dta"),
                 col_select = iamr7c_male_variables$iamr7a)  %>% 
  rename_with(~ iamr7c_male_variables$new_var[which(iamr7c_male_variables$iamr7a == .x)], 
              .cols = iamr7c_male_variables$iamr7a)

male_pr <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),
                    col_select = male_pr_variables$iapr7a_men)  %>% 
  rename_with(~ male_pr_variables$new_var[which(male_pr_variables$iapr7a_men == .x)], 
              .cols = male_pr_variables$iapr7a_men)  %>% 
  dplyr::filter(!is.na(age))

# Preprocessing --------

male_processed <- male %>% 
  mutate(sex = "Male") %>% 
  left_join(male_pr %>% 
              dplyr::select(-age),
            by=iapr_id_vars)  %>% 
  qc15to49_preprocessing(.)


saveRDS(male_processed,paste0(path_cascade_folder,"/working/nfhs5 15to49 men.RDS"))

# IAMR7A ---------------
proportion_vars <- c("dm_screened","dm_disease","dm_diagnosed","dm_treated","dm_controlled",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")

# Choose only ID variables from IAIR
iamr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iamr7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(new_var %in% c("cluster","hhid","linenumber"))

# Get all remaining variables from IAPR processed using ncp_preprocessing
nfhs5_iamr <- read_dta(paste0(path_dhs_data,"/IA/IAMR7CDT/IAMR7CFL.dta"),col_select = iamr7a_variables$selected) %>% 
  rename_with(~ iamr7a_variables$new_var[which(iamr7a_variables$selected == .x)], 
              .cols = iamr7a_variables$selected) %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")),
            by = c("cluster","hhid","linenumber")) %>% 
  dplyr::filter(!is.na(age)) %>% 
  
  # Join with variables created using qc15to49_preprocessing
  left_join(readRDS(paste0(path_cascade_folder,"/working/nfhs5 15to49 men.RDS")) %>% 
              dplyr::select(cluster,hhid,linenumber,one_of(proportion_vars),contains("current"),contains("soughttx")) %>% 
              rename_at(vars(one_of(proportion_vars)),~paste0("qc",.)),
            by = c("cluster","hhid","linenumber"))


saveRDS(nfhs5_iamr,paste0(path_cascade_folder,"/working/nfhs5 iamr_men.RDS"))
