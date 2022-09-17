require(haven)
require(tidyverse)
require(lubridate)

source("preprocessing/ncp_preprocessing.R")

iair7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iair7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(new_var %in% c("cluster","hhid","linenumber"))

nfhs5_iair <- read_dta(paste0(path_dhs_data,"/IA/IAIR7CDT/IAIR7CFL.dta"),col_select = iair7a_variables$selected) %>% 
  rename_with(~ iair7a_variables$new_var[which(iair7a_variables$selected == .x)], 
              .cols = iair7a_variables$selected) %>% 
  left_join(readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")),
            by = c("cluster","hhid","linenumber")) %>% 
  dplyr::filter(!is.na(age))


saveRDS(nfhs5_iair,paste0(path_cascade_folder,"/working/nfhs5 iair_women.RDS"))
