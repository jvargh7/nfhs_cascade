rm(list=ls());gc();source(".Rprofile")


source("preprocessing/ncp_preprocessing.R")

iapr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iapr7a_women) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

nfhs5_women <- read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),col_select = iapr7a_variables$selected) %>% 
  rename_with(~ iapr7a_variables$new_var[which(iapr7a_variables$selected == .x)], 
              .cols = iapr7a_variables$selected) %>% 
  dplyr::filter(!is.na(age)) 

iair7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iair7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(new_var %in% c("cluster","hhid","linenumber"))

# Get all remaining variables from IAPR processed using ncp_preprocessing
nfhs5_iair <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 15to49 women.RDS")) 



iapr7a_variables_men <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables")%>% 
  rename("selected" = iapr7a_men) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

nfhs5_men <- haven::read_dta(paste0(path_dhs_data,"/IA/IAPR7CDT/IAPR7CFL.dta"),col_select = iapr7a_variables_men$selected) %>% 
  rename_with(~ iapr7a_variables_men$new_var[which(iapr7a_variables_men$selected == .x)], 
              .cols = iapr7a_variables_men$selected) %>% 
  dplyr::filter(!is.na(age)) 

# Choose only ID variables from IAIR
iamr7a_variables <- readxl::read_excel("data/NFHS Cascade Variable List.xlsx",sheet="7a variables") %>% 
  rename("selected" = iamr7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(new_var %in% c("cluster","hhid","linenumber"))

# Get all remaining variables from IAPR processed using ncp_preprocessing
nfhs5_iamr <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 15to49 men.RDS"))



df = bind_rows(nfhs5_women %>% mutate(sex = "Female") %>% dplyr::filter(pregnant %in% c(0,9,NA_real_)),
          nfhs5_men %>% mutate(sex = "Male")) %>% 
  dplyr::filter(age>=18) %>% 
  dplyr::select(age,sex,pregnant,cluster,hhid,linenumber)




a = nfhs5_iair %>% 
  dplyr::filter(age>=18,pregnant %in% c(0,9)) 

b = nfhs5_iamr %>% 
  dplyr::filter(age>=18) 

other_men <- df %>%
  dplyr::filter(sex == "Male") %>% 
  anti_join(b %>% 
              dplyr::select(cluster,hhid,linenumber),
            by = c("cluster","hhid","linenumber"))

other_women <- df %>%
  dplyr::filter(sex == "Female") %>% 
  anti_join(a %>% 
              dplyr::select(cluster,hhid,linenumber),
            by = c("cluster","hhid","linenumber"))

c = other_women %>% 
  dplyr::filter(age>=18,pregnant %in% c(0,9,NA_real_))

d = other_men %>% 
  dplyr::filter(age>=18) 

nrow(a) + nrow(b) + nrow(c) + nrow(d)


available_women <- df %>%
  dplyr::filter(sex == "Female") %>% 
  inner_join(a %>% 
              dplyr::select(cluster,hhid,linenumber),
            by = c("cluster","hhid","linenumber"))

available_men <- df %>%
  dplyr::filter(sex == "Male") %>% 
  inner_join(b %>% 
               dplyr::select(cluster,hhid,linenumber),
             by = c("cluster","hhid","linenumber"))

nrow(available_women) + nrow(available_men) + nrow(c) + nrow(d)
