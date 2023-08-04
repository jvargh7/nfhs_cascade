require(haven)
require(tidyverse)
require(survey)
require(srvyr)
require(lubridate)


path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
path_cascade_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS Diabetes Cascade"
path_cascade_repo <- "C:/code/external/nfhs_cascade"
path_india_shapefiles <- "C:/Cloud/OneDrive - Emory University/data/India Shapefiles/"
v024_nfhs5_14states <- c(12,4,22,6,20,23,7,21,34,3,8,33,9,5)

as_formula = function(...){as.formula(...)}


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

female_covariates <- " ~ ns(f_age,df=4)"
male_covariates <- " ~ ns(m_age,df=4)"

fasting_time <- 7.9

bmi_max = 6000

bmi_cutoff <- c(1850, 2500, 3000)
bmiasian_cutoff <- c(1850,2300,2750)

# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9

fpg_cutoff <- 126
rpg_cutoff <- 220
# Alternative cutoff for RPG --> used in ncp_preprocessing2.R
rpg_cutoff2 <- 200
# Alternative cutoff for RPG --> used in ncp_preprocessing3.R
rpg_cutoff3 <- 160


sbp_cutoff <- 140
dbp_cutoff <- 90

fpg_target <- 126
rpg_target <- 180 #Indian DM guidelines
# Indian HTN guidelines (Shah 2020: 130/80 for <= 60y, 140/90 otherwise)
# ICMR 2016 guidelines
# 4.4.1 The current target for control of BP for patients under 80 years of age should
# be systolic blood pressure less than 140 mm and diastolic blood pressure less than 90 mm.
# 4.4.2 The current target for control of BP for patients 80 years or older should be
# less than 150 mm systolic and less than 90 mm diastolic.

sbp_target <- c(140,150) 
agebp_cutoff <- 80
dbp_target <- c(90,90)

fpgpre_cutoff <- 100
rpgpre_cutoff <- 140
sbppre_cutoff <- 130
dbppre_cutoff <- 85

gc()

# Need to check sdist since it results in missing values for some districts
sdist <- readr::read_csv("C:/code/external/nfhs5_on_map2016/data/psu_on_mapnfhs5.csv")
district_matching <- c("psu" = "DHSCLUST","district" = "DHSREGCO")
