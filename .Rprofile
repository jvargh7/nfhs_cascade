require(tidyverse)
require(survey)
require(srvyr)


path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
path_cascade_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS Cascade"
path_cascade_repo <- "C:/code/external/nfhs_cascade"

v024_nfhs5_14states <- c(12,4,22,6,20,23,7,21,34,3,8,33,9,5)

as_formula = function(...){as.formula(...)}


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

female_covariates <- " ~ ns(f_age,df=4)"
male_covariates <- " ~ ns(m_age,df=4)"
