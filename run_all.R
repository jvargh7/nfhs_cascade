# ANALYSIS: national and state ------

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca02_national level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca03_state level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca05_state unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca06_population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca07_analytic sample characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca09_national unmet need care cascade.R")


# AGE STANDARDIZED: national and state --------
rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz01_age standardized national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz03_age standardized national unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz02_age standardized state cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz04_age standardized state unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz07_age standardized missing caste stratified.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz08_age standardized national conditional cascade.R")

# PAPER -------
rm(list=ls()); gc(); source(".Rprofile")
source("paper/abstract_national cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_heatmap cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_maps state care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_column plot cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("paper/figure_national care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_analytic sample characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_population characteristics.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_sociodemographic population care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_analytic sample.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/text_state WHO compact.R")

rm(list=ls()); gc(); source(".Rprofile")
source("paper/table_conditional care cascade.R")

# ANALYSIS: district ------

rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca04_district2018 level care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("analysis/nca08_district unmet need care cascade.R")

# AGE STANDARDIZED: district --------

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz05_age standardized district unmet care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("age_standardized/ncz06_age standardized district cascade.R")


# SHINY --------

rm(list=ls()); gc(); source(".Rprofile")
source("diabetes_cascade/code/setup_code.R")






























