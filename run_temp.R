
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200a03_state level care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200a05_state unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200a09_national unmet need care cascade.R")

# AGE STANDARDIZED: National --------

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z01_age standardized national care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z02_age standardized state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z03_age standardized national unmet care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z04_age standardized state unmet care cascade.R")

# CRUDE: District -------
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200a04_district2018 level care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200a08_district unmet need care cascade.R")


# AGE STANDARDIZED: District --------

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z06_age standardized district cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff200/ncc200z05_age standardized district unmet care cascade.R")

