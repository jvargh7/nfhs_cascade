
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a02_national level care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a03_state level care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a05_state unmet need care cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a09_national unmet need care cascade.R")

# AGE STANDARDIZED: National --------

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z01_age standardized national care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z02_age standardized state cascade.R")

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z03_age standardized national unmet care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z04_age standardized state unmet care cascade.R")

# CRUDE: District -------
rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a04_district2018 level care cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160a08_district unmet need care cascade.R")


# AGE STANDARDIZED: District --------

rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z06_age standardized district cascade.R")


rm(list=ls()); gc(); source(".Rprofile")
source("cutoff160/ncc160z05_age standardized district unmet care cascade.R")

