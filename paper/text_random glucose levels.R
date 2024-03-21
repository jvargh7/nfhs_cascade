source("C:/code/external/functions/survey/svysummary.R")

source("preprocessing/ncpre03_nfhs5 total svydesign.R")

rm(nfhs5_svydesign)


df = nfhs5_df %>% 
  mutate(glucose_levels = case_when(glucose %in% c(20:349) ~ "20 to 349 mg/dL",
                                    glucose %in% c(350:499) ~ "350 to 499 mg/dL",
                                    TRUE ~ NA_character_))



with(df[df$sex == "Female",],table(glucose_levels)) %>% prop.table()
with(df[df$sex=="Female",],summary(glucose))
