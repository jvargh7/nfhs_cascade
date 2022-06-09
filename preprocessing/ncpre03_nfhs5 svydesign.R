require(srvyr)

women_svydesign <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_women.RDS")) %>%
  dplyr::filter(hv024 %in% v024_nfhs5_14states) %>% 
  as_survey_design(.data = .,
                   ids = hv021,strata = hv024,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

men_svydesign <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iapr_men.RDS")) %>%
  dplyr::filter(hv024 %in% v024_nfhs5_14states) %>% 
  as_survey_design(.data = .,
                   ids = hv021,strata = hv024,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

