
# preprocessing/nlpre05_district covid19 case data.R -------
nfhs5d_map <- readxl::read_excel("C:/code/external/nfhs_lockdown/data/NFHS5 Mapping.xlsx")
nfhs5sdist_map <- readxl::read_excel("C:/code/external/nfhs_lockdown/data/NFHS Lockdown Variable List.xlsx",sheet="mapnfhs5_sdist") %>% 
  dplyr::select(REGCODE,v024) %>% 
  rename(sdist=REGCODE)

nfhs5d_factsheets <- read.csv("https://raw.githubusercontent.com/jvargh7/nfhs5_factsheets/main/data%20for%20analysis/districts.csv",header=TRUE) %>% 
  left_join(nfhs5d_map %>% 
              dplyr::select(nfhs5_factsheet_state,nfhs5_factsheet_district,sdist),
            by=c("state" = "nfhs5_factsheet_state","district"="nfhs5_factsheet_district")) %>% 
  dplyr::filter(str_detect(Indicator,"^(4|7|8|9|10|12|14|15|16)\\.")) %>% 
  mutate(variable = paste0("V",str_extract(Indicator,"^[0-9]+\\.") %>% str_replace(.,"\\.",""))) 

nfhs5u_factsheets <- read.csv("https://raw.githubusercontent.com/jvargh7/nfhs5_factsheets/main/data%20for%20analysis/states.csv",header=TRUE) %>% 
  dplyr::filter(state %in% c("Chandigarh","Lakshadweep"),
                str_detect(Indicator,"^(4|7|8|9|10|12|14|16|20)\\.")
  ) %>% 
  mutate(variable = paste0("V",str_extract(Indicator,"^[0-9]+\\.") %>% str_replace(.,"\\.",""))) %>% 
  mutate(variable = case_when(variable == "V16" ~ "V15",
                              variable == "V20" ~ "V16",
                              TRUE ~ variable),
         sdist = case_when(state == "Chandigarh" ~ 55,
                           state == "Lakshadweep" ~ 587,
                           TRUE ~ NA_real_)) %>% 
  rename(NFHS5 = Total)

factsheets <- bind_rows(nfhs5d_factsheets,
                        nfhs5u_factsheets)%>% 
  dplyr::select(sdist,variable,NFHS5) %>% 
  pivot_wider(names_from=variable,values_from=NFHS5) %>% 
  left_join(nfhs5sdist_map,
            by=c("sdist")) 

# Borrowed from analysis/nlg_district development.R   ------
pca_obj <- factsheets %>% 
  mutate(V4 = case_when(is.na(V4) ~ median(V4,na.rm=TRUE),
                        TRUE ~ V4)) %>% 
  ungroup() %>% 
  dplyr::select(-sdist) %>% 
  prcomp(.,scale. = TRUE)

factsheets$pc1 = pca_obj$x[,1]
