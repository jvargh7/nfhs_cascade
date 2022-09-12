unmet_cascade <- bind_rows(read_csv(file = "analysis/nca05_state unmet need care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                           read_csv(file="analysis/nca03_state level care cascade.csv") %>% 
                             dplyr::filter(is.na(stratification)) %>% 
                             mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                             dplyr::filter(variable == "Disease") %>% 
                             mutate(variable = "Diabetes")
) %>% 
  dplyr::filter(n > 100) %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Unscreened","Undiagnosed","Untreated","Uncontrolled")))

source("functions/state_map.R")

         

# figA <- state_cascade %>% 
#   dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "A. Rural Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

# figB <- state_cascade %>% 
#   dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "B. Urban Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

figA <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Diabetes",plot_title = "A. Rural Diabetes",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figB <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Undiagnosed",plot_title = "B. Rural Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figC <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Untreated",plot_title = "C. Rural Untreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figD <- unmet_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Uncontrolled",plot_title = "D. Rural Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figE <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Diabetes",plot_title = "E. Urban Diabetes",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figF <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Undiagnosed",plot_title = "F. Urban Undiagnosed",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

figG <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Untreated",plot_title = "G. Urban Untreated",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")



figH <- unmet_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "Uncontrolled",plot_title = "H. Urban Uncontrolled",breaks = seq(0,100,by=20),palette_chr = "-RdYlGn")

tmap_arrange(
             figA,figB,figC,figD,
             figE,figF,figG,figH,
             ncol = 4,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_state care cascade.png"),width=28,height=15,dpi=200)


