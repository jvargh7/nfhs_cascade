state_cascade <- read_csv(file = "analysis/nca03_state level care cascade.csv")

source("functions/state_map.R")

state_cascade %>% 
  dplyr::filter(is.na(stratification)) %>% 
  group_by(residence,variable) %>% 
  summarize_at(vars(estimate),.funs=list(~min(.),~max(.)))
         

# figA <- state_cascade %>% 
#   dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "A. Rural Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

# figB <- state_cascade %>% 
#   dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
#   state_map(.,plot_variable = "dm_screened",plot_title = "B. Urban Screening",breaks = seq(0,100,by=20),palette_chr = "RdYlGn")

figA <- state_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_disease",plot_title = "A. Rural Diabetes",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figB <- state_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_diagnosed",plot_title = "B. Rural Diagnosed",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figC <- state_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_treated",plot_title = "C. Rural Treated",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figD <- state_cascade %>% 
  dplyr::filter(residence == "Rural",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_controlled",plot_title = "D. Rural Controlled",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figE <- state_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_disease",plot_title = "E. Urban Diabetes",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figF <- state_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_diagnosed",plot_title = "F. Urban Diagnosed",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

figG <- state_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_treated",plot_title = "G. Urban Treated",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")



figH <- state_cascade %>% 
  dplyr::filter(residence == "Urban",is.na(stratification)) %>% 
  state_map(.,plot_variable = "dm_controlled",plot_title = "H. Urban Controlled",breaks = seq(0,20,by=5),palette_chr = "-RdYlGn")

tmap_arrange(
             figA,figB,figC,figD,
             figE,figF,figG,figH,
             ncol = 4,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_state care cascade.png"),width=28,height=15,dpi=200)


