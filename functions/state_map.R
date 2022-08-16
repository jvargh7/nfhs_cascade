
# df <- read_csv("analysis/nca03_state level care cascade.csv")
# plot_variable = "dm_diagnosed"
require(tmap)
state_map <- function(df,plot_variable,map_version = 2016,
                      plot_title = "A",breaks = c(0,2.5,5,7.5,10,15,20),
                      palette_chr = "-RdYlGn"){
  
  
  
  if(map_version == 2016){
    path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
    map_shp <- rgdal::readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2") %>% 
      sp::merge(df %>%
                  dplyr::filter(variable == plot_variable
                                # residence == "Rural",is.na(strata)
                                  ) %>% 
                  dplyr::select(n5_state,estimate)  %>% 
                  mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                                           n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                                           n5_state == "Nct Of Delhi" ~ "Delhi",
                                           TRUE ~ n5_state)),
                by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
    
    
  }
  
  
  s_p <- tm_shape(map_shp,ext=1.2) + 
    tm_fill(title= "",
            col="estimate",
            palette = palette_chr,
            style = "fixed",
            breaks= breaks,
            # midpoint = NA,
            textNA="Data not available",
            colorNA = "white")+ 
    tm_borders(col="black") + 
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))+ 
    tm_xlab("") +
    tm_ylab("") +
    tm_layout(plot_title,title.size = 2,
              legend.text.size = 1,
              legend.title.size = 1)
  
  return(s_p)
  
  
  
}
