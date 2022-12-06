require(tmap)
district_map <- function(df,plot_variable,dmap_version = 2019,smap_version=2016,
                      plot_title = "A",breaks = c(0,2.5,5,7.5,10,15,20),
                      palette_chr = "RdYlGn"){
  
  if(smap_version == 2016){
    path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
    
    state_shp <- rgdal::readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")
  }
  
  if(dmap_version == 2018){
    path_district_files <- "C:/Cloud/OneDrive - Emory University/data/India Shapefiles/INDIA_2018_DISTRICTS-master"
    map_shp <- rgdal::readOGR(paste0(path_district_files),"DISTRICTS_2018") %>% 
      sp::merge(df %>%
                  dplyr::filter(variable == plot_variable
                                # residence == "Rural",is.na(strata)
                  ) %>% 
                  dplyr::select(district_df,estimate),
                by.x="D_CODE",by.y="district_df",all.x=TRUE)
    
    
  }
  if(dmap_version == 2019){
    path_district_files <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA/IASHP7C/shps"
    district_shp <- rgdal::readOGR(paste0(path_district_files),"sdr_subnational_boundaries2")
    district_shp@data <- district_shp@data %>% 
      mutate(REGCODE = case_when(REGNAME == "Hamirpur" & OTHREGNA == "Uttar Pradesh" ~ 168,
                                 REGNAME == "Bijapur" & OTHREGNA == "Karnataka" ~ 557,
                                 REGNAME == "Aurangabad" & OTHREGNA == "Maharashtra" ~ 515,
                                 REGNAME == "Balrampur" & OTHREGNA == "Chhattisgarh" ~ 824,
                                 REGNAME == "Bilaspur" & OTHREGNA == "Chhattisgarh" ~ 827,
                                 REGNAME == "Pratapgarh" & OTHREGNA == "Uttar Pradesh" ~ 173,
                                 REGNAME == "Raigarh" & OTHREGNA == "Maharashtra" ~ 520,
                                 TRUE ~ REGCODE))
    
    
    map_shp <- district_shp %>% 
      sp::merge(df %>%
                  dplyr::filter(variable == plot_variable
                                # residence == "Rural",is.na(strata)
                  ) %>% 
                  dplyr::select(REGCODE,estimate),
                by.x="REGCODE",by.y="REGCODE",all.x=TRUE)
    
    
  }
  
  d_p <- tm_shape(map_shp,ext=1.2) + 
    tm_fill(title= "",
            col="estimate",
            palette=palette_chr,
            style = "fixed",
            breaks= breaks,
            # midpoint = NA,
            textNA="Data not available",
            colorNA = "white")+ 
    tm_borders() + 
    tm_shape(state_shp) + tm_borders(col="black") +
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))+ 
    tm_xlab("") +
    tm_ylab("") +
    tm_layout(plot_title,title.size = 2,
              legend.text.size = 1,
              legend.title.size = 1)
  
  return(d_p)
  
  
  
}