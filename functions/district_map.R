require(tmap)
district_map <- function(df,plot_variable,dmap_version = 2019,smap_version=2016,
                      plot_title = "A",breaks = c(0,2.5,5,7.5,10,15,20),
                      palette_chr = "RdYlGn"){
  
  if(smap_version == 2016){
    state_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/smapsmaster_sp.RDS"))
  }
  
  if(dmap_version == 2018){
    map_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/d2018_sp.RDS")) %>% 
      sp::merge(df %>%
                  dplyr::filter(variable == plot_variable
                                # residence == "Rural",is.na(strata)
                  ) %>% 
                  dplyr::select(district_df,estimate),
                by.x="D_CODE",by.y="district_df",all.x=TRUE)
    
    
  }
  if(dmap_version == 2019){
    district_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/dnfhs5_sp.RDS"))
    
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