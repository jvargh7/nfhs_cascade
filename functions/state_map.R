
# df <- read_csv("analysis/nca03_state level care cascade.csv")
# plot_variable = "dm_diagnosed"
library(tmap)
library(RColorBrewer)
state_map <- function(df,plot_variable,map_version = 2016,
                      plot_title = "A",breaks = c(0,2.5,5,7.5,10,15,20),
                      palette_chr = "-RdYlGn",
                      lgd_width = 0.25,
                      lgd_height = 0.25,type = "tmap",
                      include_state_names = TRUE){
  
  df_to_merge = df %>%
    dplyr::filter(variable == plot_variable,
                  # residence == "Rural",is.na(strata)
    ) %>% 
    dplyr::select(n5_state,estimate)  %>% 
    mutate(ST_NM = case_when(n5_state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                             n5_state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                             n5_state == "Nct Of Delhi" ~ "Delhi",
                             TRUE ~ n5_state))
  
  if(type == "tmap"){
    
    if(map_version == 2016){
      path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
      map_shp <- rgdal::readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2") %>% 
        sp::merge(df_to_merge,
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
      tm_borders(col="black") 
    
    if(is.null(include_state_names) | include_state_names == TRUE){
      s_p <- s_p + 
        tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)
    }
    
    s_p <- s_p + 
      tm_legend(legend.position = c("right","top"),
                legend.outside=FALSE,
                legend.just=c("left","top"))+ 
      tm_xlab("") +
      tm_ylab("") +
      tm_layout(plot_title,title.size = 2,
                legend.text.size = 1,
                legend.title.size = 1,
                legend.width = lgd_width,
                legend.height = lgd_height)
  }
  
  if(type == "ggplot2"){
    path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
    
    map_sf = sf::read_sf(paste0(path_shape_files,"/maps-master/States"),"Admin2") %>%
      left_join(df_to_merge,
                by = c("ST_NM"))
    
    palette_chr_gg = str_replace(palette_chr,"-","")
    direction_gg = ifelse(str_detect(palette_chr,"-"),-1,1)
    
    s_p = ggplot(data=map_sf,aes(fill = estimate)) +
      geom_sf() +
      # https://stackoverflow.com/questions/43515112/reversing-default-scale-gradient-ggplot2
      scale_fill_distiller(name = "",palette =palette_chr_gg, direction = direction_gg,
                           limits=c(min(breaks),max(breaks))) + 
      theme_bw() +
      ggtitle(plot_title) +
      theme(axis.text = element_blank(),
            legend.position = "bottom",
            # https://www.statology.org/ggplot2-legend-size/
            legend.key.size = unit(1, 'cm'), #change legend key size
            legend.key.height = unit(1, 'cm'), #change legend key height
            legend.key.width = unit(1, 'cm'), #change legend key width
            # legend.title = element_text(size=14), #change legend title font size
            legend.text = element_text(size=10)) #change legend text font size +
      
    
    
  }
  
  return(s_p)
  
  
  
}
