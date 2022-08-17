
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(tmap)
run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  district_shp <- readRDS(file.path("diabetes_cascade/data","district_shp.RDS"))
  state_shp <- readRDS(file.path("diabetes_cascade/data","state_shp.RDS"))
  
  nca04_district <- readRDS(file.path("diabetes_cascade/data","nca04_district.RDS"))
  nca03_state <- readRDS(file.path("diabetes_cascade/data","nca03_state.RDS"))
  nca02_national <- readRDS(file.path("diabetes_cascade/data","nca02_national.RDS"))
  
  input = data.frame(stateinput = "Kerala",districtinput = "Kottayam",
                     varinput = "Diagnosed",mapinput = "Rural",stratainput = "Total") %>% mutate_all(~as.character(.))
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")

district_shp <- readRDS(file.path("data","district_shp.RDS"))
state_shp <- readRDS(file.path("data","state_shp.RDS"))
nca04_district <- readRDS(file.path("data","nca04_district.RDS"))
nca03_state <- readRDS(file.path("data","nca03_state.RDS"))
nca02_national <- readRDS(file.path("data","nca02_national.RDS"))




# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  
  n5_state_input <- reactive({
    input$stateinput
  })
  
  # https://stackoverflow.com/questions/64796206/dynamically-update-two-selectinput-boxes-based-on-the-others-selection-in-r-shin
  observe({
    d_i = map2018_sdist[map2018_sdist$n5_state == n5_state_input(),]$D_NAME
    updateSelectInput(session, "districtinput", choices = na.omit(d_i)) 
  })  

  
  nm_merge <- reactive({
    ss <- state_shp %>% 
    sp::merge(nca03_state %>%
                dplyr::filter(variable == input$varinput,
                              residence == input$mapinput,
                              strata == input$stratainput)  %>% 
                dplyr::select(n5_state,ST_NM,estimate),
              by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
    
    ss
  })
  
  
  palette_chr <- reactive({
    case_when(input$varinput == "Screened" ~ "RdYlGn",
                   TRUE ~ "-RdYlGn")
        })
  
  breaks <- reactive({
    if(input$varinput == "Screened"){
      seq(0,100,by=20)
      
    }
    else{c(0,2.5,5,7.5,10,20,30)}
    
  })
  

  output$nationalmap <- tmap::renderTmap({
    
    
    
    
    nm <- tmap_mode("view") +
      tm_shape(shp = nm_merge(),id = "n5_state") +
      tm_fill(title= "",
              col="estimate",
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="n5_state",col="black",size=0.5,remove.overlap = TRUE)+
      tm_view(view.legend.position = c("right","top")) +
      tm_legend(
                legend.outside=FALSE,
                legend.just=c("left","top"))
    nm
    
    
    
  })
  
  sm_merge <- reactive({
    ds <- district_shp %>% 
      sp::merge(nca04_district %>%
                  dplyr::filter(variable == input$varinput,
                                strata == input$stratainput)  %>% 
                  dplyr::select(D_CODE,n5_state,estimate),
                by.x="D_CODE",by.y="D_CODE",all.x=TRUE) 
    
    ds@data <- ds@data %>% 
      dplyr::select(D_NAME,D_CODE,everything())
    
    # https://stackoverflow.com/questions/52384937/subsetting-spatial-polygon-dataframe
    subset(ds,n5_state == input$stateinput)
  })
  
  
  
  
  output$statemap <- tmap::renderTmap({
    
    sm <- tmap_mode("view") +
      tm_shape(sm_merge(),ext=1.2,id="D_NAME") + 
      tm_fill(title= "",
              col="estimate",
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="D_NAME",col="black",size=0.5,remove.overlap = TRUE)+
      tm_view(view.legend.position = c("right","top"))
      tm_legend(
                legend.outside=FALSE,
                legend.just=c("left","top"))
    # tm_layout(plot_title,title.size = 2,
    #           legend.text.size = 1,
    #           legend.title.size = 1)
    
    sm
    
  })
  
  # Table --------
  tab1 <- reactive({
    
    st_df <- nca03_state %>% 
      dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
      dplyr::filter(n5_state == input$stateinput) %>% 
      dplyr::select(variable,residence,strata,est_ci) %>% 
      mutate(residence = paste0(input$stateinput," ",residence," ",strata))  %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    nt_df <- nca02_national %>% 
      dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
      dplyr::select(variable,strata,residence,est_ci) %>% 
      mutate(residence = paste0("India ",residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    dt_df <- nca04_district %>% 
      dplyr::filter(strata == input$stratainput,
                    D_NAME == input$districtinput) %>% 
      dplyr::select(variable,strata,D_NAME,est_ci) %>% 
      rename(residence = D_NAME)  %>% 
      mutate(residence = paste0(residence," ",strata)) %>% 
      dplyr::select(-strata) %>% 
      pivot_wider(names_from=residence,values_from=est_ci) 
    
    left_join(
      nt_df ,
      st_df,
      by = "variable"
    ) %>% 
      left_join(dt_df,
                by="variable") %>% 
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>% 
      rename(Cascade = variable)
  })

  
  output$tableoutput <- renderTable({
    
    tab1()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  

})
