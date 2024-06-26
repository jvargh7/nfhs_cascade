
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(tmap)
library(ggpubr)
run_manual = FALSE



if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  mapnfhs5_sdist <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="mapnfhs5_sdist")
  mapnfhs5_v024 <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="mapnfhs5_v024")
  
  
  district_shp <- readRDS(file.path("diabetes_cascade/data","district_shp.RDS"))
  state_shp <- readRDS(file.path("diabetes_cascade/data","state_shp.RDS"))
  
  nca02_national <- bind_rows(
    readRDS(file.path("diabetes_cascade/data","nca02_national.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
    readRDS(file.path("diabetes_cascade/cutoff200","ncc200a02_national.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
    readRDS(file.path("diabetes_cascade/cutoff160","ncc160a02_national.RDS")) %>% mutate(cutpoint = "160 mg/dL")
    )
  
  national_nested <- bind_rows(
    readRDS(file.path("diabetes_cascade/data","national_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
    readRDS(file.path("diabetes_cascade/cutoff200","national_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
    readRDS(file.path("diabetes_cascade/cutoff160","national_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
    )
  
  nationalz_nested <- bind_rows(readRDS(file.path("diabetes_cascade/data","nationalz_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                                readRDS(file.path("diabetes_cascade/cutoff200","nationalz_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                                readRDS(file.path("diabetes_cascade/cutoff160","nationalz_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                                )
  
  
  nca03_state <- bind_rows(readRDS(file.path("diabetes_cascade/data","nca03_state.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                           readRDS(file.path("diabetes_cascade/cutoff200","ncc200a03_state.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                           readRDS(file.path("diabetes_cascade/cutoff160","ncc160a03_state.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                           )
  
  nca05_state_unmet <- bind_rows(readRDS(file.path("diabetes_cascade/data","nca05_state_unmet.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                                 readRDS(file.path("diabetes_cascade/cutoff200","ncc200a05_state_unmet.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                                 readRDS(file.path("diabetes_cascade/cutoff160","ncc160a05_state_unmet.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                                 )
  
  state_nested <- bind_rows(readRDS(file.path("diabetes_cascade/data","state_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                            readRDS(file.path("diabetes_cascade/cutoff200","state_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                            readRDS(file.path("diabetes_cascade/cutoff160","state_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                            )
  
  statez_nested <- bind_rows(readRDS(file.path("diabetes_cascade/data","statez_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                             readRDS(file.path("diabetes_cascade/cutoff200","statez_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                             readRDS(file.path("diabetes_cascade/cutoff160","statez_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                             )
   
  nca04_district <- bind_rows(readRDS(file.path("diabetes_cascade/data","nca04_district.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                              readRDS(file.path("diabetes_cascade/cutoff200","ncc200a04_district.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                              readRDS(file.path("diabetes_cascade/cutoff160","ncc160a04_district.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                              )
  
  nca08_district_unmet <- bind_rows(readRDS(file.path("diabetes_cascade/data","nca08_district_unmet.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                                    readRDS(file.path("diabetes_cascade/cutoff200","ncc200a08_district_unmet.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                                    readRDS(file.path("diabetes_cascade/cutoff160","ncc160a08_district_unmet.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                                    )
  
  district_nested <- bind_rows(readRDS(file.path("diabetes_cascade/data","district_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                               readRDS(file.path("diabetes_cascade/cutoff200","district_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                               readRDS(file.path("diabetes_cascade/cutoff160","district_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                               )
  
  districtz_nested <- bind_rows(readRDS(file.path("diabetes_cascade/data","districtz_nested.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                                readRDS(file.path("diabetes_cascade/cutoff200","districtz_nested.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                                readRDS(file.path("diabetes_cascade/cutoff160","districtz_nested.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                                )

  source("diabetes_cascade/code/cascade_plot.R")
  
  
  input = data.frame(stateinput1 = "Kerala",districtinput1 = "Kottayam",
                     varinput1 = "Diagnosed",mapinput1 = "Rural",stratainput1 = "Total") %>% mutate_all(~as.character(.))
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")
mapnfhs5_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_sdist")
mapnfhs5_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="mapnfhs5_v024")

district_shp <- readRDS(file.path("data","district_shp.RDS"))
state_shp <- readRDS(file.path("data","state_shp.RDS"))

ncz01_national <- bind_rows(readRDS(file.path("data","ncz01_national.RDS")) %>% mutate(cutpoint = "220 mg/dL"),
                            readRDS(file.path("cutoff200","ncc200z01_national.RDS")) %>% mutate(cutpoint = "200 mg/dL"),
                            readRDS(file.path("cutoff160","ncc160z01_national.RDS")) %>% mutate(cutpoint = "160 mg/dL")
                            )

nca02_national <- bind_rows(readRDS(file.path("data","nca02_national.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                            readRDS(file.path("cutoff200","ncc200a02_national.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                            readRDS(file.path("cutoff160","ncc160a02_national.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                            )
national_nested <- bind_rows(readRDS(file.path("data","national_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                             readRDS(file.path("cutoff200","national_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                             readRDS(file.path("cutoff160","national_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                             )
nationalz_nested <- bind_rows(readRDS(file.path("data","nationalz_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                              readRDS(file.path("cutoff200","nationalz_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                              readRDS(file.path("cutoff160","nationalz_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                              )

ncz02_state <- bind_rows(readRDS(file.path("data","ncz02_state.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                         readRDS(file.path("cutoff200","ncc200z02_state.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                         readRDS(file.path("cutoff160","ncc160z02_state.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                         )

nca03_state <- bind_rows(readRDS(file.path("data","nca03_state.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                         readRDS(file.path("cutoff200","ncc200a03_state.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                         readRDS(file.path("cutoff160","ncc160a03_state.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                         )

nca05_state_unmet <- bind_rows(readRDS(file.path("data","nca05_state_unmet.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                               readRDS(file.path("cutoff200","ncc200a05_state_unmet.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                               readRDS(file.path("cutoff160","ncc160a05_state_unmet.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                               )

state_nested <- bind_rows(readRDS(file.path("data","state_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                          readRDS(file.path("cutoff200","state_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                          readRDS(file.path("cutoff160","state_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                          )

statez_nested <- bind_rows(readRDS(file.path("data","statez_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                           readRDS(file.path("cutoff200","statez_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                           readRDS(file.path("cutoff160","statez_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                           )

nca04_district <- bind_rows(readRDS(file.path("data","nca04_district.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                            readRDS(file.path("cutoff200","ncc200a04_district.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                            readRDS(file.path("cutoff160","ncc160a04_district.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                            )

nca08_district_unmet <- bind_rows(readRDS(file.path("data","nca08_district_unmet.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                                  readRDS(file.path("cutoff200","ncc200a08_district_unmet.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                                  readRDS(file.path("cutoff160","ncc160a08_district_unmet.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                                  )

district_nested <- bind_rows(readRDS(file.path("data","district_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                             readRDS(file.path("cutoff200","district_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                             readRDS(file.path("cutoff200","district_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                             )

districtz_nested <- bind_rows(readRDS(file.path("data","districtz_nested.RDS"))  %>% mutate(cutpoint = "220 mg/dL"),
                              readRDS(file.path("cutoff200","districtz_nested.RDS"))  %>% mutate(cutpoint = "200 mg/dL"),
                              readRDS(file.path("cutoff160","districtz_nested.RDS"))  %>% mutate(cutpoint = "160 mg/dL")
                              )

source("code/cascade_plot.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  # https://stackoverflow.com/questions/50326110/r-shiny-popup-window-before-app
  
  
  
  # Panel 1: About -----------
  
  output$tabledef <- renderTable({
    
    data.frame(Indicator = c("Age standardization","Screening","Diabetes","",
                             "Diagnosed","Treated","Controlled"),
                                Definition = c("Age standardized to national distribution as per NFHS-5 [18-39: 50.26%, 40-64: 38.78%, 65+: 10.96%] for national (up to subgroups of sociodemographic covariates), state (up to urban/rural strata) and district",
                                               "Blood glucose ever checked previously",
                                               "(a) Self-reported diabetes",
                                               "(b) High blood glucose (≥126 mg/dL if fasting [≥8 hours] or ≥220 mg/dL if not fasting [random])",
                                               "Told had high glucose on two or more occasions by a medical provider among those with Diabetes",
                                               "Currently taking a prescribed medicine to lower glucose among those with Diagnosed Diabetes",
                                               "Blood glucose in non-hyperglycemic range (<126 mg/dL if fasted and ≤180 mg/dL if not fasting) among those with Diagnosed Diabetes")
                                )
    })


  
  
  
  # Panel 2: Overview ----------------
  # https://waiter.john-coene.com/#/waiter/examples#on-render -----
  w <- Waiter$new(id = c("nationalmap", "statemap"))
  
  welcome_modal <- modalDialog(
    title = "Welcome!",
    "This comprehensive dashboard takes 15-20 seconds to load. In the meantime, you can easily visualize state-level summary statistics at",
    # a("shodha.stopncd.org", href="http://157.245.97.237/"),
    tags$a("shodha.stopncd.org", href="http://shodha.stopncd.org/",target="_blank"),
    
    
    easyClose = F
    
  )
  
  # Show the modal on start up ...
  showModal(ui = welcome_modal)
  
  nested_n1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(nationalz_nested)
      
      
    } else{
      return(national_nested)
    }
  })
  
  nested_s1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(statez_nested)
      
      
    } else{
      return(state_nested)
    }
  })
  print(head(nested_s1))
  
  n5_cutpoint_input <- reactive({
    input$cutoffinput1
  })
  
  
  n5_state_input <- reactive({
    input$stateinput1
  })
  
  tab_national <- reactive({
    
    nested_n1() %>%
      dplyr::filter(strata %in% c("Total","Male","Female"),cutpoint == input$cutoffinput1) %>%
      dplyr::select(variable,strata,residence,est_ci) %>%
      mutate(residence = case_when(is.na(residence) ~ "",
                                   TRUE ~ residence)) %>%
      mutate(residence = paste0("India ",residence," ",strata)) %>%
      dplyr::select(-strata) %>%
      pivot_wider(names_from=residence,values_from=est_ci) %>%
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>%
      rename(Cascade = variable)
    
  })
  
  tab_national <- reactive({
    
    nested_n1() %>%
      dplyr::filter(strata %in% c("Total","Male","Female"),cutpoint == input$cutoffinput1) %>%
      dplyr::select(variable,strata,residence,est_ci) %>%
      mutate(residence = case_when(is.na(residence) ~ "",
                                   TRUE ~ residence)) %>%
      mutate(residence = paste0("India ",residence," ",strata)) %>%
      dplyr::select(-strata) %>%
      pivot_wider(names_from=residence,values_from=est_ci) %>%
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>%
      rename(Cascade = variable) %>% 
      dplyr::select(Cascade,one_of(c("India  Male","India  Female")),
                    one_of(c("India Rural Total","India Urban Total")),
                    everything())
    
  })
  
  tab_state <- reactive({
    
    nested_s1() %>%
      dplyr::filter(strata %in% c("Total","Male","Female"),cutpoint == input$cutoffinput1) %>%
      dplyr::filter(n5_state == input$stateinput1) %>%
      dplyr::select(variable,residence,strata,est_ci) %>%
      mutate(residence = paste0(input$stateinput1," ",residence," ",strata))  %>%
      dplyr::select(-strata) %>%
      pivot_wider(names_from=residence,values_from=est_ci) %>%
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>%
      rename(Cascade = variable)
    
  })
  
  tab_district <- reactive({
    
    nested_d1() %>% 
      dplyr::filter(
        REGNAME == input$districtinput1,cutpoint == input$cutoffinput1) %>%
      dplyr::select(variable,strata,REGNAME,est_ci) %>%
      rename(residence = REGNAME)  %>%
      mutate(residence = paste0(residence," ",strata)) %>%
      dplyr::select(-strata) %>%
      pivot_wider(names_from=residence,values_from=est_ci)  %>%
      mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>%
      rename(Cascade = variable)
    
  })
  
  # output$tableoutput -----------
  output$tableoutput1 <- renderTable({
    
    tab_national()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  output$tableoutput2 <- renderTable({
    
    tab_state()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  output$tableoutput3 <- renderTable({
    
    tab_district()
    
  },bordered = TRUE, sanitize.text.function=identity,align = "c")
  
  
  # https://stackoverflow.com/questions/64796206/dynamically-update-two-selectinput-boxes-based-on-the-others-selection-in-r-shin
  observe({
    
    d_i = mapnfhs5_sdist[mapnfhs5_sdist$n5_state == n5_state_input(),]$REGNAME
    updateSelectInput(session, "districtinput1", choices = na.omit(d_i)) 
  })  

  nm_merge <- reactive({
    
    ss <- state_shp %>% 
    sp::merge(nested_s1() %>%
                dplyr::filter(variable == input$varinput1,
                              residence == input$mapinput1,
                              cutpoint == input$cutoffinput1,
                              strata == input$stratainput1)  %>% 
                dplyr::select(n5_state,ST_NM,estimate) %>% 
                rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1," ")),
              by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
    # https://waiter.john-coene.com/#/waiter/examples#on-render 
    w$show()
    ss
    
    # w$show()
    # on.exit({
    #   w$hide()
    # })
  })
  
  
  palette_chr <- reactive({
    case_when(input$varinput1 == "Diabetes" ~ "-RdYlGn",
                   TRUE ~ "RdYlGn")
        })
  
  breaks <- reactive({
    if(input$varinput1 == "Diabetes"){
      c(0,5,10,15,25,40)
      
    }
    else{seq(0,100,by=20)}
    
  })
  
  
  
  
# output$nationalmap -----------
  output$nationalmap <- tmap::renderTmap({
    
    

    nm <- tmap_mode("view") +
      tm_shape(shp = nm_merge(),id = "n5_state") +
      tm_fill(title= "",
              col=paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1," "),
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
  
  
  
  
  
  # output$statemap -----------
  
  nested_d1 <- reactive({
    if(input$zinput1 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput1 == "No"){
      return(district_nested)
    }
  })
  
  
  sm_merge <- reactive({
    ds <- district_shp %>% 
      sp::merge(nested_d1() %>%
                  dplyr::filter(variable == input$varinput1,
                                cutpoint == input$cutoffinput1,
                                strata == input$stratainput1)  %>% 
                  dplyr::select(REGCODE,n5_state,estimate) %>% 
                  rename_at(vars(estimate),~paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1)),
                by.x="REGCODE",by.y="REGCODE",all.x=TRUE) 
    
    ds@data <- ds@data %>% 
      dplyr::select(REGNAME,REGCODE,everything())
    
    # https://stackoverflow.com/questions/52384937/subsetting-spatial-polygon-dataframe
    subset(ds,n5_state == input$stateinput1)
  })
  
  
  output$statemap <- tmap::renderTmap({
    
    sm <- tmap_mode("view") +
      tm_shape(sm_merge(),ext=1.2,id="REGNAME") + 
      tm_fill(title= "",
              col=paste0(input$mapinput1," ",input$stratainput1," ",input$varinput1),
              palette = palette_chr(),
              style = "fixed",
              breaks= breaks(),
              # midpoint = NA,
              textNA="Data not available",
              colorNA = "white")+ 
      tm_borders(col="black") + 
      tm_text(text="REGNAME",col="black",size=0.5,remove.overlap = TRUE)+
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
  # tab1 <- reactive({
  #   
  #   st_df <- nested_s1() %>% 
  #     dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
  #     dplyr::filter(n5_state == input$stateinput1) %>% 
  #     dplyr::select(variable,residence,strata,est_ci) %>% 
  #     mutate(residence = paste0(input$stateinput1," ",residence," ",strata))  %>% 
  #     dplyr::select(-strata) %>% 
  #     pivot_wider(names_from=residence,values_from=est_ci) 
  #   
  #   nt_df <- nested_n1() %>% 
  #     dplyr::filter(strata %in% c("Total","Male","Female")) %>% 
  #     dplyr::select(variable,strata,residence,est_ci) %>% 
  #     mutate(residence = case_when(is.na(residence) ~ "",
  #                                  TRUE ~ residence)) %>% 
  #     mutate(residence = paste0("India ",residence," ",strata)) %>% 
  #     dplyr::select(-strata) %>% 
  #     pivot_wider(names_from=residence,values_from=est_ci) 
  #   
  #   dt_df <- nested_d1() %>% 
  #     dplyr::filter(strata == input$stratainput1,
  #                   REGNAME == input$districtinput1) %>% 
  #     dplyr::select(variable,strata,REGNAME,est_ci) %>% 
  #     rename(residence = REGNAME)  %>% 
  #     mutate(residence = paste0(residence," ",strata)) %>% 
  #     dplyr::select(-strata) %>% 
  #     pivot_wider(names_from=residence,values_from=est_ci) 
  #   
  #   left_join(
  #     nt_df ,
  #     st_df,
  #     by = "variable"
  #   ) %>% 
  #     left_join(dt_df,
  #               by="variable") %>% 
  #     mutate_all(function(x) str_replace(x," \\(","<br>\\(")) %>% 
  #     rename(Cascade = variable)
  # })

  # Panel 3: State ------------------
  
  
  unmet_d2 <- reactive({
    if(input$zinput2 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput2 == "No"){
      return(district_nested)
    }
  })
  
  
  observeEvent(input$stateinput2,
               {
                 updateSelectInput(session = session,
                                   inputId = "stateinput3",
                                   selected = input$stateinput2)
               })
  
  
  
  panel2_n5_state <- reactive({
    input$stateinput2
  })
  
  panel2_n5_cutpoint <- reactive({
    input$cutoffinput2
  })
  
  district_cm_merge2 <- reactive({
    dcm2 <- unmet_d2() %>% 
        dplyr::filter(strata == input$stratainput2,
                      cutpoint == input$cutoffinput2,
                      n5_state == panel2_n5_state())
    
    
    dcm2
    
  })
  
  # output$unmet_districts -----------
  
  output$unmet_districts2 <- renderPlot({
    
    fig_prevalence <- district_cm_merge2() %>% 
      dplyr::filter(variable == "Diabetes") %>% 
      ggplot(data=.,aes(x = REGNAME,y = estimate,ymin = lci,ymax=uci,
                        group=REGNAME)) +
      geom_col(position=position_dodge(width=0.9),fill="lightblue") +
      geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
      theme_bw() + 
      coord_flip() +
      facet_grid(~variable,scales="free",space="free_y") +
      scale_y_continuous(limits=c(0,40),breaks=seq(0,40,by=10)) +
      facet_grid(~variable,scales="free_y",space="free_y") +
      theme(
        legend.text = element_text(size=12),
        axis.text = element_text(size = 12),
        strip.background.y = element_blank(),
        strip.text.x = element_text(size=12),
        strip.text.y = element_blank(),
        legend.position = "bottom") +
      # scale_y_continuous(limits=c(0,50)) +
      ylab("Prevalence (%)") +
      xlab("") 
    
    fig_uc <- district_cm_merge2() %>% 
      dplyr::filter(!variable %in% c("Diabetes","Screened")) %>% 
      ggplot(data=.,aes(x = REGNAME,y = estimate,ymin = lci,ymax=uci,
                        group=REGNAME),fill="lightblue") +
      geom_col(position=position_dodge(width=0.9)) +
      geom_errorbar(position = position_dodge(width=0.9),width=0.1) +
      theme_bw() + 
      coord_flip() +
      facet_grid(~variable,scales="free",space="free_y") +
      scale_y_continuous(limits=c(0,100),breaks=c(0,25,50,75,100)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=12),
        axis.text = element_text(size = 12),
        strip.text = element_text(size=12),
        legend.position = "bottom") +
      # scale_y_continuous(limits=c(0,50)) +
      ylab("Prevalence (%)") +
      xlab("") 
    
    ggarrange(fig_prevalence,fig_uc,nrow=1,ncol=2,
              common.legend = TRUE,legend="bottom",
              widths = c(1.5,2))
  })
  
  # Panel 4: Stratified ---------
  
  unmet_d3 <- reactive({
    if(input$zinput3 == "Yes"){
      return(districtz_nested)
    }
    if(input$zinput3 == "No"){
      return(district_nested)
    }
  })
  
  
  unmet_s3 <- reactive({
    if(input$zinput3 == "Yes"){
      return(statez_nested)
    }
    if(input$zinput3 == "No"){
      return(state_nested)
    }
  })
  
  panel3_n5_state <- reactive({
    input$stateinput3
  })
  
  district_cm_merge3 <- reactive({
    dcm3 <- unmet_d3() %>% 
      dplyr::filter(strata == input$stratainput3,
                    cutpoint == input$cutoffinput3,
                    n5_state == panel3_n5_state())
    
    dcm3
    
  })
  
  # output$cascade_state3 --------------
  
  state_cs_merge <- reactive({
    # panel2_n5_state = "Kerala"
    scm <- unmet_s3() %>% 
      dplyr::filter(n5_state == panel3_n5_state(),cutpoint == input$cutoffinput3) %>% 
      # mutate(cascade = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
      mutate(cascade = factor(variable,levels=c("Screened","Diabetes","Diagnosed","Treated","Controlled"),
                              labels=c("Screened","Diabetes","Diagnosed","Taking Medication","Under Control"))) %>% 
      mutate(group = case_when(is.na(strata) ~ paste0(residence,"\nTotal"),
                               TRUE ~ paste0(residence,"\n",strata)))
    scm
    
  })
  
  output$cascade_state3 <- renderPlot({
    
    figA <- state_cs_merge() %>% 
      dplyr::filter(is.na(stratification)|stratification == "sex") %>% 
      cascade_plot(.,limits_y = c(0,100))
    figB <- state_cs_merge() %>% 
      dplyr::filter(stratification == "age_category") %>% 
      cascade_plot(.,limits_y = c(0,100))
    figC <- state_cs_merge() %>%
      dplyr::filter(stratification == "education") %>%
      mutate(group = factor(group, levels=c("Rural\nNo education","Rural\nPrimary","Rural\nSecondary","Rural\nHigher",
                                            "Urban\nNo education","Urban\nPrimary","Urban\nSecondary","Urban\nHigher"
                                            ))) %>% 
      cascade_plot(.,limits_y = c(0,100))
    figD <- state_cs_merge() %>%
      dplyr::filter(stratification == "caste") %>%
      cascade_plot(.,limits_y = c(0,100))
    figE <- state_cs_merge() %>%
      dplyr::filter(stratification == "swealthq_ur") %>%
      mutate(group = factor(group,
                             levels = paste0(rep(c("Rural","Urban"),each =5),
                                             "\n",
                                             rep(c("Wealth: Lowest","Wealth: Low",
                                                   "Wealth: Medium","Wealth: High",
                                                   "Wealth: Highest"),times=2)),ordered=TRUE)) %>% 
      cascade_plot(.,limits_y = c(0,100))
    
    ggarrange(figA,
              figB,
              figC,
              figD,
              figE,
              labels = LETTERS[1:5],ncol = 1,nrow=5,common.legend = TRUE,legend="top")
    
    
  })
  

})
