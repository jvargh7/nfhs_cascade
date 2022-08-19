
library(shiny)
library(shinydashboard)
library(tmap)
run_manual = FALSE
if(run_manual){
  map2016_v024 <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2016_v024")
  map2018_sdist <- readxl::read_excel(file.path("diabetes_cascade/data","maps.xlsx"),sheet="map2018_sdist")
  
}

map2016_v024 <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2016_v024")
map2018_sdist <- readxl::read_excel(file.path("data","maps.xlsx"),sheet="map2018_sdist")

sidebar_overview <- conditionalPanel(condition="input.selectedpanel==1",
                                     h3("Please wait for Maps to load before changing inputs"),
                                     selectInput("stateinput1","Select State:",unique(map2016_v024$n5_state)),
                                     selectInput("districtinput1","Select District:",c("")),
                                     selectInput("varinput1","Select Variable:",c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
                                     selectInput("mapinput1","Select Display:",c("Urban","Rural")),
                                     selectInput("stratainput1","Select Strata:",c("Total","Male","Female"))
                                     )

sidebar_state <- conditionalPanel(condition="input.selectedpanel==2",
                                     h3("Please select inputs for unmet need chart"),
                                     selectInput("stateinput2","Select State:",unique(map2016_v024$n5_state)),
                                     # selectInput("varinput2","Select Variable:",c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
                                     # selectInput("mapinput2","Select Display:",c("Urban","Rural")),
                                     selectInput("stratainput2","Select Strata:",c("Total","Male","Female"))
)


sidebar <- dashboardSidebar(
  sidebar_overview,
  sidebar_state
  
  )

panel_overview <- tabPanel("Overview",value = 1,
                           fluidRow(
                             
                             box(tmap::tmapOutput("nationalmap",height=600)),
                             
                             box(tmap::tmapOutput("statemap",height=600),),
                             
                           ),
                           box(solidHeader=FALSE,status="warning",width=20,title = "Diabetes Care Cascade",
                               background = "light-blue",tableOutput("tableoutput"))
)

panel_state <- tabPanel("State",value = 2,
                        fluidRow(
                          box(plotOutput("unmet_districts",height = 800)),
                          box(plotOutput("cascade_state",height = 600))
                          
                          
                        )
                        
)

body <- dashboardBody(
  tabsetPanel(
    panel_overview,
    panel_state,
    id = "selectedpanel"
  )
  
)
  
  


dashboardPage(
  dashboardHeader(title = "Diabetes Care Cascade, 2019-21",titleWidth = 400),
  
  sidebar,
  
  body
  
  
)