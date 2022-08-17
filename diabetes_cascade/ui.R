
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


sidebar <- dashboardSidebar(
  h3("Please wait for Maps to load before changing inputs"),
  selectInput("stateinput","Select State:",unique(map2016_v024$n5_state)),
  selectInput("districtinput","Select District:",c("")),
  selectInput("varinput","Select Variable:",c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
  selectInput("mapinput","Select Display:",c("Urban","Rural")),
  selectInput("stratainput","Select Strata:",c("Total","Male","Female"))
  
  )

body <- dashboardBody(
  
  fluidRow(
    box(
      tmap::tmapOutput("nationalmap",height=600),
      ),
    
    box(
    tmap::tmapOutput("statemap",height=600),
        ),
  ),
  
  box(solidHeader=FALSE,status="warning",width=20,title = "Diabetes Care Cascade",
      background = "aqua",
      tableOutput("tableoutput")
      )
)
  
  


dashboardPage(
  dashboardHeader(title = "Diabetes Care Cascade, 2019-21",titleWidth = 400),
  
  sidebar,
  
  body
  
  
)