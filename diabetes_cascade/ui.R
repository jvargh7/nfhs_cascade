
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
  selectInput("stateinput","Select State:",unique(map2016_v024$n5_state)),
  selectInput("districtinput","Select District:",c("")),
  selectInput("varinput","Select Variable:",c("Screened","Diabetes","Diagnosed","Treated","Controlled")),
  selectInput("mapinput","Select Display:",c("Urban","Rural")),
  selectInput("stratainput","Select Strata:",c("Total","Male","Female"))
  
  )

body <- dashboardBody(
  
  fluidRow(
    box(
      tmap::tmapOutput("nationalmap",height=800),
      ),
    
    box(
    tmap::tmapOutput("statemap",height=800),
        ),
  ),
  
  box("Care Cascade",solidHeader=TRUE,status="warning",width=20,
      tableOutput("tableoutput")
      )
)
  
  


dashboardPage(
  dashboardHeader(title = "Diabetes Care Cascade, 2019-21"),
  
  sidebar,
  
  body
  
  
)