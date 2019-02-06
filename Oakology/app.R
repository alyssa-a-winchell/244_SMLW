#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

## Shiny App ##
library(shiny)
library(shinydashboard)
library(leaflet)
#library(RColorBrewer)
#library(raster)
#library(sf)
#library(dplyr)

#Define the header, sidebar, and body for the application
header<- dashboardHeader(title = "Oakology")

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "summary",icon=icon("tree deciduous")),
    menuItem("Islands", tabName = "islands",icon=icon("globe")),
    menuItem("SDM", tabName = "sdm",icon=icon("leaf"))
  )
)

body<-dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "summary",
        h2("Oakology Group Project Summary")
    ),
    
    # Second tab content
    tabItem(tabName = "islands",
        h2("Island Environmental Layers"),
        fluidRow(
          box(
            title="Santa Cruz", width=NULL, solidHeader = TRUE,status="primary",
            plotOutput("plot1", height = 250)),
          
          box(
            title = "Santa Rosa",width=NULL, solidHeader = TRUE,status="primary",
            sliderInput("slider", "Number of observations:", 1, 100, 50))
          
          # ,box(width = NULL, leafletOutput("islandmap"))
        )
    ),
    
    # Third tab content
    tabItem(tabName = "sdm",
        h2("Species Distribution Models")
        # ,fluidRow(
        #   box(width=NULL, solidHeader = TRUE),
        #   leafletOutput("sdmmap", height=500)
        # )#fluidrow
    )
    
  )
)

ui<-dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # island_maps<-read_sf(dsn="Oakology/data/islands/scr/extent.shp")
  # output$islandmap<-renderPlot({
  #   island_maps
  # })
  
  # ras<-raster("data/sdm/scr/nofog/historic.tif")
  # pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(map),
  #                     na.color = "transparent")
  # 
  # 
  # output$sdmmap<-renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>% 
  #     addRasterImage(ras, colors = pal, opacity = 0.8)
  # 
  # }) #end leaflet
}#end server

shinyApp(ui, server)
