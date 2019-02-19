#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

#Notes
#Function for select box is selectInput.
#For slider it's sliderInput


# load the shiny package
library(shiny)
library(shinythemes) #perhaps need to install package while running too?
library(raster)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)

setwd("G:/data/GitHub/244_SMLW/Oakology")#Set wd just for running here, the app wd includes Oakology

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Oakology"),
              navbarPage("",
              tabPanel("Summary", "Put in summary info"),
              tabPanel("Example",
                       sidebarPanel(
                         fileInput("file", "File input:"),
                         textInput("txt", "Text input:", "general"),
                         sliderInput("slider", "Slider input:", 1, 100, 30),
                         tags$h5("Deafult actionButton:"),
                         actionButton("action", "Search"),
                         
                         tags$h5("actionButton with CSS class:"),
                         actionButton("action2", "Action button", class = "btn-primary")
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Tab 1",
                                    h4("Table"),
                                    tableOutput("table"),
                                    h4("Verbatim text output"),
                                    verbatimTextOutput("txtout"),
                                    h1("Header 1"),
                                    h2("Header 2"),
                                    h3("Header 3"),
                                    h4("Header 4"),
                                    h5("Header 5")
                           ),
                           tabPanel("Tab 2", "This panel is intentionally left blank"),
                           tabPanel("Tab 3", "This panel is intentionally left blank")
                         )
                       )
              ),
              tabPanel("Islands", "This panel is intentionally left blank"),
              tabPanel("SDM",
                       mainPanel(
                         selectInput("sdmcolor", "Choose a Color Palette", c("Spectral", "Viridis")),
                         leafletOutput("SCR"),
                                 leafletOutput("SRI")
                                 )
                       
                       )
              )
)

# Define server logic ----
server <- function(input, output) {

  #Can if else statement through which rasters to load based on input
  scr<-raster("data/sdm/scr/nofog/historic.tif") #current wd is "G:/data/GitHub/244_SMLW"
  proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  scr<-projectRaster(scr,crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
  
  sri<-raster("data/sdm/sri/nofog/historic.tif") #current wd is "G:/data/GitHub/244_SMLW"
  proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  sri<-projectRaster(sri,crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
  
  
  #palscr <- colorNumeric(palette = "Spectral", values(scr), na.color = "transparent", reverse=TRUE)
  palsri <- colorNumeric(palette = "Spectral", values(sri), na.color = "transparent", reverse=TRUE)
  
   output$SCR <- renderLeaflet({
     sdmcol <- switch(input$sdmcolor,
                      "Spectral" = colorNumeric(palette = "Spectral", domain=values(scr), na.color = "transparent", reverse=TRUE),
                      "Viridis" = colorNumeric(palette = "viridis", domain=values(scr), na.color = "transparent", reverse=TRUE))
     
     leaflet() %>% addTiles() %>%
       addRasterImage(scr, colors = sdmcol, opacity = 0.8, project=FALSE) %>%
       addRasterImage(sri, colors = sdmcol, opacity = 0.8, project=FALSE) %>% 
       addLegend("topright", pal = sdmcol, values = values(scr),
                 title = "SDM", labFormat = labelFormat(transform=function(scr) sort (scr, decreasing=TRUE)))
   })
   
   output$SRI<- renderLeaflet({
     leaflet() %>% addTiles() %>%
       addRasterImage(sri, colors = palsri, opacity = 0.8) %>%
       addLegend("topright", pal = palsri, values = values(sri),
                 title = "SDM", labFormat = labelFormat(transform=function(sri) sort (sri, decreasing=TRUE)))

#Need to create a zoomout maximum (only to extent of island, but that they are allowed to zoom in)
   })
   #But can have both image in one map
   #And then give summary statistics table below that reacts based on map selected!
 
   
}

# Run the app ----
shinyApp(ui = ui, server = server)



# 
# ## Shiny App ##
# library(shiny)
# library(shinydashboard)
# library(leaflet)
# #library(RColorBrewer)
# #library(raster)
# #library(sf)
# #library(dplyr)
# 
# #Define the header, sidebar, and body for the application
# header<- dashboardHeader(title = "Oakology")
# 
# sidebar<-dashboardSidebar(
#   sidebarMenu(
#     menuItem("Summary", tabName = "summary",icon=icon("tree deciduous")),
#     menuItem("Islands", tabName = "islands",icon=icon("globe")),
#     menuItem("SDM", tabName = "sdm",icon=icon("leaf"))
#   )
# )
# 
# body<-dashboardBody(
#   tabItems(
#     # First tab content
#     tabItem(tabName = "summary",
#         h2("Oakology Group Project Summary")
#     ),
#     
#     # Second tab content
#     tabItem(tabName = "islands",
#         h2("Island Environmental Layers"),
#         fluidRow(
#           box(
#             title="Santa Cruz", width=NULL, solidHeader = TRUE,status="primary",
#             plotOutput("plot1", height = 250)),
#           
#           box(
#             title = "Santa Rosa",width=NULL, solidHeader = TRUE,status="primary",
#             sliderInput("slider", "Number of observations:", 1, 100, 50))
#           
#           # ,box(width = NULL, leafletOutput("islandmap"))
#         )
#     ),
#     
#     # Third tab content
#     tabItem(tabName = "sdm",
#         h2("Species Distribution Models")
#         # ,fluidRow(
#         #   box(width=NULL, solidHeader = TRUE),
#         #   leafletOutput("sdmmap", height=500)
#         # )#fluidrow
#     )
#     
#   )
# )
# 
# ui<-dashboardPage(
#   header,
#   sidebar,
#   body
# )
# 
# server <- function(input, output) {
#   set.seed(122)
#   histdata <- rnorm(500)
#   
#   output$plot1 <- renderPlot({
#     data <- histdata[seq_len(input$slider)]
#     hist(data)
#   })
#   
#   # island_maps<-read_sf(dsn="Oakology/data/islands/scr/extent.shp")
#   # output$islandmap<-renderPlot({
#   #   island_maps
#   # })
#   
#   # ras<-raster("data/sdm/scr/nofog/historic.tif")
#   # pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(map),
#   #                     na.color = "transparent")
#   # 
#   # 
#   # output$sdmmap<-renderLeaflet({
#   #   leaflet() %>%
#   #     addTiles() %>% 
#   #     addRasterImage(ras, colors = pal, opacity = 0.8)
#   # 
#   # }) #end leaflet
# }#end server
# 
# shinyApp(ui, server)
