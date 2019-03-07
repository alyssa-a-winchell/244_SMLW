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
library(shinyWidgets)
library(colorspace)

setwd("G:/data/GitHub/244_SMLW/Oakology")#Set wd just for running here, the app wd includes Oakology

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Oakology"),
              navbarPage("",
              tabPanel("Summary", "jjjknjk",
                       mainPanel(img(src = "sri_oaks.png"))
                       ),
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
              tabPanel("Islands",
                       fluidRow(
                         column(2,selectInput("islandvar", "Choose an Island Variable", c("DEM", "Vegetation"))),
                         column(4,leafletOutput("islandmap", width=800, height=500))
                       )
                       # sidebarPanel(
                       #   selectInput("islandvar", "Choose an Island Variable", c("DEM", "Vegetation"))
                       # ),
                       # mainPanel(
                       #  
                       #   leafletOutput("islandmap", width=1000, height=500)
                       # )
                       ),
              # tabPanel("SDM",
              #          # sidebarPanel(selectInput("histsdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
              #          #              selectInput("histscenario", "Choose a Scenario", c("No Fog",
              #          #                                                             "Fog"))),
              #          #mainPanel(leafletOutput("histsdmmap", width=1000, height=500)),
              #          sidebarPanel(
              #            selectInput("histsdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
              #            selectInput("histscenario", "Choose a Scenario", c("No Fog",
              #                                                               "Fog")),
              #            br(),
              #            br(),
              #            selectInput("sdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
              #            selectInput("scenario", "Choose a Scenario", c("No Fog",
              #                                                           "Constant Fog",
              #                                                           "Fog Increase",
              #                                                           "Fog Decrease",
              #                                                           "Fog Elevation Threshold")),
              #            selectInput("projection", "Choose a Projection", c("Hot-Wet",
              #                                                               "Warm-Wet",
              #                                                               "Warm-Dry",
              #                                                               "Hot-Dry")), #figure out how to make historic conditional for time period
              #            #selectInput("timeperiod", "Choose a Time Period", c("2010-2039","2040-2069", "2070-2099")),
              #            sliderTextInput("timeperiod", "Choose a Time Period", choices = c("2010-2039", 
              #                                                                              "2040-2069", 
              #                                                                              "2070-2099"), animate=TRUE)
              #          ),
              #        
              #          mainPanel(
              #            leafletOutput("histsdmmap", width=1000, height=500),
              #            leafletOutput("sdmmap", width=1000, height=500)
              #                    )
              #          
              #          )
              tabPanel("SDM",
                       fluidRow(
                         column(2,
                                h4("Historic"),
                                selectInput("histsdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
                                selectInput("histscenario", "Choose a Scenario", c("No Fog",
                                                                                   "Fog"))),
                         column(4,
                                leafletOutput("histsdmmap", width=800, height=500))
                       ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,
                                h4("Projected"),
                                selectInput("sdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
                                selectInput("scenario", "Choose a Scenario", c("No Fog",
                                                                               "Constant Fog",
                                                                               "Fog Increase",
                                                                               "Fog Decrease",
                                                                               "Fog Elevation Threshold")),
                                selectInput("projection", "Choose a Projection", c("Hot-Wet",
                                                                                   "Warm-Wet",
                                                                                   "Warm-Dry",
                                                                                   "Hot-Dry")), #figure out how to make historic conditional for time period
                                #selectInput("timeperiod", "Choose a Time Period", c("2010-2039","2040-2069", "2070-2099")),
                                sliderTextInput("timeperiod", "Choose a Time Period", choices = c("2010-2039", 
                                                                                                  "2040-2069", 
                                                                                                  "2070-2099"), animate=TRUE)),
                         column(4,
                                leafletOutput("sdmmap", width=800, height=500))
                       )
                       
              )
              
              )
)

# Define server logic ----
server <- function(input, output, session) {

  #current wd is "G:/data/GitHub/244_SMLW" for all files

  # output$oakimage <- renderImage({
  # 
  #   # Return a list containing the filename
  #   list(src = "data/images/sri_oak.png",
  #        contentType = 'image/png',
  #        width = 400,
  #        height = 300
  #        )
  # }, deleteFile = FALSE)
  
  
     output$sdmmap <- renderLeaflet({
       
       
    scen<-switch(input$scenario,
                 "No Fog"=scen<-"nofog",
                 "Constant Fog"=scen<-"fogconstant", 
                 "Fog Increase"=scen<-"foginc", 
                 "Fog Decrease"=scen<-"fogdec", 
                 "Fog Elevation Threshold"=scen<-"fogelev")
    proj<-switch(input$projection,
                 "Hot-Wet"=proj<-"CCSM4_rcp85",
                 "Warm-Wet"=proj<-"MPI_rcp45", 
                 "Warm-Dry"=proj<-"MIROC_rcp45", 
                 "Hot-Dry"=proj<-"MIROC_rcp85") #Need to include historic
    time<-switch(input$timeperiod,
                 "2010-2039"=time<-"_2010_2039",
                 "2040-2069"=time<-"_2040_2069", 
                 "2070-2099"=time<-"_2070_2099") #Need to include historic, if historic then no time period and time default defined as ""
    
     
       scr<-raster(paste0("data/sdm/scr/",scen,"/", proj, time, ".tif")) 
       proj4string(scr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       
       sri<-raster(paste0("data/sdm/sri/",scen,"/", proj, time, ".tif")) 
       proj4string(sri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       merged<-merge(scr, sri)
       
     sdmcol <- switch(input$sdmcolor,
                      "Spectral" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=TRUE),
                      "Spectral2" = colorNumeric(palette = "Spectral", domain=values(merged), na.color = "transparent", reverse=FALSE),
                      "Viridis" = colorNumeric(palette = "viridis", domain=values(merged), na.color = "transparent", reverse=TRUE),
                      "Magma" = colorNumeric(palette = "magma", domain=values(merged), na.color = "transparent", reverse=TRUE))
     
     leaflet() %>% addTiles() %>%
       addRasterImage(merged, colors = sdmcol, opacity = 0.8) %>%
       #setView(lng=-13388304, lat=4012916, zoom=20) %>% #Figure out how to set view extent
       addLegend("topright", pal = sdmcol, values = values(merged),
                 title = "Suitability", 
                 labFormat = labelFormat(transform=function(merged) sort (merged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
     
   }) #end render leaflet
     
     
     output$histsdmmap <- renderLeaflet({
       
       
       histscen<-switch(input$histscenario,
                    "No Fog"=histscen<-"nofog",
                    "Fog"=histscen<-"fogconstant")
       
       histscr<-raster(paste0("data/sdm/scr/",histscen,"/historic.tif")) 
       proj4string(histscr) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       
       histsri<-raster(paste0("data/sdm/sri/",histscen,"/historic.tif")) 
       proj4string(histsri) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       histmerged<-merge(histscr, histsri)
       
       histsdmcol <- switch(input$histsdmcolor,
                        "Spectral" = colorNumeric(palette = "Spectral", domain=values(histmerged), na.color = "transparent", reverse=TRUE),
                        "Spectral2" = colorNumeric(palette = "Spectral", domain=values(histmerged), na.color = "transparent", reverse=FALSE),
                        "Viridis" = colorNumeric(palette = "viridis", domain=values(histmerged), na.color = "transparent", reverse=TRUE),
                        "Magma" = colorNumeric(palette = "magma", domain=values(histmerged), na.color = "transparent", reverse=TRUE))
       
       leaflet() %>% addTiles() %>%
         addRasterImage(histmerged, colors = histsdmcol, opacity = 0.8) %>%
         #setView(lng=-13388304, lat=4012916, zoom=20) %>% #Figure out how to set view extent
         addLegend("topright", pal = histsdmcol, values = values(histmerged),
                   title = "Suitability", 
                   labFormat = labelFormat(transform=function(histmerged) sort (histmerged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
       
     }) #end render leaflet
     
     output$islandmap <- renderLeaflet({
       
       # scrdem<-raster("data/islands/scr/DEM.tif") 
       # proj4string(scrdem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       # sridem<-raster("data/islands/sri/DEM.tif") 
       # proj4string(sridem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       # mergeddem<-raster::merge(scrdem, sridem, tolerance = 0.5)
       # 
       scrveg<-raster("data/islands/scr/veg.tif")
       sriveg<-raster("data/islands/sri/veg.tif")
       mergedveg<-raster::merge(scrveg, sriveg, tolerance = 0.5)
       
       dem<-raster("data/islands/both/DEM.tif")
       proj4string(dem) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       veg<-raster("data/islands/both/veg50.tif")
       proj4string(veg) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
       
       island <- switch(input$islandvar,
                        "DEM" = island <- dem,
                        "Vegetation" = island <- veg)
       
       col <- switch(input$islandvar,
                     "DEM" = col <- colorNumeric(palette = grDevices::terrain.colors(20), domain=values(island), na.color = "transparent", reverse=FALSE),
                     "Vegetation" = col <- colorFactor(palette = "Set1", domain=values(island), na.color = "transparent", reverse=FALSE))
       
       DEMlabels<-""
       veglabels<-c(": Woodland",": Chaparral",": Coastal Scrub",": Grassland",": Riparian",": Dune",": Other")
       
       legendlabels <- switch(input$islandvar,
                     "DEM" = legendlabels <- DEMlabels,
                     "Vegetation" = legendlabels <- veglabels)
       
       DEMtitle<-"Elevation (m)"
       vegtitle<-"Vegetation Class"
       
       legendtitle <- switch(input$islandvar,
                              "DEM" = legendtitle <- DEMtitle,
                              "Vegetation" = legendtitle <- vegtitle)
       
       leaflet() %>% addTiles() %>%
         addRasterImage(island, colors = col, opacity = 0.8, method = "ngb") %>% 
         #addLegend("topright", pal = col, values = values(island))
         addLegend("topright", pal = col, values = values(island), labFormat = labelFormat(suffix = legendlabels), title = legendtitle)
                  
                
       
         
  
        
       
     }) #end render leaflet
   
   
}#end server

# Run the app ----
shinyApp(ui = ui, server = server)


