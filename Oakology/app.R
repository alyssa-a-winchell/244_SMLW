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
library(kableExtra)
library(gt)
# install.packages("devtools")
# remotes::install_github("rstudio/gt")

#setwd("G:/data/GitHub/244_SMLW/Oakology")#Set wd just for running here, the app wd includes Oakology

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Climate Change Vulnerability Assessment of Island Oaks"),
              navbarPage("Oakology",
              tabPanel("Summary", "Put the summary text here",
                       mainPanel(img(src = "sri_oaks.png", width=1200, height=600))
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
                         column(4,leafletOutput("islandmap", width=800, height=400))
                       )
                       ),
              tabPanel("SDM",
                       fluidRow(
                         column(2,
                                h4("Historic"),
                                selectInput("histsdmcolor", "Choose a Color Palette", c("Spectral","Spectral2" ,"Viridis", "Magma")),
                                selectInput("histscenario", "Choose a Scenario", c("No Fog",
                                                                                   "Fog"))),
                         column(4,
                                leafletOutput("histsdmmap", width=800, height=400))
                         
                       ),
                       br(),
                       fluidRow(column(6,
                                       #htmlOutput("historictable")
                                       gt_output("historictable")
                                       )),
                       
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
                                leafletOutput("sdmmap", width=800, height=400))
                       ),
                       br(),
                       fluidRow(column(8, offset=2,
                                       htmlOutput("projtable")))
                       
              ) #SDM tab panel
              
              )#nav bar page
)#ui 

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
  
  output$historictable <- render_gt({
    
    histscendf<-switch(input$histscenario,
                     "No Fog"=histscendf<-"nofog",
                     "Fog"=histscendf<-"fogconstant")

    scrhistcsv<-read_csv(paste0("data/sdm/scr/", histscendf, "/historicsummarytable.csv"))
    scrhistdata<-scrhistcsv[1,2:4]
    colnames(scrhistdata)<-c("AUC", "HighestSuit", "PerSuit")
    scrhistdf<-scrhistdata %>% 
      mutate(AUC=round(AUC,2)) %>% 
      mutate(HighestSuit=round(HighestSuit,2)) %>% 
      mutate(PerSuit=PerSuit*100) %>% 
      mutate(PerSuit=round(PerSuit,2)) %>% 
      mutate(Island="Santa Cruz") %>% 
      select(Island, everything())

    srihistcsv<-read_csv(paste0("data/sdm/sri/", histscendf, "/historicsummarytable.csv"))
    srihistdata<-srihistcsv[1,2:4]
    colnames(srihistdata)<-c("AUC", "HighestSuit", "PerSuit")
    srihistdf<-srihistdata %>% 
      mutate(AUC=round(AUC,2)) %>% 
      mutate(HighestSuit=round(HighestSuit,2)) %>% 
      mutate(PerSuit=PerSuit*100) %>% 
      mutate(PerSuit=round(PerSuit,2)) %>% 
      mutate(Island="Santa Rosa") %>% 
      select(Island, everything())
    
    histdf<-rbind(scrhistdf,srihistdf)
    colnames(histdf)<- c("Island", "Average Test AUC", "Highest Predicted Suitability", "Percent Suitable Area")
    #Renaming so I can have spaces
    
   histdf %>% 
      gt() %>% 
     tab_header(
       title = "Historic SDM"
     ) %>% 
     tab_options(
       table.background.color = "white",
       column_labels.background.color = "lightblue",
       heading.border.bottom.color = "black",
       heading.title.font.size = 16,
       heading.subtitle.font.size = 14,
       column_labels.font.size = 14,
       table.font.size = 12
     )
    
    # kable(histdf, caption="**Table 1. Title Names.** Info and Data Source.",
    #       booktabs=TRUE, align=c(rep('c',times=4))) %>% 
    #   kable_styling(bootstrap_options=c("striped", "condensed",full_width=F, font_size=12)) %>% 
    #   row_spec(0, color="black", background="lightblue", bold=TRUE) 
    
  })
  
  
  
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
                 "Hot-Dry"=proj<-"MIROC_rcp85")
    time<-switch(input$timeperiod,
                 "2010-2039"=time<-"_2010_2039",
                 "2040-2069"=time<-"_2040_2069", 
                 "2070-2099"=time<-"_2070_2099")
    
     
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
     
     output$projtable <- renderText({
       
       scendf<-switch(input$scenario,
                    "No Fog"=scendf<-"nofog",
                    "Constant Fog"=scendf<-"fogconstant", 
                    "Fog Increase"=scendf<-"foginc", 
                    "Fog Decrease"=scendf<-"fogdec", 
                    "Fog Elevation Threshold"=scendf<-"fogelev")
       projdf<-switch(input$projection,
                    "Hot-Wet"=projdf<-"CCSM4_rcp85",
                    "Warm-Wet"=projdf<-"MPI_rcp45", 
                    "Warm-Dry"=projdf<-"MIROC_rcp45", 
                    "Hot-Dry"=projdf<-"MIROC_rcp85")
       timedf<-switch(input$timeperiod,
                    "2010-2039"=timedf<-"_2010_2039",
                    "2040-2069"=timedf<-"_2040_2069", 
                    "2070-2099"=timedf<-"_2070_2099")
       
       projection<-paste0(projdf,timedf)
       if(projection=="MPI_rcp45_2010_2039"){
         rownum<-1
       }else if(projection=="MPI_rcp45_2040_2069"){
         rownum<-2
       }else if(projection=="MPI_rcp45_2070_2099"){
         rownum<-3
       }else if(projection=="CCSM4_rcp85_2010_2039"){
         rownum<-4
       }else if(projection=="CCSM4_rcp85_2040_2069"){
         rownum<-5
       }else if(projection=="CCSM4_rcp85_2070_2099"){
         rownum<-6
       }else if(projection=="MIROC_rcp45_2010_2039"){
         rownum<-7
       }else if(projection=="MIROC_rcp45_2040_2069"){
         rownum<-8
       }else if(projection=="MIROC_rcp45_2070_2099"){
         rownum<-9
       }else if(projection=="MIROC_rcp85_2010_2039"){
         rownum<-10
       }else if(projection=="MIROC_rcp85_2040_2069"){
         rownum<-11
       }else if(projection=="MIROC_rcp85_2070_2099"){
         rownum<-12
       }else
         rownum<-4 #default to hot-wet 2010-2039 (shiny first projection)
       
       scrprojcsv<-read_csv(paste0("data/sdm/scr/", scendf, "/projectionssummarytable.csv"))
       scrprojdata<-scrprojcsv[rownum,-(4:6)]
       colnames(scrprojdata)<-c("Project", "AUC", "HighestSuit", "PerSuit", "PerChg")#rename so easier to change
       scrprojdf<-scrprojdata[2:5] %>% 
         mutate(AUC=round(AUC,2)) %>% 
         mutate(HighestSuit=round(HighestSuit,2)) %>% 
         mutate(PerSuit=PerSuit*100) %>% 
         mutate(PerSuit=round(PerSuit,2)) %>% 
         mutate(PerChg=PerChg*100) %>% 
         mutate(PerChg=round(PerChg,2)) %>% 
         mutate(Island="Santa Cruz") %>% 
         select(Island, everything())
       
       sriprojcsv<-read_csv(paste0("data/sdm/sri/", scendf, "/projectionssummarytable.csv"))
       sriprojdata<-sriprojcsv[rownum,-(4:6)]
       colnames(sriprojdata)<-c("Project", "AUC", "HighestSuit", "PerSuit", "PerChg")#rename so easier to change
       sriprojdf<-sriprojdata[2:5] %>% 
         mutate(AUC=round(AUC,2)) %>% 
         mutate(HighestSuit=round(HighestSuit,2)) %>% 
         mutate(PerSuit=PerSuit*100) %>% 
         mutate(PerSuit=round(PerSuit,2)) %>% 
         mutate(PerChg=PerChg*100) %>% 
         mutate(PerChg=round(PerChg,2)) %>% 
         mutate(Island="Santa Rosa") %>% 
         select(Island, everything())
       
       projecttable<-rbind(scrprojdf,sriprojdf)
       colnames(projecttable)<- c("Island", "Average Test AUC", "Highest Predicted Suitability", "Percent Suitable Area", "Percent Change Suitable Area")
       #Renaming so I can have spaces
       
       #Use kable() function to produce beautiful table of top5 df 
       # kable(projecttable, caption="**Table 1. Title Names.** Info and Data Source.",
       #       booktabs=TRUE, align=c(rep('c',times=5))) %>% 
       #   kable_styling(bootstrap_options=c("striped", "condensed", full_width=F, font_size=12)) %>% 
       #   row_spec(0, color="black", background="lightblue", bold=TRUE) 
       kable(projecttable) %>% 
         kable_styling(bootstrap_options=c("striped", "condensed", full_width=F, font_size=12))
       
       
     })
   
   
}#end server

# Run the app ----
shinyApp(ui = ui, server = server)


