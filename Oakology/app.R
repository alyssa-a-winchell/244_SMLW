#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/


# load the shiny package
library(shiny)
library(shinythemes)

ras<-raster("data/sdm/scr/nofog/historic.tif")

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Oakology"),
              navbarPage("NavBar",
              tabPanel("Summary", "Put in summary infro"),
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
              tabPanel("SDM", "This panel is intentionally left blank")
              )
)

# Define server logic ----
server <- function(input, output) {
  
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
