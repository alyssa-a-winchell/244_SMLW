#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

# library(shiny)
# library(shinydashboard)
# 
# #Define the UI for the application
# 
# ui <- dashboardPage(
#   dashboardHeader(title="Oakology"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Summary", tabName = "summary", icon=icon("tree deciduous"), lib="glyphicon"),
#       menuItem("Islands", tabName="islands",icon=icon("globe"), lib="glyphicon"),
#       menuItem("SDM", tabName = "sdm", icon=icon("leaf"), lib="glyphicon")
#     ) #sidebarmenu end
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "Summary",
#         h2("Dashboard tab content")
#               
#       ),
#       
#       tabItem(tabName = "islands",
#         h2("Widgets tab content"),
#         # Boxes need to be put in a row (or column)
#         fluidRow(
#           box(plotOutput("plot1", height = 250)),
#           
#           box(
#             title = "Controls",
#             sliderInput("slider", "Number of observations:", 1, 100, 50)
#           )
#         )
#       )
#     )
#   
#   
#   )# end dashboard body
# )#End UI
# 
# #Define server logic
# server <- function(input, output) { 
#   set.seed(122)
#   histdata <- rnorm(500)
#   
#   output$plot1 <- renderPlot({
#     data <- histdata[seq_len(input$slider)]
#     hist(data)
#   })
#   
#   }#end server
# 
# #Run the application
# shinyApp(ui = ui, server = server)


## app.R ##
library(shiny)
library(shinydashboard)

#Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Oakology"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary",icon=icon("tree deciduous")),
      menuItem("Islands", tabName = "islands",icon=icon("globe")),
      menuItem("SDM", tabName = "sdm",icon=icon("leaf"))
    )
  ),
  dashboardBody(
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
              title="Santa Cruz", width=4, solidHeader = TRUE,
              plotOutput("plot1", height = 250)),
            
            box(
              title = "Controls",
              sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
          )
      ),
      
      # Third tab content
      tabItem(tabName = "sdm",
          h2("Species Distribution Models")
      )
      
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
