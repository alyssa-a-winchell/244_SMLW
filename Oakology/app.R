#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)

#Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title="Oakology"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "Summary", icon=icon("tree deciduous"), lib="glyphicon"),
      menuItem("Islands", tabName="Islands",icon=icon("globe"), lib="glyphicon"),
      menuItem("SDM", tabName = "SDM", icon=icon("leaf"), lib="glyphicon")
    ) #sidebarmenu end
  ), #sidebar end
  dashboardBody()
)

#Define server logic
server <- function(input, output) { }

#Run the application
shinyApp(ui = ui, server = server)
