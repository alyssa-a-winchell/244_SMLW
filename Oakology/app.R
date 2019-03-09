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

#setwd("G:/data/GitHub/244_SMLW/Oakology")#Set wd just for running here, the app wd includes Oakology

ui<-fluidPage(theme = shinytheme("readable"),
              titlePanel("Climate Change Vulnerability Assessment of Island Oaks"),
              navbarPage("Oakology",
              tabPanel("Summary",
                       sidebarPanel(width=3,
                        h2("Oakology Bren Group Project"),
                        br(),
                          h5("Creators:"),
                           p("Sofie McComb, Jazmine Uy, Alyssa Winchell, and Laura Wolf"),
                        br(),
                         h3("Purpose of the App"),
                          p("This app was created to easily explore the data and selected results from the Oakology
                            Group Project at the Bren School of Environmental Science & Management, titled Climate 
                            Change Vulnerability Assessment of the Island Oak (Quercus tomentella). The application 
                            focuses on Santa Cruz and Santa Rosa Islands, due to data sharing restrictions and ease of visualization. "),
                          br(),
                         h3("How to Use the App"),
                          p("The various tabs in this application provide an overview of the data, methodology, and results 
                            used in the analysis of island oaks and their potential climate vulnerability. All tabs provide 
                            the relevant information for Santa Cruz and Santa Rosa Islands, the focus of the application. 
                            For more in-depth information on what each tab provides, see “Tab Summary” under the Summary tab."),
                         br(),
                         p("For more information on the project and our team, you can go to our group project website:"),
                        uiOutput("tab1"),
                        br()
                         ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Project Overview",
                                    br(),
                                    img(src = "sri_oaks.png", width=750, height=500),
                                    p("Photo: Denise Knapp"),
                                    h3("Background"),
                                    p("Quercus tomentella is the rarest oak species in California and is endemic
                                      to only six islands in the California Island Archipelago (CAIA): Anacapa,
                                      Guadalupe, San Clemente, Santa Catalina, Santa Cruz, and Santa Rosa
                                      (Pavlik et al., 1991). Islands oaks are considered a dominant species in the
                                      CAIA’s oak woodlands,where they provide forest litter, protective habitat for
                                      other species, and most importantly, soil moisture through fog drip (McCune,
                                      2005,citation). The past introduction of invasive herbivores from ranching
                                      activity left lasting impacts on the island oaks’ ability to successful establish
                                      and disperse."),
                                    p("Though historical threats have largely been removed from the islands
                                      in recent years, legacy impacts from grazers are still widespread, and Q. tomentella
                                      is still encumbered by damaged and fragmented habitat. As a spatially-constrained
                                      species endemic to the CAIA, Q. tomentella is particularly susceptible to extinction
                                      from habitat loss or fragmentation and has limited opportunity to re-establish in more
                                      suitable regions if additional threats force such movement(Harter et al. 2015). As the
                                      effects of human-induced climate change intensify, island ecologists and managers would
                                      like to determine appropriate management practices to ensure survival and health of Q.
                                      tomentella on all six islands across its range."),
                                    h3("Significance"),
                                    p("Our clients, The Nature Conservancy and the Santa Barbara Botanic Garden,
                                      are concerned with the decline of this species as well as other endemic
                                      species on the Channel Islands. They tasked us with investigating how 
                                      climate change might affect future island oak distribution to help them 
                                      adaptively manage this species to increase its resilience and likelihood
                                      of persistence. Our goal was to model the potential distributional changes
                                      of the island oak in the future, taking into account the wide uncertainty
                                      in projections of future climate. Managers can use our comprehensive analysis
                                      in conjunction with prior known information about the species to inform
                                      adaptive management planning on the islands to best conserve and protect
                                      the species under climate change."),
                                    h3("Objectives"),
                                    p("List our objectives")
                                    ),
                           tabPanel("Data", 
                                    br(),
                                    img(src = "stilted_oaks.png", width=750, height=500),
                                    p("Photo: Denise Knapp"),
                                    h3("Overview"),
                                    p("Data was gathered to perform analyses of how climate change will
                                      likely impact island oak presence across the CAIA. Our analyses
                                      required oak presence points, present climate data, future climate
                                      projections, and island specific data. All layers were resampled
                                      to the resolution of the coarsest data, 270m, and were projected
                                      into the NAD83 Teale-Albers coordinate system. Sufficient data
                                      for the analysis was only available for three of the six islands 
                                      that contain the island oak— Santa Cruz, Santa Rosa, and Santa 
                                      Catalina— so analyses were performed for only these three islands."),
                                    h3("Oak Points"), 
                                    p("Oak presence points were obtained from The Nature Conservancy,
                                      The National Park Service, and Laura Kindsvater."),
                                    h3("Climate Data"),
                                    p("We acquired current and future climate data and projections from
                                      the Basin Characterization Model (BCM), a regional hydrologic climate
                                      model statistically downscaled for California to 270-meter resolution 
                                      (Flint et al., 2013). BCM provides climate data as averaged 30-year 
                                      summaries for the current time period defined as 1981-2010 as well as
                                      for three future time periods (2010-2039, 2040-2069, 2070-2099). We
                                      selected four future climate scenarios that capture some of the variability
                                      in climate futures most likely to occur in California. These climate scenarios
                                      include MIROC rcp8.5 (“hot-dry”), MIROC rcp4.5 (“warm-dry”), CCSM4 rcp4.5
                                      (“hot-wet”), and MPI rcp4.5 (“warm-wet”) projections."),
                                    uiOutput("tab2"),
                                    h3("Fog Data"),
                                    p("BCM provides precipitation and temperature-based climate variables, but does 
                                      not have data available for fog, an important variable for oak species. We obtained
                                      fog data from Rastogi et al. 2016, that shows the current probability of fog inundation
                                      for Santa Rosa and Santa Cruz Islands. We developed future fog predictions from the
                                      current data by projecting historic trends into the future for four possible fog 
                                      scenarios: constant fog, decreasing fog, increasing fog, and change in fog based
                                      on an elevational threshold. We used these predictions in tandem with the climate
                                      future scenarios."),
                                    h3("Island Data"),
                                    p("We obtained island specific data from various island managers and sources, including
                                      island outlines (NPS and CIC), elevation layers (NPS and CIC), soil data (USGS), 
                                      and vegetation community shapefiles (NPS, TNC, CIC).")
                                    
                                    ),
                           tabPanel("Methodology",
                                    br(),
                                    img(src = "tree_tunnel.jpg", width=750, height=500),
                                    p("Photo: Denise Knapp"),
                                    h3("Species Distribution Modeling"),
                                    p("More on Methods"),
                                    img(src = "maxent.png", width=550, height=300)
                                    
                         ),
                         tabPanel("Tab Summary",
                                  br(),
                                  img(src = "quto_fog.jpg", width=750, height=500),
                                  p("Photo: Denise Knapp"),
                                  p("This section provides a very detailed explanation of what can be found and explored on each tab."),
                                  h3("Summary"),
                                  p("The summary tab provides essential information on the project background, significance, objectives, 
                                    data, methodology, and sources, and should be used as a reference for the other slides."),
                                  h3("Oaks"),
                                  p("The oak tab displays the island oak presence points on both islands, which can be visualized in a wide variety
                                    of colors. Santa Rosa has age structure information explorable for a subset of its points, so users can see which of 
                                    the oak points are adults and which are seedlings. The all oaks selection visualizes adults and seedlings, in addition
                                    to unknown age oaks. "),
                                  h3("Islands"),
                                  p("The island tab visualizes the digital elevation model (DEM) and vegetation class layers for Santa Cruz and Santa Rosa.
                                    Users can select between these two layers and can interactively zoom in."),
                                  h3("Climate"),
                                  p("The climate tab provides the Basin Characterization Model (BCM) climate layers available for the islands. Users can select
                                    between four different climate variables, four future climate projections, and three future time periods. Users can compare differences
                                    in climate between projected future climate and current (historic) climate."),
                                  h3("SDM"),
                                  p("The SDM tab provides results from the species distribution model (SDM) analyses for the island oak. These results show the predicted
                                    current and future probability of presence of island oaks across the islands. It is the most complex tab, so we recommend thoroughly reading
                                    below to have a better understanding of what we are visualizing. For more information on SDM analysis and the tools used, see the methodology
                                    tab under summary."),
                                  p("The top panel displays the SDM results for the historic time period (1981 – 2010), showing the current predicted distribution of the species is
                                    on Santa Cruz and Santa Rosa. Users can select different color palettes, for which color scale best translates the information, and can select between
                                    two scenarios— ‘no fog’ or ‘fog’—based on whether or not fog data was included in the analysis."),
                                  p("The bottom panel displays the SDM results for future projections, and can be visualized for a combination of fog scenarios, climate projections, and time
                                    periods, across a variety of color palettes. If the ‘no fog’ option is selected in historic, then ‘no fog’ would be the best choice to visualize for 
                                    the projected option to make an accurate comparison. If the ‘fog’ option for historic is selected, users should choose one of the four future fog scenarios
                                    (constant, increase, decrease, elevation threshold). Users can select the play button under the time period slider bar to visualize the change in results of
                                    a future scenario across time."),
                                  p("The tables underneath each map are updated based on the selection of historic or future projected SDM scenario. The tables provide information on the average 
                                    test Area Under the Curve (AUC) metric, highest predicted suitability value, and percent of the island deemed suitable for island oak climate habitat under each
                                    scenario. In the projected table, the percent change in predicted suitable habitat compared to the historic scenario is provided. AUC values are good metrics of 
                                    model fit and model predictive power, and range from 0-1, with a value close to 1 meaning that errors are minimized with regards to false positives
                                    and false negatives. The SDM model output predicts the probability of species presence from 0 to 1, or low to high potential habitat suitability,
                                    as illustrated in the map and given by the highest predicted suitability value in the table. Lastly, we determined the percent of the island
                                    deemed as suitable island oak habitat under each scenario. This calculation is based on a binary threshold value set by the MaxEnt output
                                    maximum training specificity + sensitivity value, which is the current practice for selecting MaxEnt suitability threshold values 
                                    (Liu, White, and Newell 2013). The tables give the percent suitable area and percent change in suitability depending on these 
                                    threshold values, which typically range from about 0.15-0.4.  The binary interpretation of island oak habitat suitability facilitates
                                    comparisons between future scenarios to better understand the range of variability and uncertainty in the persistence of the oak 
                                    across islands and potential futures."),
                                  br(),
                                  br()
                                  ),
                         tabPanel("Sources",
                                  br(),
                                  h3("References"),
                                  p("Flint, Lorraine E., Alan L. Flint, James H. Thorne, and Ryan Boynton. 2013. 
                                    “Fine-Scale Hydrologic Modeling for Regional Landscape Applications: The California
                                    Basin Characterization Model Development and Performance.” Ecological Processes 2
                                    (July): 25. https://doi.org/10.1186/2192-1709-2-25."),
                                  p("Flint, L.E. and Flint, A.L., 2014, California Basin Characterization Model: A Dataset
                                    of Historical and Future Hydrologic Response to Climate Change, (ver. 1.1, May 2017):
                                    U.S. Geological Survey Data Release, https://doi.org/10.5066/F76T0JPB."),
                                  p("Franklin, J. (2010). Mapping Species Distributions: Spatial Inference and Prediction.
                                    Cambridge: Cambridge University Press. https://doi.org/10.1017/CBO9780511810602."),
                                  p("Harter, David E. V., Severin D. H. Irl, Bumsuk Seo, Manuel J. Steinbauer, 
                                      Rosemary Gillespie, Kostas A. Triantis, José-María Fernández-Palacios, and Carl 
                                    Beierkuhnlein. 2015. “Impacts of Global Climate Change on the Floras of Oceanic 
                                    Islands – Projections, Implications and Current Knowledge.” Perspectives in Plant
                                    Ecology, Evolution and Systematics 17 (2): 160–83.
                                    https://doi.org/10.1016/j.ppees.2015.01.003."),
                                  p("Hutchinson, E. (1957). “Concluding Remarks.” Cold Spring Harbor Symposia on 
                                    Quantitative Biology 22 (2): 415–27."),
                                  p("Kindsvater, L. (2006). Conservation and Restoration of the Endemic Island Oak, Quercus 
                                    tomentella in Channel Islands National Park using a Habitat Approach. University of 
                                    California - Davis, Davis, CA."),
                                  p("Kindsvater, L. (2010). Plant communities associated with the rare, paleoendemic oak, 
                                    Quercus tomentella, on Santa Cruz and Santa Rosa, Islands, California. Oak Ecosystem 
                                    Restoration on Santa Catalina Island, California: Proceedings of an on-Island Workshop, 
                                    February 2-4, 2007, 16."),
                                  p("Liu, Canran, Matt White, and Graeme Newell. 2013. “Selecting Thresholds for the Prediction
                                    of Species Occurrence with Presence-Only Data.” Journal of Biogeography 40 (4): 778–89. 
                                    https://doi.org/10.1111/jbi.12058."),
                                  p("McCune, J. (2005). Report on the census and survey of Island oak (Quercus 
                                    tomentella Engelm.) and canyon live oak (Quercus chrysolepis Liebm.) groves
                                    on Catalina Island, 2004 and 2005. Unpublished report prepared for the Catalina
                                    Island Conservancy, Avalon, CA."),
                                  p("Pavlik, B.M., P.C. Muick, et al. 1991. Oaks of California. Cachuma Press,
                                    Los Olivos, CA and the California Oak Foundation, Oakland, CA."),
                                  p("Perkins, S.E., Alexander, L.V., and Nairn, J.R. (2012). Increasing frequency, 
                                    intensity and duration of observed global heatwaves and warm spells. Geophysical 
                                    Research Letters, 39(20)."),
                                  p("Phillips, Steven J., Robert P. Anderson, Miroslav Dudík, Robert E. Schapire, and 
                                    Mary E. Blair. 2017. “Opening the Black Box: An Open-Source Release of Maxent.” 
                                    Ecography 40 (7): 887–93. https://doi.org/10.1111/ecog.03049."),
                                  p("Phillips, Steven J., Robert P. Anderson, and Robert E. Schapire. 2006. “Maximum 
                                    Entropy Modeling of Species Geographic Distributions.” Ecological Modelling 190 
                                    (3–4): 231–59. https://doi.org/10.1016/j.ecolmodel.2005.03.026."),
                                  p("Phillips, Steven J., Miroslav Dudík, and Robert E. Schapire. 2004. “A Maximum 
                                    Entropy Approach to Species Distribution Modeling.” In Twenty-First International 
                                    Conference on Machine Learning  - ICML ’04, 83. Banff, Alberta, Canada: ACM Press. 
                                    https://doi.org/10.1145/1015330.1015412."),
                                  p("Rastogi, B., Williams, A.P., Fischer, D.T., Iacobellis, S.F., McEachern, K., Carvalho, 
                                    L., Jones, C., Baguskas, S., and Still, C.J. (2016). Spatial and temporal patterns of 
                                    cloud cover and fog inundation in coastal California: Ecological implications. Earth 
                                    Interactions, 20(15), 1-19."),
                                  p("Williams, A.P., Schwartz, R.E., Iacobellis, S., Seager, R., Cook, B.I., Still, C.J., Husak, G., 
                                    and Michaelsen, J. (2015). Urbanization causes increased cloud base height and decreased 
                                    fog in coastal Southern California. Geophysical Research Letters, 42(5), 1527-1536."),
                                  br(),
                                  img(src = "landscape_soledad_oaks.jpg", width=900, height=600),
                                  p("Photo: Denise Knapp"),
                                  br(),
                                  br()
                                  
                         )
                       ),
                       br(),
                       br(),
                       fluidRow(
                         column(2,img(src = "bren.png", width=180, height=60)),
                         column(2, offset = 2, img(src = "tnc_logo.png", width=210, height=60)),
                         column(2, offset = 2, img(src = "sbbg_logo.jpg", width=210, height=60))
                                ),
                       
                       br(),
                       br(),
                       br()
                    
      
                       )),
              tabPanel("Islands",
                       fluidRow(
                         column(8, offset=2,
                                p("The Islands tab allows users to select between viewing digital elevation models
                                  or vegetation class layers for Santa Cruz and Santa Rosa Islands. The layers may
                                  take a second to load as they are relatively detailed and fine resolution. More information 
                                  on the layers can be found in the tab summary and data tab under Summary."))
                       ),
                       br(),
                       fluidRow(
                         column(2,selectInput("islandvar", "Choose an Island Variable", c("DEM", "Vegetation"))),
                         column(4,leafletOutput("islandmap", width=1200, height=650))
                       ),
                       br(),
                       br()
                       ),
              tabPanel("SDM",
                       fluidRow(
                         column(5, offset=2,
                                p("The SDM tab provides our species distribution model (SDM) results for the island oak. 
                         Before exploring the results shown in this tab, we highly recommend reading the tab summary
                         page under project summary as well as the methodology tab."))
                       ),
                       br(),
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
                       fluidRow(column(6, offset=2,
                                       htmlOutput("historictable")
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
  
  url1 <- a("Oakology Wesbite", href="https://oakology19.wixsite.com/oakology/island-oaks")
  output$tab1 <- renderUI({
    tagList("", url1)
  })
  
  url2 <- a("BCM Data", href="http://climate.calcommons.org/bcm")
  output$tab2 <- renderUI({
    tagList("Link:", url2)
  })
  
  output$islandmap <- renderLeaflet({
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
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      addRasterImage(island, colors = col, opacity = 0.8, method = "ngb") %>% 
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
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      addRasterImage(histmerged, colors = histsdmcol, opacity = 0.8) %>%
      addLegend("topright", pal = histsdmcol, values = values(histmerged),
                title = "Suitability", 
                labFormat = labelFormat(transform=function(histmerged) sort (histmerged, decreasing=FALSE))) #decreasing false until can figure out how to reverse legend colors
    
  }) #end render leaflet
  
  output$historictable <- renderText({ #switch to render_gt if want gt table
    
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
    colnames(histdf)<- c("Island", "Avg Test AUC", "Highest Suitability", "% Suitable")
    #Renaming so I can have spaces
    
    kable(histdf, caption="Historic SDM Results for Santa Cruz and Santa Rosa Island. 
             Results updated based on scenario selection, providing the  
          Average Test AUC for the model run, the highest predicted
          suitability value, and the percent of the island deemed suitable.",
          booktabs=TRUE, align=c(rep('c',times=4))) %>%
      kable_styling(bootstrap_options=c("condensed", font_size=12),full_width=F, position="left") %>%
      row_spec(0, color="black", background="lightblue", bold=TRUE) %>% 
      row_spec(1:2, background="#F8F9F9") %>% 
      column_spec(1:4, width="5cm")
    
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
       addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
       addRasterImage(merged, colors = sdmcol, opacity = 0.8) %>%
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
       colnames(projecttable)<- c("Island", "Avg Test AUC", "Highest Suitability", "% Suitable", "% Change")
       #Renaming so I can have spaces
       
       kable(projecttable, caption="Projected SDM Results for Santa Cruz and Santa Rosa Island. 
             Results updated based on selected scenario combination, providing the  
          Average Test AUC for the model run, the highest predicted
             suitability value, the percent of the island deemed suitable, and percent change 
          in suitability relative to the historic model.",
             booktabs=TRUE, align=c(rep('c',times=5))) %>%
         kable_styling(bootstrap_options=c("striped", "condensed",font_size=12), full_width=FALSE,position="left") %>%
         row_spec(0, color="black", background="lightblue", bold=TRUE) %>% 
         row_spec(1:2, background="#F8F9F9") %>% 
         column_spec(1:5, width="4.5cm")
       
       
     })
   
   
}#end server

# Run the app ----
shinyApp(ui = ui, server = server)


