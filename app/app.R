library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)

# Read in data 
airports <- read_csv("airports.csv")
passengers <- read_csv("International_Report_Passengers.csv")
data <- merge(airports, passengers, by.x="IATA", by.y="usg_apt")

# Discard three columns that are non useful identifiers of the airports
# data <- data %>% select(-fg_apt_id, -fg_apt, -fg_wac)

data
states <- st_read("gz_2010_us_040_00_500k.json") 
# Add centroids to states in case I want to use them
states_centroids <- states %>% 
  st_centroid() %>% 
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)
states <- states %>% 
  left_join(states_centroids)


# # Green Infrastructure
# library(shiny)
# library(shinythemes)
# library(leaflet)
# library(leaflet.extras)
# 
# library(shinyjs)
# library(rgeos)



icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)


# Define UI for application
ui <- navbarPage("U.S. Airports",
                 # theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Select State
                              selectInput("airport_select",
                                          "Select aiport",
                                          choices = unique(sort(airports$AIRPORT)),
                                          selected = c("Albuquerque International"),
                                          selectize = T,
                                          multiple = T)
                              # ,
                              # # Select NYC Borough
                              # radioButtons("boroSelect",
                              #              "Borough Filter:",
                              #              choices = unique(sort(greenInf.load$borough)),
                              #              selected = "Bronx")
                            ),
                            # Map Panel
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Page
                              leafletOutput("leaflet")
                            )
                          )
                 ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
  # Basic Map
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-97.8216, 40.4469, 4) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })

  # Airport Filtered data
  AirportInputs <- reactive({
    if(length(input$airport_select) > 0){
    airport_filtered <- airports

    # Airports
    airport_filtered <- subset(airport_filtered, AIRPORT %in% input$airport_select)
    }
    # # Sewer type
    # if (length(input$sewerSelect) > 0) {
    #   airport_filtered <- subset(airport_filtered, sewer_type %in% input$sewerSelect)
    # }

    return(airport_filtered)
  })
  
  
    
  # # Green Infrastructure Filtered data
  # greenInfInputs <- reactive({
  #   greenInf <- greenInf.load 
  #   
  #   # Boros
  #   greenInf <- subset(greenInf, borough == input$boroSelect)
  #   # Sewer type
  #   if (length(input$sewerSelect) > 0) {
  #     greenInf <- subset(greenInf, sewer_type %in% input$sewerSelect)
  #   }
  #   
  #   return(greenInf)
  # })
  
  # Replace layer with filtered airports
  observe({
    # If the greenInf changes, then we do the rest
    airport_filtered <- AirportInputs()
    # Data is greenInf
    leafletProxy("leaflet", data = airport_filtered) %>%
      # In this case either lines 90 or 92 will work
      # 1 First clear all the markers, make the map blank
      clearMarkers() %>%
      # we can also clear group, just in case
      # clearGroup(group = "greenInf") %>%
      # Apply the awesome markers, sewer_type is the data that we want to apply the icons to. The icons is defined up there
      addAwesomeMarkers(icon = ~icons[AIRPORT], popup = ~paste0("<b>", 'project_na', "</b>: ", AIRPORT), group = "airport_filtered")#, layerId = ~asset_id)
    # leafletProxy("leaflet") %>% update
  })
  # Data Table
  output$table <- DT::renderDataTable(greenInfInputs()@data, options = list(scrollX = T))
  # Print Inputs
  observe({
    print(reactiveValuesToList(input))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)














# 
# # Create user interface
# ui <- fluidPage(
#   
#   # App title
#   titlePanel("US Airports Map & Histogram"),
#   
#   # Sidebar layout with a input and output definitions
#   sidebarLayout(
#     
#     # Sidebar panel for inputs
#     sidebarPanel(
#       
#       leafletOutput("map", width = "100%")
#     ),
#     
#     # Main panel for displaying outputs
#     mainPanel(
#       plotOutput("hist")
#     )
#   )
# )
# 
# # Create server logic
# server <- function(input, output) {
#   
#   # Render map
#   output$map <- renderLeaflet({
#     # Use leaflet() here, and only pass in the data
#     leaflet() %>% 
#       addTiles() %>%
#       addMarkers(data=data, lng=~long, lat=~lat)
#   })
#   
#   # Create a reactive expression to filter the data based on user input
#   filteredData <- reactive({
#     data %>%
#       filter(long == input$map_marker_lng & lat == input$map_marker_lat)
#   })
#   
#   # Render histogram
#   output$hist <- renderPlot({
#     ggplot(data=filteredData(), aes(x=Month, y=Total)) + 
#       geom_histogram(binwidth=100000, fill="blue", color="black") +
#       labs(x="Month", y="Total Passengers")
#   })
#   
# }
# 
# # Create Shiny app object
# shinyApp(ui = ui, server = server)