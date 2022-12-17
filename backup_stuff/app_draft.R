library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(datasets)
library(shinyWidgets)
library(stringr)
library(shinyjs)
library(plotly)

# Read in data 
airports <- read_csv("airports.csv")
passengers <- read_csv("International_Report_Passengers.csv")
data <- merge(airports, passengers, by.x="IATA", by.y="usg_apt")
states_abb <- data.frame(datasets::state.name, datasets::state.abb)
names(states_abb) <- c("states", "abbreviations") 
states <- st_read("gz_2010_us_040_00_500k.json") 
Airlines_code <- read_csv("Airlines_code.csv")


# Discard three columns that are non useful identifiers of the airports
data <- data %>% select(-fg_apt_id, -fg_apt, -fg_wac)
# Adios Alaska
states_abb <- states_abb[!states_abb$states == 'Alaska',]

data <- merge(data, states_abb, by.x = "STATE", by.y = "abbreviations")
data$carriergroup = ifelse(data$carriergroup == 1, "Domestic", "Foreign")

# Adios Alaska and territories
airports <- merge(airports, states_abb, by.x = "STATE", by.y = "abbreviations")

# Add centroids to states in case I want to use them
states_centroids <- states %>% 
  st_centroid() %>% 
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)
states <- states %>% 
  left_join(states_centroids)

states <- states[!(states$NAME %in% c('Alaska', 'District of Columbia', 'Puerto Rico')),]
data <- merge(data, Airlines_code, by.x = "carrier", by.y = "Code", all.x = TRUE)

# discard very small airports: discard those that have less than 500 cumulative passengers
nosmall_airports <- data %>% group_by(STATE, AIRPORT) %>% 
  summarise(total_passengers = sum(Total)) %>% 
  filter(total_passengers >= 500)

data <- data %>% filter(data$AIRPORT %in% nosmall_airports$AIRPORT)

# Have the same airports in airports and in data
airports <- subset(airports, airports$AIRPORT %in% data$AIRPORT)

# Discard data with nulls
data <- na.omit(data)

icons <- makeAwesomeIcon(icon = "plane" , library = "fa", markerColor = "blue")

# Define UI for application
ui <- navbarPage("U.S. Airports",
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              
                              # Select State
                              pickerInput("state_select",
                                          "Select State: (the map will show your states selected in RED",
                                          choices = unique(sort(states$NAME)),
                                          selected = c("California", "Pennsylvania"), 
                                          multiple = T),
                              
                              # Select years
                              sliderInput("years_select",
                                          "Select consumer's age range:",
                                          value = c(1998, 2014), 
                                          max = max(data$Year), min = min(data$Year), 
                                          step = 1, sep = ""),
                              
                              # Select State
                              selectInput("airport_select",
                                          "Select airport",
                                          choices = unique(sort(airports$AIRPORT)),
                                          selected = c("San Diego International-Lindbergh", "Pittsburgh International"),
                                          selectize = T,
                                          multiple = T), 
                              
                              # Two graph to display in two tabs in the left panel
                              tabsetPanel(
                                tabPanel(title = "Total flyers over time, by carrier type", plotlyOutput("barchart_carrier")),
                                tabPanel(title = "Top 5 carriers, based on total flyers", plotlyOutput("bar_carrier"))
                              ),
                              
                              downloadButton("downloadData", "Download Raw Data of Selection")
                              
                            ),
                            
                            
                            # Map Panel
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Page
                              leafletOutput("airports_map")
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
server <- function(input, output, session) {
  # Basic Map
  output$airports_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$OpenStreetMap.Mapnik, group = "Base") %>%
      addProviderTiles(provider = providers$Stamen.Watercolor, group = "Watercolor") %>%
      setView(-97.8216, 40.4469, 5) %>%
      addLayersControl(baseGroups = c("Base", "Watercolor"))
  })
  
  # Airport Filtered data
  AirportInputs <- reactive({
    
    airport_filtered <- airports
    
    # Airports
    airport_filtered <- subset(airport_filtered, airport_filtered$AIRPORT %in% input$airport_select)
    # }
    return(airport_filtered)
    
  })
  
  data_filtered <- reactive({
    req(input$state_select) # I can control here, that if no state is selected, the map will not update
    data_filtered <- data %>% 
      
      # # Airports previously
      # data_filtered <- subset(data_filtered, data_filtered$AIRPORT %in% input$airport_select)
      #Years
      filter(Year >= input$years_select[1] & Year <= input$years_select[2]) %>% 
      filter(AIRPORT %in% input$airport_select)
    return(data_filtered)
  })
  
  # When input$state_select is changed, then the options available for aiport_select must change accordingly
  observeEvent(
    input$state_select, {
      updateSelectInput(session, "airport_select",
                        label = "Select airport",
                        choices = subset(airports$AIRPORT, airports$states %in% input$state_select), 
                        selected = input$airport_select)
    })
  
  data_barchart <- reactive({
    req(input$airport_select)
    data_forbar <- data_filtered()
    data_forbar %>% 
      group_by(Year, carriergroup) %>%
      summarise(total_passengers = sum(Total), .groups = "drop_last")
  })
  
  data_carriers <- reactive({
    req(input$airport_select)
    data_filtered() %>%
      group_by(Airline) %>%
      summarise(total_passengers = sum(Total), .groups = "drop_last") %>%
      arrange(desc(total_passengers)) %>%
      top_n(5, total_passengers)
  })
  
  
  output$barchart_carrier <- renderPlotly({
    ggplot(data_barchart(), aes(x = Year, y = total_passengers, fill = carriergroup)) + 
      geom_bar(stat = 'identity')
  })
  
  
  # cambios: hacer bargraph y con top 5
  output$bar_carrier <- renderPlotly({
    ggplot(data_carriers(), aes(x = reorder(Airline, desc(total_passengers)), y = total_passengers)) +
      geom_bar(stat = 'identity') +
      scale_x_discrete(labels =str_wrap((data_carriers()$Airline), width = 10)) +
      theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 12, face = 'bold')) + 
      xlab("Airline") 
  })
  
  # Replace layer with filtered airports
  observe({
    # If the airports change, then we do the rest
    req(input$state_select)
    airport_filtered <- AirportInputs()
    
    # What if I create a join here to add the total of the airports selected
    # This is the data already filtered, containing only the relevant airports, same airports as in airport_filtered
    data_filtered()
    
    aggregate_passengers <- data_filtered() %>%
      group_by(AIRPORT) %>%
      summarise(Total_Passengers = sum(Total))
    passengers_airports <-  merge(aggregate_passengers, airport_filtered, by.x = "AIRPORT", by.y = "AIRPORT")
    
    leafletProxy("airports_map", data = passengers_airports) %>%
      # clear markers when airports changes, and the re do selection
      clearMarkers() %>%
      addAwesomeMarkers(icon = icons, popup = ~paste0("<b>", 'Airport selected', "</b>: ", AIRPORT, '<p>', '<b>', 'Total passengers for selected period: ','</b>', formatC(Total_Passengers, big.mark=",", format="d") ), group = "passengers_airports") %>% 
      # clear shapes when state selection changes, and then re do selection
      clearShapes() %>%
      addPolygons(data = states_tomap(), fillColor = "red", fillOpacity = 0.5, color = "black", weight = 1, popup = ~paste0("<b>", 'Selected state: ', "</b>", NAME))
  })
  
  # This is for the state part of the map
  states_tomap <- reactive({
    states_map <- subset(states, states$NAME %in% input$state_select)
    
    return(states_map)
  })
  
  # Data Table
  output$table <- DT::renderDataTable(data_filtered(), options = list(scrollX = T))
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Airport_passengersdata_", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(data_filtered(), file, row.names = FALSE)
    }
  )
  
  # Print Inputs
  observe({
    print(reactiveValuesToList(input))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
