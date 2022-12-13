library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(datasets)
library(shinyWidgets)
library(treemapify)
library(stringr)

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
                              pickerInput("state_select",
                                           "Select State:",
                                           choices = unique(sort(states$NAME)),
                                           selected = "New Mexico", 
                                          multiple = T),
          
                              
                              # # Select State
                              selectInput("airport_select",
                                          "Select airport",
                                          choices = unique(sort(airports$AIRPORT)),
                                          # choices = unique(sort(states_filtered())),
                                          # selected = tail(unique(sort(airports$AIRPORT)), 1),
                                          selected = c("Albuquerque International"),
                                          selectize = T,
                                          multiple = T), 
                              tabsetPanel(
                                tabPanel(title = "Passengers per type of carrier", plotOutput(outputId = "barchart_carrier")),
                                tabPanel(title = "Tree Map", plotOutput(outputId = "treemap_carrier")),
                                tabPanel(title = "Heat Map", plotOutput(outputId = "heat_map"))
                              )
                              
                                                  
                              # Select State
                              # pickerInput("airport_select",
                              #             "Select airport",
                              #             choices = unique(sort(airports$AIRPORT)),
                              #             # choices = unique(sort(states_filtered())),
                              #             selected = c("Albuquerque International"),
                              #             multiple = T)
                              
                              # # Select State
                              # radioButtons("state_select",
                              #              "Select State:",
                              #              choices = unique(sort(states$NAME)),
                              #              selected = "New Mexico"),
                              # 
                              
                              
                              
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
server <- function(input, output, session) {
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
    req(input$state_select) # I can control here, that if no state is sleected, the map will not update
    # if(length(input$airport_select) > 0){
    airport_filtered <- airports
    
    # Airports

    airport_filtered <- subset(airport_filtered, airport_filtered$AIRPORT %in% input$airport_select)

    # # states
    # airport_filtered <- subset(airport_filtered, states %in% input$state_select)
    # }
    return(airport_filtered)
    })
  
  data_filtered <- reactive({
    req(input$state_select) # I can control here, that if no state is sleected, the map will not update
    # if(length(input$airport_select) > 0){
    data_filtered <- data
    
    # Airports
    
    data_filtered <- subset(data_filtered, data_filtered$AIRPORT %in% input$airport_select)
    
    # # states
    # airport_filtered <- subset(airport_filtered, states %in% input$state_select)
    # }
    return(data_filtered)
    
  })
    
  # When input$state_select is changed, then the options available for aiport_select must change accordingly
  observeEvent(
    input$state_select, {
      
      # if (is.null(input$state_select)){     # Not working with this
      #   input$airport_select = NULL 
      # }
      
  updateSelectInput(session, "airport_select",
                      label = "Select airport",
                      choices = subset(airports$AIRPORT, airports$states %in% input$state_select), 
                      selected = input$airport_select)
      

                      #selected = tail(subset(airports$AIRPORT, airports$states %in% input$state_select), 1))
                      # selected = c("Albuquerque International"))
      # if (is.null(input$state_select)) {
      #   updateSelectInput(session, "airport_select",
      #                     label = "Select airport",
      #                     choices = subset(airports$AIRPORT, airports$states %in% input$state_select), 
      #                     selected = character(0))
        #selected = tail(subset(airports$AIRPORT, airports$states %in% input$state_select), 1))
        # selected = c("Albuquerque International"))
      # }
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
    
    # data_carriers <- add_row(data_carriers(), Airline = 'Other', total_passengers = sum(data_filtered()$Total) - sum(data_carriers()$total_passengers))
  })
  # Then I add the Others
  # data_carriers() <- add_row(data_carriers(), Airline = 'Other', total_passengers = sum(data_filtered()$Total) - sum(data_carriers()$total_passengers))

  
  output$barchart_carrier <- renderPlot({
    ggplot(data_barchart(), aes(x = Year, y = total_passengers, fill = carriergroup)) + 
      geom_bar(stat = 'identity')
  
    
    
  })
  
  
  # THIS IS MY SECOND GRAPH< BUT NOT RUNNING, this error:
  # `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.
  # ver esto: https://statisticsglobe.com/dplyr-message-summarise-has-grouped-output-r
  # 

  
  # cambios: hacer bargraph y con top 5
  output$treemap_carrier <- renderPlot({
    ggplot(data_carriers(), aes(x = reorder(Airline, desc(total_passengers)), y = total_passengers)) +
      geom_bar(stat = 'identity') +
      scale_x_discrete(labels =str_wrap((data_carriers()$Airline), width = 10)) +
      theme(axis.text.x = element_text(size = 11), axis.title.x = element_text(size = 12, face = 'bold')) + 
      xlab("Airline") 
  })

  
  
  
  
  # ESTO YA NO ES PARTE DE MI GRAFICA
  
  # observe({
  #   airport_select_filtered <- input$state_select
  #   if (is.null(airport_select_filtered))
  #     airport_select_filtered <- character(0)
  #   
  #   updateSelectInput(session, "airport_select", 
  #                     label = "Select airport", 
  #                     choices = sort(subset(airports$AIRPORT, states %in% airport_select_filtered)), 
  #                     selected = tail(airport_select_filtered, 1))
  #                     # selected = c("Albuquerque International"))
  # })
  # 
  # data_filtered <- reactive({
  #   data_altered <- data 
  #   data_altered <- subset(data_filtered, AIRPORT %in% input$airport_select)
  #   data_altered <- subset(data_filtered, states %in% input$state_select)
  #   return(data_altered)
  # })
  
  # states_filtered <- reactive({
  #   states_altered <- data$states
  #   states_altered <- subset(data_filtered, states %in% input$state_select)
  #   return(states_altered)
  # })
  
  
  

  # Replace layer with filtered airports
  observe({
    # If the airports change, then we do the rest
    airport_filtered <- AirportInputs()
    
    # What if I create a join here to add the total of the airports selected
    # This is the data already filtered, containing only the relevant airports, same airports as in airport_filtered
    data_filtered()
    
    
    aggregate_passengers <- data_filtered() %>%
      group_by(AIRPORT) %>%
      summarise(Total_Passengers = sum(Total))
    passengers_airports <-  merge(aggregate_passengers, airport_filtered, by.x = "AIRPORT", by.y = "AIRPORT")
    
    
    # Data is greenInf
    leafletProxy("leaflet", data = passengers_airports) %>%
      # In this case either lines 90 or 92 will work
      # 1 First clear all the markers, make the map blank
      clearMarkers() %>%
      # we can also clear group, just in case
      # clearGroup(group = "greenInf") %>%
      # Apply the awesome markers, sewer_type is the data that we want to apply the icons to. The icons is defined up there
      addAwesomeMarkers(icon = ~icons[AIRPORT], popup = ~paste0("<b>", 'project_na', "</b>: ", AIRPORT, '<p>', 'Total passengers: ', formatC(Total_Passengers, big.mark=",", format="d"), '</p>' ), group = "passengers_airports")#, layerId = ~asset_id)
    # leafletProxy("leaflet") %>% update
  })
  # Data Table
  output$table <- DT::renderDataTable(data_filtered(), options = list(scrollX = T))
  # Print Inputs
  observe({
    print(reactiveValuesToList(input))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
