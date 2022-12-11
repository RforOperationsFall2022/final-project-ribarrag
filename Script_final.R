# Source for passengers:
# https://data.transportation.gov/Aviation/International_Report_Passengers/xgub-n9bw

# Source for airports
# https://www.kaggle.com/datasets/aravindram11/list-of-us-airports

# Source polygons states:
# https://eric.clst.org/tech/usgeojson/

library(readr)
library(magrittr)
library(dplyr)
library(leaflet)
library(sf)
library(rgdal)
library(datasets)
library(ggplot2)

  
datasets::state.abb
datasets::state.name
  
  
states_abb <- data.frame(datasets::state.name, datasets::state.abb)
names(states_abb) <- c("states", "abbreviations") 
# Adios Alaska
states_abb <- states_abb[!states_abb$states == 'Alaska',]



airports <- read_csv("airports.csv")
passengers <- read_csv("International_Report_Passengers.csv")
data <- merge(airports, passengers, by.x="IATA", by.y="usg_apt")


# Only keep airports in one of 50 states except Alaska
data <- merge(data, states_abb, by.x = "STATE", by.y = "abbreviations")

# Adios Alaska and territories
airports <- merge(airports, states_abb, by.x = "STATE", by.y = "abbreviations")


class(data)

# Discard three columns that are non useful identifiers of the airports
data <- data %>% select(-fg_apt_id, -fg_apt, -fg_wac)

colnames(data)
 
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

nrow(states)
states <- states[!(states$NAME %in% c('Alaska', 'District of Columbia', 'Puerto Rico')),]


View(states)

leaflet(data = states) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons()


colnames(airports)

airports[order(airports$AIRPORT),]


unique(sort(airports$AIRPORT))
subset(airports$AIRPORT, airports$states %in% "Nebraska")



ggplot(data[0:500, ], aes_string(x = 'Total', y = 'Charter')) +
    geom_point()

colnames(data)






