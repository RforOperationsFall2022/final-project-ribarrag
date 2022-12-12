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
library(lubridate)
library(reshape2)
library(tidyr)

  
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

data$carriergroup = ifelse(data$carriergroup == 1, "Domestic", "Foreign")
 

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
airports <- subset(airports, airports$AIRPORT %in% data$AIRPORT)






# Only keep the main airports for each state:
data


aggregate_passengers <- data %>%
  group_by(AIRPORT) %>%
  summarise(Total_Passengers = sum(Total))
passengers_airports <-  merge(aggregate_passengers, airports, by.x = "AIRPORT", by.y = "AIRPORT")
passengers_airports




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


data_reshaped <- tidyr::gather(data, key = "AIRPORT", value = "Total", -Month)
data_reshaped
ggplot(data = data_reshaped) +
  geom_histogram(aes(x = Month, y = Total, color = AIRPORT))

ggplot(data = data_reshaped) +
  geom_boxplot(aes(x = AIRPORT, y = Total))




melted_sale_amount <- melt(data[, c('Year', 'carriergroup', "Total")], id = 'Year')

melted_sale_amount
ggplot(data = melted_sale_amount, aes(x = Year, fill = value)) + 
  geom_bar(stat = "count") + 
  ggtitle('How much do people spend in each type of grocery, by month') +
  labs(y = 'Total Passengers') 

  scale_x_continuous(breaks = 1:12, labels = month.name)


ggplot(data = data, aes(x = Year, fill = carriergroup)) + 
    geom_bar(stat = "count") + 
    labs(x = "Year", y = "Total Passengers")





df_transformed <- data %>% 
  group_by(Year, carriergroup) %>%
  summarise(total_passengers = sum(Total)) %>%
  pivot_wider(id_cols = Year, names_from = carriergroup, values_from = total_passengers)

df_transformed <- data %>% 
  group_by(Year, carriergroup) %>%
  summarise(total_passengers = sum(Total))
  
df_transformed

ggplot(df_transformed, aes(x = Year, y = total_passengers, fill = carriergroup)) + 
  geom_bar(stat = 'identity')
  






ggplot(df_transformed, aes(x = Year, y = c(Domestic, Foreign), fill = c("Domestic", "Foreign"))) +
  geom_col(position = "dodge") +
  labs(x = "Year", y = "Total Passengers") +
  ggtitle("Total Passengers by Year") +
  scale_fill_discrete(name = "Carrier")

ggplot(df_transformed, aes(x = Year, y = Domestic)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_bar(aes(x = Year, y = Foreign), stat = "identity", width = 0.5, position = "stack") 


ggplot(df_transformed, aes(x = Year, y = Foreign)) +
  geom_bar(stat = "identity", width = 0.5)

ggplot(df_transformed, aes(x = Year, y = Domestic)) +
  geom_bar(stat = "identity", width = 0.5)

melted_sale_amount <- reactive({
  data()[, c('Month', "MntWines", "MntFruits", 
             "MntMeatProducts", "MntFishProducts", 
             "MntSweetProducts", "MntGoldProds")] %>%
    melt(id = 'Month')
})


# Stacked bar chart for Products tab
output$stackedbar_purchases<- renderPlotly({
  req(melted_sale_amount()$variable)
  ggplot(data = melted_sale_amount(), aes(x = Month, fill = variable)) + 
    geom_bar(stat = "count") + 
    ggtitle('How much do people spend in each type of grocery, by month') +
    labs(y = 'Amount ($)', fill = 'Gorcery type ') +
    scale_x_continuous(breaks = 1:12, labels = month.name)
})


