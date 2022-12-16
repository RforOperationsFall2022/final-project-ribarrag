# Source for passengers:
# https://data.transportation.gov/Aviation/International_Report_Passengers/xgub-n9bw

# Source for airports
# https://www.kaggle.com/datasets/aravindram11/list-of-us-airports

# Source polygons states:
# https://eric.clst.org/tech/usgeojson/

# Airlines codes:
# https://www.bts.gov/topics/airlines-and-airports/airline-codes

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
library(treemapify)

  
datasets::state.abb
datasets::state.name
  
  
states_abb <- data.frame(datasets::state.name, datasets::state.abb)
names(states_abb) <- c("states", "abbreviations") 
# Adios Alaska
states_abb <- states_abb[!states_abb$states == 'Alaska',]

Airlines_code <- read_csv("app/Airlines_code.csv")

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

data <- merge(data, Airlines_code, by.x = "carrier", by.y = "Code", all.x = TRUE)


# discard very small airports
nosmall_airports <- data %>% group_by(STATE, AIRPORT) %>% 
  summarise(total_passengers = sum(Total)) %>% 
  filter(total_passengers >= 500)

data <- data %>% filter(data$AIRPORT %in% nosmall_airports$AIRPORT)
data <- na.omit(data)
# hasta aqui el discard de pequenos
# Fill some data
# data$Airline[data$airlineid == 20402] <- "Miami Air International"


aggregate_passengers <- data %>%
  group_by(AIRPORT) %>%
  summarise(Total_Passengers = sum(Total))
passengers_airports <-  merge(aggregate_passengers, airports, by.x = "AIRPORT", by.y = "AIRPORT")
passengers_airports

# END CLEAN DATA, not sure if previous two lines are RELEVANT???!!!

nrow(data_2)
nrow(data)
((setdiff(data, data_2))$carrier)
View(data[data$Airline == "", ])


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



df_transformed <- data %>% 
  group_by(Year, carriergroup) %>%
  summarise(total_passengers = sum(Total))
  
df_transformed

ggplot(df_transformed, aes(x = Year, y = total_passengers, fill = carriergroup)) + 
  geom_bar(stat = 'identity')
  
# this is the second graph I need

ggplot(data, aes(x = Year, y = Total)) + 
  geom(stat = 'identity')

data$date_column <- ymd(paste(data$Year, data$Month, 1, sep = "-"))

df_last_6_years <- data %>%
  filter(Year >= max(Year) - 6)

df_last_6_years <- df_last_6_years %>%
  group_by(date_column) %>%
  summarise(total_passengers = sum(Total))
df_last_6_years

ggplot(df_last_6_years, aes(x=date_column, y=total_passengers)) + 
  geom_line() 
  # scale_x_discrete(name="Year")  +
  scale_y_continuous(name="Total") +
  labs(title="Total Over Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

colnames(data)
table(data$carrier)

# we can do something witht he carriers, lets say, plot the proportion of flights by the top 5 carriers
# This will be reactive on the filtered data

# Im creating the top 5
df_carriers <- data %>%
  filter(data$STATE %in% c('AL')) %>% 
  group_by(Airline, carrier) %>%
  summarise(total_passengers = sum(Total)) %>%
  arrange(desc(total_passengers)) %>% 
  top_n(5, total_passengers)
df_carriers

View(data %>% filter(data$STATE %in% c('AL')))

# Then I add the Others 
df_carriers <- add_row(df_carriers, Airline = 'Other', total_passengers = sum(data$Total) - sum(df_carriers$total_passengers))
# Then I plot this thing in a beautiful PIE

# DEBUG
df_carriers <- data %>% 
  group_by(Airline) %>%
  summarise(total_passengers = sum(Total)) %>%
  arrange(desc(total_passengers)) %>% 
  top_n(5, total_passengers) 
#%>% 
  add_row(df_carriers, Airline = 'Other', total_passengers = sum(data$Total) - sum(df_carriers$total_passengers))
  
df_carriers

# Im here with the treemap

ggplot(df_carriers, aes(area = total_passengers, fill = total_passengers, label = Airline)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)

df_carriers

sum(is.na(data$Airline))

# Create category other

df_carriers2 <- data %>%
  group_by(Airline) %>%
  summarise(total_passengers = sum(Total)) %>%
  arrange(desc(total_passengers)) %>%
  top_n(5) %>%
  mutate(Airline = ifelse(row_number() == 6, "Other", Airline))

df_carriers2
total_carriers2 <- sum(data$Total)
df_carriers2 <- data %>%
  group_by(Airline) %>%
  summarise(total_passengers = sum(Total)) %>%
  top_n(5, total_passengers) %>%
  summarise(other_passengers = sum(total_passengers, na.rm=TRUE), 
            count = n())

df_carriers

# Now create a pie chart



colnames(Airlines_code)



# Deberia de deshacerme de algunos aeropuertos?

# cual es la suma de pasajeros para cada estado?



View(data_nopequenos)
data %>% group_by(STATE, AIRPORT) %>% 
  summarise(total_passengers = sum(Total)) %>% 
  filter(total_passengers <= 500)




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

# falta:
# Usar la otra layer: estados
# Download button
# Cambiar los iconos
# Limpiar comentarios anteriores y var names y comentar
# POner titulos
# Cambiar formatos y colores
# Pimpear mapa
# Hacer graficas interactivas (plotly)


subset_selection <- c("New Mexico", "Idaho")
subset(states, states$NAME == subset_selection)


class(airports)
class(min(data$Year))
