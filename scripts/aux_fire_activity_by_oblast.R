# This script analyzes and visualizes fire locations and war-related fires by oblast (administrative regions) in Ukraine and Russia.

# Load required packages
library(readr)
library(sf)
library(tidyverse)
library(dplyr)

# Load data for Ukraine fires (by ADM3 region)
fires_by_adm3 <- read_csv('output-data/war_fires_by_ADM3.csv')

# Classify these by whether they are in Russia-controlled areas:
russia_controlled <- st_read('/Users/sondresolstad/Github/ukraine-war-data/output-data/ISW-shapefiles/2024-08-17/UkraineControlMapAO17AUG2024.geojson')

# Convert fires_by_adm3 to an sf object if it isn't already
fires_by_adm3_sf <- st_as_sf(fires_by_adm3, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Ensure both datasets use the same CRS
russia_controlled <- st_transform(russia_controlled, crs = st_crs(fires_by_adm3_sf))

# This line appends a column to fires_by_adm3_sf denoting whether or not it is within the shape russia_controlled
# 1. Perform spatial join to determine if fires are within Russia-controlled areas
fires_by_adm3_sf$russia_controlled <- st_within(fires_by_adm3_sf, russia_controlled, sparse = FALSE)

# 2. Convert the logical matrix to a simple TRUE/FALSE vector
fires_by_adm3_sf$russia_controlled <- apply(fires_by_adm3_sf$russia_controlled, 1, any)

# 3. Similarly, we check for activity within 30km of this area. Basically, not within Russia-controlled areas, but 30km or less than that away from Russia-controlled areas:
# 3.1. Create a buffer of 30 km around Russia-controlled areas
russia_controlled_buffer <- st_buffer(russia_controlled, dist = 30000)  # distance in meters

# 3.2. Perform a spatial join to determine if fires are within 30 km of Russia-controlled areas
fires_by_adm3_sf$within_30km <- st_within(fires_by_adm3_sf, russia_controlled_buffer, sparse = FALSE)

# 3.3. Convert the logical matrix to a simple TRUE/FALSE vector
fires_by_adm3_sf$within_30km <- apply(fires_by_adm3_sf$within_30km, 1, any)

# Update original data set:
fires_by_adm3 <- fires_by_adm3_sf

ggplot()+geom_sf(data=fires_by_adm3_sf[sample(1:nrow(fires_by_adm3_sf), 5000), ], aes(col=russia_controlled, shape=within_30km & !russia_controlled))

# Load data for Russia fires (from FIRMS data set)
fires_in_russia <- read_csv('output-data/firms_update_RU.csv')

# Load and validate the GeoJSON file for Russian oblasts (ADM1 regions)
oblasts <- st_make_valid(st_read('source-data/geoBoundaries-RUS-ADM1.geojson'))

# Convert Russian fire data to a spatial (sf) object using WGS84 coordinate system
fires_sf <- st_as_sf(fires_in_russia, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Perform a spatial join to assign each fire to an oblast based on its location
fires_with_oblast <- st_join(fires_sf, oblasts, join = st_intersects)

# Select relevant columns and rename the oblast column (assuming 'shapeName' is the name column in the GeoJSON)
fires_with_oblast <- fires_with_oblast %>%
  select(-geometry) %>%
  rename(oblast = shapeName) %>%
  mutate(russia_controlled = T,
         within_30km = T)# Adjust 'shapeName' if your GeoJSON has a different column name

# Add a 'country' column to differentiate between Russian and Ukrainian fires
fires_with_oblast$country <- "RUS"
fires_by_adm3$country <- "UKR"

# Generate bar plots for specific oblasts in Ukraine
ggplot(fires_by_adm3 %>% filter(ADM1_EN %in% c("Luhanska", 'Zaporizka', 'Khersonska', 'Kharkivska', 'Donetska')),
       aes(x = date, fill = ADM1_EN)) +
  geom_bar() +
  facet_wrap(~ADM1_EN) +
  labs(x = '', y = '')

# Generate bar plots for specific oblasts in Russia
ggplot(fires_with_oblast %>% filter(oblast %in% c('Belgorod Oblast', 'Kursk Oblast')),
       aes(x = acq_date, fill = oblast)) +
  geom_bar() +
  facet_wrap(~oblast) +
  labs(x = '', y = '')

# Harmonize column names for merging
fires_with_oblast$date <- fires_with_oblast$acq_date
fires_by_adm3$oblast <- fires_by_adm3$ADM1_EN

# Combine Ukrainian and Russian fire data into one dataset
oblast_data <- bind_rows(
  fires_by_adm3 %>% select(country, date, oblast, russia_controlled, within_30km),
  fires_with_oblast %>% select(country, date, oblast, russia_controlled, within_30km)
)

# Remove the current day
oblast_data <- oblast_data[oblast_data$date < Sys.Date(), ]

# Create a daily dataset that sums the number of events within each oblast per day
daily_oblast_data <- oblast_data %>%
  group_by(country, date, oblast, russia_controlled, within_30km) %>%
  summarise(events = n(), .groups = 'drop')

# View the first few rows of the daily dataset
head(daily_oblast_data)

# Load the zoo package for rolling averages
library(zoo)

# Calculate the 7-day moving average for the number of events in each oblast
daily_oblast_data <- daily_oblast_data %>%
  group_by(oblast, russia_controlled) %>%
  arrange(date) %>%
  mutate(rolling_avg = rollmean(events, k = 7, fill = NA, align = 'right')) %>%
  ungroup()

# The next line summaries the number of rows per day:
ggplot(daily_oblast_data %>% filter(oblast %in% c('Zaporizka', 'Khersonska', 'Kharkivska', 'Donetska', 'Kursk Oblast') & date >= as.Date('2024-06-01') & !(russia_controlled & country == 'UKR')),
       aes(x = date, y = events, fill = within_30km, color = within_30km))+geom_vline(aes(xintercept=as.Date('2024-08-06'))) +# geom_vline(aes(xintercept=as.Date('2024-08-16'))) +
  geom_bar(stat = "identity") + geom_line(aes(y=rolling_avg, group ='1'), color = "black", size = 1, linetype = "solid") +
  facet_wrap(country~oblast) +
  labs(x = 'Vertical lines = Kursk incursion begins, black line = 7-day average\n\n*War-related fires in parts of Ukraine not held by Russia, as \n assessed by statistical model. In Russia, all fire events in oblast.', title = 'Detected fires*', y='')

ggplot(daily_oblast_data %>% filter(oblast %in% c('Zaporizka', 'Khersonska', 'Kharkivska', 'Donetska', 'Kursk Oblast') & date >= as.Date('2024-08-06') & !(russia_controlled & country == 'UKR')),
       aes(x = date, y = events, fill = country, color = country))+geom_vline(aes(xintercept=as.Date('2024-08-06'))) +# geom_vline(aes(xintercept=as.Date('2024-08-16'))) +
  geom_bar(stat = "identity") + geom_line(aes(y=rolling_avg), color = "black", size = 1, linetype = "solid") +
  facet_wrap(~oblast) +
  labs(x = 'Vertical lines = Kursk incursion begins, black line = 7-day average\n\n*War-related fires in parts of Ukraine not held by Russia, as \n assessed by statistical model. In Russia, all fire events in oblast.', title = 'Detected fires*', y='')
