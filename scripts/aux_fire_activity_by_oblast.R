# This script analyzes and visualizes fire locations and war-related fires by oblast (administrative regions) in Ukraine and Russia.

# Load required packages
library(readr)
library(sf)
library(tidyverse)
library(dplyr)
library(zoo)

# Load data for Ukraine fires (by ADM3 region)
fires_by_adm3 <- read_csv('output-data/war_fires_by_ADM3.csv')

# Classify these by whether they are in Russia-controlled areas:
russia_controlled <- st_read('/Users/sondresolstad/Github/ukraine-war-data/output-data/ISW-shapefiles/2024-08-17/UkraineControlMapAO17AUG2024.geojson')

# Convert fires_by_adm3 to a spatial (sf) object if it isn't already
fires_by_adm3_sf <- st_as_sf(fires_by_adm3, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Ensure both datasets use the same CRS
russia_controlled <- st_transform(russia_controlled, crs = st_crs(fires_by_adm3_sf))

# Determine if fires are within Russia-controlled areas
fires_by_adm3_sf$russia_controlled <- apply(
  st_within(fires_by_adm3_sf, russia_controlled, sparse = FALSE),
  1,
  any
)

# Check for fires within 30km of Russia-controlled areas
russia_controlled_buffer <- st_buffer(russia_controlled, dist = 30000)  # distance in meters
fires_by_adm3_sf$within_30km <- apply(
  st_within(fires_by_adm3_sf, russia_controlled_buffer, sparse = FALSE),
  1,
  any
)

# Update original dataset
fires_by_adm3 <- fires_by_adm3_sf

# Plot a sample of the data
ggplot() +
  geom_sf(data = fires_by_adm3_sf[sample(1:nrow(fires_by_adm3_sf), 5000), ],
          aes(col = russia_controlled, shape = within_30km & !russia_controlled))

# Load data for Russia fires (from FIRMS data set)
fires_in_russia <- read_csv('output-data/firms_update_RU.csv')

# Load and validate the GeoJSON file for Russian oblasts (ADM1 regions)
oblasts <- st_make_valid(st_read('source-data/geoBoundaries-RUS-ADM1.geojson'))

# Convert Russian fire data to a spatial (sf) object
fires_sf <- st_as_sf(fires_in_russia, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Assign each fire to an oblast based on location
fires_with_oblast <- st_join(fires_sf, oblasts, join = st_intersects) %>%
  select(-geometry) %>%
  rename(oblast = shapeName) %>%
  mutate(russia_controlled = TRUE, within_30km = TRUE)

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

# Filter out the current day
oblast_data <- oblast_data[oblast_data$date < Sys.Date(), ]

# Summarize the number of events within each oblast per day
daily_oblast_data <- oblast_data %>%
  group_by(country, date, oblast, russia_controlled, within_30km) %>%
  summarise(events = n(), .groups = 'drop')

# Calculate the 7-day moving average for the number of events in each oblast
daily_oblast_data <- daily_oblast_data %>%
  group_by(oblast, russia_controlled) %>%
  arrange(date) %>%
  mutate(rolling_avg = rollmean(events, k = 7, fill = NA, align = 'right')) %>%
  ungroup()

# Visualize the data with a focus on key oblasts
ggplot(daily_oblast_data %>%
         filter(oblast %in% c('Zaporizka', 'Khersonska', 'Kharkivska', 'Donetska', 'Kursk Oblast') &
                  date >= as.Date('2024-06-01') &
                  !(russia_controlled & country == 'UKR')),
       aes(x = date, y = events, fill = within_30km, color = within_30km)) +
  geom_vline(aes(xintercept = as.Date('2024-08-06'))) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = rolling_avg, group = '1'), color = "black", size = 1, linetype = "solid") +
  facet_wrap(country ~ oblast) +
  labs(x = 'Vertical lines = Kursk incursion begins, black line = 7-day average\n\n*War-related fires in parts of Ukraine not held by Russia, as \n assessed by statistical model. In Russia, all fire events in oblast.',
       title = 'Detected fires*', y = '')

ggplot(daily_oblast_data %>%
         filter(oblast %in% c('Zaporizka', 'Khersonska', 'Kharkivska', 'Donetska', 'Kursk Oblast') &
                  date >= as.Date('2024-08-06') &
                  !(russia_controlled & country == 'UKR')),
       aes(x = date, y = events, fill = country, color = country)) +
  geom_vline(aes(xintercept = as.Date('2024-08-06'))) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = rolling_avg), color = "black", size = 1, linetype = "solid") +
  facet_wrap(~ oblast) +
  labs(x = 'Vertical lines = Kursk incursion begins, black line = 7-day average\n\n*War-related fires in parts of Ukraine not held by Russia, as \n assessed by statistical model. In Russia, all fire events in oblast.',
       title = 'Detected fires*', y = '')
