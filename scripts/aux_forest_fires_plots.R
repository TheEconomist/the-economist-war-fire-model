# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)

# Load the dataset
fires <- read.csv("source-data/forest_fire_locations_2022_2024.csv")

# Convert the start_date and end_date columns to Date type
fires$start_date <- as.Date(fires$start_date, format = "%m/%d/%Y")
fires$end_date <- as.Date(fires$end_date, format = "%m/%d/%Y")

# Ukraine map data from rnaturalearth
library(rnaturalearth)
library(rnaturalearthdata)
ukraine <- ne_countries(scale = "medium", country = "Ukraine", returnclass = "sf")

# Create bounding boxes for fire areas based on coordinates
fires_sf <- st_as_sf(fires, coords = c("lng1", "lat1"), crs = 4326, agr = "constant") %>%
  st_bbox() %>%
  st_as_sfc()

# Plot using ggplot
ggplot(data = ukraine) +
  geom_sf() +
  geom_rect(aes(xmin = lng1, xmax = lng2, ymin = lat2, ymax = lat1, fill = as.factor(start_date)),
            data = fires, alpha = 0.5, color = "black") +
  scale_fill_viridis_d(name = "Fire Start Date") +
  labs(title = "Fire Locations in Ukraine (2022-2024)",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
