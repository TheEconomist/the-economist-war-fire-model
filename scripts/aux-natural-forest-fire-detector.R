# Load packages
library(tidyverse)
library(readr)
library(dbscan)    # for spatial clustering
library(ggplot2)   # for plotting
library(sf)
library(dplyr)

# Load data
fires <- read_csv('output-data/ukraine_fires.csv')
forest_fires <- read_csv('source-data/forest_fire_locations_2022_2024.csv')

# Subset to un-checked period
fires <- fires[fires$date >= max(as.Date(forest_fires$end_date, format = "%m/%d/%Y"), na.rm = TRUE) & fires$war_fire_restrictive, ]

# Assume `fires` is your FIRMS dataset with columns: latitude, longitude, and date (YYYY-MM-DD)

# Convert data to sf object
fires_sf <- st_as_sf(fires, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Project coordinates into a metric system (meters) for accurate distance calculations
fires_proj <- st_transform(fires_sf, 3857)
coords <- st_coordinates(fires_proj)

# Run DBSCAN clustering (eps = 5000 meters = 5 km)
db <- dbscan(coords, eps = 5000, minPts = 40)
fires_proj$cluster <- db$cluster

# Filter out noise points (cluster = 0)
clusters <- fires_proj %>%
  filter(cluster != 0)

# Identify clusters that burn for at least 2 distinct days
clusters_summary <- clusters %>%
  group_by(cluster) %>%
  summarize(n_fires = n(),
            duration_days = n_distinct(date)) %>%
  filter(duration_days >= 2)

# Join back to original clustered data
final_clusters <- clusters %>%
  filter(cluster %in% clusters_summary$cluster)

# Plot these
ggplot(final_clusters, aes(x=x, y=y, col=as.factor(week(date))))+geom_point()

# Plot with more detail
library(rnaturalearth)
library(rnaturalearthdata)
ukraine <- ne_countries(scale = "medium", country = "Ukraine", returnclass = "sf")

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=final_clusters, aes(x=x, y=y, col=as.factor(date)))+facet_wrap(.~date)

forest_and_large_crop_firest <- read.table(text = "
lat	lng	date_start	date_end	what
46.38	29.62	10-03-2025	20-03-2025	river delta fire
49.51	23.63	15-02-2025	05-03-2025	large crop / forest fire
49.31	23.62	15-02-2025	20-03-2025	large crop / forest fire near (treeline)
45.36	29.08	15-02-2025	20-03-2025	fire in national park near river delta
49.52	23.62	25-01-2025	15-03-2025	very large persistent crop fires
51.6	26.65	05-02-2025	15-02-2025	forest fire near river delta
51.9	26.08	05-02-2025	15-02-2025	forest fire near river delta / border
51.9	26.08	15-03-2025	25-03-2025	forest fire near river delta / border
48.98	30.09	15-02-2025	01-03-2025	forest fire near river delta
49.69	33.27	06-03-2025	01-04-2025	forest fire in river delta
", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Convert date columns into Date format\n
forest_and_large_crop_firest$date_start <- as.Date(forest_and_large_crop_firest$date_start, format = "%d-%m-%Y")
forest_and_large_crop_firest$date_end <- as.Date(forest_and_large_crop_firest$date_end, format = "%d-%m-%Y")

forest_and_large_crop_firest <- forest_and_large_crop_firest %>%
  rowwise() %>%
  mutate(date = list(seq(date_start, date_end, by = "day"))) %>%
  unnest(cols = date) %>%
  select(lat, lng, date, what)

ggplot(data=ukraine)+geom_sf(data=ukraine)+
  geom_point(data=final_clusters, aes(x=x, y=y, col=as.factor(date)))+
  geom_point(data=forest_and_large_crop_firest[forest_and_large_crop_firest$date %in% final_clusters$date, ], aes(x=lng, y=lat, col='x', alpha= 0.1, size = 2, shape = "o", fill=NA))+
  facet_wrap(.~date)

# Alternative approach:

# Round locations
fires$LATITUDE <- round(fires$LATITUDE, 0)
fires$LONGITUDE <- round(fires$LONGITUDE, 0)

# Round dates
fires$week <- lubridate::week(fires$date)

# Define ID's
fires$ID <- paste0(fires$LATITUDE, '_', fires$LONGITUDE, '_', fires$week)

# Count fires:
fires$n <- as.numeric(ave(fires$ID, fires$ID, FUN = function(x) length(x)))

# Deduplicate:
fires <- fires[order(fires$date), ]
fires <- fires[!duplicated(fires$ID), ]

# Show only N > 5
fires <- fires[fires$n >= 50, ]
fires$ID <- 1:nrow(fires)

# Plot
library(rnaturalearth)
library(rnaturalearthdata)
ukraine <- ne_countries(scale = "medium", country = "Ukraine", returnclass = "sf")

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires, aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week < 5, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

# Below step is manual:
forest_fire_locations <- c(1) # Apparent forest fire in river delta

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week %in% 6:8, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

forest_fire_candidates <- c(forest_fire_candidates, 2, 3, 4) # Crop fire, crop fire, river delta fire

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week %in% 9, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

forest_fire_candidates <- c(forest_fire_candidates, 9) # Seeming forest fire in carphantian foothills

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week %in% 10, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

forest_fire_candidates <- c(20:25, 28, 30, 31, forest_fire_candidates)

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week %in% 10, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

forest_fire_candidates <- c(24, 25, 26, 27, 28, 29, 30, 33, 34, 35, 36, forest_fire_candidates)

ggplot(data=ukraine)+geom_sf(data=ukraine)+geom_point(data=fires[fires$week %in% 10, ], aes(x=LONGITUDE, y=LATITUDE, size=n, col=n))+facet_wrap(ID~.)

forest_fire_candidates <- c(43, 44, 45, 46,52, 53, 54, 55 , 56, 57, 58, 59, 60, 62, 63, forest_fire_candidates)



fires <- fires[order(fires$date), c('LONGITUDE', 'LATITUDE', 'n', 'date', 'week', 'ID')]
fires
# Check dimensions and preview the data
dim(fires)
head(fires)

# --- Identify clusters of persistent fires ---

# Define adjustable clustering parameters
eps_value <- 0.010      # radius for clustering, adjust as needed
minPts_value <- 25       # minimum number of points to form a cluster

# Use spatial coordinates (longitude and latitude) for clustering
fires_coords <- fires %>% select(LONGITUDE, LATITUDE)

# Run DBSCAN clustering
db <- dbscan(fires_coords, eps = eps_value, minPts = minPts_value)

# Append cluster labels to the fires dataframe
fires <- fires %>% mutate(cluster = db$cluster)

# Define a threshold for a 'large' cluster (e.g., at least 30 observations)
cluster_min_size <- 25

# Identify clusters that are persistent (fires on at least 2 different days)
persistent_clusters <- fires %>%
  group_by(cluster) %>%
  summarise(
    num_days = n_distinct(date),
    cluster_size = n()
  ) %>%
  filter(cluster != 0,      # Exclude noise (cluster 0)
         num_days >= 2,     # Persistent: fires across at least 2 days
         cluster_size >= cluster_min_size)

# Subset the original data to include only the persistent clusters
persistent_fires <- fires %>%
  filter(cluster %in% persistent_clusters$cluster)

# --- Calculate cluster centroids (lat/long) and start dates ---
cluster_centers <- persistent_fires %>%
  group_by(cluster) %>%
  summarise(
    center_lat = mean(LATITUDE, na.rm = TRUE),
    center_long = mean(LONGITUDE, na.rm = TRUE),
    start_date = min(date, na.rm = TRUE)
  )

# Print the centroids along with the start dates
print(cluster_centers)

# --- Plot the clusters and their centroids with start date labels ---

ggplot() + geom_sf(data=ukraine)+
  # Plot the individual fire points associated with the persistent clusters
  geom_point(data = persistent_fires,
             aes(x = LONGITUDE, y = LATITUDE, color = factor(cluster)),
             alpha = 0.6) +
  # Plot the centroids with a distinct symbol (e.g., a star)
  geom_point(data = cluster_centers,
             aes(x = center_long, y = center_lat),
             size = 4, shape = 8, color = "black") +
  # Add text labels for the start date near the centroids
  geom_text(data = cluster_centers,
            aes(x = center_long, y = center_lat, label = paste0("Start: ", start_date)),
            vjust = -1, hjust = 0.5, size = 3) +
  labs(color = "Cluster",
       title = "Persistent Fire Clusters with Centroids and Start Dates",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(aspect.ratio = 1)
