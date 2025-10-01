# Script to deal with NASA API outage. Files downloaded manually for all 3 systems here: https://nrt3.modaps.eosdis.nasa.gov/archive/FIRMS/ (selecting the global version)

# List of packages you need
pkgs <- c("rnaturalearth", "rnaturalearthdata", "rnaturalearthhires")

# Install only if not already installed
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)

# 1) Get Ukraine’s country polygon (WGS84 / EPSG:4326)
ukr <- ne_countries(country = "Ukraine", scale = "large", returnclass = "sf")
ukr <- st_make_valid(ukr)  # safety

# Add Crimea
rus_admin1 <- ne_states(country = "Russia", returnclass = "sf") |>
  st_make_valid() |>
  st_transform(4326)
crimea <- rus_admin1[tolower(rus_admin1$name) %in% c("crimea", "krym", "sevastopol"), ]
crimea <- st_union(st_geometry(crimea)) |> st_make_valid()

# Merge with Ukraine geometry
ukr <- st_union(st_geometry(ukr), crimea) |> st_make_valid() |>
  st_transform(4326)

# 2) Load fires data from last 2 weeks
add_fires <- data.frame()
for(file in dir('source-data/firms-imports/2025/')){
  temp <- read_csv(paste0('source-data/firms-imports/2025/', file))

  instrument <- NA
  if(grepl("VIIRS", file)){
    instrument <- 'VIIRS'
  } else {
    if(grepl('MODIS', file)){
      instrument <- 'MODIS'
    }
  }
  temp$instrument <- instrument

  if(max(as.Date(temp$acq_date)) < as.Date('2025-08-01')){
    stop()
  }

  if(nrow(add_fires) == 0){
    add_fires <- temp
  } else {
  add_fires <- rbind(
    add_fires[, intersect(colnames(add_fires), colnames(temp))],
    temp[, intersect(colnames(add_fires), colnames(temp))])
  }
}

add_fires$acq_time <- as.POSIXct(add_fires$acq_time, format = "%H:%M:%S", tz = "UTC")
add_fires$acq_time <- format(add_fires$acq_time, "%H%M")

# 3) Convert coordinates to points:
add_fires$fire_id <- 1:nrow(add_fires)
add_fires_w_coords <- st_as_sf(add_fires, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
add_fires_w_coords <- add_fires_w_coords[order(add_fires_w_coords$fire_id), ]

# 4) Point-in-polygon: TRUE if inside (or on boundary) of Ukraine
inside <- st_within(add_fires_w_coords, ukr, sparse = FALSE)[,1]

# 5) Attach result back to your data
add_fires <- add_fires[inside, ]
add_fires$country_id <- 'UKR'

# Load old fires:
old_fires <- read_csv('output-data/firms_update.csv')

# Ensure common columns:
add_fires <- add_fires[, colnames(old_fires)]

# 6) Deduplicate
add_fires <- add_fires[!duplicated(paste0(add_fires$latitude, '_',
                                          add_fires$longitude, '_',
                                          add_fires$acq_date, '_',
                                          add_fires$acq_time)), ]

# To inspect:
# library(ggplot2)
# ggplot()+geom_sf(data=ukr)+geom_point(data=add_fires, aes(x=longitude, y=latitude))

fires <- rbind(old_fires, add_fires)
fires <- fires[!duplicated(paste0(fires$latitude, '_',
                                  fires$longitude, '_',
                                  fires$acq_date, '_',
                                  fires$acq_time)),]


# Record successful update:
updated <- read_csv('output-data/dates_of_successfully_acquired_fire_data.csv')
if(length(min(add_fires$acq_date):max(add_fires$acq_date)) == length(unique(add_fires$acq_date))){
  updated_data <- unique(c(min(add_fires$acq_date):max(add_fires$acq_date), read_csv('output-data/dates_of_successfully_acquired_fire_data.csv')$dates))
  write_csv(data.frame(dates = updated_data), 'output-data/dates_of_successfully_acquired_fire_data.csv')

} else {
  stop('Are you missing some dates?')
}

# Export back to fire archive:
write_csv(fires, 'output-data/firms_update.csv')

