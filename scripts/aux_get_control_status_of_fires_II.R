# Get control status of fires (i.e. whether in Ukraine-controlled territory)
library(sf)
library(readr)

# Load data linking names of files to dates to which they apply:
fires <- read_csv('output-data/all_fires_all_cols_exact_pop_and_filter_2022_2023.csv')
fires$obs_ID <- 1:nrow(fires)

# Load map of Russia and Ukraine
library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
russia_raw <- world[world$geounit == 'Russia', ]
russia_raw <- st_make_valid(russia_raw)
russia_raw <- st_crop(russia_raw, xmin = -20, xmax = 55,
                      ymin = 30, ymax = 60)
ukraine_raw <- world[world$geounit == 'Ukraine', ]
ukraine_raw <- st_make_valid(ukraine_raw)

# Load shapefiles:
ru_control <- readRDS('output-data/area_assessed_as_controlled_by_Russia_by_day.RDS')
ua_counters <- readRDS('output-data/area_counterattacked_by_ukraine_by_day.RDS')
ru_attacks <- readRDS('output-data/area_attacked_by_russia_by_day.RDS')
ru_claimed <- readRDS('output-data/area_claimed_as_controlled_by_Russia_by_day.RDS')

# Ukraine-held is defined as the complement of the intersection of c('in Ukraine') and c('in area of Russian attacks', 'in area of Ukrainian counterattacks', 'in area of Russia control', and 'in area of Russian claimed control'). Since all fires are in Ukraine, that simplifies to the below:
big_shp <- rbind(ru_attacks[, c("geometry", 'date')],
                 ua_counters[, c("geometry", 'date')],
                 ru_control[, c("geometry", 'date')],
                 ru_claimed[, c("geometry", 'date')])

# Loop through dates:
in_ukraine_held_area <- NA
for(i in unique(fires$date)){
  temp_fires <- fires[fires$date == i, ]
  temp_shp <- st_union(big_shp[big_shp$date == i, ])

  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp_shp))
  points <- st_intersection(point.sf, temp_shp)

  if(length(points) > 0){
    fires$in_ukraine_held_area[fires$obs_ID %in% points$obs_ID] <- 0
    fires$in_ukraine_held_area[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 1
  }
  cat(paste0('\r\r\r Checking : ', i, '.....'))
}

# Save results:
fires$obs_ID <- NULL
fires <- fires[, c('LATITUDE', 'LONGITUDE', 'date', 'ACQ_TIME', "in_ukraine_held_area")]
write_csv(fires, 'output-data/control_status_of_fires_II.csv')
