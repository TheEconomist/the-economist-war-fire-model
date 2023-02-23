# This script assesses whether a given fire event is taking place in Russia-held territory, and within certain km from the border of such territory.

library(sf)
library(readr)

# Load data linking names of files to dates to which they apply:
dates_and_names <- read_csv("output-data/area_assessed_as_controlled_by_Russia.csv")

# Load data to assess:
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

# Define baseline:
fires$in_russia_held_area <- NA
fires$in_russia_held_area_plus_25 <- NA
fires$in_russia_held_area_plus_50 <- NA
fires$in_russia_held_area_plus_100 <- NA
fires$in_russia_held_area_minus_25 <- NA
fires$in_russia_held_area_minus_50 <- NA
fires$in_russia_held_area_minus_100 <- NA

for(i in dir('source-data/Ukraine_Conflict_Mapping_ARCGIS_Shapefiles/Russian_CoT_in_Ukraine_Shapefiles/')){

  # Get corresponding name
  temp_date <- dates_and_names$date[dates_and_names$name == i]

  # Generate temporary folder
  unlink("source-data/temp", recursive=TRUE)
  dir.create('source-data/temp')

  # Unzip shapefile:
  zipF <- paste0("source-data/Ukraine_Conflict_Mapping_ARCGIS_Shapefiles/Russian_CoT_in_Ukraine_Shapefiles/", i)
  outDir <- "source-data/temp"
  unzip(zipF, exdir=outDir)

  # Load shapefile
  temp_shp <- grep('shp', dir('source-data/temp/'), value = T)[1]
  temp <- st_read(paste0('source-data/temp/', temp_shp))
  temp  <- st_make_valid(temp)
  temp_raw <- temp

  # Subset fires to relevant date:
  temp_fires <- fires[fires$date == temp_date, ]

  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
  fires$in_russia_held_area[fires$obs_ID %in% points$obs_ID] <- 1
  fires$in_russia_held_area[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  temp <- st_buffer(temp, dist = 25000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_plus_25[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_plus_25[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  temp <- st_buffer(temp_raw, dist = 50000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_plus_50[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_plus_50[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  temp <- st_buffer(temp_raw, dist = 100000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_plus_100[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_plus_100[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  # Merge in Russia to avoid second "front line" near the Russian border:

  # First generate black sea coast polygon:
  azov <- st_as_sf(data.frame('LONGITUDE' = 38.42, 'LATITUDE' = 40.80), coords = c("LONGITUDE", "LATITUDE"))
  st_crs(azov) <- st_crs("WGS84")
  azov <- st_transform(azov, st_crs(temp))
  azov <- st_buffer(azov, dist = 1250000)
  azov <- st_transform(azov, st_crs(ukraine_raw))
  azov <- st_difference(azov, ukraine_raw)
  azov <- st_transform(azov, st_crs(temp))

  # Then enter in Russia
  russia <- st_transform(russia_raw, st_crs(temp_raw))
  russia <- st_buffer(russia, dist = 5000)
  temp <- st_union(temp_raw, russia)
  azov <- st_transform(azov, crs = st_crs(temp_raw))
  azov <- st_buffer(azov, dist = 500)
  temp <- st_union(temp, azov)
  temp <- st_combine(temp)
  temp <- st_simplify(temp, preserveTopology = FALSE, dTolerance = 15000)

  temp <- st_buffer(temp, dist = -25000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_minus_25[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_minus_25[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  # First generate black sea coast polygon:
  azov <- st_as_sf(data.frame('LONGITUDE' = 38.42, 'LATITUDE' = 40.80), coords = c("LONGITUDE", "LATITUDE"))
  st_crs(azov) <- st_crs("WGS84")
  azov <- st_transform(azov, st_crs(temp))
  azov <- st_buffer(azov, dist = 1250000)
  azov <- st_transform(azov, st_crs(ukraine_raw))
  azov <- st_difference(azov, ukraine_raw)
  azov <- st_transform(azov, st_crs(temp))

  # Then enter in Russia
  russia <- st_transform(russia_raw, st_crs(temp_raw))
  russia <- st_buffer(russia, dist = 5000)
  temp <- st_union(temp_raw, russia)
  azov <- st_transform(azov, crs = st_crs(temp_raw))
  azov <- st_buffer(azov, dist = 500)
  temp <- st_union(temp, azov)
  temp <- st_combine(temp)
  temp <- st_simplify(temp, preserveTopology = FALSE, dTolerance = 15000)

  temp <- st_buffer(temp, dist = -50000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_minus_50[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_minus_50[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }

  # First generate black sea coast polygon:
  azov <- st_as_sf(data.frame('LONGITUDE' = 38.42, 'LATITUDE' = 40.80), coords = c("LONGITUDE", "LATITUDE"))
  st_crs(azov) <- st_crs("WGS84")
  azov <- st_transform(azov, st_crs(temp))
  azov <- st_buffer(azov, dist = 1250000)
  azov <- st_transform(azov, st_crs(ukraine_raw))
  azov <- st_difference(azov, ukraine_raw)
  azov <- st_transform(azov, st_crs(temp))

  # Then enter in Russia
  russia <- st_transform(russia_raw, st_crs(temp_raw))
  russia <- st_buffer(russia, dist = 5000)
  temp <- st_union(temp_raw, russia)
  azov <- st_transform(azov, crs = st_crs(temp_raw))
  azov <- st_buffer(azov, dist = 500)
  temp <- st_union(temp, azov)
  temp <- st_combine(temp)
  temp <- st_simplify(temp, preserveTopology = FALSE, dTolerance = 15000)

  temp <- st_buffer(temp, dist = -100000)
  sf::sf_use_s2(FALSE)
  point.sf <- st_as_sf(temp_fires, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(point.sf) <- "WGS84"
  point.sf <- st_transform(point.sf, st_crs(temp))
  points <- st_intersection(point.sf, temp)

  if(length(points) > 0){
    fires$in_russia_held_area_minus_100[fires$obs_ID %in% points$obs_ID] <- 1
    fires$in_russia_held_area_minus_100[fires$obs_ID %in% setdiff(temp_fires$obs_ID, points$obs_ID)] <- 0
  }
}

# Generate categories:
fires$position <- NA
fires$position[fires$in_russia_held_area == 1] <- "Russia-held area"
fires$position[fires$in_russia_held_area == 1 & fires$in_russia_held_area_minus_25 == 1] <- "Russia-held area, <25km from front line"
fires$position[fires$in_russia_held_area == 1 & fires$in_russia_held_area_minus_50 == 1] <- "Russia-held area, <50km from front line"
fires$position[fires$in_russia_held_area == 1 & fires$in_russia_held_area_minus_100 == 1] <- "Russia-held area, <100km from front line"
fires$position[fires$in_russia_held_area == 0] <- "Ukraine-held area"
fires$position[fires$in_russia_held_area == 0 & fires$in_russia_held_area_plus_100 == 1] <- "Ukraine-held area, <100km from front line"
fires$position[fires$in_russia_held_area == 0 & fires$in_russia_held_area_plus_50 == 1] <- "Ukraine-held area, <50km from front line"
fires$position[fires$in_russia_held_area == 0 & fires$in_russia_held_area_plus_25 == 1] <- "Ukraine-held area, <25km from front line"

# Save results:
fires$obs_ID <- NULL
fires <- fires[, c('LATITUDE', 'LONGITUDE', 'date', 'ACQ_TIME', "in_russia_held_area", "in_russia_held_area_plus_25",
                   "in_russia_held_area_plus_50", "in_russia_held_area_plus_100", "in_russia_held_area_minus_25",
                   "in_russia_held_area_minus_50", "in_russia_held_area_minus_100", "position")]
write_csv(fires, 'output-data/control_status_of_fires.csv')
