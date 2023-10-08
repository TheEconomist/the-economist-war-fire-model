# This script gets the municipality of fire locations in Ukraine:
library(ggplot2)
library(sf)
library(readr)
library(tidyverse)

# Load fires data:
input <- read_csv('output-data/ukraine_war_fires.csv')

# Load map of Ukrainian municipalities:
# https://data.humdata.org/dataset/cod-ab-ukr?
ukraine_raw <- st_read('source-data/ukraine-detailed-map/ukr_admbnda_adm3_sspe_20230201.shp')

# Transform map to be a bit simpler to work with:
library(sf)
ukraine <- st_transform(ukraine_raw, "WGS84") #azimuthal equidistant

# Get list of locations to check:
unique_locations <- unique(input[, c('LATITUDE', 'LONGITUDE')])
unique_locations$y <- round(unique_locations$LATITUDE, 3)
unique_locations$x <- round(unique_locations$LONGITUDE, 3)
check_points <- unique(unique_locations[, c('x', 'y')])
check_points <- st_as_sf(check_points, coords = c('x', 'y'), crs = st_crs(ukraine))

# Transform into good projection for Ukraine:
check_points <- st_transform(check_points, crs = "EPSG:6381")
ukraine <- st_transform(ukraine, crs = "EPSG:6381")

# Load cache:
known_points <- readRDS('output-data/model-objects/points_to_municipalities.RDS')

# Loop through points and get relevant municipality:
check_points <- check_points[!check_points$geometry %in% known_points$geometry, ]

if(nrow(check_points) > 0){
  check_points$municipality <- NA
  for(i in 1:nrow(check_points)){
    res <- check_points[i, ] %>% mutate(
      intersection = as.integer(st_intersects(geometry, ukraine))
      , area = if_else(is.na(intersection), '', ukraine$ADM3_PCODE[intersection])
    )
    check_points$municipality[i] <- res$area[1]
    cat(paste0('\r\r ', round(i/nrow(check_points)*100, 2), '% complete ......... \r\r'))
  }

  known_points <- rbind(check_points, known_points)
}

# Save cache
saveRDS(known_points, 'output-data/model-objects/points_to_municipalities.RDS')

# Merge back in with specific points
known_points <- st_transform(known_points, "WGS84")
known_points <- cbind.data.frame(known_points, st_coordinates(known_points))
known_points$geometry <- NULL
known_points$X <- round(known_points$X, 3) # R being R...
known_points$Y <- round(known_points$Y, 3) # R being R...
locations <- merge(data.frame(unique_locations),
                   known_points[!is.na(known_points$municipality), ],
                   by.x=c('x', 'y'),
                   by.y=c('X', 'Y'), all.x = T)

# Add in covariates to these municipalities:
munis_covars <- as.data.frame(ukraine[, c(3:6, 14:22)])
locations$ADM3_PCODE <- locations$municipality
locations <- merge(locations, munis_covars, by = 'ADM3_PCODE', all.x = T)
locations$geometry <- NULL
locations <- merge(locations, input[, c('LATITUDE', 'LONGITUDE', 'year', 'ACQ_TIME', 'date', 'pop_exact')], by=c('LATITUDE', 'LONGITUDE'), all.y=T)
locations$x <- NULL
locations$y <- NULL

# Export:
saveRDS(locations, 'output-data/points_to_municipality_with_extra_cols.RDS')
write_csv(locations, 'output-data/war_fires_by_ADM3.csv')
