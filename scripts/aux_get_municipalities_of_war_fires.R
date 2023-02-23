# This script gets the municipality of fire locations in Ukraine:
library(ggplot2)
library(sf)
library(readr)
library(tidyverse)

# Load fires data:
input <- read_csv('output-data/all_fires_all_cols_exact_pop_and_filter_2022_2023.csv')

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

# Loop through points and get relevant municipality:
check_points$municipality <- NA
for(i in 1:nrow(check_points)){
  res <- check_points[i, ] %>% mutate(
    intersection = as.integer(st_intersects(geometry, ukraine))
    , area = if_else(is.na(intersection), '', ukraine$ADM3_PCODE[intersection])
  )
  check_points$municipality[i] <- res$area[1]
  cat(paste0('\r\r ', round(i/nrow(check_points)*100, 2), '% complete ......... \r\r'))
}

# Merge back in with specific points:
munis <- cbind.data.frame(check_points, unique(unique_locations[, c('x', 'y')]))
munis <- merge(unique_locations, munis, by = c('x', 'y'), all = T)

# Add in covariates to these municipalities:
munis_covars <- as.data.frame(ukraine[, c(3:6, 14:22)])
munis$ADM3_PCODE <- munis$municipality
munis <- merge(munis, munis_covars, by = 'ADM3_PCODE', all.x = T)
munis$geometry.x <- munis$geometry.y <- NULL

# Export:
saveRDS(munis[, ], 'output-data/points_to_municipality_with_extra_cols.RDS')
saveRDS(munis[, c('LATITUDE', 'LONGITUDE', 'municipality')], 'output-data/points_to_municipality.RDS')
