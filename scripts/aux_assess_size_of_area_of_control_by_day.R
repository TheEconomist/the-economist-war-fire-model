# This script ingests ISW shapefiles and returns a csv of assessed Russian control of Ukraine as sq km, %, as well as change in %, %p, and square km
library(tidyverse)
library(sf)

# Generate results data frame
res <- data.frame()

# Define the root directory where your folders are located
root_directory <- '/Users/sondresolstad/Github/ukraine-war-data/output-data/ISW-shapefiles'

# List all subdirectories (folders) in the root directory
date_folders <- list.dirs(root_directory, full.names = TRUE, recursive = FALSE)

# Iterate through the date folders
for (date_folder in date_folders) {
  # List all GeoJSON files in the current date folder
  geojson_files <- list.files(date_folder, pattern = "\\.geojson$", full.names = TRUE)

  # Iterate through the GeoJSON files in the current date folder
  for (geojson_file in geojson_files) {
    # Load the GeoJSON file using sf::st_read
    geojson_data <- st_read(geojson_file, quiet = T)

    # Get date:
    date <- rev(unlist(strsplit(date_folder, '/')))[1]

    # Get area:
    # If there are invalid geometries, try to make them valid
    if (!all(st_is_valid(geojson_data))) {
      geojson_data <- st_make_valid(geojson_data)
    }
    if(all(st_is_valid(geojson_data))){
      area <- sum(st_area(geojson_data))
      res <- rbind(res, data.frame(date = date, area = area, name = rev(unlist(strsplit(geojson_file, '/')))[1]))
    } else {
      res <- rbind(res, data.frame(date = date, area = NA, name = rev(unlist(strsplit(geojson_file, '/')))[1]))
    }
  }
}

# Get type of data from file name:

# Partisan warfare
res$type <- NA
res$type[grep('ReportedUkrainianParti', res$name, ignore.case = T)] <- "Reported Ukrainian partisan warfare"
res$type[grep('UkrainianPartisanWarfare', res$name, ignore.case = T)] <- "Reported Ukrainian partisan warfare"

# Assessed Russian advances
res$type[grep('AssessedRussianAdvance', res$name, ignore.case = T)] <- "Assessed Russian advances"

# Claimed Ukrainian counter-offensives
res$type[grep('ClaimedUkrainianCounter', res$name, ignore.case = T)] <- "Claimed Ukrainian counter-offensive"
res$type[grep('ClaimedUkrainian Counter', res$name, ignore.case = T)] <- "Claimed Ukrainian counter-offensive"
res$type[grep('Claimed_Ukrainian_Counter', res$name, ignore.case = T)] <- "Claimed Ukrainian counter-offensive"
res$type[grep('ClaimedUkrianianCounter', res$name, ignore.case = T)] <- "Claimed Ukrainian counter-offensive"
res$type[grep("ClaimedUkrainianControl", res$name, ignore.case = T)] <- "Claimed Ukrainian counter-offensive" # This correspondence was checked manually

# Claimed Russian territory
res$type[grep('ClaimedRussianTerritory', res$name, ignore.case = T)] <- "Claimed Russian control"
res$type[grep('ClaimedRusianTerritory', res$name, ignore.case = T)] <- "Claimed Russian control"

# Territory assessed as Russia-controlled
res$type[grep('UkraineControlMa', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"
res$type[grep('Ukraine_ControlMa', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"
res$type[grep('UkrainianControlMa', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"
res$type[grep('UkaineControlMa', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"
res$type[grep('RussianAssessedCoTinUkraine', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"
res$type[grep('AssessedRussianControl', res$name, ignore.case = T)] <- "Territory assessed as Russia-controlled"

# Manually fix one corrupted file:
for(i in unique(res$type)){
  res$area[res$date == '2022-12-18' & res$type == i] <- res$area[res$date == '2022-12-17' & res$type == i]
}

# Neaten up formats:
res$date <- as.Date(res$date)
res$area_in_square_km <- as.numeric(res$area)/(1000^2)
res$area_as_percent_of_Ukraine <- 100*res$area_in_square_km/603500

# Generate change variables:
res <- res[order(res$date), ]
res$change_in_square_km <- ave(res$area_in_square_km, res$type, FUN = function(x) x - c(NA, x)[1:length(x)])
res$change_in_percent <- ave(res$area_in_square_km, res$type, FUN = function(x) c(NA, diff(x) / x[-length(x)]) * 100)

# ggplot(res[res$type == 'Territory assessed as Russia-controlled', ], aes(x=date, y=change_in_percent, col=type))+geom_line()
# ggplot(res[res$type == 'Territory assessed as Russia-controlled', ], aes(x=date, y=change_in_square_km, col=type))+geom_line()
# ggplot(res[res$type == 'Territory assessed as Russia-controlled', ], aes(x=date, y=area_in_square_km, col=type))+geom_line()
# ggplot(res, aes(x=date, y=area_in_square_km, col=type))+geom_line()

# Add source
res$source <- 'Institute for the Study of War'

# Export:
write_csv(res, 'output-data/areas_of_control_daily_summary.csv')
