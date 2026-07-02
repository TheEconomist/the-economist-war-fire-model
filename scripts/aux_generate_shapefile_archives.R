# Generate shapefiles by day datasets:
# I.e.:
# ru_control = readRDS('source-data/ISW_historical/area_assessed_as_controlled_by_Russia_by_day.RDS')
# ua_counters = readRDS('source-data/ISW_historical/area_counterattacked_by_ukraine_by_day.RDS')
# ru_attacks = readRDS('source-data/ISW_historical/area_attacked_by_russia_by_day.RDS')
# ru_claimed = readRDS('source-data/ISW_historical/area_claimed_as_controlled_by_Russia_by_day.RDS')

library(sf)
library(tidyverse)
library(units)
library(readr)
library(anytime)

default_isw_shapefiles_dir <- '/Users/sondresolstad/Github/ukraine-war-data/output-data/ISW-shapefiles'
isw_shapefiles_dir <- Sys.getenv('ISW_SHAPEFILES_DIR', unset = default_isw_shapefiles_dir)

if (!dir.exists(isw_shapefiles_dir)) {
  stop(paste0('ISW shapefiles directory not found: ', isw_shapefiles_dir))
}

days_with_data <- dir(isw_shapefiles_dir)
days_with_data <- days_with_data[grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', days_with_data)]
days_with_data <- days_with_data[order(as.Date(days_with_data))]

if (length(days_with_data) == 0) {
  stop(paste0('No dated ISW shapefile folders found in: ', isw_shapefiles_dir))
}

# Generate containers and set index
for(j in c("ControlMap", "RussianAdvances", "ClaimedRussian", "ClaimedUkrainian")){
  res <- data.frame()
  ind <- 0

  for(i in sort(days_with_data)){
    files <- dir(file.path(isw_shapefiles_dir, i))
    control <- grep(j, files, value = T)
    if(length(control) != 1){
      message(paste0('Warning: Missing control map for ', i))
      # If missing, default to previous day's file
    } else {
      tryCatch(
        {
          temp <- st_read(file.path(isw_shapefiles_dir, i, control), quiet = T) %>%
            st_transform("EPSG:6381") %>%
            st_zm() %>%
            st_simplify(dTolerance = 100)
        },
        error = function(e) {
          message(paste0("Warning: failed to read shapefile -- see below:
                         ", e$message))
          return(NULL)
        })
    }

    # if(any(c(NA, F) %in% st_is_valid(temp))){
    #   message('Warning: Ignoring invalid geometries')
    #   temp <- st_make_valid(temp)
    #   temp <- temp[st_is_valid(temp), ]
    # }

    # Optional: Simplify geometry
    temp <- st_simplify(temp, dTolerance = 100)

    # Save area to big list (including data name to later get the date)
    temp$data_name <- i
    if(ind == 0){
      big_shp <- temp
    } else {
      big_shp <- rbind(big_shp[, intersect(colnames(big_shp), colnames(temp))],
                       temp[, intersect(colnames(big_shp), colnames(temp))])
    }
    ind <- ind + 1

    print(paste0(j, ' -- --- -- ', i))
  }
  saveRDS(big_shp, paste0("source-data/ISW_historical/", j, '_shapes.RDS'))
}


