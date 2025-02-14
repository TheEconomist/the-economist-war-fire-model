# Generate training data:

# This script now defines a function, allowing one to call it from other scripts while setting options and to have more memory efficiency
generate_data <- function(year_min = 2015,
                          rounding_para =  readRDS('output-data/model-objects/rounding_para.RDS'), # degree lat/long
                          get_pop_data = T,
                          get_urban_areas = T,
                          get_nightlights = T,
                          get_clouds = T,
                          get_engineered_features = T,
                          get_engineered_features_no_cloud_days = T,
                          get_continuous_fire = T,
                          get_30d_averages = T,
                          update_fires_df = T,
                          save_cache = F,
                          redo = F){

  cat('\n\n Constructing data frame: \n')

  # 0. Import data and packages ------------------------------
  library(tidyverse)
  library(ggplot2)
  library(sf)
  library(lubridate)
  library(anytime)
  library(agtboost)
  library(readxl)
  library(readr)
  library(anytime)
  library(tidyverse)
  library(geosphere)
  library(data.table)
  library(anytime)
  library(lubridate)

  # 1. Load data ------------------------------
  cat('1. Load data\n')

  # Load fire data:
  source('scripts/aux_load_and_merge_fire_data.R') # This loads data as "fire"

  # 2. Reduce resolution ------------------------------
  cat('2. Reduce resolution\n')

  # Select subset of years
  fire <- fire[fire$year >= year_min, ]

  # Generate grid location
  # For fires
  fire$x <- round(fire$LONGITUDE/rounding_para)*rounding_para
  fire$y <- round(fire$LATITUDE/rounding_para)*rounding_para
  fire$id <- paste0(fire$x, '-', fire$y)

  # 3. Generate target grid: --------------------
  cat('3. Generate target grid\n')
  target_grid <- expand.grid('x'= seq(21.9, 40.3,
                                      by = rounding_para),
                             'y'= seq(44.4, 52.4,
                                      by = rounding_para))

  # 4. Move to grid format and generate model variables ------------------------------
  cat('4. Move to grid format\n')

  # Define model variables
  fire$date <- fire$ACQ_DATE
  fire$day <- yday(fire$ACQ_DATE)
  model_vars <- c('x', 'y', 'year', 'time_of_year', 'LONGITUDE', 'LATITUDE', 'ACQ_TIME', 'date')
  fire$time_of_year <- fire$day
  saveRDS('day', 'output-data/model-objects/time_of_year.RDS')

  # Subset relevant columns
  X_fire <- as.data.frame(fire[, model_vars])
  X_fire$geometry <- NULL
  X_fire$fire <- 1

  # Generate no-fire observations
  X_nofire <- expand.grid('x'= seq(21.9, 40.3,
                                   by = rounding_para),
                          'y'= seq(44.4, 52.4,
                                   by = rounding_para),
                          'date' = as.Date(as.Date(paste0(min(X_fire$year), '-01-01')):as.Date(paste0("2023-12-31")),
                                           origin = '1970-01-01'))
  X_nofire$year <- year(X_nofire$date)
  X_nofire$time_of_year <- yday(X_nofire$date)
  X_nofire$date <- NULL
  X_nofire$fire <- 0

  X_fire$id <- paste0(X_fire$x, '-', X_fire$y)
  X_nofire$id <- paste0(X_nofire$x, '-', X_nofire$y)

  # 5. Add population data and restrict to borders of Ukraine------------------------------

  # This also restricts to cells with urban density (i.e. the country of Ukraine, excluding bodies of water)
    if(get_pop_data){
    cat('5. Add population data\n')

    # Data on urban density
    # Source: https://www.worldpop.org/geodata/summary?id=49349
    worldpop_raw <- read_csv('source-data/worldpop/ukr_pd_2020_1km_UNadj_ASCII_XYZ.csv')

    # For urban density
    worldpop <- worldpop_raw
    worldpop$x <- as.numeric(round(worldpop$X/rounding_para)*rounding_para)
    worldpop$y <- as.numeric(round(worldpop$Y/rounding_para)*rounding_para)
    worldpop$id <- paste0(worldpop$x, '-', worldpop$y)
    worldpop$pop_density <- ave(worldpop$Z, worldpop$id, FUN = mean)
    worldpop <- worldpop[!duplicated(worldpop$id), ]

    saveRDS(worldpop[!is.na(worldpop$pop_density),
                     c('x', 'y', 'id')], 'output-data/model-objects/ukraine_mask.RDS')

    X_fire <- X_fire[X_fire$id %in% worldpop$id, ]
    X_nofire <- X_nofire[X_nofire$id %in% worldpop$id, ]

    X_fire <- merge(X_fire, worldpop[, c('id', 'pop_density')], by = 'id')
    X_nofire <- merge(X_nofire, worldpop[, c('id', 'pop_density')], by = 'id')

    X_nofire <- X_nofire[!is.na(X_nofire$pop_density), ]
    X_fire <- X_fire[!is.na(X_fire$pop_density), ]
    model_vars <- c(model_vars, 'pop_density')
  } else {
    cat('5. Add population data - skipped\n')

    mask <- readRDS('output-data/model-objects/ukraine_mask.RDS')
    X_fire <- X_fire[X_fire$id %in% mask$id, ]
    X_nofire <- X_nofire[X_nofire$id %in% mask$id, ]
  }

  if(get_urban_areas){
    cat(' - Add data on urban areas...\n')

    # Data on urban areas
    # Source: https://earthworks.stanford.edu/catalog/stanford-yk247bg4748
    urban_raw <- st_read('source-data/urban-areas/ne_10m_urban_areas_landscan.shp', quiet =T)
    urban_raw <- urban_raw[urban_raw$min_bb_yma > 44 & urban_raw$min_bb_xma > 22 &
                             urban_raw$max_bb_yma < 52 & urban_raw$max_bb_xma < 40, ]

    # Add urban area information:
    X_fire$obs_ID <- 1:nrow(X_fire)
    urban <- urban_raw
    urban <- st_make_valid(urban)
    st_crs(urban) <- 'EPSG:6383'
    urban <- st_transform(urban, crs = 'EPSG:6383')
    urban <- suppressWarnings(suppressMessages(st_buffer(urban, dist = 10000)))
    urban <- st_transform(urban, crs = "EPSG:4326")

    sf::sf_use_s2(FALSE)
    point.sf <- st_as_sf(X_fire, coords = c("LONGITUDE", "LATITUDE"))
    st_crs(point.sf) <- st_crs(urban)

    X_fire$city <- NA
    urban <- urban[order(urban$max_pop_al), ] # Order by total population
    cat('\nEstablishing region of fire events.\n\nCompleted for: ')
    for(i in 1:nrow(urban)){

      urban_temp <- urban[i, ]

      st_crs(point.sf) <- st_crs(urban_temp)
      points_in_urban_area <- suppressWarnings(suppressMessages(st_intersection(point.sf, urban_temp)))

      if(length(points_in_urban_area$obs_ID) > 0){
      X_fire$city[X_fire$obs_ID %in% points_in_urban_area$obs_ID] <- as.character(urban_temp$name_conv)[1]
      }
    }
    cat('\n')

  # Fix a few city names:
  X_fire$city[X_fire$city == 'Donetsk, Donetsk'] <- 'Donetsk'
  X_fire$city[X_fire$city == 'Makiyivka, Donetsk'] <- 'Donetsk'
  X_fire$city[X_fire$city == 'Odessa2'] <- 'Odessa'
  }

  # Generate dummy
  X_fire$in_urban_area <- is.na(X_fire$city)
  X_fire <- X_fire[order(X_fire$date), ]

  if(update_fires_df){
    # Save and export these files:
    write_csv(X_fire[X_fire$date >= as.Date('2022-02-24'), ], 'output-data/model-objects/all_fires.csv')
    cat('\n ->> Updated fires data saved to "output-data/model-objects/all_fires.csv"\n\n')
  }

  # 6. Combine fire and no-fire observations------------------------------
  cat('6. Combine fire and no-fire observations\n')

  for(i in setdiff(colnames(X_fire), colnames(X_nofire))){
    X_nofire[, i] <- NA
  }

  X <- rbind(X_nofire, X_fire)
  rm(X_fire)
  rm(X_nofire)

  # Generate ID variables
  X$id_w_time <- paste0(X$x, '-', X$y, '-', X$year, '-', X$time_of_year)
  X$id <- paste0(X$x, '-', X$y)
  X$id_5x5 <- paste0(round(X$x*2), '-', round(X$y*2))

  # Make fires the sum of fires in area in that time interval
  X$fire <- ave(X$fire, X$id_w_time, FUN = sum)

  # Deduplicate data set
  X <- X[!duplicated(X$id_w_time), ]

  # Save cache at this stage:
  if(save_cache){
    saveRDS(X, 'output-data/X_temp.RDS')
    X <- readRDS('output-data/X_temp.RDS')
  }
  # Fix encoding errors
  X$x <- as.numeric(as.character(X$x))
  X$y <- as.numeric(as.character(X$y))

  # Generate date column
  X$date <- as.Date(X$time_of_year-1, origin = paste0(X$year, '-01-01'))
  if(save_cache){
    saveRDS(X, 'output-data/X_matrix.RDS') # Save cache
  }

  # 7. Add nightlights data ------------------------------
  if(get_nightlights){
    cat('7. Add nightlights data\n')

    # source('scripts/aux_get_nightlights_data.R')
    nightlights <- readRDS('output-data/nightlights.RDS')
    nightlights <- nightlights[nightlights$id %in% X$id, ]

    X <- merge(X, nightlights[, c('id', 'nightlights')], by = 'id', all.x = T)
  } else {cat('7. Add nightlights data - skipped\n')}

  # 8. Add cloud data ------------------------------
  if(get_clouds){
  cat('8. Add cloud data\n')

    # The script to process cloud image TIFFs to RDS files is: source('scripts/aux_process_cloud_tiffs.R'). Running that script generates the data loaded in the next two lines.
  processed_cloud_files <- dir('source-data/cloud-data-processed')
  cloud_data <- rbindlist(lapply(processed_cloud_files, FUN = function(i) readRDS(paste0('source-data/cloud-data-processed/', i))), fill = T)
  cloud_data$date <- as.Date(cloud_data$date)

  # Cloud observations are all 0, and thus not assigned a point. We take missing points in the cloud data as indicating clouds:
  X$cloud <- !paste0(X$date, '-', X$id) %in% paste0(cloud_data$date, '-', cloud_data$id)

  # If we do not have cloud data for date, assign NA
  dates <- as.Date(unlist(strsplit(processed_cloud_files, '.RDS')))
  X$cloud[!X$date %in% dates] <- NA

  saveRDS(X, 'output-data/X_matrix.RDS') # Save cache

  temp <- X[, c('date', 'cloud')]
  temp$ave_cloud_cover <- ave(X$cloud, X$date, FUN = function(x) mean(x, na.rm = T))
  temp <- temp[!duplicated(temp$date), ]
  temp <- temp[order(temp$date), ]
  temp$cloud <- NULL
  ggplot(na.omit(temp), aes(x=date, y=ave_cloud_cover))+geom_line()

  write_csv(temp, 'output-data/ave_cloud_cover.csv')
  } else {cat('8. Add cloud data - skipped\n')}

  # 9. Feature-engineering on fire data in grid format------------------------------

  # Define 5-year moving average function:
  ma <- function(x, n = 5){if(length(x) < n){rep(mean(x, na.rm = T), length(x))} else {stats::filter(x, rep(1 / n, n), sides = 1)}}

  if(get_engineered_features){
  cat('9. Feature-engineering on fire data in grid format\n')

  cat('Generate fire in month, area, and area and month\n')
  # Define area:
  X$id_5x5 <- paste0(round(X$x*2), '-', round(X$y*2))

  # Actual fire in month:
  X$month <- month(X$date)
  X$fire_in_month <- ave(X$fire, paste0(X$id, '-', X$month, '-', X$year),
                         FUN = function(x) sum(x, na.rm = T))

  # Actual fires in 5x5 cell:
  X$fire_5x5 <- ave(X$fire, paste0(X$id_5x5, '-', X$time_of_year, '-', X$year),
                    FUN = function(x) sum(x, na.rm = T))
  X$fire_in_month_5x5 <- ave(X$fire, paste0(X$id_5x5, '-', X$month, '-', X$year),
                             FUN = function(x) sum(x, na.rm = T))

  cat('Generate mean number of fires in the cell in the past:')
  X$average_past_fires_by_1x1_location <-
    ave(X$fire, X$id, FUN = function(x){
      c(NA, cumsum(x)/1:length(x))[1:length(x)]
    })

  # To inspect:
  if(inspect){
    ggplot(X[X$time_of_year %in% 30:90 &
               X$id %in% sample(X$id, 9) &
               X$year == 2020, ], aes(x=date, y=average_past_fires_by_1x1_location, col = 'average past'))+
      geom_line()+geom_line(aes(y=fire/10, col = 'fires'))+facet_wrap(.~id)+scale_y_continuous(trans = 'pseudo_log')
  }

  cat('Generate mean number of fires in the cell in the past in same time of year:')
  X$average_past_fires_by_1x1_location_time_of_year <-
    ave(X$fire, paste0(X$id, X$time_of_year), FUN = ma)

  # To inspect:
  if(inspect){
    ggplot(X[X$id %in% sample(X$id, 9), ],
           aes(x=date, y=average_past_fires_by_1x1_location_time_of_year, col='average past time-of-year'))+
      geom_line()+geom_line(aes(y=fire, col ='obs'))+facet_wrap(.~id)
  }

  cat('Generate mean number of fires in groups of cells in the past in same time of year:')

  temp <- X
  temp <- temp[!duplicated(paste0(temp$id_5x5, '-', temp$date)), ]
  temp <- temp[order(temp$date), ]
  temp$average_past_fires_by_5x5_location_time_of_year <-
    ave(temp$fire_5x5, paste0(temp$id_5x5, temp$time_of_year), FUN = ma)

  X$average_past_fires_by_5x5_location_time_of_year <- NULL
  X <- merge(X, temp[, c('date', 'id_5x5', 'average_past_fires_by_5x5_location_time_of_year')],
             by = c('date', 'id_5x5'), na.rm = T)
  X <- X[order(X$date), ]
  rm(temp)

  # To inspect:
  if(inspect){
    ggplot(X[X$id %in% sample(X$id, 9), ],
           aes(x=date, y=average_past_fires_by_5x5_location_time_of_year, col='average past time-of-year'))+
      geom_line()+geom_line(aes(y=fire_5x5, col ='obs'))+facet_wrap(.~id)
  }

  cat('Then by month')
  temp <- X
  temp <- temp[!duplicated(paste0(temp$month, '_', temp$year, '_', temp$id)), ]

  cat('Generate mean number of fires in the cell in the past in same month of year:')
  temp$average_past_fires_by_1x1_location_month_of_year <-
    ave(temp$fire_in_month, paste0(temp$id, '_', temp$month), FUN = ma)

  X$average_past_fires_by_1x1_location_month_of_year <- NULL
  X <- merge(X, temp[, c('id', 'month', 'year', 'average_past_fires_by_1x1_location_month_of_year')], by = c('id', 'month', 'year'), all.x = T)
  X <- X[order(X$date), ]
  rm(temp)

  cat('Generate mean number of fires in groups of cells in the past in same month of year:')
  temp <- X
  temp <- temp[!duplicated(paste0(temp$month, '_', temp$year, '_', temp$id)), ]

  temp <- temp[!duplicated(paste0(temp$id_5x5, '-', temp$date)), ]
  temp$average_past_fires_by_5x5_location_month_of_year <-
    ave(temp$fire_in_month_5x5, paste0(temp$id, '_', temp$month), FUN = ma)
  X$average_past_fires_by_5x5_location_month_of_year <- NULL
  X <- merge(X, unique(temp[, c('id_5x5', 'month', 'year', 'average_past_fires_by_5x5_location_month_of_year')]), by = c('id_5x5', 'month', 'year'), all.x = T)
  X <- X[order(X$date), ]
  rm(temp)

  # To inspect:
  if(inspect){
    ggplot(X[X$id %in% unique(X$id)[1:9], ],
           aes(x=date, y=average_past_fires_by_1x1_location_month_of_year, col='average past time-of-year'))+
      geom_line()+geom_line(aes(y=fire_in_month, col ='obs'))+facet_wrap(.~id)

    ggplot(X[X$id %in% unique(X$id)[1:9], ],
           aes(x=date, y=average_past_fires_by_5x5_location_month_of_year, col='average past time-of-year'))+
      geom_line()+geom_line(aes(y=fire_in_month_5x5, col ='obs'))+facet_wrap(.~id)
  }

  if(save_cache){
    saveRDS(X, 'output-data/X_matrix.RDS') # Save cache
  }
  } else {cat('9. Feature-engineering on fire data in grid format - skipped\n')}

  # 10. Feature-engineering on fire data in grid format, excluding cloud days ------------------------------
  if(get_engineered_features_no_cloud_days){
    cat('10. Feature-engineering on fire data in grid format, excluding cloud days\n')

    X <- X[order(X$date), ]

    cat('Generate mean number of fires in the cell in the past:\n')
    X$average_past_fires_by_1x1_location_no_clouds <-
      ave(X$fire, X$id, FUN = function(x){
        c(NA, cumsum(x)/1:length(x))[1:length(x)]
      })

    cat('Generate mean number of fires in the cell in the past in same time of year:\n')
    X$average_past_fires_by_1x1_location_time_of_year_no_clouds <-
      ave(X$fire, paste0(X$id, X$time_of_year), FUN = function(x){ma(x)})

    cat('Generate mean number of fires in groups of cells in the past in same time of year:\n')
    X$average_past_fires_by_5x5_location_time_of_year_no_clouds <- NA
    X$id_5x5 <- paste0(round(X$x*2), '-', round(X$y*2))
    X$average_past_fires_by_5x5_location_time_of_year_no_clouds <-
      ave(X$fire, paste0(X$id_5x5, X$time_of_year), FUN = function(x){
        sum(x, na.rm = T)
      })
    X$average_past_fires_by_5x5_location_time_of_year_no_clouds <-
      ave(X$average_past_fires_by_5x5_location_time_of_year_no_clouds, paste0(X$id, X$time_of_year), FUN = ma)

    cat('Then by month\n')
    X$month <- month(X$date)
    temp <- X
    temp$fire <- ave(temp$fire, paste0(temp$month, '_', temp$year, '_', temp$id), FUN = function(x) sum(x, na.rm = T))
    temp <- temp[!duplicated(paste0(temp$month, '_', temp$year, '_', temp$id)), ]

    cat('Generate mean number of fires in the cell in the past in same month of year:\n')
    temp$average_past_fires_by_1x1_location_month_of_year_no_clouds <-
      ave(temp$fire, paste0(temp$id, temp$month), FUN = ma)

    cat('Generate mean number of fires in groups of cells in the past in same month of year:\n')
    temp$average_past_fires_by_5x5_location_month_of_year_no_clouds <- NA
    temp$id_5x5 <- paste0(round(temp$x*2), '-', round(temp$y*2))
    temp$average_past_fires_by_5x5_location_month_of_year_no_clouds <-
      ave(temp$fire, paste0(temp$id_5x5, temp$month), FUN = function(x){
        sum(x, na.rm = T)
      })
    temp$average_past_fires_by_5x5_location_month_of_year_no_clouds <-
      ave(temp$average_past_fires_by_5x5_location_month_of_year_no_clouds, paste0(temp$id, temp$month), FUN = ma)

    X$average_past_fires_by_1x1_location_month_of_year_no_clouds <- NULL
    X$average_past_fires_by_5x5_location_month_of_year_no_clouds <- NULL
    X <- merge(X, temp[, c('id', 'month', 'year', 'average_past_fires_by_1x1_location_month_of_year_no_clouds', 'average_past_fires_by_5x5_location_month_of_year_no_clouds')], by = c('id', 'month', 'year'), all.x = T)
    X <- X[order(X$date), ]
    rm(temp)

    X$fire <- X$fire_raw
    X$fire_raw <- NULL

    if(save_cache){
      saveRDS(X, 'output-data/X_matrix.RDS') # Save cache
    }
  } else {cat('10. Feature-engineering on fire data in grid format, excluding cloud days - skipped\n')}

  # 11. Generate average cloud cover variables ------------------------------
  if(get_clouds){
    cat('11. Generate average cloud cover variables\n')
    X <- X[order(X$date), ]

    # For small number of missing cloud days, impute mean:
    X$cloud <- as.numeric(X$cloud)
    X$cloud[is.na(X$cloud)] <- mean(X$cloud, na.rm = T)

    # Get average clouds in area
    X$cloud_5x5 <- ave(X$cloud, paste0(X$id_5x5, X$date), FUN = function(x) mean(x, na.rm = T))

    # Get clouds in month
    X$cloud_in_month <- ave(X$cloud, paste0(X$id, '-', X$month, '-', X$year),
                            FUN = function(x) mean(x, na.rm = T))
    X$cloud_in_month_5x5 <- ave(X$cloud, paste0(X$id_5x5, '-', X$month, '-', X$year),
                                FUN = function(x) mean(x, na.rm = T))


    cat('Generate mean number of clouds in the cell in the past in same time of year:\n')
    X$average_past_clouds_by_1x1_location_time_of_year <-
      ave(X$cloud, paste0(X$id, X$time_of_year), FUN = ma)


    cat('Generate mean number of clouds in groups of cells in the past in same time of year:\n')
    X$average_past_clouds_by_5x5_location_time_of_year <- NA
    X$id_5x5 <- paste0(round(X$x*2), '-', round(X$y*2))
    X$average_past_clouds_by_5x5_location_time_of_year <-
      ave(X$cloud, paste0(X$id_5x5, X$time_of_year), FUN = ma)
    X$average_past_clouds_by_5x5_location_time_of_year <-
      ave(X$average_past_clouds_by_5x5_location_time_of_year, paste0(X$id, X$time_of_year), FUN = ma)

    cat('Then by month')
    X$month <- month(X$date)
    temp <- X
    temp$cloud <- ave(temp$cloud, paste0(temp$month, '_', temp$year, '_', temp$id), FUN = function(x) mean(x, na.rm = T))
    temp <- temp[!duplicated(paste0(temp$month, '_', temp$year, '_', temp$id)), ]

    cat('Generate mean number of clouds in the cell in the past in same month of year:\n')
    temp$average_past_clouds_by_1x1_location_month_of_year <-
      ave(temp$cloud, paste0(temp$id, temp$month), FUN = ma)

    cat('Generate mean number of clouds in groups of cells in the past in same month of year:\n')
    temp$average_past_clouds_by_5x5_location_month_of_year <- NA
    temp$id_5x5 <- paste0(round(temp$x*2), '-', round(temp$y*2))
    temp$average_past_clouds_by_5x5_location_month_of_year <-
      ave(temp$cloud, paste0(temp$id_5x5, temp$month), FUN = function(x){
        mean(x, na.rm = T)
      })
    temp$average_past_clouds_by_5x5_location_month_of_year <-
      ave(temp$average_past_clouds_by_5x5_location_month_of_year, paste0(temp$id, temp$month), FUN = ma)

    X$average_past_clouds_by_1x1_location_month_of_year <- NULL
    X$average_past_clouds_by_5x5_location_month_of_year <- NULL
    X <- merge(X, temp[, c('id', 'month', 'year', 'average_past_clouds_by_1x1_location_month_of_year', 'average_past_clouds_by_5x5_location_month_of_year')], by = c('id', 'month', 'year'), all.x = T)
    X <- X[order(X$date), ]
    rm(temp)

  } else {cat('11. Generate average cloud cover variables - skipped\n')}

  # 12. Generate continuous fire in cell variables ------------------------------
  if(get_continuous_fire){
    cat('12. Get continuous fire in cell variables\n')
    X <- X[order(X$date), ]

    X$fire_raw <- X$fire
    X$fire_binary <- X$fire
    X$fire_binary[X$fire_binary != 0] <- 1

    X$continuous_length_of_any_fire_in_cell <-
      ave(X$fire_binary, X$id, FUN = function(x){
        ave(c(0, x[-length(x)]), cumsum(c(0, x[-length(x)]) == 0), FUN = cumsum)
      })

    X$fire_raw <- X$fire
    X$fire[X$cloud == 1] <- 0

    X$fire_binary <- X$fire
    X$fire_binary[X$fire_binary != 0] <- 1

    X$continuous_length_of_any_fire_in_cell_no_clouds <-
      ave(X$fire_binary, X$id, FUN = function(x){
        ave(c(0, x[-length(x)]), cumsum(c(0, x[-length(x)]) == 0), FUN = cumsum)
      })

    X$fire <- X$fire_raw
    } else {cat('12. Get continuous fire in cell variables - skipped\n')}

  # 13. Generate 30-day averages ------------------------------
  if(get_30d_averages){
    cat('13. Get 30-day averages')
    X <- X[order(X$date), ]

    ma <- function(x, n = 30){stats::filter(x, rep(1 / n, n), sides = 1)}

    for(i in c("continuous_length_of_any_fire_in_cell",
               "average_past_fires_by_1x1_location_time_of_year",
               "continuous_length_of_any_fire_in_cell_no_clouds",
               "average_past_fires_by_1x1_location_time_of_year_no_clouds",
               "average_past_clouds_by_1x1_location_time_of_year",
               "fire", 'fire_5x5',
               "cloud", 'cloud_5x5')){
      cat(i)
      cat('\n\n')
      X[, paste0(i, '_30da')] <- as.numeric(ave(X[, i], X$id, FUN = ma))
    }

    for(i in c("average_past_fires_by_5x5_location_time_of_year", "average_past_fires_by_5x5_location_time_of_year_no_clouds",
               "average_past_clouds_by_5x5_location_time_of_year")){
      cat(i)
      cat('\n\n')
      X[, paste0(i, '_30da')] <- as.numeric(ave(X[, i], X$id_5x5, FUN = ma))
    }
  } else {cat('13. Get 30-day averages - skipped\n')
}

  rm(fire, envir = globalenv())
  cat('\nCompleted.\n\n')
  return(X)
}
