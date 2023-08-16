# This script defines the war fire classifier function
war_fire_classifier <- function(cell_day_data, # Data frame of cells with number of fires in each one on a given day
                                cell_day_predictions, # Data frame of cells with predicted number of fires in each on a given day
                                fires = fire_location_time_data, # Data frame of fires with the time and location of each one
                                offset = 2, # Parameter: Fires must be at least 'offset' in excess of predictions
                                offset_months = 1:12, # Parameter: Offset should only be used for these months
                                min_length_of_fire_in_area = 7, # Parameter: For a location to have war events, excess fires must be detected at least this many days apart within a given 365 day interval.
                                days_to_assign_to_war_fire_after_excess = 10, # Parameter: For how many days after a large war fire event should fire events be assigned to the 'war' category.
                                exclude, # Parameter (optional): Data frame of events locations (defined by lat1, lat2, lng1, lng2, start_date, end_date), which have been established as not war-related
                                exclude_dates # Parameter (optional): vector of dates to exclude due to extreme heat
){

  # Merge cell data and cell predictions
  cells <- merge(cell_day_data, unique(cell_day_predictions[, c('id', 'time_of_year', 'year', 'predicted_fire')]), by= c('id', 'time_of_year', 'year'))

  # Set offset in relevant months
  library(lubridate)
  cells$month <- month(cells$date)
  for(i in offset_months){
    cells$offset[cells$month == i] <- offset
  }

  # Exclude fires if identified as not-war-related:
  if(!missing(exclude)){
    exclude$start_date <- as.Date(exclude$start_date, format = "%d/%m/%Y")
    exclude$end_date <- as.Date(exclude$end_date, format = "%d/%m/%Y")
    if(any(is.na(c(exclude$end_date, exclude$start_date))) | any(!c(exclude$end_date, exclude$start_date) %in% as.Date(as.Date('2022-01-01'):as.Date('2030-01-01'), origin = '1970-01-01'))){
      print(exclude)
      stop("Manually added forest fire locations have a date issue, please inspect manually.")
    }
    for(i in 1:nrow(exclude)){
      cells$fire[cells$x >= min(exclude[i, c('lng1', 'lng2')]) &
                   cells$x <= max(exclude[i, c('lng1', 'lng2')]) &
                   cells$y >= min(exclude[i, c('lat1', 'lat2')]) &
                   cells$y <= max(exclude[i, c('lat1', 'lat2')]) &
                   cells$date >= exclude$start_date[i] &
                   cells$date <= exclude$end_date[i]] <- 0
    }
  }

  # Exclude fires on extreme heat days
  if(!missing(exclude_dates)){

    print(paste0(sum(cells$fire[as.numeric(cells$date) %in% as.numeric(exclude_dates)]), ' fires on extreme heat days excluded from the analysis'))
    cells$fire[as.numeric(cells$date) %in% as.numeric(exclude_dates)] <- 0
  }

  # Calculate excess fires
  cells$excess_fires <- cells$fire - cells$predicted_fire - cells$offset

  # Do not consider deficit fires:
  cells$excess_fire[cells$excess_fire < 0] <- 0
  cells$excess_fire <- ceiling(cells$excess_fire)

  # Generate cell-day ID
  fires$id_w_time <- paste0(fires$id, '-', fires$time_of_year, '-', fires$year)
  cells$id_w_time <- paste0(cells$id, '-', cells$time_of_year, '-', cells$year)

  # Merge excess fire information into fire-location-time data set:
  cells$fire_in_window <- cells$fire
  fires <- merge(fires, cells[, c('excess_fire', 'id_w_time', 'predicted_fire', 'fire_in_window')], by='id_w_time')

  # Set war fire tag to default:
  fires$war_fire <- NA
  fires$war_fire[fires$excess_fire == 0] <- 0

  # Loop cells and distribute excess fires
  cat('\n Loop through cells to distribute excess fires...\n')

  ind <- 0
  total <- length(unique(fires$id_w_time[fires$excess_fire > 0]))
  for(i in unique(fires$id_w_time[fires$excess_fire > 0])){
    ind <- ind + 1
    start <- Sys.time()
    temp_vector <- fires$excess_fire[fires$id_w_time == i]
    if(temp_vector[1] > length(temp_vector)){
      fires$war_fire[fires$id_w_time == i] <- 1
    } else {
      set.seed(112358)
      fires$war_fire[fires$id_w_time == i] <- sample(c(rep(1, temp_vector[1]), rep(0, length(temp_vector)-temp_vector[1])))
    }
    cat(paste0('\r\r\r\r calculating for: ', i, '// time per cell & day: ', round(difftime(Sys.time(), start, units = 'secs'), 5), 's // ', round(100*ind/total, 5), '% complete...............'))
  }

  # In areas with mass excess (i.e. beyond median of cells in fire locations with excess), set all fires to war-related (this assumes none not-war-related fire events in places with beyond median excess)
  fires$war_fire[fires$excess_fire >= median(fires$excess_fire[fires$excess_fire > 0])] <- 1

  # Exclude locations which see anomalous events spanning less than 'min_length_of_fire_in_area' days within the same year
  fires$sustained_excess <- NA
  fires$sustained_excess[fires$war_fire == 1] <- fires$date[fires$war_fire == 1]

  # These lines find if there exists an interval greater than 'min_length_of_fire_in_area' but smaller than 365 in the data
  fires$year <- year(fires$date)
  fires$id_big <- paste0(round(fires$x), '-', round(fires$y))
  # Combine areas to their neighbors if they are partially covered by large internal bodies of water
  fires$id_big[fires$id_big == "31-51"] <- '30-51'
  fires$id_big[fires$id_big == "35-48"] <- '35-47'
  fires$id_big[fires$id_big == "32-49"] <- '33-49'

  fires$sustained_excess <- ave(fires$sustained_excess, fires$id, FUN = function(x) {
    x <- na.omit(unique(x))
    if(length(x) <= 1){
      F
    } else {
      if(length(x) > (1+length(unique(fires$year)))*min_length_of_fire_in_area){ # This greatly speeds this up
        T
      } else {
        if(any(abs(combn(x, 2)[1, ] - combn(x, 2)[2, ]) %in% (min_length_of_fire_in_area-1):(90-1))){
          T
        } else {
          F
        }
      }
    }
  })

  # Calculate zones of sustained excess (defined as within 25km of a cell with sustained excess)
  library(geosphere)
  temp <- unique(fires[fires$sustained_excess == T, c('x', 'y')])
  for(i in unique(fires$id)){
    if(any(distm(unique(fires[fires$id == i, c('x', 'y')]),
                 temp, fun = distHaversine) < 50000)){
      fires$sustained_excess[fires$id == i] <- T
    }
  }
  rm(temp)
  # fires$sustained_excess <- ave(fires$sustained_excess, fires$id_big, FUN = function(x) any(as.logical(x)))
  fires$war_fire[!fires$sustained_excess] <- 0

  # And in areas
  fires$length_of_war_fire_area <- NA
  fires$length_of_war_fire_area[fires$war_fire == 1] <- fires$date[fires$war_fire == 1]
  fires$length_of_war_fire_area <- ave(fires$length_of_war_fire, fires$id_big, FUN = function(x) {
    x <- na.omit(unique(x))
    if(length(x) <= 1){
      1
    } else {
      length(min(x):max(x))
    }
  })

  # Save war fires thus identified as "war_fire_restrictive"
  fires$war_fire_restrictive <- fires$war_fire

  # Assign fires taking place in the 'assign_to_war_fire_after_large_excess' days after war-classified fire to war-related
  if(days_to_assign_to_war_fire_after_excess > 0){
    fire_area <- fires[fires$war_fire == 1 & fires$excess_fire >= 1
                       , c('id', 'date')]
    for(i in 1:days_to_assign_to_war_fire_after_excess){
      fire_area_id <- paste0(fire_area$id, '_', fire_area$date + i)
      fires$war_fire[paste0(fires$id, '_', fires$date) %in% fire_area_id] <- 1
    }

    # Ensure fires not marked as war-related if taking place in locations where manual inspection has established that cause is not war related (done once again to avoid errors being introduced in preceeding step:
    if(!missing(exclude)){
      for(i in 1:nrow(exclude)){
        fires$war_fire[fires$x >= min(exclude[i, c('lng1', 'lng2')]) &
                         fires$x <= max(exclude[i, c('lng1', 'lng2')]) &
                         fires$y >= min(exclude[i, c('lat1', 'lat2')]) &
                         fires$y <= max(exclude[i, c('lat1', 'lat2')]) &
                         fires$date >= as.Date(exclude$start_date[i]) &
                         fires$date <= as.Date(exclude$end_date[i])] <- 0
      }
    }
  }

  # Similarly exclude fires on extreme heat days
  if(!missing(exclude_dates)){

    fires$war_fire[as.numeric(fires$date) %in% as.numeric(exclude_dates)] <- 0
  }




  # Return fire-location-time data frame with "war_fire" tag:
  cat(paste0('\n Classified ', sum(fires$war_fire == 1), ' as likely war-related fires.\n'))
  cat(paste0('\n            ', sum(fires$war_fire_restrictive == 1), ' (restrictive). \n'))
  return(fires)
}

