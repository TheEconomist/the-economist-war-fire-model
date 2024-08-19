# This script downloads the latest data from FIRMS and then saves it to the file "output-data/fires_update_RU.csv"
library(readr)

archive_cache <- T
if(!archive_cache){
  # Get archive:
  library(sf)
  j1v <- st_read('/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/DL_FIRE_J1V-C2_501987/fire_nrt_J1V-C2_501987.shp')
  j2v <- st_read('/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/DL_FIRE_J2V-C2_501988/fire_nrt_J2V-C2_501988.shp')
  m <- st_read('/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/DL_FIRE_M-C61_501986/fire_archive_M-C61_501986.shp')
  sv <- st_read('/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/DL_FIRE_SV-C2_501989/fire_archive_SV-C2_501989.shp')

  # Combine the data
  archive <- do.call(rbind, lapply(list(j1v, j2v, m, sv), function(df) df[, Reduce(intersect, list(colnames(j1v), colnames(j2v), colnames(m), colnames(sv))), drop = FALSE]))

  colnames(archive) <- tolower(colnames(archive))
  archive$country_id <- 'RUS'
  archive <- archive[, c("country_id", "latitude", "longitude", "scan", "track",
                         "acq_date", "acq_time", "satellite", "instrument",
                         "confidence", "version", "frp", "daynight")]
  archive <- archive[!duplicated(paste0(archive$latitude, '-', archive$longitude, '-', archive$acq_date, '-', archive$acq_time, '-', archive$satellite)), ]
  write_csv(archive, '/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/archive_big.csv')
  write_csv(archive[as.Date(archive$acq_date) >= as.Date('2020-01-01'), ], '/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/archive_since_2020.csv')
  write_csv(archive[as.Date(archive$acq_date) >= as.Date('2022-01-01'), ], '/Users/sondresolstad/Github/war-torch-detection-system/source-data/fire-archive-data/RUS/archive_since_2022.csv')

  old_fires <- read_csv('output-data/firms_update_RU.csv')
  old_fires <- rbind(old_fires, archive[as.Date(archive$acq_date) >= as.Date('2022-02-21') & archive$latitude < 53.2 & archive$longitude < 43.1, colnames(old_fires)])
  old_fires[!duplicated(paste0(old_fires$latitude, '-', old_fires$longitude, '-', old_fires$acq_date, '-', old_fires$acq_time, '-', old_fires$satellite)), ]
  write_csv(old_fires, 'output-data/firms_update_RU.csv')
}

# Define map key and sat systems used
map_key <- Sys.getenv("FIRMS_API_KEY")
sat_systems <- c("VIIRS_SNPP_NRT", "MODIS_NRT", "VIIRS_NOAA20_NRT", "VIIRS_SNPP_NRT") # This excludes standard processing systems "MODIS_SP", and "VIIRS_SNPP_SP"
days <- 10

fires <- data.frame()
for(i in sat_systems){
  link <- paste0("https://firms.modaps.eosdis.nasa.gov/api/country/csv/",
                 map_key, "/",
                 i,
                 "/RUS/", days)
  temp <- read_csv(link, show_col_types = F)

  if(length(temp) > 0){
    if(nrow(fires) > 0 & nrow(temp) > 0){
      fires <- rbind(fires[, intersect(colnames(fires), colnames(temp))], temp[, intersect(colnames(fires), colnames(temp))])
    } else {
      fires <- temp}
  }

  cat(paste0('Loaded most recent ', days, " days of data from ", i, ". New fires = ", nrow(fires), "\n"))
}

# Remove duplicates:
fires <- unique(fires)

if(nrow(fires) > 0){
  # 4. Ensure full coverage:
  updated_data <- unique(c(Sys.Date():(Sys.Date()-days+1), read_csv('output-data/dates_of_successfully_acquired_fire_data_RU.csv')$dates))
  temp <- setdiff(as.Date('2023-03-03', origin = '1970-01-01'):Sys.Date(), updated_data)
  if(length(temp) > 0){
   # stop(paste0("Error: Missing fire data for: ", as.Date(temp, origin = '1970-01-01'), ' --- aborting update.\n'))
  }
  # Record successful update:
  write_csv(data.frame(dates = updated_data), 'output-data/dates_of_successfully_acquired_fire_data_RU.csv')

  # Append to data and save:
  old_fires <- read_csv('output-data/firms_update_RU.csv')
  fires <- rbind(fires[, colnames(old_fires)], old_fires)
  fires <- fires[!duplicated(paste0(fires$latitude, '-', fires$longitude, '-', fires$acq_date, '-', fires$acq_time, '-', fires$satellite)), ]
  fires <- fires[fires$latitude < 53.2 & fires$longitude < 43.1, ] # Restrict to nearby Russia
  write_csv(fires, 'output-data/firms_update_RU.csv') } else {
    stop('Update failed - check rate limits.')
  }
