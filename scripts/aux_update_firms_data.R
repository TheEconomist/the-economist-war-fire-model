# This script downloads the latest data from FIRMS and then saves it to the file "output-data/fires_update.csv"
library(readr)

# Define map key and sat systems used
map_key <- Sys.getenv("FIRMS_API_KEY")
sat_systems <- c("VIIRS_SNPP_NRT", "MODIS_NRT", "VIIRS_NOAA20_NRT", "VIIRS_SNPP_NRT") # This excludes standard processing systems "MODIS_SP", and "VIIRS_SNPP_SP"
days <- 10

# Helper: One retry on failure
download_with_one_retry <- function(url, ...) {
  out <- tryCatch(
    readr::read_csv(url, ...),
    error = function(e) {
      cat("First attempt failed, waiting 10 min and retrying...\n")
      Sys.sleep(601)
      # Second (final) attempt
      tryCatch(
        readr::read_csv(url, ...),
        error = function(e2) {
          stop("Second attempt failed: ", conditionMessage(e2))
        }
      )
    }
  )
  out
}

fires <- data.frame()
for(i in sat_systems){
  link <- paste0("https://firms.modaps.eosdis.nasa.gov/api/country/csv/",
                 map_key, "/",
                 i,
                 "/UKR/", days)
  temp <- download_with_one_retry(url = link, show_col_types = FALSE)

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
updated_data <- unique(c(Sys.Date():(Sys.Date()-days+1), read_csv('output-data/dates_of_successfully_acquired_fire_data.csv')$dates))
temp <- setdiff(as.Date('2023-03-03', origin = '1970-01-01'):Sys.Date(), updated_data)
if(length(temp) > 0){
  stop(paste0("Error: Missing fire data for: ", as.Date(temp, origin = '1970-01-01'), ' --- aborting update.\n'))
}
# Record successful update:
write_csv(data.frame(dates = updated_data), 'output-data/dates_of_successfully_acquired_fire_data.csv')

# Append to data and save:
old_fires <- read_csv('output-data/firms_update.csv')
fires <- rbind(fires[, colnames(old_fires)], old_fires)
fires <- fires[!duplicated(paste0(fires$latitude, '-', fires$longitude, '-', fires$acq_date, '-', fires$acq_time, '-', fires$satellite)), ]
write_csv(fires, 'output-data/firms_update.csv') } else {
  stop('Update failed - check rate limits.')
}

rm(temp)
rm(fires)
rm(link)
rm(map_key)
rm(i)
rm(days)
rm(sat_systems)
rm(updated_data)
