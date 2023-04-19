cat('Updating cloud data...\n')

# The script to process cloud image TIFFs to RDS files is: source('scripts/aux_process_cloud_tiffs.R'). Running that script generates the data loaded in the next two lines.
library(data.table)
processed_cloud_files <- dir('source-data/cloud-data-processed')
cloud_data <- rbindlist(lapply(processed_cloud_files, FUN = function(i) readRDS(paste0('source-data/cloud-data-processed/', i))), fill = T)
cloud_data$date <- as.Date(cloud_data$date)

# Cloud observations are all 0, and thus not assigned a point. We take missing points in the cloud data as indicating clouds:
library(readr)
cloud_mat <- expand.grid(date = as.Date(as.Date('2022-01-01'):Sys.Date(), origin = '1970-01-01'),
                         id = unique(unique(read_csv('output-data/model-objects/stable_covariates_2022.csv')$id)))
cloud_mat$cloud <- !paste0(cloud_mat$date, '-', cloud_mat$id) %in% paste0(cloud_data$date, '-', cloud_data$id)

# If we do not have cloud data for date, assign NA
dates <- as.Date(unlist(strsplit(processed_cloud_files, '.RDS')))
cloud_mat$cloud[!cloud_mat$date %in% dates] <- NA

# Subset to relevant time interval:
cloud_mat <- cloud_mat[cloud_mat$date >= as.Date('2022-01-01') & cloud_mat$date <= Sys.Date(), ]
cloud_mat$cloud[!cloud_mat$cloud %in% c(1,0)] <- NA # we don't impute means for missing data here
cloud_mat$cloud_cover_in_country <- ave(cloud_mat$cloud, cloud_mat$date, FUN = function(x) mean(x, na.rm = T))

cat('\n Calculating cloud cover for eastern Ukraine by day \n.')

# Eastern Ukraine is defined as Ukraine east of 30 degree longitude:
cloud_mat$x <-   gsub("-.*", "", cloud_mat$id)
cloud_mat <- cloud_mat[cloud_mat$x >= 30, ]
cloud_mat$cloud_cover_in_east_of_country <- ave(cloud_mat$cloud, cloud_mat$date, FUN = function(x) mean(x, na.rm = T))

clouds <- cloud_mat[!duplicated(cloud_mat$date), c('date', 'cloud_cover_in_country',
                                           'cloud_cover_in_east_of_country')]

library(readr)
write_csv(clouds[order(clouds$date), ], 'output-data/cloud_cover_in_ukraine_by_day.csv')
cat('-> Updated cloud data saved to "output-data/cloud_cover_in_ukraine_by_day.csv"\n')
