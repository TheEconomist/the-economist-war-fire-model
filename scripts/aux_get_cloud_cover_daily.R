# This script loads data generated previously to calculate and save a separate file detailing daily cloud cover in Ukraine and in the east of the country.

cat('\n Calculating cloud cover for country by day \n.')

# Load data
X_mat <- readRDS('output-data/X_matrix.RDS')

# Subset to relevant time interval:
X_mat <- X_mat[X_mat$date >= as.Date('2022-01-01') & X_mat$date <= Sys.Date(), ]
X_mat$cloud[!X_mat$cloud %in% c(1,0)] <- NA # we don't impute means for missing data here
X_mat$cloud_cover_in_country <- ave(X_mat$cloud, X_mat$date, FUN = function(x) mean(x, na.rm = T))

cat('\n Calculating cloud cover for eastern Ukraine by day \n.')

# Eastern Ukraine is defined as Ukraine east of 30 degree longitude:
X_mat <- X_mat[X_mat$x >= 30, ]
X_mat$cloud_cover_in_east_of_country <- ave(X_mat$cloud, X_mat$date, FUN = function(x) mean(x, na.rm = T))

clouds <- X_mat[!duplicated(X_mat$date), c('date', 'cloud_cover_in_country',
                                           'cloud_cover_in_east_of_country')]

library(readr)
write_csv(clouds[order(clouds$date), ], 'output-data/cloud_cover_in_ukraine_by_day.csv')
plot(clouds$cloud_cover_in_country)

cat('\n Calculations complete and files saved \n.')

