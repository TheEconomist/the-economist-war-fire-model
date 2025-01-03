# This function generates a data frame with all variables that do not depend on data collected since the war began:
library(readr)

# Read raw data:
X_mat <- readRDS('output-data/X_matrix.RDS')

# Subset to first year of the war (we use covariates as they were here for succeeding predictions)
X_mat <- X_mat[X_mat$date %in% as.Date('2022-02-24'):as.Date('2023-02-23'), ]

# Define stable covariates
stable <- c("id", "month", "id_5x5", "x", "y",
            "time_of_year", "pop_density", "in_urban_area",  "city",
            "nightlights",
            "average_past_fires_by_1x1_location",
            "average_past_fires_by_1x1_location_time_of_year",
            "average_past_fires_by_5x5_location_time_of_year" ,
            "average_past_fires_by_1x1_location_month_of_year" ,
            "average_past_fires_by_5x5_location_month_of_year",
            "average_past_fires_by_1x1_location_no_clouds",
            "average_past_fires_by_1x1_location_time_of_year_no_clouds",
            "average_past_fires_by_5x5_location_time_of_year_no_clouds" ,
            "average_past_fires_by_1x1_location_month_of_year_no_clouds",
            "average_past_fires_by_5x5_location_month_of_year_no_clouds" ,
            "average_past_clouds_by_1x1_location_time_of_year",
            "average_past_clouds_by_5x5_location_time_of_year",
            "average_past_clouds_by_1x1_location_month_of_year",
            "average_past_clouds_by_5x5_location_month_of_year",
            "average_past_fires_by_1x1_location_time_of_year_30da",
            "average_past_fires_by_1x1_location_time_of_year_no_clouds_30da",
            "average_past_clouds_by_1x1_location_time_of_year_30da",
            "average_past_fires_by_5x5_location_time_of_year_30da",
            "average_past_fires_by_5x5_location_time_of_year_no_clouds_30da",
            "average_past_clouds_by_5x5_location_time_of_year_30da")

# Subset to stable covariates:
X_mat <- X_mat_year_1 <- X_mat[, c('year', 'date', stable)]

# Generate frame for next 3 years of war:
for(i in 1:3){
  temp <- X_mat_year_1
  temp$year <- temp$year + i
  temp$date <- temp$date + i*365
  X_mat <- rbind(X_mat, temp)
}

# Fix for 2024 being a leap year
temp <- X_mat[X_mat$year == 2024 & X_mat$time_of_year == 59, ]
X_mat$time_of_year[X_mat$year == 2024 & X_mat$time_of_year >= 59] <- X_mat$time_of_year[X_mat$year == 2024 &
                                                                                          X_mat$time_of_year >= 59]+ 1
X_mat$date[X_mat$year == 2024 & X_mat$time_of_year >= 59] <- X_mat$date[X_mat$year == 2024 &
                                                                          X_mat$time_of_year >= 59]+ 1
X_mat <- rbind(X_mat, temp)

write_csv(X_mat, 'output-data/model-objects/stable_covariates.csv')

library(lubridate)
write_csv(X_mat[year(X_mat$date) == 2022, ], 'output-data/model-objects/stable_covariates_2022.csv')
write_csv(X_mat[year(X_mat$date) == 2023, ], 'output-data/model-objects/stable_covariates_2023.csv')
write_csv(X_mat[year(X_mat$date) == 2024, ], 'output-data/model-objects/stable_covariates_2024.csv')

# Appendum: years beyond 2024. We use 2023 as the source because it is not a leap year
X_mat <- read_csv('output-data/model-objects/stable_covariates_2023.csv')

# Shift dates forward by exactly 2 calendar years
X_mat$date <- X_mat$date %m+% years(2)

# Update 'year' column from the new dates
X_mat$year <- year(X_mat$date)

# Filter to 2025 onward
X_mat <- X_mat[X_mat$date >= as.Date('2025-01-01'), ]
write_csv(X_mat[year(X_mat$date) == 2025, ], 'output-data/model-objects/stable_covariates_2025.csv')

