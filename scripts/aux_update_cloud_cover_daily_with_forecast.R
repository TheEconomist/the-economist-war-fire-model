# This script updates daily cloud cover numbers using forecasts for the recent past made available by open-meteo.
library(readr)
library(httr)
library(jsonlite)

# Step 1: Get a grid of Ukraine
grid <- readRDS('output-data/model-objects/ukraine_mask.RDS')
grid$x <- round(grid$x)
grid$y <- round(grid$y)
grid <- unique(grid[, c('x', 'y')])

# Step 2: Get dates to get forecast for:
clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day.csv')
start_date <- max(clouds$date[!is.na(clouds$cloud_cover_in_country)])
end_date <- Sys.Date()

# Step 3: Query API to get data, simplify, and save

# Define function to get data from api:
get_weather_forecast_by_lat_lng <- function(url){
  web_content <- httr::GET(url)
  web_content <- content(web_content,"text")
  json_data <- fromJSON(web_content, flatten = TRUE)
  df <- as.data.frame(json_data)
  return(df)
}

# Loop through locations and save results:
df <- data.frame()
for(i in 1:nrow(grid)){
  url <- paste0('https://api.open-meteo.com/v1/forecast?latitude=', grid$y[i], '&longitude=', grid$x[i], '&hourly=temperature_2m,cloudcover&start_date=', start_date, '&end_date=', end_date)
  df<- bind_rows(df,get_weather_forecast_by_lat_lng(url))
  cat('.')
}

# Simplify
df$date <- anydate(df$hourly.time)
df$hourly.cloudcover <- df$hourly.cloudcover/100
df$cloud_cover_in_country <- ave(df$hourly.cloudcover, df$date, FUN = function(x) mean(x, na.rm = T))
df <- df[df$longitude >= 30, ]
df$cloud_cover_in_east_of_country <- ave(df$hourly.cloudcover, df$date, FUN = function(x) mean(x, na.rm = T))
df <- unique(df[, c('date', 'cloud_cover_in_country', 'cloud_cover_in_east_of_country')])

# Save
clouds <- clouds[clouds$date < start_date, ]
clouds <- rbind(clouds, df)
write_csv(clouds, 'output-data/cloud_cover_in_ukraine_by_day_with_forecast_for_recent_days.csv')


