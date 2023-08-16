#  This script can be used to identify extreme-heat spells (one in twenty-year events) for which the models predictions of war-fires are not reliable. This script should be run after periods of extreme heat in Ukraine

# First acquire a grid of the country:
fires <-  read_csv('output-data/ukraine_fires.csv')
country_grid <- fires[, c('LATITUDE', 'LONGITUDE')]
country_grid$lat <- round(country_grid$LATITUDE)
country_grid$lng <- round(country_grid$LONGITUDE)
country_grid <- unique(country_grid[, c('lat', 'lng')])

# Then set start and end dates:
start_date <- as.Date('2010-01-01')
end_date <- Sys.Date()

# Then acquire weather data, and calculate average max temperatures by day
library(httr)
library(jsonlite)

get_weather_forecast_by_lat_lng <- function(url){
  web_content <- httr::GET(url)
  web_content <- content(web_content,"text")
  json_data <- fromJSON(web_content, flatten = TRUE)
  df <- as.data.frame(json_data)
  return(df)
}
df <- data.frame()
for(i in 1:nrow(country_grid)){
  url <- paste0('https://archive-api.open-meteo.com/v1/archive?latitude=', country_grid$lat[i], '&longitude=', country_grid$lng[i], '&hourly=temperature_2m,cloudcover,relativehumidity_2m,precipitation,soil_moisture_0_to_7cm&start_date=', start_date, '&end_date=', end_date)
  df<- bind_rows(df,get_weather_forecast_by_lat_lng(url))
  cat('.')
}

library(anytime)
df$date <- anydate(df$hourly.time, calcUnique = T)
df$id <- paste0(df$latitude, df$longitude)
df$temp <- ave(df$hourly.temperature_2m, paste0(df$id, '_', df$date), FUN = function(x) max(x, na.rm = T)) # Use max?
df$temp <- ave(df$temp, df$date, FUN = function(x) mean(x, na.rm = T))

df <- df[!duplicated(df$date), ]

# Next, generate a model for plausible daily temperatures (based on pre-war data):
lm_fit <- lm(temp ~ as.factor(yday(date)), data = df[df$date < as.Date('2022-02-24'), ])

# Generate predictions and confidence intervals:
df[, c('pred', 'lwr', 'upr')] <- predict(newdata = df, lm_fit, se.fit = T, interval = 'confidence')[[1]]

# Identify days of extreme heat:
ex <- df$date[df$temp >= max(df[, 'upr'])]

# Subset to such days since the war began:
ex <- ex[ex >= as.Date('2022-02-24')]

# Assess when abnormality began (look as far as the past week, set it to the day before the start of temperatures above the 95% percentile of predictions):
start <- rep(NA, length(ex))
ind <- 1
for(i in ex){
  for(j in 1:6){
    if(df$temp[df$date == i - 1] > df$upr[df$date == i - 1]){
      i <- i - 1
    }
  }
  #
  start[[ind]] <- i-1
  ind <- ind + 1
}

# Do the same for when it ended:
end <- rep(NA, length(ex))
ind <- 1
for(i in ex){
  for(j in 1:6){
    if(df$temp[df$date == i + 1] > df$upr[df$date == i + 1]){
      i <- i + 1
    }
  }
  #
  end[[ind]] <- i+1
  ind <- ind + 1
}

# Convert back to date:
start <- as.Date(start)
end <- as.Date(end)

# Generate intervals, equal to whichever is greater of start to start + 7 or start to end.
exclude_dates <- list()
for(i in 1:length(start)){
  exclude_dates[[i]] <- c(start[i]:max(c(end[i], start[i]+7)))
}

# Export to file:
saveRDS(as.Date(unlist(exclude_dates)), 'output-data/model-objects/exclude_dates.RDS')
