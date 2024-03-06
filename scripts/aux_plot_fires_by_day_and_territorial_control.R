# This generates a chart of fires by day, adjusting for cloud coverage

# Step 0: Set options and load base packages ----------------------------------------
library(readr)
library(data.table)
library(lubridate)
library(sf)
library(anytime)
library(tidyverse)
options(readr.show_col_types = F)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

max_cloud_coverage <- 0.5

# Step 1: Load data ----------------------------------------

# Update and load control status of fires:
source('scripts/aux_get_control_status_of_fires_II.R')
control <- read_csv('output-data/control_status_of_fires_II.csv')

# Load fire data
fires <- read_csv('output-data/ukraine_fires.csv')
clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day_with_forecast_for_recent_days.csv')

# Step 2: Merge ----------------------------------------

# Merge control data and generate daily totals:
fires$in_ukraine_held_area <- NULL
fires <- merge(fires, control, by = c('LATITUDE', 'LONGITUDE', 'date', 'ACQ_TIME'), all.x = T)
fires$fires_per_day <- ave(fires$fire, fires$date, FUN = function(x) sum(x, na.rm = T))
fires$war_fires_per_day <- ave(fires$war_fire, fires$date, FUN = function(x) sum(x, na.rm = T))
fires$fires_per_day_in_ukraine_held_area <- ave(fires$in_ukraine_held_area, fires$date, FUN = function(x) sum(x, na.rm = T))
fires$war_fires_per_day_in_ukraine_held_area <- ave(fires$war_fire*fires$in_ukraine_held_area, fires$date, FUN = function(x) sum(x, na.rm = T))
fires$fires_per_day_in_russia_held_area <- ave(!fires$in_ukraine_held_area, fires$date, FUN = function(x) sum(x, na.rm = T))
fires$war_fires_per_day_in_russia_held_area <- ave(fires$war_fire*!fires$in_ukraine_held_area, fires$date, FUN = function(x) sum(x, na.rm = T))

war <- merge(clouds, unique(fires[, c('date', 'fires_per_day', 'war_fires_per_day',
                                      'fires_per_day_in_ukraine_held_area',
                                      'war_fires_per_day_in_ukraine_held_area',
                                      'fires_per_day_in_russia_held_area',
                                      'war_fires_per_day_in_russia_held_area')]), all.x = T)

war <- war[war$date >= as.Date('2022-02-23'), ]

# Step 3: Generate averages for these
war <- war[order(war$date),]

for(j in setdiff(colnames(war), c('date',
                                  'cloud_cover_in_country',
                                  'cloud_cover_in_east_of_country'))){
  war[, paste0(j, '_non_cloud_days_7dma')] <- NA
  war$temp <- war[, j]
  war$temp[war$cloud_cover_in_east_of_country > max_cloud_coverage] <- NA
  
  for(i in 1:nrow(war)){
    war[i, paste0(j, '_non_cloud_days_7dma')] <-
      mean(war$temp[max(c(1, i-3)):min(c(nrow(war), i+3))], na.rm = T)
  }
  war$temp <- NULL

  war[, paste0(j, '_7dma')] <- NA
  for(i in 1:nrow(war)){
    war[i, paste0(j, '_7dma')] <-
      mean(war[max(c(1, i-3)):min(c(nrow(war), i+3)), j], na.rm = T)
  }
}

# Step 4: Chart ----------------------------------------
war[, 2] <- round(war[, 2], 3)
war[, 3] <- round(war[, 3], 3)
war <- war[!is.na(war$date), ]                                                   
write_csv(war[nrow(war):1, ], 'output-data/strikes_by_location_and_day.csv')
ggplot(war[, ], aes(x=date))+
  geom_line(aes(col=paste0('7-day centered average, Russia-held, claimed or contested area\n(days with <', 100*max_cloud_coverage, '% cloud cover)'), y=war_fires_per_day_in_russia_held_area_non_cloud_days_7dma))+
  geom_line(aes(col=paste0('7-day centered average, Ukraine-held area\n(days with <', 100*max_cloud_coverage, '% cloud cover)'), y=war_fires_per_day_in_ukraine_held_area_non_cloud_days_7dma))+theme_minimal()+xlab('Sources: ISW, The Economist')+ylab('')+theme(legend.pos = 'bottom', legend.title = element_blank())+ggtitle('Fire activity assessed as war-related per day, by location of strike')

ggsave('plots/attacks_per_day_by_location_of_strike.png', width = 10, height = 4)


