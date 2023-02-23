# Get % assessed as the site of counter-attacked by Ukraine:
library(sf)
library(tidyverse)
library(units)
library(readr)
library(anytime)
library(lubridate)

# Generate containers and set index
res <- data.frame()
ind <- 0

for(i in dir('source-data/Ukraine_Conflict_Mapping_ARCGIS_Shapefiles/Claimed_Ukrainian_Counteroffensives/')){
  # Generate temporary folder
  unlink("source-data/temp", recursive=TRUE)
  dir.create('source-data/temp')

  # Unzip shapefile:
  # Map source: Institute for the Study of War
  zipF <- paste0("source-data/Ukraine_Conflict_Mapping_ARCGIS_Shapefiles/Claimed_Ukrainian_Counteroffensives/", i)
  outDir <- "source-data/temp"
  unzip(zipF, exdir=outDir)

  # Load shapefile
  temp_shp <- grep('shp', dir('source-data/temp/'), value = T)[1]
  temp <- st_read(paste0('source-data/temp/', temp_shp)) %>% st_transform("EPSG:6381")
  temp <- st_zm(temp)

  # Calculate area
  area_assessed_as_ukraine_controlled <- sum(st_area(temp))

  # Optional: Simplify geometry
  temp <- st_simplify(temp, dTolerance = 100)

  # Save area to big list (including data name to later get the date)
  temp$data_name <- i
  if(ind == 0){
    big_shp <- temp
  } else {
    big_shp <- rbind(big_shp[, intersect(colnames(big_shp), colnames(temp))],
                     temp[, intersect(colnames(big_shp), colnames(temp))])
  }
  ind <- ind + 1

  # Save
  res <- rbind(res, data.frame(area_assessed_as_ukraine_controlled, name = i))
}

# Next, get the date based on the folder name:
res$date <- NA
for(i in 1:nrow(res)){
  res$date[i] <-  unlist(strsplit(strsplit(res$name[i], "Counteroffensives")[[1]][2], '.zip'))
  if(is.na(res$date[i])){
    res$date[i] <- unlist(strsplit(strsplit(res$name[i], "Control_AO_")[[1]][2], '.zip'))
  }
  if(is.na(res$date[i])){
    res$date[i] <- unlist(strsplit(strsplit(res$name[i], "CounterOffensives")[[1]][2], '.zip'))
  }
  if(is.na(res$date[i])){
    res$date[i] <- unlist(strsplit(strsplit(res$name[i], "UkrainianControl_")[[1]][2], '.zip'))
  }
  if(is.na(res$date[i])){
    res$date[i] <- unlist(strsplit(strsplit(res$name[i], "Counteroffensive")[[1]][2], '.zip'))
  }
}
res$year <- ifelse(grepl('2023', res$date), 2023, 2022)
res$date <- gsub('O0', '0', res$date)
res$date <- gsub('AO', '', res$date)
res$date <- gsub(' (1)', '', res$date)
res$date <- gsub(' (1)', '', res$date, fixed = T)
res$date <- gsub('.shp', '', res$date, fixed = T)
res$date <- gsub('OJUN03', '03JUN', res$date)
res$date <- gsub('March', 'MAR', res$date)
res$date <- gsub('June', 'JUN', res$date)
res$date <- gsub('SEPT', 'SEP', res$date)
res$date <- gsub('22SEP22', '22SEP2022', res$date)
res$date <- gsub('011JUL2022', '11JUL2022', res$date)
res$date <- gsub('JUN032022', '03JUN2022', res$date)
res$date <- gsub('13MAR', '13MAR2022', res$date)
res$date <- gsub('25APR2025', '25APR2022', res$date)
res$date <- gsub('__09_MAR_2022', '09MAR2022', res$date)
res$date[res$date == 'MAR2'] <- '02MAR2022'
res$date <- gsub('_', '', res$date)
res$date <- gsub('O13NOV', '13NOV', res$date)
res$date <- gsub('ARP', 'APR', res$date)
res$date <- gsub('2023', '', res$date)
res$date <- gsub('2022', '', res$date)
res$date_guess <- anydate(res$date)

# Getting around limitation of anydate
res$date[is.na(res$date_guess)] <- paste0(res$year[is.na(res$date_guess)], '_', res$date[is.na(res$date_guess)])
res$date[is.na(res$date_guess)] <- gsub('10', '_10', res$date[is.na(res$date_guess)])
res$date[is.na(res$date_guess)] <- gsub('20', '_20', res$date[is.na(res$date_guess)])
res$date[is.na(res$date_guess)] <- gsub('30', '_30', res$date[is.na(res$date_guess)])
res$date[is.na(res$date_guess)] <- gsub('_2022', '2022', res$date[is.na(res$date_guess)])
res$date[is.na(res$date_guess)] <- gsub('_2023', '2023', res$date[is.na(res$date_guess)])

res$date_guess[is.na(res$date_guess)] <- anydate(res$date[is.na(res$date_guess)])
res$date <- res$date_guess
res$date_guess <- NULL

year(res$date) <- res$year
res$year <- NULL

# Ensure we get all dates into the dataset:
dates <- data.frame(date = as.Date(min(res$date):max(res$date), origin = '1970-01-01'))
res <- merge(res, dates, all = T)

# Get this information into shape-file dataset:
big_shp <- merge(big_shp, res, by.x="data_name", by.y='name', all.x = T)

# Calculate change in area assessed as Ukraine controlled
res <- res[order(res$date), ]
res$change_in_area_assessed_as_ukraine_controlled <- as.numeric(res$area_assessed_as_ukraine_controlled)-as.numeric(c(NA, res$area_assessed_as_ukraine_controlled)[1:nrow(res)])

res$change_in_area_assessed_as_ukraine_controlled_in_past_7_days <- NA
res$change_in_area_assessed_as_ukraine_controlled_in_past_30_days <- NA

for(i in 1:nrow(res)){
  res$change_in_area_assessed_as_ukraine_controlled_in_past_7_days[i] <- sum(res$change_in_area_assessed_as_ukraine_controlled[max(c(1, i-6)):i], na.rm = T)
  res$change_in_area_assessed_as_ukraine_controlled_in_past_30_days[i] <- sum(res$change_in_area_assessed_as_ukraine_controlled[max(c(1, i-29)):i], na.rm = T)
}

# Get this in %
res$area_assessed_as_ukraine_controlled_percent <- res$area_assessed_as_ukraine_controlled / (1000*1000*603628)
write_csv(res, "output-data/area_counterattacked_by_ukraine.csv")

# Generate maximal extent of Ukrainen-assessed control (i.e. "ever" controlled by Ukraine since war began):
big_shp_combined <- st_union(big_shp)
saveRDS(big_shp_combined, 'output-data/area_ever_counterattacked_by_ukraine.RDS')
saveRDS(big_shp, 'output-data/area_counterattacked_by_ukraine_by_day.RDS')

if(inspect){
ggplot()+geom_sf(data=big_shp[order(big_shp$date), ],
                 alpha = 0.02,
                 fill='blue', col=NA)+
  geom_sf(data=big_shp_combined, fill=NA, col='black')+theme_void()}
