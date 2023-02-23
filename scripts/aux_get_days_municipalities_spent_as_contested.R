# Get time municipalities have spent as contested:
library(sf)
library(readr)

# Load shapefiles:
ru_control <- readRDS('output-data/area_assessed_as_controlled_by_Russia_by_day.RDS')
ua_counters <- readRDS('output-data/area_counterattacked_by_ukraine_by_day.RDS')
ru_attacks <- readRDS('output-data/area_attacked_by_russia_by_day.RDS')
ru_claimed <- readRDS('output-data/area_claimed_as_controlled_by_Russia_by_day.RDS')
ukraine_raw <- st_read('source-data/ukraine-detailed-map/ukr_admbnda_adm3_sspe_20230201.shp')
ukraine <- st_simplify(ukraine_raw, dTolerance = 1000)

# Contested defined as not "assessed as Russia controlled", but "claimed by Russia", "site of Russian attacks", or "site of Russian counter-attack"
big_shp <- rbind(ru_attacks[, c("geometry", 'date')],
                 ua_counters[, c("geometry", 'date')],
                 ru_claimed[, c("geometry", 'date')])
big_shp_compliment <- ru_control
big_shp <- st_make_valid(st_transform(big_shp, crs = st_crs(ukraine)))
big_shp_compliment <- st_make_valid(st_transform(big_shp_compliment, crs = st_crs(ukraine)))

big_shp <- st_simplify(big_shp, dTolerance = 1000)
big_shp_compliment <- st_simplify(big_shp_compliment, dTolerance = 1000)
sf_use_s2(FALSE)

# Loop through dates:
big_area <- data.frame()
ind <- 0
for(i in unique(big_shp$date)){
  suppressMessages(temp_shp <- st_make_valid(st_difference(st_union(big_shp[big_shp$date == i, ]), st_union(big_shp_compliment[big_shp_compliment$date == i, ]))))

  # Then through municipalities
  area <- c()
  for(j in 1:nrow(ukraine)){
    suppressMessages(temp_area <- st_area(st_intersection(temp_shp, ukraine[j, ])))
    area <- c(area, ifelse(length(temp_area) > 0, temp_area, 0))
  }
  big_area <- rbind(big_area, cbind.data.frame(area = area, place = ukraine$ADM3_PCODE, date= i))

  cat(paste0('\r\r\r Checking : ', 100*ind/length(unique(big_shp$date)), '%.....'))
  ind <- ind + 1
}

# Fix missing areas:
big_area$date <- as.Date(big_area$date, origin = '1970-01-01')
dates <- expand.grid(date = as.Date(min(big_area$date):max(big_area$date), origin = '1970-01-01'), place = unique(big_area$place))
big_area <- merge(big_area, dates, by=c("date", 'place'), all = T)
big_area <- big_area[order(big_area$date), ]
big_area$area <- ave(big_area$area, big_area$place, FUN = function(x){
  for(i in 2:length(x)){
    if(is.na(x[i])){
      x[i] <- x[i-1]
    }
  }
  x
})
big_area$mean_area <- ave(big_area$area, big_area$place, FUN = function(x) mean(x))

base_area <- data.frame()
for(i in 1:nrow(ukraine)){
  base_area <- rbind(base_area, data.frame(place = ukraine$ADM3_PCODE[i], total_area = st_area(ukraine[i, ])))
}
big_area <- merge(big_area, base_area, by="place", all = T)
big_area$mean_share <- big_area$mean_area/big_area$total_area

# Save this:
saveRDS(big_area, 'output-data/contested_areas_by_day.RDS')
saveRDS(big_area[!duplicated(big_area$place), c('place', 'mean_area', 'total_area', 'mean_share')], 'output-data/contested_areas.RDS')

ukraine <- merge(ukraine, big_area[!duplicated(big_area$place), c('place', 'mean_area', 'total_area')], by.x='ADM3_PCODE', by.y='place')
ukraine$mean_share <- ukraine$mean_area/ukraine$total_area
ukraine$mean_share_01 <- ukraine$mean_area > 0


