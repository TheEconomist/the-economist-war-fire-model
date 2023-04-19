# Get very exact urban density of points (1k by 1km):

# Load data on urban density

cat('\nCalculating exact urban density of fires.\n')
# Source: https://www.worldpop.org/geodata/summary?id=49349
library(readr)
worldpop <- read_csv('source-data/worldpop/ukr_pd_2020_1km_UNadj_ASCII_XYZ.csv')

# Load points:
points <- unique(read_csv('output-data/model-objects/all_fires.csv')[, c('LONGITUDE', 'LATITUDE')])

# This loads the cache unless re-do requested:
if(redo){
  big_pop <- data.frame()
} else {
  cat('\nLoading cache for exact urban density of fires....\n')
  big_pop <- readRDS('output-data/model-objects/exact_pop_merge_df.RDS')
}

points <- points[!paste0(points$LONGITUDE, '_', points$LATITUDE) %in%
                   paste0(big_pop$LONGITUDE, '_', big_pop$LATITUDE), ]
if(nrow(points) > 0){
  # Get closest point:
  library(dplyr)
  library(tidyr)

  # Splitting points up into groups of 500 for speed. This takes a bit of time.

  start <- Sys.time()
  for(i in split(1:nrow(points), ceiling(seq_along(1:nrow(points))/50))){
    pop <- points[unlist(i), ] %>%
      crossing(worldpop) %>%
      mutate(dist=sqrt((Y-LATITUDE)^2+(X-LONGITUDE)^2)) %>%
      group_by(LATITUDE, LONGITUDE) %>%
      filter(dist==min(dist)) %>%
      ungroup() %>%
      select(LATITUDE, LONGITUDE, Z) %>%
      rename(pop_exact = Z) %>%
      data.frame()

    big_pop <- rbind(big_pop, pop)
    cat(paste0('\r\r\r ', max(i), ' --- ', round(100*max(i)/nrow(points), 2), ' % -- ', (Sys.time()-start)/length(i), ' per obs.'))
    start <- Sys.time()
  }

  # Update cache:
  saveRDS(big_pop, 'output-data/model-objects/exact_pop_merge_df.RDS')
}

# Merge into fires data:
fires <- read_csv('output-data/model-objects/all_fires.csv')
fires$pop_exact <- NULL
fires <- merge(fires, big_pop, by=c('LATITUDE', 'LONGITUDE'), all.x = T)
write_csv(fires, 'output-data/model-objects/all_fires.csv')

rm(fires)
rm(big_pop)
rm(worldpop)
rm(points)

cat('\nExact urban density merged into data and saved.\n')
