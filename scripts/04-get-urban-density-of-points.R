# Get very exact urban density of points (1k by 1km):

# Load data on urban density
if(redo){
  cat('\nCalculating exact urban density of fires.\n')
# Source: https://www.worldpop.org/geodata/summary?id=49349
library(readr)
worldpop <- read_csv('source-data/worldpop/ukr_pd_2020_1km_UNadj_ASCII_XYZ.csv')

# Load points:
points_raw <- readRDS('output-data/all_fires_all_cols.RDS')[, c('LONGITUDE', 'LATITUDE')]
points <- unique(points_raw)

# Get closest point:
library(dplyr)
library(tidyr)

# Splitting points up into groups of 500 for speed. This takes a bit of time.
big_pop <- data.frame()
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
fires <- saveRDS(big_pop, 'output-data/exact_pop_merge_df.RDS')
} else {
  cat('\nUsing cache for exact urban density of fires.\n')

  big_pop <- readRDS('output-data/exact_pop_merge_df.RDS')
}

fires <- readRDS('output-data/all_fires_all_cols.RDS')
fires <- merge(fires, big_pop, by=c('LATITUDE', 'LONGITUDE'), all.x = T)
saveRDS(fires, 'output-data/all_fires_all_cols_exact_pop.RDS')

cat('\nExact urban density merged into data and saved.\n')
