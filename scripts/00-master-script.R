# This serves as the master script for The Economist's war-fire model
# Assuming the relevant input data has been correctly downloaded and required packages are all installed, it replicates the war-fire data and analysis.

# Step 1: Load a packages used in master script and set options: -----------------------------------
library(readr)
library(ggplot2)
inspect <- F # Setting this to FALSE means plots used to inspect intermediary outputs are not drawn
redo <- T # Setting this to FALSE means cached data is used.

# Step 2: Construct training and prediction data, train models, and generate predictions: -----------------------------------
# Generate grid-cell features and merge in covariates for both training data and recent fires:
source('scripts/01-construct-training-and-prediction-data.R')
rounding_para <- 0.10
saveRDS(rounding_para, 'output-data/rounding_para.RDS')
X <- generate_data(year_min = 2015,
              rounding_para =  readRDS('output-data/rounding_para.RDS'), # degree lat/long
              get_urban_areas = T,
              get_nightlights = T,
              get_clouds = T,
              get_engineered_features = T,
              get_engineered_features_no_cloud_days = T,
              get_30d_averages = T, save_cache = T) #------ Note: This takes a long time
saveRDS(X, 'output-data/X_matrix.RDS') # Export final training data set.

# Train model of fire events 2015-2021:
# source('scripts/02-train-model-ensemble.R') # Note: This takes a long time

# Generate predictions:
X_mat <- readRDS('output-data/X_matrix.RDS')
source('scripts/03-generate-predictions-from-model-ensemble.R')
preds <- ensemble_predict(X_mat = X_mat[X_mat$year >= 2022 & X_mat$date <= Sys.Date()+14, ]) # Note: This takes a long time
saveRDS(preds[[1]], 'output-data/X_mat_res_matrix.RDS')
saveRDS(preds[[2]], 'output-data/X_mat_with_preds.RDS')
saveRDS(preds[[2]][, c('x', 'y', 'time_of_year', 'year')], 'output-data/X_mat_res_matrix_correspondance.RDS')

# Step 3: Get near-exact urban density of fires detected since war began: -----------------------------------
# Get exact urban density of points:
source('scripts/04-get-urban-density-of-points.R') # Note: This takes a long time

# Step 4: Run the classifier to determine which fires are war-related: -----------------------------------
# Note: this leverages the data created in Step 2 and 3.

# Run classifier to assess which are can be classified as sufficiently abnormal to be war fires:
source('scripts/05-war-fire-classifier.R')

#
# The scripts below generates auxiliary data and analysis used
# to understand and investigate the data constructed above.
#

# Step 5: Get daily cloud cover in Ukraine: -----------------------------------
# Get percentage of cloud cover by day and cell and overall:
source('scripts/aux_get_cloud_cover_daily.R')

# Step 6: Get assessed territorial control by day, using near-daily maps created by the Institute for the Study of War -----------------------------------
# % of territory controlled by Russia, Ukraine, and Russian attacks, and Ukrainian counter-attacks
source('scripts/aux_get_assessed_russian_attacks.R')
source('scripts/aux_get_assessed_ukrainian_counterattacks.R')
source('scripts/aux_get_assessed_russian_control.R')
source('scripts/aux_get_claimed_russian_control.R')

if(inspect){

  # Plot all areas of control:
  ru_attacks <- readRDS('output-data/area_ever_attacked_by_russia.RDS')
  ua_counters <- readRDS('output-data/area_ever_counterattacked_by_ukraine.RDS')
  ru_control <- readRDS('output-data/area_ever_assessed_as_controlled_by_Russia.RDS')
  ru_claimed <- readRDS('output-data/area_ever_claimed_as_controlled_by_Russia.RDS')
  ru_prewar <- readRDS('output-data/area_assessed_as_controlled_by_Russia_pre_war.RDS')

  library(ggplot2)
  ggplot()+geom_sf(data=ru_attacks, fill='red', alpha= 0.5)+
    geom_sf(data=ua_counters, fill='blue', alpha= 0.5)+
    geom_sf(data=ru_control, fill='darkred', alpha= 0.5)+
    geom_sf(data=ru_claimed, fill = 'darkblue', alpha = 0.5)+
    geom_sf(data=ru_prewar, fill = 'black', alpha = 0.9)

  # export it as shapefile
  names <- c('ru_attacks', 'ua_counters', 'ru_control', 'ru_claimed', 'ru_prewar')
  ind <- 1
  for(i in list(ru_attacks, ua_counters, ru_control, ru_claimed, ru_prewar)){
  temp_shp <- as_Spatial(st_make_valid(i))
  temp_shp$id <- names[ind]
  temp_shp$area_assessed_as_russia_controlled <- NULL
  class(temp_shp)
   writeOGR(obj = temp_shp, dsn = '/Users/sondresolstad/Github/ukraine-fire-map/output-data/temp-shapefile', driver='ESRI Shapefile', layer = names[ind])
   ind <- ind +1
  }

}

# Step 7: Run scripts to assess which fires are in which zones of control: -----------------------------------
# Assess which fires are in which zones of control:
# source('scripts/aux_get_control_status_of_fires.R') # Note: This takes a long time
source('scripts/aux_get_control_status_of_fires_II.R')

# Step 8: Run scripts to get the municipalities of war locations: -----------------------------------
# Get municipality of fire locations
# source('scripts/aux_get_municipalities_of_war_fires.R') # Note: This takes a long time


# Calculate time each m2 of each municipality on average spent assessed as contested: -----------------------------------
source('scripts/aux_get_days_municipalities_spent_as_contested.R')

# Step 9: Load and merge all data generated in previous steps: -----------------------------------
# Load data:
X_fire <- read_csv('output-data/all_fires_all_cols_exact_pop_and_filter_2022_2023.csv')
control <- read_csv('output-data/area_assessed_as_controlled_by_Russia.csv')
clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day.csv')
control_of_fires <- read_csv('output-data/control_status_of_fires.csv')
control_of_fires_II <- read_csv('output-data/control_status_of_fires_II.csv')
municipalities <- readRDS('output-data/points_to_municipality_with_extra_cols.RDS')
municipalities$x <- municipalities$y <- NULL
municipalities <- municipalities[municipalities$ADM3_PCODE != "", ]

# Merge data:
X_fire <- merge(X_fire, control, all.x = T, by = 'date')
X_fire <- merge(X_fire, clouds, all.x = T, by = 'date')
X_fire <- merge(X_fire, control_of_fires, all.x = T, by = c("LATITUDE", "LONGITUDE", "ACQ_TIME", "date"))
X_fire <- merge(X_fire, control_of_fires_II, all.x = T, by = c("LATITUDE", "LONGITUDE", "ACQ_TIME", "date"))
X_fire <- merge(X_fire, municipalities, all.x = T, by = c('LATITUDE', 'LONGITUDE'))

saveRDS(X_fire, 'output-data/X_analysis_data.RDS')

# Step 10: Plot and export the resulting fire map: -----------------------------------
# Load detailed map of Ukraine (from the UN):
ukraine <- st_read('source-data/ukraine-detailed-map/ukr_admbnda_adm3_sspe_20230201.shp')
ukraine <- st_transform(ukraine, "WGS84")

ggplot()+geom_sf(data=st_union(ukraine), col=NA, fill='lightgray')+
  geom_point(data = X_fire[X_fire$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
             col = 'black', alpha = 0.01, cex = 0.5)+
  geom_point(data = X_fire[X_fire$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
             alpha = 0.02, col='red', cex = 0.5)+
  theme_void()+theme(legend.position = 'none')
ggsave('plots/ukraine_fire_map.png', width = 10, height = 8)

# Step 11: Generate analysis charts: -----------------------------------
# These charts are saved in the "plots" folder
source('scripts/aux_generate_analysis_charts.R')

# Step 12: Export fire location data minimal files: -----------------------------------
write_csv(X_fire[, c('LATITUDE', 'LONGITUDE', 'ACQ_TIME', 'date', 'war_fire')], 'output-data/all_fires_small.csv')
write_csv(X_fire[X_fire$war_fire == 1, c('LATITUDE', 'LONGITUDE', 'ACQ_TIME', 'date', 'war_fire')], 'output-data/war_fires_small.csv')
