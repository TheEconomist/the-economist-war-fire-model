# This serves as the update script for The Economist's war-fire model
# Assuming the relevant input data has been correctly downloaded and required packages are all installed, it updates the war-fire data.

# Step 0: Set options and load base packages ----------------------------------------
library(readr)
library(data.table)
library(lubridate)
library(sf)
library(anytime)
options(readr.show_col_types = F)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
redo <- F
update_charts_and_animations <- T
render_animations <- wday(Sys.Date())==1

# Step 1: Load stable data ----------------------------------------
cat("\n.... Load stable data ...\n")

# This script details a function which generates a data-frame with all variables which do not depend on recent fires:
if(redo){
  source('scripts/aux_generate_dataframe_with_all_stable_covariates.R')
}

# The below loads the stable covariates used by the models year-by-year and binds them to a data frame
model_vars <- readRDS('output-data/model-objects/model_vars.RDS')
stable <- data.frame()
for(i in unique(year('2022-02-24'):year(Sys.Date()))){
  stable <- rbind(stable, read_csv(paste0('output-data/model-objects/stable_covariates_', i, '.csv'), col_select = any_of(c(model_vars, 'date'))))
}
stable <- stable[stable$date <= Sys.Date() & stable$date >= as.Date('2022-02-24'), ]

# Step 2: Update fire data ----------------------------------------
cat("\n.... Updating fire data ...\n")

# This script updates the firms data to the present day:
source('scripts/aux_update_firms_data.R')

# This script defines the function "generate_data" which generates a day-cell data frame of fire activity (and selected covariates), and, if selected, updates a csv at the fire-location-time level with population density and region covariates. Since we load all stable covariates in the step above, these are not needed in the update run.
source('scripts/01-construct-training-and-prediction-data.R')
X <- generate_data(year_min = 2021,
                   rounding_para = readRDS('output-data/model-objects/rounding_para.RDS'), # degree lat/long
                   get_pop_data = T,
                   get_urban_areas = T,
                   get_nightlights = F,
                   get_clouds = F,
                   get_engineered_features = F,
                   get_engineered_features_no_cloud_days = F,
                   get_continuous_fire = T,
                   get_30d_averages = F,
                   update_fires_df = T,
                   save_cache = F,
                   redo = F)

# Restrict data to the war
X <- X[X$date <= Sys.Date() & X$date >= as.Date('2022-02-24'), ]

# Merge in stable covariates:
X <- merge(X, stable[, c(setdiff(colnames(stable), intersect(colnames(X), colnames(stable))),
                      'x', 'y', 'date')],
           by=c('x', 'y', 'date'))
rm(stable)

# Get near-exact population density of fire event:
source('scripts/04-get-urban-density-of-points.R')
rm(pop)

# Step 3: Generate predictions ----------------------------------------
# This script loads a function that loads models and uses them to predict outcomes for an X-matrix:
cat("\n.... Generating updated predictions....\n")

source('scripts/03-generate-predictions-from-model-ensemble.R')

# The following lines generates predictions for each cell-day:
if(redo){
  preds <- ensemble_predict(X_mat = X)
  write_csv(as.data.frame(preds[[1]]), 'output-data/model-objects/boot_predictions.csv')
  write_csv(as.data.frame(preds[[2]]), 'output-data/model-objects/pred_matrix.csv')
} else {
  # Load cached predictions:
  cached_preds_1 <- read_csv('output-data/model-objects/boot_predictions.csv')
  cached_preds_2 <- read_csv('output-data/model-objects/pred_matrix.csv')

  # Predict for missing dates and cells:
  if(any(!paste0(X$date, '_', X$id) %in% paste0(cached_preds_2$date, '_', cached_preds_2$id))){

    saveRDS(X, 'output-data/model-objects/temp.RDS') # Save temporary cache for memory optim
    X <- X[!paste0(X$date, '_', X$id) %in%
             paste0(cached_preds_2$date, '_', cached_preds_2$id), ]

    preds <- ensemble_predict(X_mat = X)

    X <- readRDS('output-data/model-objects/temp.RDS') # Load temporary cache for memory optim
    unlink('output-data/model-objects/temp.RDS') # Unlink temporary cache

    # Append new predictions to cache:
    write_csv(rbind(as.data.frame(cached_preds_1), as.data.frame(preds[[1]])), 'output-data/model-objects/boot_predictions.csv')
    write_csv(rbind(as.data.frame(cached_preds_2), as.data.frame(preds[[2]])), 'output-data/model-objects/pred_matrix.csv')
  }
}

# Step 4: Run classifier ----------------------------------------
cat("\n.... Running classifier ....\n")

# Get prediction matrix
pred_mat <- read_csv('output-data/model-objects/pred_matrix.csv')
pred_mat <- pred_mat[pred_mat$date >= as.Date('2022-02-24'), ]
pred_mat$predicted_fire <- pred_mat$prediction_upper_95
pred_mat$predicted_fire[pred_mat$predicted_fire < 0] <- 0
pred_mat$predicted_fire <- ceiling(pred_mat$predicted_fire)
pred_mat <- pred_mat[, c('predicted_fire', 'year', 'time_of_year', 'id')]

# Load data on fires by location
all_fires <- read_csv('output-data/model-objects/all_fires.csv')
all_fires <- all_fires[all_fires$date >= as.Date('2022-02-24'), ]
all_fires$obs_ID <- NULL

# Load classifier function
source('scripts/aux_war_fire_classifier_function.R')
fires <- war_fire_classifier(cell_day_data = X[X$date >= as.Date('2022-02-24'), ],
                             cell_day_predictions = pred_mat,
                             fires = all_fires,
                             offset = 2,
                             min_length_of_fire_in_area = 10,
                             days_to_assign_to_war_fire_after_excess = 10,
                             exclude = read_csv('source-data/forest_fire_locations_2022_2024.csv'),
                             exclude_dates = readRDS('output-data/model-objects/exclude_dates.RDS'))

ggplot(fires[!fires$war_fire & fires$date >= as.Date('2023-07-16'), ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact, col = sustained_excess))+geom_point(alpha = 0.2)+
  geom_point(data=fires[fires$war_fire == T & fires$date >= as.Date('2023-07-16'), ], col = 'red', alpha = 0.2)

rm(X)
rm(pred_mat)
rm(all_fires)

# 5 - Run tests: --------------------------------------------------
cat("\n.... Running tests....\n")

# Test: vast number of new war fires?
old <- read_csv('output-data/ukraine_fires.csv')
new_fires <- sum(fires$war_fire) - sum(old$war_fire)
 cat(paste0('\n Recorded ', new_fires, ' new war fires in this update.\n'))
#if(new_fires > 2000){
#  stop('Over 2000 new war fires detected in this update - please inspect manually.')
#}

# Test: fighting in new area of the country?
library(geosphere)
temp <- unique(fires[!fires$id %in% old$id & fires$war_fire == T,
                     c('x', 'y')])
new_area <- F
if(nrow(temp) > 0){
  for(i in 1:nrow(temp)){
    distances <- distm(unique(old[old$war_fire == T, c('x', 'y')]),
                      temp[i, ], fun = distHaversine)
    if(!any(distances < 2000000)){
      cat(paste0('- War fire detected very far from past activity: lat=', temp[i, 'y'],' lng=', temp[i, 'x']))
      new_area <- T
    }
  }
  if(new_area){
    stop('New area of fighting - please inspect manually.')
  }
}
rm(temp)

# Test: fewer fires than previously?
if(new_fires < 0){

  if(nrow(read_csv('source-data/forest_fire_locations_2022_2024.csv')) > readRDS('output-data/model-objects/n_manual_exceptions.RDS')){
    message('Note: Fewer war fires than previously - probably due to manual exclusion (e.g. forest fire).')
  } else {
    message('Fewer war fires than previously - please inspect manually.')
  }
}
saveRDS(nrow(read_csv('source-data/forest_fire_locations_2022_2024.csv')), 'output-data/model-objects/n_manual_exceptions.RDS')
rm(new_fires)
rm(old)

# 6 - Update and save charts and data --------------------------------------------------

# Update cloud data:
source('scripts/aux_update_cloud_cover_daily_with_forecast.R')

# Define war fires data frame:
war_fires <- fires[fires$war_fire == T, ]

# Export to file:
write_csv(fires, 'output-data/ukraine_fires.csv')
write_csv(war_fires, 'output-data/ukraine_war_fires.csv')

if(update_charts_and_animations){
  cat("\n.... Updating charts and data exports....\n")

  ukraine <- st_read('source-data/ukraine-detailed-map/ukr_admbnda_adm2_sspe_20230201.shp')
  ukraine <- st_transform(ukraine, "WGS84")
  ukraine$date <- Sys.Date()

  urban <- st_read('source-data/urban-areas/ne_10m_urban_areas_landscan.shp')
  urban <- urban[urban$mean_bb_xc >= 22 & urban$mean_bb_xc <= 40.2 &
                   urban$mean_bb_yc >= 45 & urban$mean_bb_yc <= 52.5, ]
  urban$date <- Sys.Date()

  zones_of_control <- st_zm(st_read('source-data/ISW_APR022023/UkraineControlMapAO02APR2023.shp'))
  zones_date <- as.Date('2023-04-02')

  # Select spotlight
  spotlight_ADM1_EN <- "Donetska"
  spotlight <- ukraine[ukraine$ADM1_EN == "Donetska", ]
  spotlight_zoom <- st_bbox(spotlight)

  # Select spotlight 2
  spotlight_ADM2_EN <- "Bakhmutskyi"
  spotlight_2 <- ukraine[ukraine$ADM2_EN == "Bakhmutskyi", ]
  spotlight_zoom_2 <- st_bbox(spotlight_2)
  spotlight_zoom_2[1] <- spotlight_zoom_2[1]+0.15
  spotlight_zoom_2[2] <- spotlight_zoom_2[2]+0.27
  spotlight_zoom_2[3] <- spotlight_zoom_2[3]-0.35
  spotlight_zoom_2[4] <- spotlight_zoom_2[4]-0.3

  # Generate simplified map for animations
  ukraine_animate <- st_union(ukraine)
  ukraine_animate <- st_simplify(ukraine_animate, dTolerance = 0.01)
  spotlight_animate <- st_union(spotlight)

  spotlight <- st_simplify(spotlight, dTolerance = 0.005)
  ukraine <- st_simplify(ukraine, dTolerance = 0.005)
  clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day_with_forecast_for_recent_days.csv')

  last_week <- fires[fires$date %in% as.Date(Sys.Date():(Sys.Date()-7), origin = '1970-01-01'), ]
  last_month <- fires[fires$date %in% as.Date(Sys.Date():(Sys.Date()-30), origin = '1970-01-01'), ]

  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+
    geom_point(data = fires[fires$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               col = 'black', alpha = 0.025)+
    geom_point(data =fires[fires$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               alpha = 0.05, col='red')+
    theme_minimal()+theme(legend.position = 'none')+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))
  ggsave('plots/live_ukraine_fire_map.png', width = 10, height = 8)

  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+geom_sf(data=zones_of_control, col='red', fill=NA)+
    geom_point(data = last_month[last_month$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               col = 'black', alpha = 0.25)+
    geom_point(data =last_month[last_month$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               alpha = 0.5, col='red')+
    theme_minimal()+theme(legend.position = 'none')+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-7, ' to ', Sys.Date(), "\n(Zones of control as per ISW, ", zones_date, ")"))+
    coord_sf(xlim=c(min(last_month$LONGITUDE[last_month$war_fire == T])-3, 41),
             ylim=c(max(last_month$LATITUDE[last_month$war_fire == T])+0.5, 44), expand = F)
  ggsave('plots/live_ukraine_fire_map_last_month.png', width = 10, height = 8)

  if(nrow(last_week) > 0){
  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+
    geom_sf(data=zones_of_control, col='red', fill=NA)+
    geom_point(data = last_week[last_week$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               col = 'black', alpha = 0.25)+
    geom_point(data =last_week[last_week$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               alpha = 0.5, col='red')+
    theme_minimal()+theme(legend.position = 'none')+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-7, ' to ', Sys.Date(), "\n(Zones of control as per ISW, ", zones_date, ")"))+
    coord_sf(xlim=c(min(last_week$LONGITUDE[last_week$war_fire == T])-3, 41),
             ylim=c(max(last_week$LATITUDE[last_week$war_fire == T])+0.5, 44), expand = F)
  ggsave('plots/live_ukraine_fire_map_last_week.png', width = 10, height = 8)
  }

  if(nrow(last_month) > 0){
  # Generate spotlight plots:
  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+geom_sf(data=spotlight, col='black', alpha = 0.8)+geom_sf(data=zones_of_control, col='red', fill = NA)+geom_sf(data=urban, fill = 'darkgray')+
    geom_point(data = last_month[last_month$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               col = 'black', alpha = 0.15)+
    geom_point(data =last_month[last_month$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               alpha = 0.2, col='red')+
    theme_minimal()+theme(legend.position = 'none')+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM1_EN, ' - '))+
    coord_sf(xlim=spotlight_zoom[c(1,3)],
             ylim=spotlight_zoom[c(2,4)], expand = F)
  ggsave('plots/live_ukraine_fire_map_spotlight_1.png', width = 10, height = 8)

  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+geom_sf(data=spotlight, col='black', alpha = 0.8)+geom_sf(data=zones_of_control, col='red', fill = NA)+geom_sf(data=urban, fill = 'darkgray')+
    geom_point(data =last_month[, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact, col=date), alpha = 0.2)+theme_minimal()+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM1_EN, ' - ', "\n(Zones of control as per ISW, ", zones_date, ")"))+
    coord_sf(xlim=spotlight_zoom[c(1,3)],
             ylim=spotlight_zoom[c(2,4)], expand = F)
  ggsave('plots/live_ukraine_fire_map_spotlight_2.png', width = 10, height = 8)

# We use cache for streets
# streets <- get_base_map(bbox = spotlight_zoom_2)
# saveRDS(streets, 'output-data/model-objects/streets.RDS')
streets <- readRDS('output-data/model-objects/streets.RDS')

  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+geom_sf(data=spotlight_2, col='black', alpha = 0.8)+geom_sf(data=urban, fill = 'darkgray')+geom_sf(data = streets[[1]]$osm_lines, inherit.aes = FALSE, color = "black", size = .4, alpha = .4) +
    geom_sf(data = streets[[2]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .4,
            alpha = .3)+
    geom_sf(data = streets[[3]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            alpha = .2)+
    geom_sf(data=zones_of_control, col='red', fill = NA)+
    geom_point(data = last_month[last_month$war_fire == 0, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               col = 'black', alpha = 0.15)+
    geom_point(data =last_month[last_month$war_fire == 1, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact),
               alpha = 0.4, col='red')+
    theme_minimal()+theme(legend.position = 'none')+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 0.05),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 0.05),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM2_EN, ' - ', "\n(Zones of control as per ISW, ", zones_date, ")"))+
    coord_sf(xlim=spotlight_zoom_2[c(1,3)],
             ylim=spotlight_zoom_2[c(2,4)], expand = F)
  ggsave('plots/live_ukraine_fire_map_spotlight_3.png', width = 10, height = 8)

  ggplot()+geom_sf(data=ukraine, col='darkgray', fill='lightgray')+geom_sf(data=zones_of_control, col='red', fill = NA)+geom_sf(data=spotlight_2, col='black', alpha = 0.8)+geom_sf(data = streets[[1]]$osm_lines, inherit.aes = FALSE, color = "black", size = .4, alpha = .4) +
    geom_sf(data = streets[[2]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .4,
            alpha = .3)+
    geom_sf(data = streets[[3]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            alpha = .2)+
    geom_point(data =last_month[, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact, col=date), alpha = 0.4)+theme_minimal()+xlab('')+ylab('')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM2_EN, ' - ', "\n(Zones of control as per ISW, ", zones_date, ")"))+
    coord_sf(xlim=spotlight_zoom_2[c(1,3)],
             ylim=spotlight_zoom_2[c(2,4)], expand = F)
  ggsave('plots/live_ukraine_fire_map_spotlight_4.png', width = 10, height = 8)

  ggplot()+geom_sf(data=ukraine, col=NA, fill=NA)+geom_sf(data=zones_of_control, col='red', fill = NA)+geom_sf(data=spotlight_2, col='black', alpha = 0.8)+geom_sf(data = streets[[1]]$osm_lines, inherit.aes = FALSE, color = "black", size = .4, alpha = .4) +
    geom_sf(data = streets[[2]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .4,
            alpha = .3)+
    geom_sf(data = streets[[3]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            alpha = .2)+
    geom_point(data =last_month[, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact, col=date), alpha = 0.4)+theme_minimal()+xlab('')+ylab('')+theme(legend.position = 'none')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM2_EN, ' - ', "\n(Zones of control as per ISW, ", zones_date, ")\nby week"))+
    coord_sf(xlim=spotlight_zoom_2[c(1,3)],
             ylim=spotlight_zoom_2[c(2,4)], expand = F)+facet_wrap(week(date)~.)
  ggsave('plots/live_ukraine_fire_map_spotlight_4_by_week.png', width = 10, height = 8)

  ggplot()+geom_sf(data=ukraine, col=NA, fill=NA)+geom_sf(data=zones_of_control, col='red', fill = NA)+geom_sf(data=spotlight_2, col='black', alpha = 0.8)+geom_sf(data = streets[[1]]$osm_lines, inherit.aes = FALSE, color = "black", size = .4, alpha = .4) +
    geom_sf(data = streets[[2]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .4,
            alpha = .3)+
    geom_sf(data = streets[[3]]$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = .2,
            alpha = .2)+
    geom_point(data =last_month[, ], aes(x=LONGITUDE, y=LATITUDE, size = pop_exact, col=date), alpha = 0.4)+theme_minimal()+xlab('')+ylab('')+theme(legend.position = 'none')+
    scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+ggtitle(paste0('Fire activity between ', Sys.Date()-30, ' to ', Sys.Date(), '\n - ', spotlight_ADM2_EN, ' - ', "\n(Zones of control as per ISW, ", zones_date, ")\nby week"))+
    coord_sf(xlim=spotlight_zoom_2[c(1,3)],
             ylim=spotlight_zoom_2[c(2,4)], expand = F)+facet_wrap(date~.)
  ggsave('plots/live_ukraine_fire_map_spotlight_4_by_day.png', width = 10, height = 8)
  }

  # Save animation of fire activity so far
  if(render_animations){
    rm(ukraine)
    library(gganimate)

    # day-by-day
    anim_fires <- fires[, c('LATITUDE', 'LONGITUDE', 'date', 'war_fire')]
    N_fade <- 15
    anim_fires$s <- N_fade+1

    big <- data.frame()
    for(i in 1:N_fade){
      temp <- anim_fires
      temp$s <- temp$s - i
      temp$date <- temp$date + i
      big <- rbind(big, temp)
    }
    anim_fires <- rbind(anim_fires, big)
    rm(temp)
    rm(big)

    anim_fires <- anim_fires[order(anim_fires$date), ]

    anim <- ggplot()+geom_sf(data=ukraine_animate)+
      geom_point(data=anim_fires, aes(x=LONGITUDE, y=LATITUDE,
                           size=s, group=1:nrow(anim_fires)), col=ifelse(anim_fires$war_fire==1, 'darkred', 'darkgray'), alpha = ifelse(anim_fires$war_fire==1, 0.05, 0.01))+transition_states(date)+theme_void()+
      ylab('')+xlab('')+theme(legend.pos = 'none')+labs(title = "{closest_state}")+
      scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
      scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))
    animate(anim,  width = 10, height = 8, units = 'in', res = 150, duration = 40, nframes = length(range(fires$date)[1]:range(fires$date)[2]))
    anim_save('plots/live_ukraine_fire_map_animated_day_by_day.gif')
    rm(anim_fires)

    # Cumulative
    anim <- ggplot(fires)+geom_sf(data=ukraine_animate)+
      geom_point(data = fires[fires$war_fire == F, ],
                 aes(col=as.factor(war_fire), group = 1:sum(fires$war_fire == F), x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col = 'darkgray', )+
      geom_point(data = fires[fires$war_fire == T, ],
                 aes(group = 1:sum(fires$war_fire == T), x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col= 'darkred')+
      transition_reveal(date)+theme_void()+
      ylab('')+xlab('')+theme(legend.pos = 'none')+labs(title = "{frame_along}")+
      scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
      scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))
    animate(anim,  width = 10, height = 8, units = 'in', res = 150, duration = 20, nframes = length(range(fires$date)[1]:range(fires$date)[2]))
    anim_save('plots/live_ukraine_fire_map_animated.gif')
    # animate(anim, device = "svglite",
            # renderer = file_renderer("~/plots/svg", prefix = "gganim_plot", overwrite = TRUE)) # Optional, exports svg.
    # # Save animation of fire activity in last 30 days
    # anim <- ggplot(last_month)+geom_sf(data=ukraine_animate)+
    #   geom_point(data = last_month[last_month$war_fire == F, ],
    #              aes(col=as.factor(war_fire), group = 1:sum(last_month$war_fire == F),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col = 'darkgray')+
    #   geom_point(data = last_month[last_month$war_fire == T, ],
    #              aes(group = 1:sum(last_month$war_fire == T),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col= 'darkred')+
    #   transition_reveal(date)+theme_minimal()+
    #   ylab('')+xlab('')+theme(legend.pos = 'none')+labs(title = "{frame_along}")+
    #   scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    #   scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))
    # animate(anim,  width = 10, height = 8, units = 'in', res = 150)
    # anim_save('plots/live_ukraine_fire_map_last_month_animated.gif', duration = 20, nframes = 30)
    #
    # # Save animation of fire activity in last 7 days
    # anim <- ggplot(last_week)+geom_sf(data=ukraine_animate)+
    #   geom_point(data = last_month[last_month$war_fire == F, ],
    #              aes(col=as.factor(war_fire), group = 1:sum(last_month$war_fire == F),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col = 'darkgray')+
    #   geom_point(data = last_month[last_month$war_fire == T, ],
    #              aes(group = 1:sum(last_month$war_fire == T),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.1, col= 'darkred')+
    #   transition_reveal(date)+theme_minimal()+
    #   ylab('')+xlab('')+theme(legend.pos = 'none')+labs(title = "{frame_along}")+
    #   scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    #   scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))
    # animate(anim,  width = 10, height = 8, units = 'in', res = 150)
    # anim_save('plots/live_ukraine_fire_map_last_week_animated.gif', duration = 20, nframes = 7)
    #
    # # Save animation of fire activity in spotlight location
    # anim <- ggplot(last_month)+geom_sf(data=ukraine_animate)+geom_sf(data=spotlight_animate)+
    #   geom_point(data = last_month[last_month$war_fire == F, ],
    #              aes(col=as.factor(war_fire), group = 1:sum(last_month$war_fire == F),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.5, col = 'darkgray')+
    #   geom_point(data = last_month[last_month$war_fire == T, ],
    #              aes(group = 1:sum(last_month$war_fire == T),
    #                  x=LONGITUDE, y=LATITUDE, size = pop_exact), alpha = 0.5, col= 'darkred')+
    #   transition_states(date)+theme_minimal()+
    #   ylab('')+xlab('')+theme(legend.pos = 'none')+labs(title = "{closest_state}")+
    #   scale_x_continuous(breaks = round(seq(20, 50, by = 1),1)) +
    #   scale_y_continuous(breaks = round(seq(30, 90, by = 1),1))+
    #   coord_sf(xlim=spotlight_zoom[c(1,3)],
    #            ylim=spotlight_zoom[c(2,4)], expand = F)
    # animate(anim,  width = 10, height = 8, units = 'in', res = 150, duration = 20, nframes = 30)
    # anim_save('plots/live_ukraine_fire_map_last_month_spotlight.gif')
  }
# Save plots of fire activity by day:
ggplot(war_fires, aes(x=date))+geom_bar()+theme_minimal()+ylab('')+xlab('')+ggtitle('Fires assessed as war-related per day')+xlab('\nNote: satellites cannot detect fires through cloud clover')
ggsave('plots/fire_activity_per_day.png', width = 10, height = 4)

ggplot(clouds, aes(x=date, group=cumsum(is.na(cloud_cover_in_country))))+geom_area(aes(y=cloud_cover_in_country*100, col="% cloud cover in Ukraine", fill="% cloud cover in Ukraine", ymin=0), fill = 'gray', col='gray')+theme_minimal()+theme(legend.pos = 'bottom', legend.title = element_blank())+ylab('')+xlab('')+xlim(c(as.Date('2022-02-24'), Sys.Date()))+xlab('Cloud cover in Ukraine, %\n(Pink = no cloud data)')+geom_vline(aes(xintercept=ifelse(is.na(cloud_cover_in_country), date, NA)), alpha = 0.2, col='red')
ggsave('plots/cloud_cover_by_day.png', width = 10, height = 4)

ggplot(war_fires, aes(x=date, y=pop_exact, alpha = 0.5))+geom_point()+theme_minimal()+ylab('')+xlab('')+ggtitle('Population density of fires assessed as war-related')+xlab('\nNote: satellites cannot detect fires through cloud clover')+theme(legend.pos='none')
ggsave('plots/fire_by_pop_density_per_day.png', width = 10, height = 4)

# This script generates a plot of fires by day and zone of control
source('scripts/aux_plot_fires_by_day_and_territorial_control.R')

} else {
  cat("\n.... Updating data exports (charts + animations not updated in this run)....\n")}

# Export to file:
write_csv(fires, 'output-data/ukraine_fires.csv')
write_csv(war_fires, 'output-data/ukraine_war_fires.csv')

# Export war fires by ADM3:
source('scripts/aux_get_municipalities_of_war_fires.R')
