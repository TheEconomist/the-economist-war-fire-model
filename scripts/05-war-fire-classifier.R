# This script details the algorithm that uses predicted fires at the cell level to categorize fires into those deemed probable-war related.

# 1. Define function to get excess: ----------------------------------------
get_excess <- function(comparison,
                       current,
                       n,
                       offset = 1,
                       offset_months = 1:12,
                       X_fire = readRDS('output-data/all_fires_all_cols_exact_pop.RDS'),
                       local_excess = T,
                       use_prediction = F,
                       prediction, manually_exclude = T,
                       min_length_of_fire_in_area = 0,
                       assign_to_war_fire_after_large_excess = F){
  cat('\n Load data...\n')

  if(manually_exclude){

    # Remove the swamp fire in the Danube basin natural park:
    current$fire[current$y <= 45.545 & current$x >= 29.356 & current$x <= 29.844 & current$year == 2022] <- 0

    # Remove crop and swamp fires near Danube (inspected manually):
    current$fire[current$y <= 45.545 & current$x >= 28.21 & current$x <= 29.356 & current$year == 2022] <- 0

    # Remove the crop fires north of the Danube basin natural park:
    current$fire[current$y <= 45.845 & current$x >= 28.21 & current$x <= 30.356 & current$year == 2022] <- 0

    # Remove three forest fires (some possibly crop) west of the Carpathian mountains (inspected manually):
    current$fire[current$y <= 48.48 & current$y >=  48.43 &
                   current$x >= 22.32 & current$x <= 22.36 & current$year == 2022] <- 0
    current$fire[current$y <= 48.32 & current$y >=  48.29 &
                   current$x >= 22.99 & current$x <= 23.02 & current$year == 2022] <- 0
    current$fire[current$y <= 48.88 & current$y >=  48.84 &
                   current$x >= 22.50 & current$x <= 22.55 & current$year == 2022] <- 0

    # Remove forest fire (and possibly part crop fire) at Moldovian border:
    current$fire[current$y <= 46.60 & current$y >= 46.52 &
                   current$x >= 29.85 & current$x <= 29.98 & current$year == 2022] <- 0

    current$fire[current$y <= 47.28 & current$y >= 47.18 &
                   current$x >= 29.53 & current$x <= 29.69 & current$year == 2022] <- 0

    # Remove forest/crop fires near Piddubivka
    current$fire[current$y <= 50.30 & current$y >= 50.25 &
                   current$x >= 32.18 & current$x <= 32.23 & current$year == 2022] <- 0
    current$fire[current$y <= 50.55 & current$y >= 50.45 &
                   current$x >= 31.90 & current$x <= 31.99 & current$year == 2022] <- 0

    # Remove fire in field south of Armashivka
    current$fire[current$y <= 47.252 & current$y >= 47.185 &
                   current$x >= 30.124 & current$x <= 30.225 & current$year == 2022] <- 0

    # Remove forest fires near border of Belarus north of Ivankiv
    current$fire[current$y <= 51.55 & current$y >= 50.30 &
                   current$x >= 29.455 & current$x <= 30.24 & current$date %in% as.Date('2022-06-01'):as.Date('2022-11-01')] <- 0

  }

  # Read current year data:
  current$fire_in_window <- current$fire
  current$mean_fire_in_window <- ceiling(ave(current$fire_in_window, current$id, FUN = function(x) mean(x, na.rm = T)))

  if(!use_prediction){
    # Order comparison data:
    comparison <- comparison[order(comparison$date), ]

    # Calculate mean:
    comparison$mean_fire <- ave(comparison$fire, comparison$id, FUN = function(x){
      mean(x, na.rm = T)
    })

    # Calculate max:
    comparison$max_fire <- ave(comparison$fire, comparison$id, FUN = function(x){
      max(x, na.rm = T)
    })

    # Use the ceiling:
    comparison$mean_fire <- ceiling(comparison$mean_fire)
    comparison$max_fire <- ceiling(comparison$max_fire)

    # Get id-mean-fire-data set:
    comparison <- unique(comparison[, c('mean_fire', 'max_fire', 'id')])

    cat('\n Calculate means, maxes, and merge data...\n')

    cells <- merge(comparison, current, by = c('id'))
    } else {
    cells <- current
    cells$mean_fire <- NA
    cells$max_fire <- NA
    }

    # Merge current year and comparison data:
    X_fire <- merge(X_fire, cells[, c('id', 'mean_fire', 'fire_in_window', 'mean_fire_in_window', 'time_of_year', 'year')])
    X_fire$id_w_time <- paste0(X_fire$id, '-', X_fire$time_of_year, '-', X_fire$year)

    # Specify offset:
    X_fire$offset <- offset
    if(!missing(offset_months)){
      X_fire$offset <- 0
      library(lubridate)
      X_fire$offset[month(X_fire$date) %in% offset_months] <- offset
    }

  if(use_prediction){
    X_fire <- merge(X_fire, prediction[, c('predicted_fire', 'year', 'time_of_year', 'id', 'id_5x5')], by = c('year', 'time_of_year', 'id'), all.x=T)
    X_fire$excess_fire <- X_fire$fire_in_window - X_fire$predicted_fire*n - X_fire$offset

    # Calculate for 5x5 area:
    cat('\n Calculate excess by 5x5 cell...\n')
    areas <- X_fire[!duplicated(X_fire$id_w_time), ]
    areas$excess_fire[areas$excess_fire < 0] <- 0

    areas$excess_fire_5x5 <- ave(areas$excess_fire,
                                  paste0(areas$id_5x5, '-',
                                         areas$year, '-', areas$time_of_year),
                                 FUN = function(x) sum(x, na.rm = T))
    areas$excess_fire_5x5_ind <- ave(areas$excess_fire,
                                      paste0(areas$id_5x5, '-',
                                             areas$year, '-', areas$time_of_year),
                                     FUN = function(x) sum(x > 0, na.rm = T))

    X_fire <- merge(X_fire, unique(areas[, c('id_5x5', 'time_of_year', 'year',
                                      'excess_fire_5x5',
                                      'excess_fire_5x5_ind')]),
                    all.x = T, by = c('id_5x5', 'time_of_year', 'year'))

  } else {
    # Calculate excess fire as n times mean of previous year:
    if(local_excess){
      X_fire$excess_fire <- X_fire$fire_in_window - X_fire$mean_fire*n - X_fire$offset
    } else {
      X_fire$excess_fire <- X_fire$mean_fire_in_window - X_fire$mean_fire*n
    }
    cat('\n NB: Using global mean for prediction.\n')
  }

  # Do not consider deficit fires:
  X_fire$excess_fire[X_fire$excess_fire < 0] <- 0
  X_fire$excess_fire <- ceiling(X_fire$excess_fire)

  # Set war fire tag to default:
  X_fire$war_fire <- NA

  ind <- 0
  total <- length(unique(X_fire$id_w_time[X_fire$excess_fire > 0]))

  # Set war fire tag to 0 if no excess fire in cell
  X_fire$war_fire[X_fire$excess_fire <= 0] <- 0

  cat('\n Loop through cells to distribute excess fires...\n')

  # Loop through remaining cells and distribute excess fires
  for(i in unique(X_fire$id_w_time[X_fire$excess_fire > 0])){
    ind <- ind + 1
    start <- Sys.time()
    vector <- X_fire$excess_fire[X_fire$id_w_time == i]
    if(vector[1] > length(vector)){
      X_fire$war_fire[X_fire$id_w_time == i] <- 1
    } else {
      set.seed(112358)
      X_fire$war_fire[X_fire$id_w_time == i] <- sample(c(rep(1, vector[1]), rep(0, length(vector)-vector[1])))
    }
    cat(paste0('\r\r\r\r calculating for: ', i, '// time per cell & day: ', round(difftime(Sys.time(), start, units = 'secs'), 5), 's // ', round(100*ind/total, 5), '% complete...............'))
  }

  if(manually_exclude){
    # Remove the swamp fire in the Danube basin natural park:
    X_fire$war_fire[X_fire$LATITUDE <= 45.545 & X_fire$LONGITUDE >= 29.356
                    & X_fire$LONGITUDE <= 29.844 & X_fire$year == 2022] <- 0
    # Remove crop and swamp fires near Danube (inspected manually):
    X_fire$war_fire[X_fire$LATITUDE <= 45.545 & X_fire$LONGITUDE >= 28.21
                    & X_fire$LONGITUDE <= 29.356 & X_fire$year == 2022] <- 0
    # Remove the crop fires north of the Danube basin natural park (inspected manually):
    X_fire$war_fire[X_fire$LATITUDE <= 45.845 & X_fire$LONGITUDE >= 29.356
                    & X_fire$LONGITUDE <= 30.844 & X_fire$year == 2022] <- 0

    # Remove three forest fires west of the Carpathian mountains (inspected manually):
    X_fire$war_fire[X_fire$LATITUDE <= 48.48 & X_fire$LATITUDE >= 48.43 &
                      X_fire$LONGITUDE >= 22.32 & X_fire$LONGITUDE <= 22.36 & X_fire$year == 2022] <- 0
    X_fire$war_fire[X_fire$LATITUDE <= 48.32 & X_fire$LATITUDE >= 48.29 &
                      X_fire$LONGITUDE >= 22.99 & X_fire$LONGITUDE <= 23.02 & X_fire$year == 2022] <- 0
    X_fire$war_fire[X_fire$LATITUDE <= 48.88 & X_fire$LATITUDE >= 48.84 &
                      X_fire$LONGITUDE >= 22.50 & X_fire$LONGITUDE <= 22.55 & X_fire$year == 2022] <- 0

    # Remove forest fire (and possibly crop fire) at Moldovian border:
    X_fire$war_fire[X_fire$LATITUDE <= 46.60 & X_fire$LATITUDE >= 46.52 &
                      X_fire$LONGITUDE >= 29.85 & X_fire$LONGITUDE <= 29.98 & X_fire$year == 2022] <- 0

    X_fire$war_fire[X_fire$LATITUDE <= 47.28 & X_fire$LATITUDE >= 47.18 &
                  X_fire$LONGITUDE >= 29.53 & X_fire$LONGITUDE <= 29.69 & X_fire$year == 2022] <- 0

    # Remove fire in field south of Armashivka
    X_fire$war_fire[X_fire$LATITUDE <= 47.252 & X_fire$LATITUDE >= 47.185 &
                      X_fire$LONGITUDE >= 30.124 & X_fire$LONGITUDE <= 30.225 & X_fire$year == 2022] <- 0

    # Remove forest/crop fires near Piddubivka
    X_fire$war_fire[X_fire$LATITUDE <= 50.30 & X_fire$LATITUDE >= 50.25 &
                      X_fire$LONGITUDE >= 32.18 & X_fire$LONGITUDE <= 32.23 & X_fire$year == 2022] <- 0
    X_fire$war_fire[X_fire$LATITUDE <= 50.55 & X_fire$LATITUDE >= 50.45 &
                      X_fire$LONGITUDE >= 31.90 & X_fire$LONGITUDE <= 31.99 & X_fire$year == 2022] <- 0

    # Remove forest fires near border of Belarus north of Ivankiv
    X_fire$war_fire[X_fire$LATITUDE <= 51.55 & X_fire$LATITUDE >= 50.30 &
                      X_fire$LONGITUDE >= 29.455 & X_fire$LONGITUDE <= 30.24 &
                      X_fire$date %in% as.Date('2022-06-01'):as.Date('2022-11-01') & X_fire$year == 2022] <- 0

  }

  # In areas with mass excess (i.e. beyond median in places with excess), set all fires to excess fires.
  X_fire$war_fire[X_fire$war_fire == 1 | X_fire$excess_fire >= median(X_fire$excess_fire[X_fire$excess_fire > 0])] <- 1

  # Check length of interval for cell with abnormal activity:
  X_fire$length_of_war_fire <- NA
  X_fire$length_of_war_fire[X_fire$war_fire == 1] <- X_fire$date[X_fire$war_fire == 1]
  X_fire$length_of_war_fire <- ave(X_fire$length_of_war_fire, X_fire$id, FUN = function(x) {
    x <- na.omit(unique(x))
    if(length(x) <= 1){
      1
    } else {
      length(min(x):max(x))
    }
  })

  # And in areas
  X_fire$id_big <- paste0(round(X_fire$x), '-', round(X_fire$y))
  X_fire$length_of_war_fire_area <- NA
  X_fire$length_of_war_fire_area[X_fire$war_fire == 1] <- X_fire$date[X_fire$war_fire == 1]
  X_fire$length_of_war_fire_area <- ave(X_fire$length_of_war_fire, X_fire$id_big, FUN = function(x) {
    x <- na.omit(unique(x))
    if(length(x) <= 1){
      1
    } else {
      length(min(x):max(x))
    }
  })

  # Exclude locations which see anomalous events spanning less than x days within the same year
  X_fire$sustained_excess <- NA
  X_fire$sustained_excess[X_fire$war_fire == 1] <- X_fire$date[X_fire$war_fire == 1]
  X_fire$sustained_excess <- ave(X_fire$sustained_excess, X_fire$id, FUN = function(x) {
    x <- na.omit(unique(x))
    if(length(x) <= 1){
      F
    } else {
      if(length(x) > 3*min_length_of_fire_in_area){
         T
      } else {
        if(any(abs(combn(x, 2)[1, ] - combn(x, 2)[2, ]) %in% (min_length_of_fire_in_area-1):(365-1))){
          T
        } else {
          F
        }
      }
    }
  })

  X_fire$war_fire[!X_fire$sustained_excess] <- 0

  # Save this version of the classifier, which does not leverage the theoretical prediction that normal activity takes 10 days to resume after a large war-related event in a location.
  X_fire$war_fire_restrictive <- X_fire$war_fire

  # Finally, assign fires happening in areas which have just seen fires in the war fires category:
  if(assign_to_war_fire_after_large_excess){
  fire_area <- X_fire[X_fire$war_fire == 1 & X_fire$excess_fire >= 1
                      , c('id', 'date')]
    for(i in 1:10){
      fire_area_id <- paste0(fire_area$id, '_', fire_area$date + i)
      X_fire$war_fire[paste0(X_fire$id, '_', X_fire$date) %in% fire_area_id] <- 1
    }
  }

  return(X_fire)}

# 2. Load data: ----------------------------------------
X_mat <- readRDS('output-data/X_matrix.RDS')

cat('\n Running war fires classifier. See script "aux_war_fire_classifier.R" for exact settings.\n')

pred_mat <- readRDS('output-data/X_mat_with_preds.RDS')
pred_mat$predicted_fire <- pred_mat$prediction_upper_90
pred_mat$predicted_fire[pred_mat$predicted_fire < 0] <- 0
pred_mat$predicted_fire <- ceiling(pred_mat$predicted_fire)

pred_mat <- pred_mat[, c('predicted_fire', 'year', 'time_of_year', 'id', 'id_5x5')]

# 3. Predict: ----------------------------------------
X_fire_exp <- get_excess(current = X_mat[X_mat$date > as.Date('2022-02-23'), ],
                         n = 1,
                         use_prediction = T,
                         prediction = pred_mat,
                         offset = 2,
                         min_length_of_fire_in_area = 7,
                         assign_to_war_fire_after_large_excess = T)


cat(paste0('\n Classified ', sum(X_fire_exp$war_fire == 1), ' as likely war-related fires.\n'))

if(inspect){

  library(ggplot2)
  ggplot(X_fire_exp[, ], aes(x=LONGITUDE, y=LATITUDE, col=date))+
    geom_point(alpha = 0.2)+geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1 , ], alpha = 0.2, col = 'red')
  sum(X_fire_exp$war_fire == 1)
  dim(X_fire_exp)

  X_fire_exp$map_col <- NA
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-02-24'):as.Date('2022-05-31')] <- "feb-may"
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-06-01'):as.Date('2022-10-31')] <- "jun-sep"
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-11-01'):as.Date('2023-02-25')] <- "nov-now"

  ggplot(X_fire_exp[, ], aes(x=LONGITUDE, y=LATITUDE))+geom_point(alpha = 0.01)+geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1, ], alpha=0.1, col = 'red')+facet_wrap(.~map_col)

  # Details for a few specific points, in case of interest:
  #+geom_point(aes(x=31.29347, y=51.72189), col = 'green', cex=0.1) # Massive attacks on Chernihiv on Sep 1st See: https://www.pravda.com.ua/eng/news/2022/09/3/7365971/
  #+geom_point(aes(x=35.15, y=47.85), col = 'green', cex=0.1) # Fighting over Zaporizhzhia
  #+geom_point(aes(x=34.6, y=48.5), col = 'green', cex=0.1) # Attacks on Dnipro and neighbouring cities.
  #+geom_point(aes(x=30.05, y=49.85), col = 'green', cex=0.1) # This was a huge kamikaze drone attack on BILA TSERKVA (by iranian drones), see; https://www.ukrainianworldcongress.org/russia-used-irans-kamikaze-drones-to-attack-bila-tserkva/
  #+geom_point(aes(x=28.66, y=50.93), col = 'green', cex=0.1) # This is detailed in the following: "Three have today been injured in air strike on western Ukraine, emergency services said, as thirteen buildings were damaged in the attack, which targeted the Korostensky district north of the region's main city Zhytomyr. 'Three people were injured,' a Facebook post from Ukraine's emergency services added, posting images of burning buildings and scattered charred debris. Also on Sunday, Russia's defence ministry said its 'high-precision missiles' hit a training centre of Ukrainian special forces in Zhytomyr region, around 150 kilometres (90 miles) west of Ukraine's capital Kyiv." # https://www.dailymail.co.uk/news/article-10634037/Russia-issues-horrifying-ultimatum-Ukraine-bombing-art-school-sheltering-400.html

  # Uncomment below to inspect by month of year:
  # ind <- 1
  #
  # ind <- ind + 1
  # ggplot(X_fire_exp[month(X_fire_exp$date) == ind, ], aes(x=LONGITUDE, y=LATITUDE, col=date))+
  #   geom_point(alpha = 0.2)+geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1 & month(X_fire_exp$date) == ind, ], alpha = 0.2, col = 'red')+geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1, ], alpha = 0.02, col = 'green', cex = 0.001)
  # ind

  }


library(readr)
write_csv(X_fire_exp, 'output-data/all_fires_all_cols_exact_pop_and_filter_2022_2023.csv')

cat('\n Demeaning filter completed. Data with filter saved as all_fires_all_cols_exact_pop_and_filter_2022_2023.csv \n.')
