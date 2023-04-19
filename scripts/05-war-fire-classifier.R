# This script details the algorithm that uses predicted fires at the cell level to categorize fires into those deemed probable-war related.

# 1. Define function to get excess: ----------------------------------------
source('scripts/aux_war_fire_classifier_function.R')

# 2. Load data: ----------------------------------------
X_mat <- readRDS('output-data/X_matrix.RDS')

cat('\n Running war fires classifier. See script "05-war-fire-classifier.R" for details.\n')

pred_mat <- read_csv('output-data/model-objects/pred_matrix.csv')
pred_mat <- readRDS('output-data/X_mat_with_preds.RDS')
pred_mat$predicted_fire <- pred_mat$prediction_upper_90
pred_mat$predicted_fire[pred_mat$predicted_fire < 0] <- 0
pred_mat$predicted_fire <- ceiling(pred_mat$predicted_fire)

pred_mat <- pred_mat[, c('predicted_fire', 'year', 'time_of_year', 'id', 'id_5x5')]

fires <- read_csv('output-data/all_fires.csv')
fires <- fires[fires$date > as.Date('2022-02-23'), ]

# 3. Predict: ----------------------------------------
X_fire_exp <- war_fire_classifier(cell_day_data = X_mat[X_mat$date > as.Date('2022-02-23'), ],
                                  cell_day_predictions = pred_mat,
                                  fires = fires,
                                  offset = 2,
                                  min_length_of_fire_in_area = 10,
                                  days_to_assign_to_war_fire_after_excess = 10,
                                  exclude = read_csv('source-data/forest_fire_locations_2022_2023.csv'))

if(inspect){

  library(ggplot2)
  ggplot(X_fire_exp[X_fire_exp$date <= as.Date('2023-02-18'), ], aes(x=LONGITUDE, y=LATITUDE, col=date))+
    geom_point(alpha = 0.2)+geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1 , ], alpha = 0.2, col = 'red')
  sum(X_fire_exp$war_fire == 1)
  dim(X_fire_exp)

  X_fire_exp$map_col <- NA
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-02-24'):as.Date('2022-05-31')] <- "feb-may"
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-06-01'):as.Date('2022-10-31')] <- "jun-oct"
  X_fire_exp$map_col[X_fire_exp$date %in% as.Date('2022-11-01'):Sys.Date()] <- "nov-now"

  ggplot(X_fire_exp[, ], aes(x=LONGITUDE, y=LATITUDE))+
    geom_point(alpha = 0.01)+
    geom_point(data=X_fire_exp[X_fire_exp$war_fire == 1, ], alpha=0.1, col = 'red')+
    facet_wrap(.~map_col)

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
