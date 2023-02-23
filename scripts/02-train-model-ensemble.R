# Generate forest fire model:

# 0. Import data and packages ------------------------------

library(tidyverse)
library(ggplot2)
library(sf)
library(lubridate)
library(anytime)
library(agtboost)
library(readxl)
library(readr)
library(anytime)
library(tidyverse)
library(geosphere)

# Import training data
X <- readRDS('output-data/X_matrix.RDS')

# 1. Define model variables ------------------------------
model_vars <- c('x', 'y', 'year', 'time_of_year', 'pop_density',
                "continuous_length_of_any_fire_in_cell",
                "nightlights",
                "average_past_fires_by_1x1_location",
                "average_past_fires_by_1x1_location_time_of_year",
                "average_past_fires_by_1x1_location_month_of_year",
                "average_past_fires_by_5x5_location_time_of_year",
                "average_past_fires_by_5x5_location_month_of_year",
                "average_past_fires_by_1x1_location_no_clouds",
                "average_past_fires_by_1x1_location_time_of_year_no_clouds",
                "average_past_fires_by_1x1_location_month_of_year_no_clouds",
                "average_past_fires_by_5x5_location_time_of_year_no_clouds",
                "average_past_fires_by_5x5_location_month_of_year_no_clouds",
                "average_past_fires_by_1x1_location_time_of_year_no_clouds_30da",
                "average_past_clouds_by_1x1_location_time_of_year",
                "average_past_clouds_by_1x1_location_month_of_year",
                "average_past_clouds_by_5x5_location_time_of_year",
                "average_past_clouds_by_5x5_location_month_of_year",
                "average_past_clouds_by_1x1_location_time_of_year_30da")

saveRDS(model_vars, 'output-data/model-objects/model_vars.RDS')

# 2. Subset data ------------------------------

# Get Ukraine outline:
ukraine <- unique(X[, c('x', 'y')])

# Plot this for a particular date
pdat <- X[X$date == sample(X$date, 1), ]
ggplot(pdat, aes(x=x, y=y, col=fire > 0))+geom_point()+geom_point(data=pdat[pdat$cloud==1, ], col='white')

# Remove cloud observations (for these we predict zero and also just remove them from graphics):
X <- X[!X$cloud == 1, ]

# Remove missing data:
for(i in model_vars){
  X <- X[!is.na(X[, i]), ]
}

# 3. Option to refine outcome for problem: more than 1, 3, or 5 fires in cell-day ---------------------------
# X$fire_class <- NA
# X$fire_class[X$fire == 0] <- 0
# X$fire_class[X$fire > 0] <- 1
# X$fire_class[X$fire > 3] <- 2
# X$fire_class[X$fire > 5] <- 5
X$fire_class <- X$fire # We use number of fires as the outcome.

# 4. Train the main model on this data ------------------------------

# Train GBT
X_train <- X[X$date <= as.Date('2022-02-23'), ]
X_train <- X_train[X_train$fire > 0 | 1:nrow(X_train) %in% sample(1:nrow(X_train), ceiling(3*sum(X_train$fire > 0))), ]
X_test <- X[X$date > as.Date('2022-02-23'), ]

# Define test and training sets. Data is down-sampled to avoid memory issues, and speed up the process.
X_test <- X_test[sample(1:nrow(X_test), 50000), ]
X_train <- X_train[sample(1:nrow(X_train), min(c(1500000, nrow(X_train)))), ]

# Set i
i <- 1
gbt_model <- gbt.train(x=as.matrix(X_train[, model_vars]),
                       y=X_train$fire_class, learning_rate = 0.05,
                       verbose = 100,
                       loss_function = 'mse',
                       nrounds = 25000)
gbt.save(gbt_model, paste0('output-data/model-objects/', 'bootstrap_model_', i, 'gbt'))

# 5. Generate bootstrap models -----------------------

# Get prediction
X_test$prediction <- predict(newdata = as.matrix(X_test[, model_vars]), gbt_model)
table(round(X_test$prediction) - X_test$fire_class)

# Get bootstrap:
res <- data.frame()
res <- rbind(res, predict(newdata = as.matrix(X_test[, model_vars]), gbt_model))
B <- 100

# This loop trains 99 stratified-bootstrap models to facilitate later generation of prediction intervals
for(i in 2:B){
  cat('\n\n')
  cat(paste0('Fitting model ', i, ' at ', Sys.time(), '\n'))

  # Randomly select subset of data:
  X_train <- X[X$date <= as.Date('2022-02-23'), ]
  X_train <- X_train[X_train$fire > 0 | 1:nrow(X_train) %in% sample(1:nrow(X_train), ceiling(3*sum(X_train$fire > 0))), ]
  X_test <- X[X$date > as.Date('2022-02-23'), ]
  X_test <- X_test[sample(1:nrow(X_test), 50000), ]
  X_train <- X_train[sample(1:nrow(X_train), min(c(1500000, nrow(X_train)))), ]

  ids <- sample(unique(X_train$id), length(unique(X_train$id)), replace = T)
  obs <- c()
  for(j in ids){
    obs <- c(obs, which(X_train$id == j))
  }

  gbt_model <- gbt.train(x=as.matrix(X_train[obs, model_vars]),
                         y=X_train$fire_class[obs], learning_rate = 0.05,
                         verbose = 100,
                         loss_function = 'mse',
                         nrounds = 25000)
  gbt.save(gbt_model, paste0('output-data/model-objects/', 'bootstrap_model_', i, 'gbt'))
  cat(paste0('\n\n', 'completed fit of model ', i))
  res <- rbind(res, predict(newdata = as.matrix(X_test[, model_vars]), gbt_model))
}
res <- t(res)

for(i in 1:nrow(res)){
  res[i, ] <- sort(res[i, ])
}

saveRDS(res, 'output-data/boot_predict_res.RDS')

X_test$prediction_lower_95 <- res[, floor(B*0.05)]
X_test$prediction_upper_95 <- res[, floor(B*0.95)]

saveRDS(X_test, 'output-data/boot_predict_res_pred_matrix.RDS')
