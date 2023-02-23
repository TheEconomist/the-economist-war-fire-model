# This script loads the model ensemble and generates predictions. It takes as input model objects, a data matrix, and list of features. Note: this process takes some time, especially for deeply trained models
library(agtboost)

# Load prediction covariates
X_mat <- readRDS('output-data/X_matrix.RDS')
X_mat <- X_mat[X_mat$year >= 2022 & X_mat$date <= Sys.Date()+14, ]

# Load model variables
model_vars <- readRDS('output-data/model-objects/model_vars.RDS')

# Loop through bootstrap models, generate prediction and save
res <- data.frame()

B <- 100
for(i in 1:B){
  cat(paste0('\n', 'Loading model: ', i))
  gbt_model <- gbt.load(paste0('output-data/model-objects/', 'bootstrap_model_', i, 'gbt'))
  cat(paste0('\n', 'Generating prediction: ', i, ' -- at: ', Sys.time()))
  res <- rbind(res, predict(newdata = as.matrix(X_mat[, model_vars]), gbt_model))
  cat(paste0('\n', 'Prediction saved at : ', Sys.time()))
}

res <- t(res)
for(i in 1:nrow(res)){
  res[i, ] <- sort(res[i, ])
}
X_mat$prediction_lower_90 <- res[, floor(B*0.05)]
X_mat$prediction_lower_95 <- res[, floor(B*0.025)]
X_mat$prediction_median <- res[, floor(ncol(res)/2)]
X_mat$prediction_mean <- rowMeans(res)
X_mat$prediction_upper_95 <- res[, floor(B*0.975)]
X_mat$prediction_upper_90 <- res[, floor(B*0.95)]

saveRDS(res, 'output-data/X_mat_res_matrix.RDS')
saveRDS(X_mat, 'output-data/X_mat_with_preds.RDS')
saveRDS(X_mat[, c('x', 'y', 'time_of_year', 'year')], 'output-data/X_mat_res_matrix_correspondance.RDS')
