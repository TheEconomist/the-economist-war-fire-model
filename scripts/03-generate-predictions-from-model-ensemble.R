# This script defines a function that loads a model ensemble and generates predictions. It takes as input model objects, a data matrix, and list of features. Note: this process takes some time, especially for deeply trained models

ensemble_predict <- function(X_mat,
                             model_vars = readRDS('output-data/model-objects/model_vars.RDS')){
library(agtboost)

# Loop through bootstrap models, generate prediction and save
res <- data.frame()

B <- 100
for(i in 1:B){
  cat(paste0('\n', 'Loading model: ', i))
  gbt_model <- gbt.load(paste0('output-data/model-objects/', 'bootstrap_model_', i, 'gbt'))
  #cat(paste0('\n', 'Generating prediction: ', i, ' -- at: ', Sys.time()))
  res <- rbind(res, predict(newdata = as.matrix(X_mat[, model_vars]), gbt_model))
  #cat(paste0('\n', 'Prediction saved at : ', Sys.time()))
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

X_mat <- X_mat[, c("date", "id",
                   'id_5x5', "year",
                   "time_of_year",
                   "prediction_lower_90",
                   "prediction_lower_95",
                   "prediction_median",
                   "prediction_mean",
                   "prediction_upper_95",
                   "prediction_upper_90")]
return(list(res, X_mat))}
