# Load the immutable base prediction matrix plus append-only update chunks.
load_prediction_matrix <- function(model_dir = 'output-data/model-objects') {
  base_file <- file.path(model_dir, 'pred_matrix.csv')
  increment_files <- sort(list.files(
    model_dir,
    pattern = '^pred_matrix_increment_.*[.]csv$',
    full.names = TRUE))
  prediction_files <- c(base_file, increment_files)
  prediction_files <- prediction_files[file.exists(prediction_files)]

  if(length(prediction_files) == 0){
    stop('No prediction matrix files found.')
  }

  readr::read_csv(prediction_files, show_col_types = FALSE)
}
