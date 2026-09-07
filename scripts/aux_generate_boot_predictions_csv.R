library(data.table)

model_dir <- "output-data/model-objects"
legacy_files <- sort(list.files(
  model_dir,
  pattern = "^boot_predictions[.]csv[.][a-z]+$",
  full.names = TRUE))
base_files <- sort(list.files(
  model_dir,
  pattern = "^boot_predictions_base_[0-9]+[.]csv$",
  full.names = TRUE))
increment_files <- sort(list.files(
  model_dir,
  pattern = "^boot_predictions_increment_.*[.]csv$",
  full.names = TRUE))

parts <- list()

if(length(legacy_files) > 0 && length(base_files) > 0){
  stop('Found both legacy and rebuilt bootstrap base chunks; refusing to combine potentially duplicated data.')
}

# The original split files contain one header in the first chunk only.
if(length(legacy_files) > 0){
  legacy_first <- fread(legacy_files[1])
  legacy_names <- names(legacy_first)
  parts <- c(parts, list(legacy_first))

  if(length(legacy_files) > 1){
    parts <- c(parts, lapply(legacy_files[-1], function(file) {
      fread(file, header = FALSE, col.names = legacy_names)
    }))
  }
}

# Rebuilt base chunks are independently readable and each contains its own header.
parts <- c(parts, lapply(base_files, fread))

# Incremental chunks are independently readable and each contains its own header.
parts <- c(parts, lapply(increment_files, fread))

if(length(parts) == 0){
  stop('No bootstrap prediction chunks found.')
}

dt <- rbindlist(parts, use.names = TRUE)

# Write back to a single file
fwrite(dt, file.path(model_dir, "boot_predictions.csv"))
