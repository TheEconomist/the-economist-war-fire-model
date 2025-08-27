files <- list.files("output-data/model-objects", pattern = "boot_predictions.csv.*", full.names = TRUE)

# Ensure correct order
files <- sort(files)

# Read and combine
library(data.table)
dt <- rbindlist(lapply(files, fread))

# Write back to a single file
fwrite(dt, "output-data/model-objects/boot_predictions.csv")
