# This script generates nightlights-data at the grid level for Ukraine

# Load data and packages:
library(raster)
# Source: https://www.mdpi.com/2072-4292/9/6/637
# See also: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/2
str_name <-'source-data/nightlights 2018/Harmonized_DN_NTL_2018_simVIIRS.tif'
imported_raster=raster(str_name)

# Generate target grid
rounding_para <- readRDS('output-data/rounding_para.RDS')
target_grid <- expand.grid('x'= seq(21.9, 40.3,
                                    by = rounding_para),
                           'y'= seq(44.4, 52.4,
                                    by = rounding_para))

# Calculate mean of cell:
e <- as(extent(min(target_grid$x), max(target_grid$x), min(target_grid$y), max(target_grid$y)), 'SpatialPolygons')
crs(e) <- crs(imported_raster)
r <- crop(imported_raster, e)
raster_data <- data.frame(rasterToPoints(r))
raster_data$x <- round(raster_data$x*10)/10
raster_data$y <- round(raster_data$y*10)/10
raster_data$id <- paste0(raster_data$x, '-', raster_data$y)
raster_data$nightlights <- as.numeric(ave(raster_data$Harmonized_DN_NTL_2018_simVIIRS, raster_data$id, FUN = mean))
raster_data$Harmonized_DN_NTL_2018_simVIIRS <- NULL
raster_data <- raster_data[!duplicated(raster_data$id), ]

saveRDS(raster_data, 'output-data/nightlights.RDS')
