# This script loads TIFF files and processes them into RDS files of cloud cover
library(raster)

cloud_files <- dir('source-data/cloud-data')
processed_cloud_files <- dir('source-data/cloud-data-processed')

for(i in cloud_files){

  # Get date
  temp_date <- strsplit(i, '\\.')[[1]][[1]]

  # Check if already processed
  if(!paste0(temp_date, '.RDS') %in% processed_cloud_files){
    cat(i)

    # If not, import data
    clouds <- stack(paste0('source-data/cloud-data/', i))
    cat('.')

    # Restrict to visible spectrum
    clouds <- dropLayer(clouds, 4:6)
    cat('.')

    clouds <- projectRaster(clouds, crs='+proj=longlat +datum=WGS84')

    # Make into data frame
    points <- data.frame(rasterToPoints(clouds))
    if(nrow(points) > 0){

    cat('.')
    # Round to grid
    points$x <- round(points$x*10)/10
    points$y <- round(points$y*10)/10
    points$id <- paste0(points$x, '-', points$y)
    cat('.')

    points$red <- ave(points$red, points$id, FUN = mean)
    points$blue <- ave(points$blue, points$id, FUN = mean)
    points$green <- ave(points$green, points$id, FUN = mean)

    points <- points[!duplicated(points$id), ]
    cat('.')

    points$date <- temp_date
    saveRDS(points, paste0('source-data/cloud-data-processed/', temp_date, '.RDS'))
    } else {
      cat('..no..points..aborting..')
    }
    cat(paste0('processed - at: ', Sys.time(), '\n'))
  }

}

