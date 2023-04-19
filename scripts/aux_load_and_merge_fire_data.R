# This scripts loads fire data in three batches:
# 1. Archived fire data (2000-early 2022)
# 2. Data from March 2022 to March 2023
# 3. Data kept updated continuously since March 3rd 2023

# 0. Load libraries
library(sf)
library(tidyverse)
library(lubridate)
library(anytime)
library(readr)

# 1. Data on past fires:
if(redo){
  # Source: https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms
  fire_raw <- st_read('source-data/firms-imports/2000-2022/fire_archive_SV-C2_256359.shp')
  fire_raw <- rbind(fire_raw, st_read("source-data/firms-imports/2000-2022/fire_archive_M-C61_256357.shp"))

  # 2. Data on recent fires:
  temp1 <- st_read('source-data/firms-imports/recent/fire_archive_M-C61_336211.shp')
  temp2 <- st_read('source-data/firms-imports/recent/fire_nrt_M-C61_336211.shp')
  temp3 <- st_read('source-data/firms-imports/recent/fire_archive_SV-C2_336213.shp')
  temp4 <- st_read('source-data/firms-imports/recent/fire_nrt_SV-C2_336213.shp')

  temp2$TYPE <- NA
  temp4$TYPE <- NA

  fire_raw_2 <- rbind(temp1, temp2, temp3, temp4)
  rm(temp1)
  rm(temp2)
  rm(temp3)
  rm(temp4)
  write_csv(fire_raw, 'source-data/firms_archive_1.csv')
  write_csv(fire_raw_2, 'source-data/firms_archive_2.csv')
  } else {
  fire_raw <- read_csv('source-data/firms_archive_1.csv')
  fire_raw_2 <- read_csv('source-data/firms_archive_2.csv')
}

# 3. Up-to-date data:
fire_raw_3 <- read_csv('output-data/firms_update.csv')
colnames(fire_raw_3) <- toupper(colnames(fire_raw_3))

fire_raw$ACQ_DATE <- anydate(fire_raw$ACQ_DATE)
fire_raw_2$ACQ_DATE <- anydate(fire_raw_2$ACQ_DATE)
fire_raw_3$ACQ_DATE <- anydate(fire_raw_3$ACQ_DATE)

# 5. Ensure no overlap
fire_raw <- fire_raw[fire_raw$ACQ_DATE < min(as.Date(fire_raw_2$ACQ_DATE)), ]
fire_raw_2 <- fire_raw_2[fire_raw_2$ACQ_DATE < min(as.Date(fire_raw_3$ACQ_DATE)), ]

# 6. Merge all together:
cols <- intersect(intersect(colnames(fire_raw), colnames(fire_raw_2)), colnames(fire_raw_3))
fire <- rbind(as.data.frame(fire_raw)[, cols], as.data.frame(fire_raw_2)[, cols], as.data.frame(fire_raw_3)[, cols])
rm(cols)

fire$week <- week(fire$ACQ_DATE)
fire$month <- month(fire$ACQ_DATE)
fire$year <- year(fire$ACQ_DATE)

rm(fire_raw_2)
rm(fire_raw_3)
rm(fire_raw)

