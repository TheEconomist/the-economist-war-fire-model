# Script to deal with NASA API outage. Files downloaded manually for all 3 systems here: https://nrt3.modaps.eosdis.nasa.gov/archive/FIRMS/ (selecting the global version)

# List of packages you need
# --- repo setup ---
repo_vec <- c(ropensci = "https://ropensci.r-universe.dev",
              CRAN     = "https://cloud.r-project.org")
options(repos = repo_vec)

# --- deps install ---
pkgs <- c("sf", "rnaturalearth", "rnaturalearthdata")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, quiet = TRUE)

# Try hires but don’t fail if it’s unavailable
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  try(install.packages("rnaturalearthhires", quiet = TRUE), silent = TRUE)
}

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

ne_scale <- if (requireNamespace("rnaturalearthhires", quietly = TRUE)) "large" else "medium"

library(readr)

# 1) Get Ukraine’s country polygon (WGS84 / EPSG:4326)
ukr <- ne_countries(country = "Ukraine", scale = "large", returnclass = "sf")
ukr <- st_make_valid(ukr)  # safety

# Add Crimea
rus_admin1 <- ne_states(country = "Russia", returnclass = "sf") |>
  st_make_valid() |>
  st_transform(4326)
crimea <- rus_admin1[tolower(rus_admin1$name) %in% c("crimea", "krym", "sevastopol"), ]
crimea <- st_union(st_geometry(crimea)) |> st_make_valid()

# Merge with Ukraine geometry
ukr <- st_union(st_geometry(ukr), crimea) |> st_make_valid() |>
  st_transform(4326)

# 2) Load fires data manually acquired
# The raw files are global (NASA offers no country subset), and NASA revises NRT files
# within the day under the same filename, so neither filename nor date can be trusted to
# tell whether a file has changed. We therefore cache the Ukraine-clipped rows of each
# file keyed on its content hash: a file is only re-read and re-clipped when its hash
# differs from the manifest. The full set of clipped rows is still rebuilt from the cache
# on every run, so the downstream dedup and contiguity checks behave as before.
raw_dir <- 'source-data/firms-imports/2025/'
cache_dir <- 'output-data/firms-clipped/'
dir.create(cache_dir, showWarnings = F)
manifest_path <- file.path(cache_dir, 'manifest.csv')
cache_path <- function(f) file.path(cache_dir, paste0(f, '.csv'))

raw_files <- dir(raw_dir)
hashes <- unname(tools::md5sum(file.path(raw_dir, raw_files)))

if(file.exists(manifest_path)){
  manifest <- read_csv(manifest_path, col_types = cols(.default = col_character()))
} else {
  manifest <- data.frame(file = character(), md5 = character(), dates = character())
}

# Drop cache entries for files no longer on disk
gone <- manifest$file[!manifest$file %in% raw_files]
unlink(cache_path(gone))
manifest <- manifest[!manifest$file %in% gone, ]

cached_md5 <- manifest$md5[match(raw_files, manifest$file)]
to_process <- raw_files[is.na(cached_md5) | cached_md5 != hashes]
cat(paste0('\n', length(raw_files), ' raw FIRMS files, ', length(to_process), ' new or changed since last run.\n'))

ukr_bbox <- st_bbox(ukr)

for(file in to_process){
  temp <- read_csv(paste0(raw_dir, file))

  if(!"acq_date" %in% colnames(temp))
  {
    stop(paste0('Problem in manually downloaded file ', file, ' please inspec'))
  }

  instrument <- NA

  if(grepl("VIIRS", file)){
    instrument <- 'VIIRS'
  } else {
    if(grepl('MODIS', file)){
      instrument <- 'MODIS'
    }
  }
  temp$instrument <- instrument

  if(max(as.Date(temp$acq_date)) < as.Date('2025-08-01')){
    stop()
  }

  file_dates <- unique(temp$acq_date)

  # Cheap bounding-box filter first, then exact point-in-polygon (incl. Crimea)
  temp <- temp[temp$longitude >= ukr_bbox['xmin'] & temp$longitude <= ukr_bbox['xmax'] &
                 temp$latitude >= ukr_bbox['ymin'] & temp$latitude <= ukr_bbox['ymax'], ]
  if(nrow(temp) > 0){
    pts <- st_as_sf(temp, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    temp <- temp[st_within(pts, ukr, sparse = FALSE)[, 1], ]
  }

  write_csv(temp, cache_path(file))
  manifest <- manifest[manifest$file != file, ]
  manifest <- rbind(manifest, data.frame(file = file,
                                         md5 = hashes[raw_files == file],
                                         dates = paste(file_dates, collapse = ';')))
}
write_csv(manifest, manifest_path)

# Rebuild clipped fires from the cache. Column types are fixed so that files with
# different instruments (numeric MODIS confidence, character VIIRS confidence) bind.
add_fires <- data.frame()
for(file in raw_files){
  temp <- read_csv(cache_path(file), col_types = cols(acq_date = col_date(),
                                                      acq_time = col_character(),
                                                      confidence = col_character(),
                                                      version = col_character(),
                                                      satellite = col_character(),
                                                      daynight = col_character(),
                                                      instrument = col_character(),
                                                      .default = col_double()))
  if(nrow(temp) == 0) next

  if(nrow(add_fires) == 0){
    add_fires <- temp
  } else {
    add_fires <- rbind(
      add_fires[, intersect(colnames(add_fires), colnames(temp))],
      temp[, intersect(colnames(add_fires), colnames(temp))])
  }
}
add_fires$acq_time <- as.POSIXct(add_fires$acq_time, format = "%H:%M:%S", tz = "UTC")
add_fires$acq_time <- format(add_fires$acq_time, "%H%M")
add_fires$fire_id <- 1:nrow(add_fires)
add_fires$country_id <- 'UKR'

# Dates covered by the raw global files (used below to check for missing files)
all_dates <- as.Date(unlist(strsplit(manifest$dates, ';')))

# Load old fires:
old_fires <- read_csv('output-data/firms_update.csv')

# Ensure common columns:
add_fires <- add_fires[, colnames(old_fires)]

# 6) Deduplicate
add_fires <- add_fires[!duplicated(paste0(add_fires$latitude, '_',
                                          add_fires$longitude, '_',
                                          add_fires$acq_date, '_',
                                          add_fires$acq_time)), ]

# To inspect:
# library(ggplot2)
# ggplot()+geom_sf(data=ukr)+geom_point(data=add_fires, aes(x=longitude, y=latitude))

fires <- rbind(old_fires, add_fires)
fires <- fires[!duplicated(paste0(fires$latitude, '_',
                                  fires$longitude, '_',
                                  fires$acq_date, '_',
                                  fires$acq_time)),]


# Record successful update:
updated <- read_csv('output-data/dates_of_successfully_acquired_fire_data.csv')
all_dates <- as.Date(all_dates)

if (length(seq(min(all_dates), max(all_dates), by = "day")) ==
    length(unique(all_dates))) {

  updated_data <- unique(c(seq(min(all_dates), max(all_dates), by = "day"),
                           read_csv('output-data/dates_of_successfully_acquired_fire_data.csv')$dates))
  write_csv(data.frame(dates = updated_data),
            'output-data/dates_of_successfully_acquired_fire_data.csv')

} else {
  stop('Are you missing some *files*?')
}

# Export back to fire archive:
write_csv(fires, 'output-data/firms_update.csv')

