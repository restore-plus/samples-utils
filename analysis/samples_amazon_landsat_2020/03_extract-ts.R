set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)

#
# Cube definitions
#

# Cube dates
classification_year <- 2020

# Cube dates

# Samples
samples_file <- "data/raw/samples/samples_landsat_amazon-rainforest_2020.gpkg"

# Cube directory
cube_base_dir <- "data/derived/cube"

# Time series directory
time_series_dir <- "data/derived/ts/2020/"

#
# Hardware definitions
#

# Multicores
multicores <- 64

# Memory size
memsize <- 220

#
# 1. Read samples
#
samples <- st_read(samples_file)

#
# 2. Generate cube
#

# 2.1. Define cube directory
cube_dir <- path(cube_base_dir) / classification_year

# 2.2. Load cube
cube <- sits_cube(
  source     = "MPC",
  collection = "LANDSAT-C2-L2",
  data_dir   = cube_dir
)

#
# 2. Extract time series
#

# 2.1. Create time series directory
fs::dir_create(time_series_dir, recurse = TRUE)

# 2.2. Time series extraction
time_series <- sits_get_data(
  cube = cube,
  samples = samples,
  multicores = multicores
)

# 2.3. Saving time series
saveRDS(time_series, path(time_series_dir) / "time-series_landsat_amazon-rainforest_2020.rds")
