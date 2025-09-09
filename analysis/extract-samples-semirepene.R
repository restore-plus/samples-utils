set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)
library(samplesutils)

#
# General definitions
#

# Processing years
target_years <- 2010

# Samples
samples_file <- "data/raw/samples/Amostras_Semiperene2010_v1.gpkg"

# Samples directory
base_samples_dir <- "data/derived/samples/semiperene"

# Cube directory
cube_directory <- "/data/cubes/cube-region-3/"


#
# Hardware definitions
#

# Multicores
multicores <- 20


#
# 1. Load samples
#
samples <- sf::st_read(samples_file)


#
# 2. Define samples roi
#
samples_roi <- sf::st_bbox(samples)


#
# 3. Extract time-series
#
for (target_year in target_years) {
  # Define cube and samples dir
  cube_dir <- fs::path(cube_directory) / target_year
  samples_dir <- fs::path(base_samples_dir) / target_year

  # Create dir
  fs::dir_create(samples_dir)

  # Define cube ``start date`` and ``end date``
  cube_start_date <- paste0(target_year, "-01-01")
  cube_end_date   <- paste0(target_year, "-12-31")

  # Load cube
  cube_year <- sits_cube(
    source      = "OGH",
    collection  = "LANDSAT-GLAD-2M",
    data_dir    = cube_dir
  )

  # Extract time-series
  samples_ts <- sits_get_data(
    cube       = cube_year,
    samples    = samples,
    multicores = multicores
  )

  # Save results
  saveRDS(samples_ts, samples_dir / paste0("samples-semiperene-", target_year, ".rds"))
}
