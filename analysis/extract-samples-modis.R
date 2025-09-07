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
target_years <- 2000:2024

# Samples
samples_file <- "data/raw/samples/samples-v1-nolbae.rds"

# Samples directory
base_samples_dir <- "data/derived/samples/mod13q1"


#
# Hardware definitions
#

# Multicores
multicores <- 8


#
# 1. Load samples
#
samples <- readRDS(samples_file) |>
  dplyr::filter(.data[["label"]] %in% c("Agr. Semiperene", "Ag_perene", "2ciclos", "Silvicultura")) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")


#
# 2. Define samples roi
#
samples_roi <- sf::st_bbox(samples)


#
# 3. Extract time-series
#
for (target_year in target_years) {
  # Define cube dir
  samples_dir <- fs::path(base_samples_dir) / target_year

  # Create dir
  fs::dir_create(samples_dir)

  # Define cube ``start date`` and ``end date``
  cube_start_date <- paste0(target_year, "-01-01")
  cube_end_date   <- paste0(target_year, "-12-31")

  # Load cube
  cube_year <- sits_cube(
    source      = "BDC",
    collection  = "MOD13Q1-6.1",
    roi         = samples_roi,
    start_date  = cube_start_date,
    end_date    = cube_end_date
  )

  # Extract time-series
  samples_ts <- sits_get_data(
    cube       = cube_year,
    samples    = samples,
    multicores = multicores
  )

  # Save results
  saveRDS(samples_ts, samples_dir / paste0("samples-mod13q1-", target_year, ".rds"))
}
