set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)
library(samplesutils)

#
# General definitions
#

# Output dir
cube_dir <- restoreutils::project_cubes_dir()

# Bands
cube_bands <- c("BLUE", "GREEN", "RED", "NIR08" , "SWIR16", "SWIR22", "CLOUD")

# Processing years
regularization_years <- c(2022, 2021, 2020)


#
# Hardware definitions
#

# Multicores
multicores <- 48

# Memory size
memsize <- 220


#
# 1. Load eco region roi
#
eco_region_roi <- restoreutils::roi_ecoregions(region_id = 3, crs = restoreutils::crs_bdc())


#
# 2. Get BDC tiles intersecting eco 3
#
bdc_tiles <- sits_roi_to_tiles(eco_region_roi,
                               crs = restoreutils::crs_bdc(),
                               grid_system = "BDC_MD_V2")


#
# 6. Process cubes
#
for (regularization_year in regularization_years) {
  # Define cube dir
  cube_year_dir <- restoreutils::create_data_dir(cube_dir, regularization_year)

  # Define cube ``start date`` and ``end date``
  cube_start_date <- paste0(regularization_year, "-01-01")
  cube_end_date   <- paste0(regularization_year, "-12-31")

  # Create cube timeline
  cube_timeline <- tibble::tibble(month = 1:12) |>
    dplyr::mutate(date = as.Date(paste0(
      regularization_year, "-", sprintf("%02d", month), "-01"
    ))) |>
    dplyr::pull()

  # Regularize tile by tile
  purrr::map(bdc_tiles[["tile_id"]], function(tile) {
    print(tile)

    # Load cube
    cube_year <- sits_cube(
      source      = "BDC",
      collection  = "LANDSAT-OLI-16D",
      tiles       = tile,
      grid_system = "BDC_MD_V2",
      start_date  = cube_start_date,
      end_date    = cube_end_date,
      bands       = cube_bands
    )

    # Regularize
    cube_year_reg <- sits_regularize(
      cube        = cube_year,
      period      = "P1M",
      res         = 30,
      multicores  = multicores,
      output_dir  = cube_year_dir,
      timeline    = cube_timeline
    )

    # Generate indices
    cube_year_reg <- restoreutils::cube_generate_indices(
      cube = cube_year_reg,
      output_dir = cube_year_dir,
      multicores = multicores,
      memsize = memsize
    )

    # Extract time-series
    samplesutils::extract_samples_ts(cube = cube_year_reg,
                                     year = regularization_year,
                                     multicores = multicores)
  })
}
