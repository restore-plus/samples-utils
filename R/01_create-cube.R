set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)

#
# Cube definitions
#

# Cube dates
cube_years <- c(1988, 2022)

# Temporal composition
cube_temporal_composition <- "P3M"

# Bands
cube_bands <- c("BLUE", "GREEN", "RED", "NIR08" , "SWIR16", "SWIR22", "CLOUD")

# Cube directory
cube_base_dir <- "data/derived/cube"

# Region
cube_region_file <- "data/raw/tiles/RO_TILES_WRS.gpkg"

#
# Hardware definitions
#

# Multicores
multicores <- 64

# Memory size
memsize <- 220

#
# Create cube directory
#
fs::dir_create(cube_base_dir, recurse = TRUE)

#
# Load region file
#
regions <- st_read(cube_region_file)
regions[["WRSPR"]] <- gsub(
  pattern = "^10",
  replacement = "0010",
  x = regions[["WRSPR"]]
)

#
# Generate cubes
#
for (cube_year in cube_years) {
  print(paste0("Processing: ", cube_year))

  #
  # Define cube dates
  #
  start_date <- paste0(cube_year, "-01-01")
  end_date   <- paste0(cube_year, "-12-31")

  #
  # Create a directory for the current year
  #
  cube_dir_year <- path(cube_base_dir) / cube_year

  fs::dir_create(cube_dir_year, recurse = TRUE)

  #
  # Load cube
  #
  cube_year <- sits_cube(
    source     = "MPC",
    collection = "LANDSAT-C2-L2",
    roi        = regions,
    start_date = start_date,
    end_date   = end_date,
    bands      = cube_bands
  )

  cube_year <- cube_year[cube_year[["tile"]] %in% regions[["WRSPR"]],]

  #
  # Regularize
  #
  reg_cube <- sits_regularize(
    cube        = cube_year,
    period      = cube_temporal_composition,
    res         = 30,
    multicores  = multicores,
    output_dir  = cube_dir_year,
    grid_system = "BDC_MD_V2"
  )
}
