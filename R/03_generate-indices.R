set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)

#
# Cube definitions
#

# Cube dates
classification_years <- c(1988, 2022)

# Cube directory
cube_base_dir <- "data/derived/cube"

#
# Hardware definitions
#

# Multicores
multicores <- 64

# Memory size
memsize <- 220

#
# 1. Generate indices
#
for (classification_year in classification_years) {
  #
  # 1.1. Define cube directory
  #
  cube_dir <- path(cube_base_dir) / classification_year

  #
  # 1.2. Load cube
  #
  cube <- sits_cube(
    source     = "MPC",
    collection = "LANDSAT-C2-L2",
    data_dir   = cube_dir
  )

  #
  # 1.3. Generate NDVI
  #
  cube <- sits_apply(
    data       = cube,
    NDVI       = (NIR08 - RED) / (NIR08 + RED),
    output_dir = cube_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  #
  # 1.4. Generate NDWI
  #
  cube <- sits_apply(
    data       = cube,
    NDWI       = (GREEN-NIR08) / (GREEN+NIR08),
    output_dir = cube_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  #
  # 1.5. Generate NDSI
  #
  cube <- sits_apply(
    data       = cube,
    NDSI       = (GREEN-SWIR16) / (GREEN+SWIR16),
    output_dir = cube_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  #
  # 1.6. Generate NDBI
  #
  cube <- sits_apply(
    data       = cube,
    NDBI       = (SWIR16-NIR08) / (SWIR16+NIR08),
    output_dir = cube_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )
}
