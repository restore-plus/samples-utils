set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)

#
# Auxiliary function
#
is_raster_valid <- function(raster) {
  # Check if the raster has values at all
  if (terra::nlyr(raster) == 0) {
    return(TRUE)
  }

  # Get values and check if all are NA
  is_valid <- tryCatch({
    value <- raster[terra::ncol(raster) * terra::nrow(raster)]
    TRUE
  }, error = function(e) {
    FALSE
  })

  return(is_valid)
}

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
    data_dir   = "data/derived/cube/1988"
  )

  #
  # 1.3. Check for empty tiles
  #
  cube_status <- slider::slide_dfr(cube, function(tile) {
    # get empty files
    files_valid <- slider::slide_vec(tile[["file_info"]][[1]], function(file) {
      rst <- terra::rast(file[["path"]])
      is_raster_valid(rst)
    })

    data.frame(tile = tile[["tile"]], is_empty = all(files_valid))
  })

  #
  # 1.4. Get empty tiles
  #
  cube_status <- dplyr::filter(cube_status, is_empty == TRUE)
  cube_empty <- dplyr::filter(cube, tile %in% cube_status[["tile"]])

  #
  # 1.5. Delete files
  #
  #file.remove(dplyr::bind_rows(cube_empty[["file_info"]])[["path"]])
}
