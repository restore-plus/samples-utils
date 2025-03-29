set.seed(777)

library(sf)
library(fs)
library(sits)
library(dplyr)

#
# Constants
#
SAMPLES_BASE_DIR <- fs::path("data/derived/ts-region-3")


#
# Auxiliary functions
#
extract_ts <- function(cube, samples, output_dir, multicores = 44) {
  # Get tile (assiming cube with one tile)
  tile <- cube[["tile"]]

  # Prepare output dir
  output_file <- fs::path(output_dir) / paste0(tile, ".rds")

  # If file exists
  if (fs::file_exists(output_file)) {
    return(NULL)
  }

  # Check if samples intersects cube
  samples_row <- sits:::.intersects(samples, sits_as_sf(cube))
  samples_row <- samples[samples_row, ]

  if (nrow(samples_row) == 0) {
    return(NULL)
  }

  # Extract time-series
  res <- sits_get_data(cube = cube,
                       samples = samples_row,
                       multicores = multicores)

  # Save RDS
  saveRDS(res, output_file)
}

extract_aml_2020_pastagens <- function(cube, year, multicores) {
  samples_name <- "Amostras_AMZ_2020_pastagens"
  samples <- readRDS("data/raw/samples/Amostras_AMZ_2020_pastagens.rds") |>
                sf::st_cast("POINT")

  output_dir <- SAMPLES_BASE_DIR / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

extract_sentinel_aml_2021_2022_embrapa <- function(cube, year, multicores) {
  samples_name <- "samples_sentinel_aml_2021-2022_embrapa"

  samples <- sf::st_read("data/raw/samples/samples_sentinel_aml_2021-2022_embrapa.gpkg")
  samples$time_series <- NULL
  samples$start_date <- NULL
  samples$end_date <- NULL
  samples$cube <- NULL

  output_dir <- SAMPLES_BASE_DIR / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

extract_amostas_embrapa_2018_2024 <- function(cube, year, multicores) {
  samples_name <- "Amostas-Embrapa-2018-2024"

  samples <- sf::st_read("data/raw/samples/Amostas-Embrapa-2018-2024/Amostas-Embrapa-2018-2024.shp")
  samples <- samples |>
    dplyr::mutate(
      data = dplyr::recode(
        data,
        "Mar/2018"     = "2018-03-01",
        "Out/2022"     = "2022-10-01",
        "Fev/2018"     = "2018-02-01",
        "Set/2019"     = "2019-09-01",
        "Mai-Jun/2023" = "2023-06-01",
        "Mai/2022"     = "2022-05-01",
        "Out/2021"     = "2021-10-01",
        "Abr/2018"     = "2018-04-01",
        "Nov/2021"     = "2021-11-01",
        "Mai/2024"     = "2024-05-01",
        "Maio/2024"    = "2024-05-01"
      )
    ) |>
    dplyr::mutate(data = as.Date(data)) |>
    dplyr::filter(data >= "2020-01-01" & data <= "2022-12-31")

  output_dir <- SAMPLES_BASE_DIR / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

extract_landsat_amazon_rainforest_2020 <- function(cube, year, multicores) {
  samples_name <- "samples_landsat_amazon-rainforest_2020"

  samples <- sf::st_read("data/raw/samples/samples_landsat_amazon-rainforest_2020.gpkg") |>
    dplyr::mutate(label = class) |>
    dplyr::select(-class)

  output_dir <- SAMPLES_BASE_DIR / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

cube_generate_indices <- function(cube, output_dir, multicores, memsize) {
  # Generate NDVI
  cube <- sits_apply(
    data       = cube,
    NDVI       = (NIR08 - RED) / (NIR08 + RED),
    output_dir = output_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
  cube <- sits_apply(
    data       = cube,
    EVI        = 2.5 * ((NIR08 - RED) / (NIR08 + 6 * RED - 7.5 * BLUE + 1)),
    output_dir = output_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  # Generate MNDWI
  cube <- sits_apply(
    data       = cube,
    MNDWI      = (GREEN - SWIR16) / (GREEN + SWIR16),
    output_dir = output_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  # 1.6. Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
  cube <- sits_apply(
    data       = cube,
    NBR        = (NIR08 - SWIR22) / (NIR08 + SWIR22),
    output_dir = output_dir,
    multicores = multicores,
    memsize    = memsize,
    progress   = TRUE
  )

  return(cube)
}

#
# General definitions
#

# Eco regions
eco_regions <- "data/raw/region/amazon-regions-bdc-md.gpkg"

# BDC tiles
bdc_tiles <- "data/raw/region/bdc-tiles/BDC_MD_V2Polygon.shp"

# Output dir
cube_dir <- "data/derived/cube-region-3"

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
# 1. Load eco regions
#
eco_regions <- sf::st_read(eco_regions)


#
# 2. Load BDC Tiles
#
bdc_tiles <- sf::st_read(bdc_tiles)


#
# 3. Convert ECO regions to BDC tiles CRS
#
eco_regions <- sf::st_transform(eco_regions, crs = sf::st_crs(bdc_tiles))


#
# 4. Filter eco regions (``eco_3`` only)
#
eco_regions <- dplyr::filter(eco_regions, layer == "eco_3") |>
  dplyr::select(-gid, -id, -grs_schema) |>
  sf::st_union() |>
  sf::st_convex_hull()


#
# 5. Filter BDC tiles
#
bdc_tiles <- sf::st_intersection(bdc_tiles, eco_regions)

#
# 6. Process cubes
#
for (regularization_year in regularization_years) {
  # Define cube dir
  cube_year_dir <- fs::path(cube_dir) / regularization_year
  fs::dir_create(cube_year_dir, recurse = TRUE)

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
  purrr::map(bdc_tiles[["tile"]], function(tile) {
    # Load cube
    cube_year <- sits_cube(
      source     = "BDC",
      collection = "LANDSAT-OLI-16D",
      tiles       = tile,
      start_date = cube_start_date,
      end_date   = cube_end_date,
      bands      = cube_bands
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
    cube_year_reg <- cube_generate_indices(
      cube = cube_year_reg,
      output_dir = cube_year_dir,
      multicores = multicores,
      memsize = memsize
    )

    # Extract time-series
    extract_aml_2020_pastagens(cube_year_reg, regularization_year, multicores)

    extract_sentinel_aml_2021_2022_embrapa(cube_year_reg, regularization_year, multicores)

    extract_amostas_embrapa_2018_2024(cube_year_reg, regularization_year, multicores)

    extract_landsat_amazon_rainforest_2020(cube_year_reg, regularization_year, multicores)
  })
}
