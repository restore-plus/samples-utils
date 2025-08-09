
#' @export
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

#' @export
extract_aml_2020_pastagens <- function(cube, year, multicores) {
  samples_name <- "Amostras_AMZ_2020_pastagens"
  samples <- readRDS("data/raw/samples/Amostras_AMZ_2020_pastagens.rds") |>
    sf::st_cast("POINT")

  output_dir <- restoreutils::project_samples_dir() / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

#' @export
extract_sentinel_aml_2021_2022_embrapa <- function(cube, year, multicores) {
  samples_name <- "samples_sentinel_aml_2021-2022_embrapa"

  samples <- sf::st_read("data/raw/samples/samples_sentinel_aml_2021-2022_embrapa.gpkg")
  samples$time_series <- NULL
  samples$start_date <- NULL
  samples$end_date <- NULL
  samples$cube <- NULL

  output_dir <- restoreutils::project_samples_dir() / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

#' @export
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
    dplyr::mutate(label = classe_pri) |>
    dplyr::mutate(data = as.Date(data)) |>
    dplyr::filter(data >= "2020-01-01" & data <= "2022-12-31") |>
    sf::st_zm() |>
    sf::st_cast("POINT")

  output_dir <- restoreutils::project_samples_dir() / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

#' @export
extract_landsat_amazon_rainforest_2020 <- function(cube, year, multicores) {
  samples_name <- "samples_landsat_amazon-rainforest_2020"

  samples <- sf::st_read("data/raw/samples/samples_landsat_amazon-rainforest_2020.gpkg") |>
    dplyr::mutate(label = class) |>
    dplyr::select(-class)

  output_dir <- restoreutils::project_samples_dir() / samples_name / year
  fs::dir_create(output_dir, recurse = TRUE)

  extract_ts(
    cube = cube,
    samples = samples,
    multicores = multicores,
    output_dir = output_dir
  )
}

#' @export
extract_samples_ts <- function(cube, year, multicores) {
  extraction_functions <- c(
    extract_aml_2020_pastagens,
    extract_sentinel_aml_2021_2022_embrapa,
    extract_amostas_embrapa_2018_2024,
    extract_landsat_amazon_rainforest_2020
  )

  purrr::map(extraction_functions, function(extraction_fnc) {
    extraction_fnc(
      cube = cube,
      year = year,
      multicores = multicores
    )
  })
}
