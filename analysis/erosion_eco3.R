library(sits)

# Função para extrai as classes únicas de um raster usando paralelismo em blocos
extract_unique_values <- function(file, multicores, output_dir) {
  rast_template <- sits:::.raster_open_rast(file)
  block <- c("nrows" = 20000, "ncols" = 20000)
  stopifnot(is.character(output_dir))
  image_size <- list(
    nrows = sits:::.raster_nrows(rast_template),
    ncols = sits:::.raster_ncols(rast_template)
  )

  # Calcula quais os blocos que serão lidos
  chunks <- sits:::.chunks_create(
    block = block,
    overlap = 0,
    image_size = image_size,
    image_bbox = sits:::.bbox(sits:::.raster_bbox(rast_template))
  )

  # Inicia os workers para o processomanto paralelo
  sits:::.parallel_start(workers = multicores)
  on.exit(sits:::.parallel_stop(), add = TRUE) # Configura evento de finalização dos Workers

  rds_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
    block <- sits:::.block(chunk)

    unique_values_dir <- fs::path("/data/experiments/erosion/output_9-12/classes") # ToDo: Alterar por uma variável para o usuário.
    fs::dir_create(unique_values_dir)

    # Define nome do arquivo por bloco
    block_unique_file <- paste(block[["col"]], block[["row"]], block[["ncols"]], block[["nrows"]], sep = "_")
    block_unique_file <- paste0(block_unique_file, ".rds")
    block_unique_file <- unique_values_dir / block_unique_file

    if (fs::file_exists(block_unique_file)) {
      return(block_unique_file)
    }

    # Lê os valores e salva os únicos em arquivo
    values <- sits:::.raster_read_rast(files = file, block = block)
    values <- terra::unique(values)

    saveRDS(as.vector(unname(values)), block_unique_file)

    return(block_unique_file)
  }, progress = TRUE)

  # Lê os arquivos gerados com valores únicos
  values <- purrr::map(rds_files, function(rds_file) {
    readRDS(rds_file)
  })

  # Caso queira apagar os arquivos gerados
  # unlink(rds_files)

  classes <- unique(unlist(unname(values)))
  sort(classes[!is.na(classes)])
}


# Função para calcular quantos pontos alocar por arquivo
calculate_points_per_file <- function(num_files, total_needed = 1500) {
  base_count <- floor(total_needed / num_files)
  remainder <- total_needed %% num_files

  counts <- rep(base_count, num_files)

  if (remainder > 0) {
    counts[1:remainder] <- counts[1:remainder] + 1
  }

  return(counts)
}

# Função para aplicar erosão em blocos para as classes especificadas e extrai amostras
erosion_chunked <- function(file,
                            multicores,
                            classes,
                            n_points,
                            output_dir) {
  rast_template <- sits:::.raster_open_rast(file)

  # block <- c("nrows" = 5500, "ncols" = 5500)
  block <- c("nrows" = 10000, "ncols" = 10000)

  image_size <- list(
    nrows = sits:::.raster_nrows(rast_template),
    ncols = sits:::.raster_ncols(rast_template)
  )

  # Calcula sobreposição para erosão com janela 9x9
  overlap <- ceiling(9 / 2L) - 1L
  chunks <- sits:::.chunks_create(
    block = block,
    overlap = overlap,
    image_size = image_size,
    image_bbox = sits:::.bbox(sits:::.raster_bbox(rast_template))
  )

  # Inicia os workers para o processomanto paralelo
  sits:::.parallel_start(workers = multicores)
  on.exit(sits:::.parallel_stop(), add = TRUE) # Configura evento de finalização dos Workers

  for (cls in classes) {
    out_file <- paste0(output_dir,
                       "/",
                       sits:::.file_pattern(file),
                       "_erosion_class_",
                       cls,
                       ".tif")

    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
      block <- sits:::.block(chunk)
      values <- sits:::.raster_read_rast(files = file, block = block)

      # Cria raster temporário com os valores do bloco
      values_rst <- terra::rast(nrows = block[["nrows"]], ncols = block[["ncols"]])
      terra::res(values_rst) <- terra::res(values_rst)
      terra::crs(values_rst) <- terra::crs(values_rst)
      # terra::ext(block_rast) <- terra::ext(values_rst, block[["col"]], block[["col"]] + block[["ncols"]] - 1, block[["row"]],  block[["row"]] + block[["nrows"]] - 1)
      terra::values(values_rst) <- values

      # Aplica erosão diretamente
      # eroded_raster <- apply_erosion(values_rst, cls)
      binary_raster <- values_rst == cls
      eroded_raster <- terra::focal(
        binary_raster,
        w = matrix(1, 9, 9),
        fun = min,
        na.rm = TRUE
      )
      eroded_raster[eroded_raster == 1] <- cls
      eroded_raster[eroded_raster == 0] <- NA

      # Salva raster do bloco
      block_file <- sits:::.file_block_name(
        pattern = paste(tools::file_path_sans_ext(basename(file)), cls, sep = "_"),
        block = block,
        output_dir = output_dir
      )
      block_points_file <- fs::path(output_dir) / paste0(
        "block_", paste(block[["col"]], block[["row"]], block[["ncols"]], block[["nrows"]], "class", cls, sep = "_"),
        ".gpkg"
      )

      sits:::.raster_write_block(
        files = block_file,
        block = block,
        bbox = sits:::.bbox(chunk),
        values = terra::values(eroded_raster),
        crop_block = NULL,
        data_type = "INT1U",
        missing_value = 255
      )

      # Gera amostras a partir do raster erodido
      erosion_rst <- terra::rast(block_file)

      n_points <- 1500
      points <- terra::spatSample(
        erosion_rst,
        size = (floor(n_points / nrow(chunks)) + (0.20 * n_points)),
        method = "random",
        na.rm = TRUE,
        as.points = TRUE,
        exhaustive = TRUE
      )

      if (is.null(points) || nrow(points) == 0) {
        points <- NULL
      } else {
        points <- sf::st_as_sf(points)
      }

      # Salva pontos se existirem
      if (!is.null(points)) {
        points$class <- cls

        sf::st_write(points, block_points_file)
      }

      return(block_file)
    }, progress = TRUE)

    # Junta os blocos em um único raster de saída
    sits:::.raster_merge_blocks(
      out_files = out_file,
      base_file = file,
      block_files = block_files,
      data_type = "INT1U",
      missing_value = 255,
      multicores = multicores
    )

    # Junta os arquivos de pontos em um único sf
    points_block_files <- fs::dir_ls(output_dir, glob = "*block_*.gpkg")
    points_in_files <- calculate_points_per_file(length(points_block_files), total_needed = n_points)

    points <- purrr::map_dfr(seq_len(length(points_block_files)), function(idx) {
      points_block_file <- points_block_files[[idx]]
      # points_in_file <- points_in_files[[idx]]

      # points_row <- sf::st_read(points_block_file)
      # points_in_file <- nrow(points_row)

      # points_row[1:points_in_file,] |>
      #   dplyr::filter(!sf::st_is_empty(geom))
      sf::st_read(points_block_file)
    })

    # Limita ao número desejado de pontos
    points_file <- fs::path(output_dir) / paste0("points_class_", cls, ".gpkg")
    points <- points[1:n_points,]

    # Salva os pontos finais da classe
    tryCatch({
      sf::st_write(points, points_file)
    }, error = function(e) {
      cat("Class with error:\n")
      print(cls)
      NA
    })

    # Remove arquivos intermediários
    unlink(block_files)
    unlink(points_block_files)
  }
}

# Número de amostras por classe
n_points <- 1500

# Número de núcleos de CPU
multicores <- 15

# Diretório de saída
output_dir <- "/data/experiments/erosion/output_9-12_2/"

# Caminho do raster de entrada
map_file <- "/data/experiments/erosion/input/map_terraclass_amz_cer_eco3.tif"

# map_rst <- terra::rast(map_file)
# class_freq <- terra::freq(map_rst)

# classes <- get_unique_raster_values(map_rst) # terra::unique(map_rst) # class_freq$value[!is.na(class_freq$value)]
# classes <- classes[!is.na(classes)]
# classes <- c(1, 23, 51, 10, 11, 22, 2, 17, 20, 15, 16, 14, 12, 9, 25)

# Extrai as classes únicas do mapa

# Se for usar valores fixos:
# classes <- c(9, 12)

classes <- extract_unique_values(map_file, multicores, output_dir)

# Executa erosão e extração de amostras por classe
erosion_chunked(
  file = map_file,
  multicores = multicores,
  classes = classes,
  n_points = n_points,
  output_dir = output_dir
)

# Junta todos os pontos em um único arquivo geopackage
points_erosion <- purrr::map_dfr(fs::dir_ls(output_dir, glob = "*points_*.gpkg"), sf::st_read)
sf::st_write(points_erosion, fs::path(output_dir) / "erosion-points.gpkg")

