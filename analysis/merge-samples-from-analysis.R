library(sits)

base_output_dir <- fs::path("data/derived/samples/")

samples_2010 <- fs::dir_ls(base_output_dir / "samples-from-evaluation") |>
                  purrr::map_dfr(readRDS)

saveRDS(samples_2010, base_output_dir / "samples-from-evaluation.rds")
