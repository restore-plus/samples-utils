library(sits)
library(purrr)

#
# General definitions
#

# Path
base_dir <- restoreutils::project_samples_dir()

# Samples years
samples_years <- c(2022, 2021, 2020)


#
# 1. List samples
#
samples_dirs <- fs::dir_ls(base_dir)


#
# 2. Process each sample dir
#
purrr::map(samples_dirs, function(samples_dir) {

  # 2.1. Process each year
  purrr::map(samples_years, function(sample_year) {

    # 2.2. Build samples path
    sample_current_year <- samples_dir / sample_year

    # 2.3. List .rds files
    sample_current_year_rds <- fs::dir_ls(sample_current_year, glob = "*.rds")

    # 2.4. Read and merge
    sample_current_year_rds <- purrr::map_dfr(sample_current_year_rds, readRDS)

    # 2.5. Save
    saveRDS(sample_current_year_rds, sample_current_year / "samples-merged.rds")
  })
})
