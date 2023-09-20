## ###########################################################

##  This script:
##  - Imports data extracted from the cohort extractor (wave1, wave2, wave3)
##  - Formats column types and levels of factors in data
##  - Saves processed data in ./output/processed/input_wave*.rds

## ###########################################################

# Load libraries
library(here)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyverse)

# Load custom functions
utils_dir <- here("analysis", "snapshot_utils")
source(paste0(utils_dir, "/extract_data.R")) # function extract_data()
source(paste0(utils_dir, "/kidney_functions.R")) # function add_kidney_vars_to_data()
source(paste0(utils_dir, "/define_vars.R")) # function define_vars()

# Print session info to metadata log file
sessionInfo()

# Set snapshot index date
index_date = "2023-09-01"

# Find input file names by globbing
input_file <- Sys.glob(here("output", "input_snapshot.csv.gz"))

# Extract data from the input_files and formats columns to correct type 
# (e.g., integer, logical etc)
data_extracted <- extract_data(file_name = input_file) %>%
  mutate(index_date = as.Date(index_date, format = "%Y-%m-%d")) %>%
  # Floor dates to avoid timestamps causing inequalities for dates on the same day
  mutate(across(where(is.Date), 
                ~ floor_date(
                  as.Date(.x, format="%Y-%m-%d"),
                  unit = "days")))

# Add kidney columns to data (egfr and ckd_rrt)
data_extracted_with_kidney_vars <- add_kidney_vars_to_data(data_extracted = data_extracted)

# Process data to use correct factor levels and create prior infection variables
data_processed <- define_vars(data_extracted_with_kidney_vars)

# Save output
output_dir <- here("output", "snapshot")
fs::dir_create(output_dir)
saveRDS(object = data_processed, file = paste0(output_dir, "/processed_snapshot.rds"), compress = TRUE)

# Save csv for local visualisation
#write_csv(data_processed, file = paste0(output_dir, "/processed_snapshot.csv"))
