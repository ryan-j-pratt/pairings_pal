# This script imports a single string from a text file for cleaning

# load packages
library(tidyverse)
library(here)

# Code --------------------------------------------------------------------

# NOTE: Data files are excluded from git repository. BYOD (Bring Your Own Data).
file_names <- list.files(path = here("data", "raw"))

# Function to parse pairing data from text files
import_txt_file <- function(file_name) {
  read_file(
    here("data", "raw", file_name)
  )
}

# Create list of file strings
raw_data <- file_names |>
  map(import_txt_file) |>
  list_c() |>
  paste(collapse = "")
