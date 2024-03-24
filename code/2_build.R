# This script parses strings created in import for pairings data

# load packages
library(tidyverse)


# Code --------------------------------------------------------------------

# Split the single string into a vector of pairings
split_data <- raw_data |>
  str_split_1(
    pattern =
    "----------------------------------------------------------------------------------------------------"
    # pattern is 100 hyphens
  )

# Define regular expression functions for parsing -------------------------

# Define the regex functions we'll need to parse this data

str_pairing_id <- function(data){
  # ex. X1011
  str_extract(data, pattern = "[[:upper:]][[:digit:]]{4}")
}

str_check_in <- function(data){
  # Check-In 12:00
  str_extract(data, pattern = "(?<=Check-In )[[:digit:]]{2}:[[:digit:]]{2}")
}

str_check_out <- function(data){
  # Check-Out 12:00
  str_extract(data, pattern = "(?<=Check-Out )[[:digit:]]{2}:[[:digit:]]{2}")
}

str_n_days <- function(data){
  # ex. 10-Day
  str_extract(data, pattern = "[[:digit:]]*(?=-Day)")
}

str_my <- function(data){
  # ex. Mmm 2000
  str_extract(data, pattern = "[[:upper:]]{3} [[:digit:]]{4}")
}

str_flight_table <- function(data){
  
  # Start of line
  # Line starts with newline and a potential space
  start_regex <- "(?<=\\n)\\s*"
  
  # End of line
  # Line ends with a |, a +, or a carriage return
  end_regex <- "[^|+\r]*"
  
  # Define Flight string
  # ex. "1     1111  LOL  12:00   OMG  12:00            222   000:00"
  # first character is a lone digit
  flight_regex <- "([[:digit:]]\\s)"
  
  # Define Night string
  # ex. "JFK   016:51  Boston Copley Square Marr     800-588-2300      000:00"
  # first characters form airport code
  night_regex <- "([[:upper:]]{3}\\s)"
  
  # Define final duty time
  # first characters are a timestamp
  duty_regex <- "([[:digit:]]{3}:[[:digit:]]{2})"
  
  # Combined
  combined_regex <- paste0("(", flight_regex, "|", night_regex, "|", duty_regex, ")")
  
  # Combined with start and end
  flight_table_regex <- paste0(start_regex, combined_regex, end_regex)
  
  str_extract_all(data, pattern = flight_table_regex)
}

str_credit <- function(data){
  # ex. Credit: 024:00
  str_extract(data, pattern = "(?<=Credit: )[[:digit:]]{3}:[[:digit:]]{2}")
}

str_tafb <- function(data){
  # ex. TAFB: 024:00
  str_extract(data, pattern = "(?<=Credit: )[[:digit:]]{3}:[[:digit:]]{2}")
}

str_crew_comp <- function(data){
  # ex. Crew Comp:  1 CA, 1 FO
  str_extract_all(data, pattern = "\\b[[:digit:]]\\s[ACFO]{2}[[:digit:]]*")
}

str_dates <- function(data){
  # ex.
  # +---------------------+
  # | S  M  T  W  T  F  S |
  # |=====================|
  # |         -- -- -- -- |
  # |-- -- -- 07 -- -- -- |
  # |-- -- -- 14 -- -- -- |
  # |-- -- -- 21 -- -- -- |
  # |-- -- -- 28 -- --    |
  # +---------------------+
  str_extract_all(
    data, 
    pattern = "(?<=\\|.{0,20})([[:digit:]]{2})(?=.{0,20}\\|)"
    )
}


# Build parsed data -------------------------------------------------------

# Now we start building out a dataframe

# Create a dataframe with each row as a different pairing
split_dataframe <- tibble(split_data)

# Now we leverage regex to create new columns
parsed_data <- split_dataframe |>
  mutate(
    # First let's pull pairing ID
    pairing_id = str_pairing_id(split_data),
    # Check-in time
    check_in = str_check_in(split_data),
    # Check-out time
    check_out = str_check_out(split_data),
    # Number of days
    n_days = str_n_days(split_data),
    # Month and Year
    my = str_my(split_data),
    # Flight Table
    flight_table = str_flight_table(split_data),
    # Credit
    credit = str_credit(split_data),
    # Time Away From Base
    tafb = str_tafb(split_data),
    # Crew Complement
    crew_comp = str_crew_comp(split_data),
    # Dates
    dates = str_dates(split_data)
  ) |>
  # As a final step we drop NAs caused by quirks of the import process
  drop_na(pairing_id)