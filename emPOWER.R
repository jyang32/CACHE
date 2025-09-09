# load emPOWER data from multiple Excel files, reshape, and save as CSV

(list = ls()) # Clear the environment

library(readxl)
library(tidyverse)
library(lubridate)
library(purrr)


# Files
files <- list.files("data/empower", pattern = "\\.xlsx$", full.names = TRUE)

# Guess the right header row by reading first 2 rows
check_headers <- function(f) {
    read_excel(f, sheet = "County", n_max = 2)
}

# Inspect if any have junk rows
header_preview <- map(files, check_headers)
# View(header_preview[[1]])  # manually confirm header is on row 2?

# Adjust this based on what you find:
correct_header_row <- 1  # try 2 if first row is blank

# Define expected ID columns
static_cols <- c("FIPS_Code", "County_FIPS_Code", "County", "State_FIPS_Code", "State")

# Final long-format reader
empower_long_all <- map_dfr(files, function(f) {
    df <- read_excel(f, sheet = "County", skip = correct_header_row - 1)
    
    # Clean column names: remove whitespace
    names(df) <- str_replace_all(names(df), "\\s+", "")
    
    # Check that expected columns exist — if not, skip file
    if (!all(static_cols %in% names(df))) {
        message("Skipping file due to missing columns: ", f)
        return(NULL)
    }
    
    df %>%
        pivot_longer(
            cols = -all_of(static_cols),
            names_to = "raw_col",
            values_to = "value"
        ) %>%
        separate(raw_col, into = c("month", "year", "type"), sep = "_", extra = "merge", fill = "right") %>%
        mutate(
            month = str_trim(month),
            year = as.integer(year),
            month_num = case_when(
                month %in% month.abb ~ match(month, month.abb),
                month %in% month.name ~ match(month, month.name),
                TRUE ~ NA_integer_
            ),
            date = as.Date(sprintf("%d-%02d-01", year, month_num))
        ) %>%
        select(-month, -month_num, -year)
})

# Final reshaping
empower_final <- empower_long_all %>%
    distinct(FIPS_Code, County, State, date, type, .keep_all = TRUE) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    relocate(date, .after = State)

#write_csv(empower_final, "data/empower/empower_long_final.csv")
