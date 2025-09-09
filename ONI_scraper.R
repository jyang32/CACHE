# parse ONI data from NOAA's website and save it in long format


# Load libraries
library(readr)
library(dplyr)
library(stringr) 
library(purrr)

# read the ONI ASCII file from NOAA
oni_raw <- read_lines("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")

# filter lines that look like data 
oni_data <- oni_raw |>
    str_squish() |>
    keep(~ str_detect(.x, "^[A-Z]{3} \\d{4}")) |>
    map_dfr(function(line) {
        parts <- str_split(line, "\\s+")[[1]]
        tibble(
            Season = parts[1],
            Year = as.integer(parts[2]),
            ONI = as.numeric(parts[4])  # the "ANOM" column
        )
    })

# filter to 1970–2023
oni_long <- oni_data |>
    filter(Year >= 1970) |>
    arrange(Year, factor(Season, levels = c("DJF","JFM","FMA","MAM","AMJ","MJJ",
                                            "JJA","JAS","ASO","SON","OND","NDJ")))

# save to CSV
#write_csv(oni_long, "oni_NOAA_1970_2023_long.csv")