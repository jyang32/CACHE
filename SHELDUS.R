# read in SHELDUS data
## check disaster differences between ENSO (2015-2016) and non-ENSO years
## Author: June Yang
## 07/18/2025

rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)

# --------------- define ENSO periods ----------------------
# https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
# https://www.cpc.ncep.noaa.gov/products/precip/CWlink/ENSO/composites/
# https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensocycle/nawinter.shtml


# load ONI data
oni <- read_csv("data/oni_NOAA_1970_2023_long.csv")

# categorize ONI value 
## With optional constraints: A “true” El Niño / La Niña event starts when this condition holds for 5+ consecutive overlapping seasons (from NOAA)

oni <- oni %>%
    mutate(
        ONI_Category = case_when(
            ONI >= 0.5 ~ "El Nino",
            ONI <= -0.5 ~ "La Nina",
            TRUE ~ "Neutral"
        )
    )


# --------------- read in SHELDUS data ---------------------
## calculate direct loss

direct_loss_path <- "data/SHELDUS-Order#3958/direct_loss_aggregated_output_5040.csv"

sheldus <- read_csv(direct_loss_path) 

# First, convert to a date
sheldus <- sheldus %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-")))

# Function to assign each month to an ONI-style 3-month season
month_to_season <- function(month) {
    c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", 
      "JJA", "JAS", "ASO", "SON", "OND", "NDJ")[month]
}

# Add seasonal label and correct season year
sheldus_seasonal <- sheldus %>%
    mutate(
        Month = month(Date),
        Season = month_to_season(Month),
        Season_Year = case_when(
            Season == "DJF" & Month == 12 ~ Year + 1,
            Season == "NDJ" & Month == 11 ~ Year + 1,
            Season == "NDJ" & Month == 12 ~ Year + 1,
            TRUE ~ Year
        )
    )

# Aggregate by County + Season + Season_Year
sheldus_agg <- sheldus_seasonal %>%
    mutate(direct_loss = `CropDmg(ADJ 2023)` + `PropertyDmg(ADJ 2023)`) %>%
    group_by(StateName, CountyName, County_FIPS, Season_Year, Season) %>%
    summarise(
        total_loss = sum(direct_loss, na.rm = TRUE),
        .groups = "drop"
    )

# rename for clarity
sheldus_agg <- sheldus_agg %>%
    rename(Year = Season_Year)


# geographical differences
# NOAA climate regions by state (https://www.ncei.noaa.gov/access/monitoring/reference-maps/us-climate-regions)
# Pueto Rico temporarily excluded from climate region (might be different from continent?)
noaa_regions <- tibble::tibble(
    StateName = c(
        "CONNECTICUT","DELAWARE","MAINE","MARYLAND","MASSACHUSETTS","NEW HAMPSHIRE","NEW JERSEY","NEW YORK","PENNSYLVANIA","RHODE ISLAND","VERMONT"
    ),
    Region = "Northeast"
) %>%
    add_row(StateName = c("IOWA", "MINNESOTA", "MICHIGAN", "WISCONSIN"), Region = "Upper Midwest") %>%
    add_row(StateName = c("MONTANA", "NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "WYOMING"), Region = "Northern Plains") %>%
    add_row(StateName = c("VIRGINIA", "NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "ALABAMA","FLORIDA"), Region = "Southeast") %>%
    add_row(StateName = c("ARKANSAS", "KANSAS", "OKLAHOMA","LOUISIANA", "MISSISSIPPI","TEXAS"), Region = "South") %>%
    add_row(StateName = c("NEW MEXICO", "ARIZONA","COLORADO","UTAH"), Region = "Southwest") %>%
    add_row(StateName = c("IDAHO", "OREGON","WASHINGTON"), Region = "Northwest") %>%
    add_row(StateName = c("CALIFORNIA","NEVADA"), Region = "West") %>%
    add_row(StateName = c("ILLINOIS","INDIANA","KENTUCKY","MISSOURI","OHIO","TENNESSEE","WEST VIRGINIA"), Region = "Ohio Valley") 

# check if all states in SHELDUS data are covered by NOAA regions

anti_join(sheldus_agg, noaa_regions, by = "StateName") %>% distinct(StateName)

# join with SHELDUS data
sheldus_agg_with_oni <- sheldus_agg %>%
    left_join(noaa_regions, by = c("StateName" = "StateName")) %>% 
    left_join(oni, by = c("Year", "Season"))

# --------------- calculate direct loss by ENSO period ---------------------

loss_summary <- sheldus_agg_with_oni %>%
    filter(!is.na(Region)) %>% 
    group_by(Region, ONI_Category) %>%
    summarise(
        avg_total_loss = mean(total_loss, na.rm = TRUE),
        median_total_loss = median(total_loss, na.rm = TRUE),
        sd_total_loss = sd(total_loss, na.rm = TRUE),
        n = n(),
        .groups = "drop"
    ) %>%
    arrange(Region, ONI_Category)

# ------------------ visualization ---------------------

p <- ggplot(loss_summary, aes(x = Region, y = avg_total_loss, fill = ONI_Category)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = avg_total_loss - sd_total_loss / sqrt(n),
                      ymax = avg_total_loss + sd_total_loss / sqrt(n)),
                  position = position_dodge(width = 0.8), width = 0.3) +
    labs(
        title = "Average Total Loss by ENSO Phase and Region",
        x = "Region",
        y = "Average Total Loss (USD)",
        fill = "ENSO Phase"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#ggsave("ENSO_direct_loss_by_region.pdf", plot = p, width = 10, height = 6)