# read in SHELDUS data
## check disaster differences between ENSO (2015-2016) and non-ENSO years
## Author: June Yang
## 07/18/2025

library(tidyverse)
library(readxl)
library(lubridate)

# --------------- read in SHELDUS data ---------------------
direct_loss_path <- "../../SHELDUS-Order#3958/direct_loss_aggregated_output_5040.csv"

# --------------- define ENSO periods ----------------------
# https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php
direct_loss_df <- read_csv(direct_loss_path) %>%
  mutate(period_label = case_when(
      Year == 2015 & Month == 9 ~ "2m_before",
      Year == 2015 & Month == 10 ~ "1m_before",
      (Year == 2015 & Month >= 11) | (Year == 2016 & Month <= 1) ~ "ENSO",
      Year == 2016 & Month == 2 ~ "1m_after",
      Year == 2016 & Month == 3  ~ "2m_after",
      Year == 2016 & Month %in% 4:6 ~ "3to6m_after",
      TRUE ~ NA_character_
  ))

# calculate direct loss 
direct_loss_df <- direct_loss_df %>%
  mutate(total_direct_loss = `CropDmg(ADJ 2023)` + `PropertyDmg(ADJ 2023)`)

# summarize loss by period for all states
direct_loss_df %>%
    filter(!is.na(period_label)) %>%
    group_by(period_label) %>%
    summarise(
        mean_loss = mean(total_direct_loss, na.rm = TRUE),
        median_loss = median(total_direct_loss, na.rm = TRUE),
        total_loss = sum(total_direct_loss, na.rm = TRUE),
        record_count = n()
    ) %>%
    arrange(match(period_label, c("2m_before", "1m_before", "ENSO", "1m_after", "2m_after", "3to6m_after")))

# geographical differences
# NOAA climate regions by state (https://www.ncei.noaa.gov/access/monitoring/reference-maps/us-climate-regions)
# Pueto Rico temporarily excluded from climate region (might be different from continent?)
noaa_regions <- tibble::tibble(
    StateName = c(
        "MAINE", "NEW HAMPSHIRE", "VERMONT", "MASSACHUSETTS", "RHODE ISLAND", "CONNECTICUT", "NEW YORK", "NEW JERSEY", "PENNSYLVANIA"
    ),
    Region = "Northeast"
) %>%
    add_row(StateName = c("OHIO", "INDIANA", "ILLINOIS", "MICHIGAN", "WISCONSIN"), Region = "Upper Midwest") %>%
    add_row(StateName = c("IOWA", "MISSOURI", "MINNESOTA", "NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "KANSAS"), Region = "Northern Plains") %>%
    add_row(StateName = c("DELAWARE", "MARYLAND", "VIRGINIA", "WEST VIRGINIA", "KENTUCKY", "TENNESSEE", "NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "ALABAMA"), Region = "Southeast") %>%
    add_row(StateName = c("ARKANSAS", "LOUISIANA", "MISSISSIPPI"), Region = "South") %>%
    add_row(StateName = c("TEXAS", "OKLAHOMA"), Region = "South") %>%
    add_row(StateName = c("NEW MEXICO", "ARIZONA"), Region = "Southwest") %>%
    add_row(StateName = c("NEVADA", "UTAH", "COLORADO", "WYOMING", "IDAHO", "MONTANA"), Region = "Northwest") %>%
    add_row(StateName = c("WASHINGTON", "OREGON"), Region = "Pacific Northwest") %>%
    add_row(StateName = c("CALIFORNIA"), Region = "West Coast") %>%
    add_row(StateName = c("FLORIDA"), Region = "Southeast") %>%
    add_row(StateName = c("ALASKA"), Region = "Alaska") %>%
    add_row(StateName = c("HAWAII"), Region = "Pacific Islands")

# check if all states in SHELDUS data are covered by NOAA regions

anti_join(direct_loss_df, noaa_regions, by = "StateName") %>% distinct(StateName)
# join with SHELDUS data
direct_loss_df <- direct_loss_df %>%
    left_join(noaa_regions, by = c("StateName" = "StateName"))

# summarize loss by period and region
direct_loss_df %>%
    filter(!is.na(period_label), !is.na(Region)) %>%
    group_by(Region, period_label) %>%
    summarise(
        mean_loss = mean(total_direct_loss, na.rm = TRUE),
        median_loss = median(total_direct_loss, na.rm = TRUE),
        total_loss = sum(total_direct_loss, na.rm = TRUE),
        record_count = n()
    ) %>%
    arrange(Region, match(period_label, c("2m_before", "1m_before", "ENSO", "1m_after", "2m_after", "3to6m_after")))

# ------------------ visualization ---------------------
df_summary <- direct_loss_df %>%
    filter(!is.na(period_label), !is.na(Region)) %>%
    group_by(Region, period_label) %>%
    summarise(mean_loss = mean(total_direct_loss, na.rm = TRUE))

df_summary <- df_summary %>%
    mutate(period_label = factor(period_label, levels = c("2m_before", "1m_before", "ENSO", "1m_after", "2m_after", "3to6m_after")))

ggplot(df_summary, aes(x = period_label, y = mean_loss, group = Region, color = Region)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ Region, scales = "free_y") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))