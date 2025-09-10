
rm(list = ls())

source("main.R")

library(tidyverse)

empower_seasonal <- empower_final %>% 
    mutate(
        Month = month(date),
        Year = year(date),
        Season = month_to_season(Month),
        Season_Year = case_when(
            Season == "DJF" & Month == 12 ~ Year + 1,
            Season == "NDJ" & Month == 11 ~ Year + 1,
            Season == "NDJ" & Month == 12 ~ Year + 1,
            TRUE ~ Year
        )
    )

empower_agg <- empower_seasonal %>%
    group_by(State_FIPS_Code, State, FIPS_Code, County_FIPS_Code, Year, Season) %>%
    summarise(
        avg_medicare_benes = mean(Medicare_Benes, na.rm = TRUE),
        avg_power_dependent_devices_dme = mean(Power_Dependent_Devices_DME, na.rm = TRUE),
        .groups = "drop"
    ) %>% 
    select(FIPS_Code, Year, Season, avg_medicare_benes, avg_power_dependent_devices_dme)

df <- left_join(empower_agg, sheldus_agg_with_oni, by = c("FIPS_Code" = "County_FIPS", "Year" = "Year", "Season"))

# ? SHELDUS only contains events data

df_final <- df %>% 
    filter(is.na(total_loss)==FALSE)