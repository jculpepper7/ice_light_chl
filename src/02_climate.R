library(here)
library(tidyverse)
library(janitor)

# 1. import data ----------------------------------------------------------

climate <- read_csv(here('data/climate/Climate data up to April 2025.csv'))


# 2. Clean data -----------------------------------------------------------

climate_clean <- climate %>% 
  clean_names() %>% 
  mutate(
    station = as.factor(station_name)
  ) %>% 
  group_by(
    station, year, month, day
  ) %>% 
  summarise(
    temp_c = mean(temp_c, na.rm = T),
    precip_mm = mean(precip_amount_mm, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    season = if_else(
      month == 11 | month == 12, as.factor('open'), as.factor('ice')
    ),
    snow = if_else(
      temp_c <= 0, precip_mm, 0
    ),
    date = make_date(year = year, month = month, day = day)
  )
