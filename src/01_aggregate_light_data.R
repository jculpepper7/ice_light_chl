#The goal of this script is to aggregate light data for analysis


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)

# 1. Import data ----------------------------------------------------------

#2024 data
#Note: 2024 data were aggregated in Excel. 
par_24_raw <- read_csv(here('data/light/par_2024.csv'))

#2025 data
#Note: 2025 data were recorded as separate csv files and must be aggregated.
par_kb_25 <- read_csv(here('data/light/simcoe_par_2025.csv'))
par_ple_25 <- read_csv(here('data/light/paint_east_par_2025.csv'))
par_plw_25 <- read_csv(here('data/light/paint_west_par_2025.csv'))

# 2. Clean data -----------------------------------------------------------

#Clean 2024 data that were consolidated in Excel
par_24 <- par_24_raw %>% 
  select(1:4) %>% 
  mutate(
    date = mdy(date),
    site = as.factor(site),
    depth = as.factor(depth),
    par = as.numeric(light)
  ) %>% 
  select(-light)

#Clean 2025 data from separate lake files
par_kn_25_clean <- par_kb_25 %>% 
  mutate(
    date = ymd(date),
    site = as.factor(site),
    depth = as.factor(depth)
  ) %>% 
  #Remove the additional columns when converting from excel file to csv
  select(1:5)

par_ple_25_clean <- par_ple_25 %>% 
  mutate(
    date = ymd(date),
    site = as.factor(site),
    depth = as.factor(depth)
  )

par_plw_25_clean <- par_plw_25 %>% 
  mutate(
    date = ymd(date),
    site = as.factor(site),
    depth = as.factor(depth)
  )

# 3. Combine data ---------------------------------------------------------

#First combine 2025 data from separate lake files
par_25 <- par_kn_25_clean %>% 
  bind_rows(par_ple_25_clean, par_plw_25_clean)

#Now combine 2024 and 2025 data
par <- par_24 %>% 
  bind_rows(par_25) %>% 
  mutate(
    year = year(date)
  ) %>% 
  filter(
    site != 'simcoe.shallow'
  )

# 4. Analysis -------------------------------------------------------------










