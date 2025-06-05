#The goal of this script is to clean the multi-perameter probe (RBR) data
#This script 
#1. reads the raw RBR data
#2. eliminates empty rows (in RBR 24)
#3. removes the top and bottom meter (due to unreliable values)
#5. takes an average for the water column
#6. takes an average for the top meter and bottom meter of the edited file


# 1. Libraries ------------------------------------------------------------
 
library(tidyverse)
library(here)
library(janitor)


# 2. Import data ----------------------------------------------------------

rbr_24 <- read_csv(here('data/rbr/RBR_24_WorkFile.csv'))

rbr_25 <- read_csv(here('data/rbr/RBR_25_WorkFile.csv'))


# 3. Clean data -----------------------------------------------------------

#**Clean 2024 data----

rbr24_clean <- rbr_24 %>%
  clean_names() %>% 
  #Many rows with missing values
  na.omit() %>% 
  select(
    site,
    date = time,
    depth,
    pressure,
    temp_c = temperature,
    do_mg_l = dissolved_o2_concentration,
    do_sat = dissolved_o2_saturation,
    par, #(i.e., upwelling PAR)
    chl_a = chlorophyll_a
  ) %>% 
  mutate(
    site = as.factor(site),
    date = dmy(date),
    year = as.factor(
      year(date)
    )
  ) %>% 
  #Remove the top meter of the water column.
  group_by(site, date) %>% 
  filter(
    depth >= min(depth) + 1 & depth <= max(depth) - 1
  ) %>% 
  ungroup() %>% 
  arrange(
    site, date
  )

#**Clean 2025 data----

rbr25_clean <- rbr_25 %>%
  clean_names() %>% 
  #Many rows with missing values
  na.omit() %>% 
  select(
    site,
    date = time,
    depth,
    pressure,
    temp_c = temperature,
    do_mg_l = dissolved_o2_concentration, #I was wrong. This is not in mg/L. It's in umol/L. That's why the values appear high.
    do_sat = dissolved_o2_saturation,
    par, #(i.e., upwelling PAR)
    chl_a = chlorophyll_a
  ) %>% 
  mutate(
    site = as.factor(site),
    date = dmy(date),
    year = as.factor(
      year(date)
    )
  ) %>% 
  #Remove the top meter of the water column.
  group_by(site, date) %>% 
  filter(
    depth >= min(depth) + 1 & depth <= max(depth) - 1
  ) %>% 
  ungroup() %>% 
  arrange(
    site, date
  )


# 4. Join data ------------------------------------------------------------


rbr_clean <- rbr24_clean %>% 
  bind_rows(rbr25_clean) %>% 
  group_by(site, year, date) %>% 
  #Add a factor for top, mid, and bottom depths
  #'top' = top meter, 'bottom' = bottom meter, all else = 'mid'
  mutate(
    position = as.factor(
      if_else(
        depth <= min(depth) + 1, 'top',
        if_else(
          depth >= max(depth) - 1, 'bottom', 'mid'
        )
      )
    )
  )

#**Write cleaned data to CSV----

#write_csv(rbr_clean, here('data/rbr/rbr_clean.csv'))

# 5. Summarize water column averages --------------------------------------


rbr_w_col_mean <- rbr_clean %>% 
  group_by(
    site, year, date
  ) %>%
  summarise(
    mean_temp = mean(temp_c, na.rm = T),
    mean_do = mean(do_mg_l, na.rm = T),
    mean_do_sat = mean(do_sat, na.rm = T),
    mean_par_up = mean(par, na.rm = T),
    mean_chl_a = mean(chl_a, na.rm = T)
  ) %>% 
  mutate(
    position = as.factor('full') #(i.e., full water column average)
  )


# 6. Summarise by discrete depths -----------------------------------------

rbr_z_mean <- rbr_clean %>% 
  group_by(
    site, year, date, position
  ) %>%
  summarise(
    mean_temp = mean(temp_c, na.rm = T),
    mean_do = mean(do_mg_l, na.rm = T),
    mean_do_sat = mean(do_sat, na.rm = T),
    mean_par_up = mean(par, na.rm = T),
    mean_chl_a = mean(chl_a, na.rm = T)
  ) 


# 7. Join full and discrete summaries -------------------------------------

rbr_all <- rbr_w_col_mean %>% 
  bind_rows(rbr_z_mean) %>% 
  arrange(site, date, position)


#**Write processed data to CSV ----

#write_csv(rbr_all, here('data/combined_data/rbr_processed.csv'))

