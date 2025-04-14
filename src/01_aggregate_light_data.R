#The goal of this script is to aggregate light data for analysis


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(ggtext)

# 1. Import data ----------------------------------------------------

#2024 data
#Note: 2024 data were aggregated in Excel. 
par_24_raw <- read_csv(here('data/light/par_2024.csv'))

#2025 data
#Note: 2025 data were recorded as separate csv files and must be aggregated.
par_kb_25 <- read_csv(here('data/light/simcoe_par_2025.csv'))
par_ple_25 <- read_csv(here('data/light/paint_east_par_2025.csv'))
par_plw_25 <- read_csv(here('data/light/paint_west_par_2025.csv'))

#Ice quality data
ice_qual_raw <- read_csv(here('data/ice/ice_quality_update.csv'))

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

ice_qual_clean <- ice_qual_raw %>% 
  mutate(
    date = mdy(date),
    year = as.factor(year(date)),
    site = as.factor(lake)
  ) %>% 
  select(-lake)

# 3. Combine data ---------------------------------------------------------

#First combine 2025 data from separate lake files
par_25 <- par_kn_25_clean %>% 
  bind_rows(par_ple_25_clean, par_plw_25_clean)

#Now combine 2024 and 2025 data
par <- par_24 %>% 
  bind_rows(par_25) %>% 
  mutate(
    year = as.factor(year(date))
  ) %>% 
  filter(
    site != 'simcoe.shallow'
  )


# 4. Data Viz - PAR -------------------------------------------------------


#**Boxplot: Surface air----
ggplot(data = par %>% filter(depth == 'air'))+
  geom_boxplot(aes(x = year, y = par, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'PAR (\u03bcE m<sup>-2</sup> s<sup>-1</sup> )',
    title = 'Surface PAR') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )
  
# ggsave(
#   here('output/data_viz/surface_par_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: Ice-Water interface----
ggplot(data = par %>% filter(depth == 'iw_int',par<1000))+
  geom_boxplot(aes(x = year, y = par, fill = site))+ #outlier.shape = NA
  #geom_jitter(aes(x = year, y = par, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'PAR (\u03bcE m<sup>-2</sup> s<sup>-1</sup> )',
    title = 'Ice-Water Interface PAR') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/iw_int_par_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

# 5. Data Viz - Ice/Snow --------------------------------------------------

#**Boxplot: White Ice----
ggplot(data = ice_qual_clean)+
  geom_boxplot(aes(x = year, y = total_ice_cm, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'Total Ice (cm)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/tot_ice_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: Black Ice----
ggplot(data = ice_qual_clean)+
  geom_boxplot(aes(x = year, y = black_ice_cm, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'Black Ice (cm)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/blk_ice_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: White Ice----
ggplot(data = ice_qual_clean)+
  geom_boxplot(aes(x = year, y = white_ice_cm, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'White Ice (cm)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/wht_ice_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: Snow----
ggplot(data = ice_qual_clean)+
  geom_boxplot(aes(x = year, y = snow_avg_cm, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'Snow Depth (cm)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/snow_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )


# 6. Analysis -------------------------------------------------------------

#**Join PAR & ice quality data ----
par_ice <- par %>%
  full_join(ice_qual_clean) 

#**Summarise data----

# par_sum <- par_ice %>% 
#   group_by(site, date, year) %>% 
#   summarise(
#     perc_blk = black_ice_cm/total_ice_cm,
#     perc_wht = white_ice_cm/total_ice_cm
#   ) %>% 
#   slice(1)

par_sum_wide <- par_ice %>% 
  select(-air_par) %>% 
  pivot_wider(names_from = 'depth', values_from = 'par') 

par_ice_wide_df <- par_sum_wide %>% 
  group_by(site, date, year) %>% 
  summarise(
    perc_blk_tot = black_ice_cm/total_ice_cm,
    perc_blk_sheet = black_ice_cm/ice_sheet_cm,
    perc_wht_tot = white_ice_cm/total_ice_cm,
    perc_wht_sheet = white_ice_cm/ice_sheet_cm,
    perc_wht_slush = wht_slush_cm/ice_sheet_cm,
    perc_trans_air = iw_int/air,
    perc_trans_surf = iw_int/surface_air,
    perc_par_no_snow = snow_removed/surface_air
  )

#**Boxplot: % ice and PAR----

#**Boxplot: black ice %----
ggplot(data = par_ice_wide_df)+
  geom_boxplot(aes(x = year, y = perc_blk, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'Black Ice (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/perc_blk_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: white ice %----
ggplot(data = par_ice_wide_df %>% filter(year == 2025))+
  geom_boxplot(aes(x = year, y = perc_wht_tot, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'White Ice (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom'
  )

# ggsave(
#   here('output/data_viz/perc_wht_box_2025.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: PAR Transmitted %----
ggplot(data = par_ice_wide_df)+
  geom_boxplot(aes(x = year, y = perc_trans_air, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'PAR Transmitted (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

# ggsave(
#   here('output/data_viz/perc_par_trans_box.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: PAR -  No Snow %----
ggplot(data = par_ice_wide_df %>% filter(year == 2025))+
  geom_boxplot(aes(x = year, y = perc_par_no_snow, fill = site))+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'PAR - No Snow (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom'
  )

ggsave(
  here('output/data_viz/perc_par_no_snow_box_2025.png'),
  dpi = 300,
  width = 7,
  height = 5,
  units = 'in'
)


# 7. Value Table ----------------------------------------------------------

#**7a. Comparison Table----
comp_tbl_df <- par_sum_wide %>% 
  group_by(site, date, year) %>% 
  summarise(
    ice_sheet_cm = ice_sheet_cm,
    total_ice_cm = total_ice_cm,
    black_ice_cm = black_ice_cm,
    white_ice_cm = white_ice_cm,
    wht_slush_cm = wht_slush_cm,
    n_white = n_white,
    n_slush = n_slush,
    snow_avg_cm = snow_avg_cm,
    perc_blk_tot = black_ice_cm/total_ice_cm,
    perc_blk_sheet = black_ice_cm/ice_sheet_cm,
    perc_wht_tot = white_ice_cm/total_ice_cm,
    perc_wht_sheet = white_ice_cm/ice_sheet_cm,
    perc_wht_slush = wht_slush_cm/ice_sheet_cm,
    perc_trans_air = iw_int/air,
    perc_trans_surf = iw_int/surface_air,
    perc_par_no_snow = snow_removed/surface_air
  )

#write_csv(comp_tbl_df, here('data/comparison_table.csv'))


#**7b. Means Table ----

mean_tbl_df <- comp_tbl_df %>% 
  group_by(site, year) %>%
  summarise(
    ice_sheet_cm = mean(ice_sheet_cm, na.rm = T),
    total_ice_cm = mean(total_ice_cm, na.rm = T),
    black_ice_cm = mean(black_ice_cm, na.rm = T),
    white_ice_cm = mean(white_ice_cm, na.rm = T),
    wht_slush_cm = mean(wht_slush_cm, na.rm = T),
    n_white = mean(n_white, na.rm = T),
    n_slush = mean(n_slush, na.rm = T),
    snow_avg_cm = mean(snow_avg_cm, na.rm = T),
    perc_blk_tot = mean(perc_blk_tot, na.rm = T),
    perc_blk_sheet = mean(perc_blk_sheet, na.rm = T),
    perc_wht_tot = mean(perc_wht_tot, na.rm = T),
    perc_wht_sheet = mean(perc_wht_sheet, na.rm = T),
    perc_wht_slush = mean(perc_wht_slush, na.rm = T),
    perc_trans_air = mean(perc_trans_air, na.rm = T),
    perc_trans_surf = mean(perc_trans_surf, na.rm = T),
    perc_par_no_snow = mean(perc_par_no_snow, na.rm = T)
  )

#write_csv(mean_tbl_df, here('data/mean_table.csv'))



# 8. Wilcoxon Test --------------------------------------------------------


# x <- tibble(value = rnorm(100, mean = 0, sd = 1)) %>% 
#   mutate(
#     type = as.factor('A')
#   )
# y <- tibble(value = rnorm(100, mean = 10, sd = 1)) %>% 
#   mutate(
#     type = as.factor('B')
#   )
# 
# test <- x %>% 
#   bind_rows(y)
# 
# ggplot(data = test)+
#   geom_boxplot(aes(x = type, y = value))
# 
# res <- wilcox.test(
#   data = test,
#   value ~ type,
#   exact = F
# )
# 
# res
# res$p.value #p.value <0.05, 
