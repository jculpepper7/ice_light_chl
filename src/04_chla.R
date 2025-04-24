
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(janitor)


# 1. Import data ----------------------------------------------------------

chl_24 <- read_csv(here('data/chla/fluorometry_2024.csv'))

chl_25 <- read_csv(here('data/chla/fluorometry_2025.csv'))


# 2. Clean data -----------------------------------------------------------


# **2024 ------------------------------------------------------------------

chl_24_clean <- chl_24 %>% 
  clean_names() %>% 
  select(
    site,
    date = date_collected_mm_dd_yyyy,
    total_conc_ug_l = total_conc_ug_l,
    green_ug_l = green_algae,
    bluegreen_ug_l = bluegreen,
    diatom_ug_l = diatoms,
    cryptophyta_ug_l = cryptophyta,
    yellow_r_u = yellow_substances
  ) %>% 
  mutate(
    site = as.factor(site),
    date = mdy(date)
  ) %>% 
  filter(
    site != 'Lake Simcoe Shallow'
  )


# **2025 ------------------------------------------------------------------

chl_25_clean <- chl_25 %>% 
  clean_names() %>% 
  select(
    site,
    date = date_collected_mm_dd_yyyy,
    total_conc_ug_l = total_conc_ug_l,
    green_ug_l = green_algae,
    bluegreen_ug_l = bluegreen,
    diatom_ug_l = diatoms,
    cryptophyta_ug_l = cryptophyta,
    yellow_r_u = yellow_substances
  ) %>% 
  mutate(
    site = as.factor(site),
    date = mdy(date),
    site = if_else(
      site == 'Simcoe K42' | site == 'Simcoe - K42', 'Lake Simcoe Deep', site
    )
  ) %>% 
  na.omit() %>% 
  filter(
    site != 'Paint East 4m',
    site != 'Paint West 14m'
  )


# 3. Combine data ---------------------------------------------------------

chla_df <- chl_24_clean %>% 
  bind_rows(chl_25_clean) %>% 
  mutate(
    year = as.factor(year(date))
  ) %>% 
  #Reorder the columns
  select(2,1,9,3:8)


# 4. Simple viz -----------------------------------------------------------

ggplot(data = chla_df) +
  geom_boxplot(aes(x = site, y = cryptophyta_ug_l, fill = year))+
  geom_jitter(aes(x = site, y = cryptophyta_ug_l), size = 2, alpha = 0.7)+
  theme_classic()+
  xlab('')+
  ylab('Cryptophyta [ug/L]')+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/chla_crypto.png'),
  dpi = 300,
  units = 'in',
  width = 6.5,
  height = 5
)
