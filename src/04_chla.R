
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
  select(2,1,9,3:8) %>% 
  mutate(
    w_year = if_else(
      month(date) >= 10, year(date)+1, year(date)
    )
  ) %>% 
  na.omit() %>% 
  mutate(
    site = if_else(
      site == 'Paint West', 'paint.deep',
      if_else(
        site == 'Paint East', 'paint.shallow', 'simcoe.deep'
      )
    )
  )

#write_csv(chla_df, here('data/chla/chl_df.csv'))

# 4. Simple viz -----------------------------------------------------------


# **Boxplots --------------------------------------------------------------


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


# **Time series -----------------------------------------------------------

ggplot(data = chla_df) +
  geom_point(aes(x = date, y = total_conc_ug_l, color = site))+
  geom_line(aes(x = date, y = total_conc_ug_l, color = site))+
  
  theme_classic()+
  xlab('')+
  ylab('Total Chl-a [ug/L]')+
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    text = element_text(size = 15)
  )+
  facet_wrap(~w_year, scales = 'free')


# 5. Chl-a stat tests -----------------------------------------------------



# **Mann Whitney U --------------------------------------------------------

wlcx_chla_rslt <- chla_df %>% 
  group_by(site) %>% 
  summarise(
    #Total chla concentration
    chl_tot_p = wilcox.test(total_conc_ug_l~year, exact = F)$p.value,
    chl_tot_w = wilcox.test(total_conc_ug_l~year, exact = F)$statistic,
    chl_tot_n = n(),
    #Green concentration
    green_p = wilcox.test(green_ug_l~year, exact = F)$p.value,
    green_w = wilcox.test(green_ug_l~year, exact = F)$statistic,
    #Bluegreen
    bg_p = wilcox.test(bluegreen_ug_l~year, exact = F)$p.value,
    bg_w = wilcox.test(bluegreen_ug_l~year, exact = F)$statistic,
    #Diatoms
    diatom_p = wilcox.test(diatom_ug_l~year, exact = F)$p.value,
    diatom_w = wilcox.test(diatom_ug_l~year, exact = F)$statistic,
    #Cryptophyta
    crypt_p = wilcox.test(cryptophyta_ug_l~year, exact = F)$p.value,
    crypt_w = wilcox.test(cryptophyta_ug_l~year, exact = F)$statistic,
    #Yellow
    yellow_p = wilcox.test(yellow_r_u~year, exact = F)$p.value,
    yellow_w = wilcox.test(yellow_r_u~year, exact = F)$statistic,
    
)

write_csv(wlcx_chla_rslt, here('output/stat_test/wilcox_chl_rslt.csv'))


# **Kolmogorov Smirnov ----------------------------------------------------

ks_chla_rslt <- chla_df %>% 
  group_by(site) %>% 
  summarise(
    #Total chla concentration
    chl_tot_p = ks.test(total_conc_ug_l~year, exact = F)$p.value,
    chl_tot_w = ks.test(total_conc_ug_l~year, exact = F)$statistic,
    chl_tot_n = n(),
    #Green concentration
    green_p = ks.test(green_ug_l~year, exact = F)$p.value,
    green_w = ks.test(green_ug_l~year, exact = F)$statistic,
    #Bluegreen
    bg_p = ks.test(bluegreen_ug_l~year, exact = F)$p.value,
    bg_w = ks.test(bluegreen_ug_l~year, exact = F)$statistic,
    #Diatoms
    diatom_p = ks.test(diatom_ug_l~year, exact = F)$p.value,
    diatom_w = ks.test(diatom_ug_l~year, exact = F)$statistic,
    #Cryptophyta
    crypt_p = ks.test(cryptophyta_ug_l~year, exact = F)$p.value,
    crypt_w = ks.test(cryptophyta_ug_l~year, exact = F)$statistic,
    #Yellow
    yellow_p = ks.test(yellow_r_u~year, exact = F)$p.value,
    yellow_w = ks.test(yellow_r_u~year, exact = F)$statistic
  )

write_csv(ks_chla_rslt, here('output/stat_test/ks_chl_rslt.csv'))


# **Brown-Forsythe --------------------------------------------------------



# 6. Combine chl-a and PAR data -------------------------------------------

par_ice <- read_csv(here('data/combined_data/par_ice.csv'))

chla_df <- read_csv(here('data/chla/chl_df.csv'))

chla_z <- read_csv(here('data/rbr/max_chl_depth.csv'))

chla_z_clean <- chla_z %>% 
  mutate(
    site = as.factor(site),
  ) %>% 
  separate(
    full.date, 
    c('month', 'day', 'year')
  ) %>% 
  mutate(
    date = make_date(
      year = year, 
      month = month, 
      day = day
    ),
    year = as.factor(year)
  ) %>% 
  arrange(site, date) %>% 
  select(
    -c('month', 'day')
  ) %>% 
  filter(
    site != 'simcoe.shallow',
    site != 'paint.south'
  )

chl_par <- chla_df %>% 
  left_join(par_ice)%>% 
  mutate(
    year = as.factor(year),
    site = as.factor(site)
  ) %>%  
  left_join(chla_z_clean) %>% 
  arrange(site, date)


# **Write clean data to CSV -----------------------------------------------


write_csv(chl_par, here('data/combined_data/chla_par_clean.csv'))

# 7. Chl-a -- Snow plots --------------------------------------------------


# **7a. Plot function -----------------------------------------------------


plt_func <- function(
    plt_df, 
    var_ind, 
    var_dep, 
    x_name = '', 
    y_name = ''
) {
  
  ggplot(data = plt_df)+
    geom_point(
      aes(
        x = {{var_ind}}, 
        y = {{var_dep}}, 
        color = site,
        shape = year
      ),
      size = 3,
      alpha = 0.7
    )+
    theme_classic()+
    scale_color_viridis_d(
      labels = c('Paint Deep', 'Paint Shallow', 'Kempenfelt Bay')
    )+
    theme(
      legend.position = 'bottom',
      legend.box = 'vertical',
      legend.margin = margin(),
      legend.title = element_blank(),
      text = element_text(size = 15)
    )+
    #labs(x = 'Ice Thickness (cm)', y = expression('PAR w/o Snow (\u03bcmol ' ~m^-2 ~s^-1* ')'))+
    labs(x = x_name, y = y_name)#+
    #scale_y_continuous(labels = percent_format())
}


# **7b. Chla ~ Snow -------------------------------------------------------


plt_func(
  plt_df = chl_par, 
  var_ind = snow_avg_cm, 
  var_dep = total_conc_ug_l, 
  x_name = 'Snow (cm)', 
  y_name = 'Chl-a Concentration (ug/L)'
)

# ggsave(
#   here('output/data_viz/chla_viz/chla_by_snow.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  


# **7c. Chla ~ Ice sheet thickness ----------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = ice_sheet_cm, 
  var_dep = total_conc_ug_l, 
  x_name = 'Ice Thickness (cm)', 
  y_name = 'Chl-a Concentration (ug/L)'
)


# **7d. Chla ~ White ice thicknes -----------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = wht_slush_cm, 
  var_dep = total_conc_ug_l, 
  x_name = 'White Ice Thickness (cm)', 
  y_name = 'Chl-a Concentration (ug/L)'
)


# **7e. Chla ~ White ice ratio --------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = wht_ratio, 
  var_dep = total_conc_ug_l, 
  x_name = 'White Ice Ratio (%)', 
  y_name = 'Chl-a Concentration (ug/L)'
)


# **7f. Chla ~ Black ice --------------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = black_ice_cm, 
  var_dep = total_conc_ug_l, 
  x_name = 'Black Ice (cm)', 
  y_name = 'Chl-a Concentration (ug/L)'
)


# **7g. Chl-a ~ Black ice ratio -------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = blk_ratio, 
  var_dep = total_conc_ug_l, 
  x_name = 'Black Ice Ratio (%)', 
  y_name = 'Chl-a Concentration (ug/L)'
)


# **7h. Chl-a ~ PAR transmitted -------------------------------------------


plt_func(
  plt_df = chl_par, 
  var_ind = par_trans, 
  var_dep = total_conc_ug_l, 
  x_name = 'PAR Transmitted (%)', 
  y_name = 'Chl-a Concentration (ug/L)'
)+
  scale_x_continuous(labels = percent_format())

#This result is pretty interesting. At first glance it seems 
#like chl-a decreases with %PAR transmitted.
#But I think what we're actually seeing is the algal mass
#moving down in the water column as the under-ice light
#environment becomes brighter.
#We can test this by getting the chl-a peak depth
#from the RBR data.

# ggsave(
#   here('output/data_viz/chla_viz/chla_by_par.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  

# **7i. Chla ~ PAR transmitted w/o snow -----------------------------------


plt_func(
  plt_df = chl_par, 
  var_ind = par_trans_no_snow, 
  var_dep = total_conc_ug_l, 
  x_name = 'PAR w/o Snow (%)', 
  y_name = 'Chl-a Concentration (ug/L)'
)+
  scale_x_continuous(labels = percent_format())

# **7j. Chla ~ PAR transmitted  -----------------------------------


plt_func(
  plt_df = chl_par, 
  var_ind = par_trans, 
  var_dep = max_chl_depth, 
  x_name = 'PAR Transmitted (%)', 
  y_name = 'Chl-a Depth (m)'
)+
  scale_x_continuous(labels = percent_format())

# ggsave(
#   here('output/data_viz/chla_viz/chlz_by_par_trans.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  


# **7k. Chla depth ~ snow -------------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = snow_avg_cm, 
  var_dep = max_chl_depth, 
  x_name = 'Snow (cm)', 
  y_name = 'Chl-a Depth (m)'
)

# ggsave(
#   here('output/data_viz/chla_viz/chlz_by_snow.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )

# **7l. Chla depth ~ black% -------------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = blk_ratio, 
  var_dep = max_chl_depth, 
  x_name = 'Black ice (%)', 
  y_name = 'Chl-a Depth (m)'
)+
  scale_x_continuous(labels = percent_format())

# ggsave(
#   here('output/data_viz/chla_viz/chlz_by_blk_ratio.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  

# **7m. Chla depth ~ white% -------------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = wht_ratio, 
  var_dep = max_chl_depth, 
  x_name = 'White ice (%)', 
  y_name = 'Chl-a Depth (m)'
)+
  scale_x_continuous(labels = percent_format())

# ggsave(
#   here('output/data_viz/chla_viz/chlz_by_wht_ratio.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  

# **7n. Chla depth ~ white% -------------------------------------------------

plt_func(
  plt_df = chl_par, 
  var_ind = ice_sheet_cm, 
  var_dep = max_chl_depth, 
  x_name = 'Ice Sheet (cm)', 
  y_name = 'Chl-a Depth (m)'
)

# ggsave(
#   here('output/data_viz/chla_viz/chlz_by_ice_thickness.png'),
#   dpi = 300,
#   width = 6.5,
#   height = 6.5,
#   units = 'in'
# )  


# 8. MWU test on chla depth -----------------------------------------------

wlcx_chla_rslt <- chl_par %>% 
  group_by(site) %>% 
  summarise(
    #Total chla concentration
    chl_z_p = wilcox.test(max_chl_depth~year, exact = F)$p.value,
    chl_z_w = wilcox.test(max_chl_depth~year, exact = F)$statistic,
    chl_z_n = n()
  )
