#The goal of this script is to aggregate light data for analysis


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(ggtext)
library(scales)
#install.packages("wacolors")
#devtools::install_github("CoryMcCartan/wacolors")
library(wacolors)
library(ggstatsplot)

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

# comp_plt_func <- function(df, x_axis, y_axis, gp, text) {
#   grouped_ggbetweenstats(
#     data             = df,
#     x                = x_axis,
#     y                = y_axis,
#     bf.message       = F,
#     results.subtitle = F,
#     paitwise.display = 's',
#     grouping.var     = gp,
#     ggsignif.args    = list(textsize = 10, tip_length = 0.01),
#     p.adjust.method  = "bonferroni",
#     palette          = "Stark",
#     package          = "tvthemes",
#     plotgrid.args    = list(nrow = 1),
#     xlab             = '',
#     ylab             = as.character(text),
#     ggplot.component = list(
#       theme_classic(),
#       theme(
#         text = element_text(size = 50),
#         strip.text = element_text(size = 25)
#       )
#     )
#   )
# }
# 
# #test
# 
# test <- comp_plt_func(
#   df = rbr, 
#   x_axis = year, 
#   y_axis = chl_a,
#   gp = site_proper,
#   text = 'chl-a'
# )

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
  geom_boxplot(aes(x = site, y = par, fill = year), outlier.shape = NA)+ #outlier.shape = NA
  #geom_violin(aes(x = year, y = par, fill = site))+ #outlier.shape = NA
  geom_jitter(
    aes(x = site, y = par),
    size = 3,
    alpha = 0.6
  )+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    text = element_text(size = 25)
  )+
  scale_fill_wa_d(wacolors$rainier)+
  # scale_fill_manual(
  #   values = c('','','')
  # )
  labs(
    x = '',
    y = 'PAR (\u03bcE m<sup>-2</sup> s<sup>-1</sup> )',
    title = 'Ice-Water Interface PAR') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )+
  ylim(0,300)

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
  geom_boxplot(
    aes(x = year, y = total_ice_cm, fill = site),
    outlier.shape = NA
  )+
  geom_point(
    aes(x = year, y = total_ice_cm, shape = site),
    alpha = 0.5,
    size = 3,
    position = position_jitterdodge(jitter.width = 0.2)
  )+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.2, 0.8),
    text = element_text(size = 25)
  )+
  labs(
    x = '',
    y = 'Total Ice (cm)') +
  scale_fill_wa_d(
    palette = 'rainier',
    which = c('lake', 'lodge', 'ground'),
    labels = c('Paint - Deep', 'Paint - Shallow', 'Kempenfelt Bay')
  )+
  scale_shape_discrete(
    labels = c('Paint - Deep', 'Paint - Shallow', 'Kempenfelt Bay')
  )+
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )

ggsave(
  here('output/data_viz/ice_viz/tot_ice_box_2025.06.22.png'),
  dpi = 300,
  width = 7,
  height = 5,
  units = 'in'
)

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


par_sum_wide <- par_ice %>% 
  select(-air_par) %>% 
  pivot_wider(names_from = 'depth', values_from = 'par') %>% 
  mutate(
    par_trans = iw_int/air,
    snow_removed = if_else(
      is.na(snow_removed), iw_int, snow_removed
    ),
    par_trans_no_snow = snow_removed/air,
    blk_ratio = black_ice_cm/ice_sheet_cm,
    wht_ratio = wht_slush_cm/ice_sheet_cm
  )


# **Write clean data to CSV -----------------------------------------------

#write_csv(par_sum_wide, here('data/combined_data/par_ice.csv'))

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
  geom_boxplot(aes(x = year, y = perc_blk_tot, fill = site))+
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
ggplot(data = par_ice_wide_df )+
  geom_boxplot(aes(x = year, y = perc_wht_tot, fill = site),
               outlier.shape = NA)+
  geom_point(
    aes(x = year, y = perc_wht_tot, shape = site), 
    size = 3, 
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2)
  )+
  theme_classic()+
  theme(
    #legend.title = element_blank(),
    legend.position = 'none',
    text = element_text(size = 25)
  )+
  scale_fill_wa_d(
    palette = 'rainier',
    which = c('lake', 'lodge', 'ground')
  )+
  scale_color_wa_d(
    palette = 'rainier',
    which = c('lake', 'lodge', 'ground')
  )+
  labs(
    x = '',
    y = 'White Ice (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )+
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# ggsave(
#   here('output/data_viz/ice_viz/perc_wht_box_2025.06.22.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: PAR Transmitted %----
ggplot(data = par_ice_wide_df)+
  geom_boxplot(aes(x = year, y = perc_trans_air, fill = site),
               outlier.shape = NA)+
  geom_point(
    aes(x = year, y = perc_trans_air, shape = site), 
    size = 3, 
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2)
  )+
  theme_classic()+
  theme(
    #legend.title = element_blank(),
    legend.position = 'none',
    text = element_text(size = 25)
  )+
  scale_fill_wa_d(
    palette = 'rainier',
    which = c('lake','lodge', 'ground')
  )+
  labs(
    x = '',
    y = 'PAR Transmitted (%)') +
  theme(axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
  )+
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# ggsave(
#   here('output/data_viz/par_viz/perc_par_trans_box_2025.06.22.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )

#**Boxplot: PAR -  No Snow %----
ggplot(data = par_ice_wide_df )+
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

# ggsave(
#   here('output/data_viz/perc_par_no_snow_box_2025.png'),
#   dpi = 300,
#   width = 7,
#   height = 5,
#   units = 'in'
# )


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

#write_csv(comp_tbl_df, here('data/combined_data/comparison_table.csv'))


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


# 7. Scatter plots --------------------------------------------------------


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
      size = 4,
      alpha = 0.8
    )+
    theme_classic()+
    # scale_color_viridis_d(
    #   labels = c('Paint Deep', 'Paint Shallow', 'Kempenfelt Bay')
    # )+
    scale_color_wa_d(
      palette = 'rainier',
      which = c('lake', 'lodge', 'ground'),
      labels = c('Paint - Deep', 'Paint - Shallow', 'Kempenfelt Bay')
    )+
    theme(
      legend.position = 'inside',
      legend.position.inside = c(0.8, 0.8),
      legend.box = 'vertical',
      legend.margin = margin(),
      legend.title = element_blank(),
      text = element_text(size = 35)
    )+
    #labs(x = 'Ice Thickness (cm)', y = expression('PAR w/o Snow (\u03bcmol ' ~m^-2 ~s^-1* ')'))+
    labs(x = x_name, y = y_name)+
    scale_y_continuous(labels = percent_format())
}

# **7b. PAR ~ Snow --------------------------------------------------------

# ggplot(data = par_sum_wide)+
#   geom_point(
#     aes(
#       x = snow_avg_cm, 
#       y = par_trans,
#       #y = iw_int, 
#       color = site,
#       shape = year
#     ),
#     size = 3,
#     alpha = 0.7
#   )+
#   theme_classic()+
#   scale_color_viridis_d(
#     labels = c('Paint Deep', 'Paint Shallow', 'Kempenfelt Bay')
#   )+
#   # facet_wrap(~year, ncol = 1)+
#   theme(
#     legend.position = 'bottom',
#     legend.title = element_blank(),
#     text = element_text(size = 15)
#   )+
#   labs(
#     x = 'Snow (cm)',
#     #y = expression('PAR w/o Snow (\u03bcmol '~m^-2~s^-1* ')')
#     y = 'PAR (%)'
#   )+
#   scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30))

library(ggpmisc)
library(ggpubr)

plt_func(
  plt_df = par_sum_wide, 
  var_ind = snow_avg_cm, 
  var_dep = par_trans, 
  x_name = 'Snow Thickness (cm)', 
  y_name = 'PAR (%)'
)+
  # scale_y_log10()+
  # scale_x_log10()+
  theme(
    legend.position = 'bottom'
  )+
  geom_smooth(
    aes(x = snow_avg_cm, y = par_trans),
    method = 'lm',
    color = 'black',
    se = T
  )+
  facet_wrap(~site)+
  scale_x_continuous(trans = 'log1p', breaks = c(0, 3, 30))+
  scale_y_continuous(trans = 'log1p', labels = scales::percent)

test <- lm(data = par_sum_wide , log(par_trans)~log(snow_avg_cm))
summary(test)
ggsave(
  here('output/data_viz/par_viz/log_par_pplt_2025.06.25.png'),
  dpi = 300,
  width = 8.5,
  height = 6.5,
  units = 'in'
)



# **7c. PAR ~ Ice thickness -----------------------------------------------

#NOTE: This is the total thickness of the ice sheet (i.e., ice + slush)
#      We use the PAR transmitted with no snow to determine what is 
#      passing through the ice sheet alone

plt_func(
  plt_df = par_sum_wide, 
  var_ind = ice_sheet_cm, 
  var_dep = par_trans_no_snow, 
  x_name = 'Ice Sheet Thickness (cm)', 
  y_name = 'PAR (%)'
)

ggsave(
  here('output/data_viz/par_viz/par_nosnow_ice_thick.png'),
  dpi = 300,
  width = 5.5,
  height = 5.5,
  units = 'in'
)  


# **7d. PAR ~ White ice thickness  ----------------------------------------

plt_func(
  plt_df = par_sum_wide, 
  var_ind = wht_slush_cm, 
  var_dep = par_trans_no_snow, 
  x_name = 'White Ice & Slush (cm)', 
  y_name = 'PAR (%)'
)

ggsave(
  here('output/data_viz/par_viz/par_white_ice.png'),
  dpi = 300,
  width = 5.5,
  height = 5.5,
  units = 'in'
)  


# **7e. PAR ~ White ice fraction ------------------------------------------



plt_func(
  plt_df = par_sum_wide, 
  var_ind = snow_avg_cm, 
  var_dep = par_trans, 
  x_name = 'Snow (cm)', 
  y_name = 'PAR (%)'
)+
  # scale_y_log10()+
  # scale_x_log10()+
  theme(
    legend.position = 'bottom'
  )+
  geom_smooth(
    aes(x = snow_avg_cm, y = par_trans),
    method = 'lm',
    color = 'black',
    se = T
  )+
  facet_wrap(
    ~site,
    labeller = labeller(site = 
                          c(
                            'simcoe.deep' = 'Kempenfelt Bay',
                            'paint.shallow' = 'Paint Lake - Shallow',
                            'paint.deep' = 'Paint Lake - Deep'
                          ))
  )+
  scale_x_continuous(
    trans = 'log1p', 
    breaks = c(0,3,30), 
    #labels = scales::percent
  )+
  scale_y_continuous(trans = 'log1p', labels = scales::percent)+
  theme(panel.spacing = unit(2, 'lines'))


#par_wht + scale_x_continuous(labels = percent_format())

ggsave(
  here('output/data_viz/par_viz/par_trans_snow_log_all.png'),
  dpi = 300,
  width = 15.5,
  height = 8.5,
  units = 'in'
)  


# **7f. PAR ~ Black ice thickness -----------------------------------------

plt_func(
  plt_df = par_sum_wide, 
  var_ind = black_ice_cm, 
  var_dep = par_trans_no_snow, 
  x_name = 'Black Ice (cm)', 
  y_name = 'PAR (%)'
)

ggsave(
  here('output/data_viz/par_viz/par_blk.png'),
  dpi = 300,
  width = 5.5,
  height = 5.5,
  units = 'in'
)  


# **7g. PAR ~ Black ice ratio ---------------------------------------------

par_blk <- plt_func(
  plt_df = par_sum_wide, 
  var_ind = blk_ratio, 
  var_dep = par_trans_no_snow, 
  x_name = 'Black Ice Ratio (%)', 
  y_name = 'PAR (%)'
)

par_blk + 
  scale_x_continuous(labels = percent_format())

ggsave(
  here('output/data_viz/par_viz/par_blk_ratio.png'),
  dpi = 300,
  width = 5.5,
  height = 5.5,
  units = 'in'
)  



# 8. Fit exponential decay curves -----------------------------------------

#I might make this it's own script.
#I'm using the par_sum_wide data which is aggregated 
#In section "3. combine data"
library(tidyverse)
library(broom)
library(here)
library(plotly)

#**MODEL: PAR ~ snow depth (cm) ----
fit2 <- nls(par_trans ~ SSasymp(snow_avg_cm, yf, y0, log_alpha), data = par_sum_wide)
fit2

y <- qplot(snow_avg_cm, par_trans, data = augment(fit2)) + geom_line(aes(y = .fitted)) 

ggplotly(y)

#**MODEL: PAR ~ white ice thickness ----
fit3 <- nls(par_trans ~ SSasymp(wht_slush_cm, yf, y0, log_alpha), data = par_sum_wide)
fit3

z <- qplot(snow_avg_cm, par_trans, data = augment(fit3)) + geom_line(aes(y = .fitted)) 

ggplotly(z)


# 9. Fancier boxplot ------------------------------------------------------


# 9a. Fancy PAR -----------------------------------------------------------

par_fancy <- grouped_ggbetweenstats(
  data             = par_ice_wide_df,
  x                = year,
  y                = perc_trans_air,
  bf.message       = F,
  results.subtitle = T,
  paitwise.display = 's',
  grouping.var     = site,
  ggsignif.args    = list(textsize = 10, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "Stark",
  package          = "tvthemes",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'PAR Transmission (%)',
  ggplot.component = list(
    theme_classic(),
    theme(
      text = element_text(size = 5),
      strip.text = element_text(size = 25)
    )
  )
)
par_fancy
