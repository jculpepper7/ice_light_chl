par_sum_wide <- read_csv(here('data/combined_data/par_ice.csv')) %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year)
  )


bar_plt_df <- par_sum_wide %>% 
  select(
    site,
    date,
    year,
    black_ice_cm,
    wht_slush_cm,
    snow_avg_cm
  ) %>% 
  pivot_longer(
    cols = c('black_ice_cm', 'wht_slush_cm', 'snow_avg_cm'),
    names_to = 'ice_qual',
    values_to = 'thickness_cm'
  ) %>% 
  arrange(
    site, date
  ) %>% 
  mutate(
    yday = yday(date)
  )

my_colors <- c("snow_avg_cm" = "snow", 
               "wht_slush_cm" = "grey50", 
               "black_ice_cm" = "black")

ggplot(
  bar_plt_df, 
  aes(
    x = yday, 
    y = thickness_cm, 
    fill = factor(
      ice_qual, 
      levels = c('snow_avg_cm', 'wht_slush_cm', 'black_ice_cm')
    )
  )
)+
  geom_bar(stat = 'identity', position = 'stack', color = 'black')+
  labs(fill = 'Ice Type')+
  scale_fill_manual(
    values = c("snow_avg_cm" = "snow", 
               "wht_slush_cm" = "grey50", 
               "black_ice_cm" = "black"), 
    labels = c("Snow", "White Ice", "Black Ice"),
  )+
  facet_wrap(
    ~site + year,
    ncol = 2,
  )+
  theme_classic()+
  theme(
    strip.text = element_blank(),
    legend.title = element_blank()
  )


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
  ) %>% 
  mutate(
    yday = yday(date) 
  )

ggplot()+
  geom_line(
    data = par_ice_wide_df %>% filter(site == 'simcoe.deep'), 
    mapping = aes(x = yday, y = perc_wht_tot)
  )+
  # geom_line(
  #   data = par_ice_wide_df %>% filter(site == 'paint.shallow'), 
  #   mapping = aes(x = yday, y = perc_wht_tot)
  # )+
  scale_y_continuous(
    # position = "right", 
    # sec.axis = sec_axis(~., labels = NULL),
    labels = scales::percent_format(scale = 100)
  )+
  xlab('')+
  ylab('')+
  facet_wrap(
    ~year,
    ncol = 2,
    #scales = 'free'
  )+
  theme_classic()

#save plot
ggsave(
  here(
    'output/data_viz/heat_maps/horz_orientation/kb_ice_qual_horz.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in',
  #bg = 'transparent'
)
