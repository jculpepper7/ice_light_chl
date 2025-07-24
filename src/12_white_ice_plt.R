par_ice_wide_df

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
