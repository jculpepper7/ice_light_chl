test <- rbr %>% 
  na.omit() %>%
  # mutate(
  #   t_diff = abs(temp - 4),         #Temp of max density (approx.)
  #   do_diff_high = abs(do_mgL - 8), #Stress threshold of some fish sp.
  #   do_diff_low = abs(do_mgL - 2),  #General hypoxia threshold
  #   chl_diff = abs(chl_a - 2)       #Oligotrophic threshold (approx.)
  # ) %>% 
  group_by(
    site, yday_fac, year
  ) %>% 
  mutate(
    z_high_chl = depth[chl_a == max(chl_a)],
    z_low_chl = depth[chl_a == min(chl_a)]
    # z_do_high = depth[do_diff_high == min(do_diff_high)],
    # z_do_low = depth[do_diff_low == min(do_diff_low)],
    # z_chla = depth[chl_diff == min(chl_diff)]
  ) 

test_sum <- test %>% 
  group_by(site, year) %>% 
  summarise(
    mean_z = mean(z_high_chl)
  )
z_high_chl 
x <- ggplot(data = test)+
  geom_line(
    mapping = aes(x = yday, y = z_high_chl),
    color = 'forestgreen'
  )+
  # geom_line(
  #   mapping = aes( x = yday, y = z_low_chl),
  #   color = 'black'
  # )+
  facet_wrap(
    ~site+year,
    ncol = 2
  )+
  scale_y_reverse()
ggplotly(x)
