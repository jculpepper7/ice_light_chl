ggplot() +
  geom_tile(
    pls_temp, 
    mapping = aes(yday, depth, fill = temp)
  ) +
  scale_y_reverse() +
  scale_fill_wa_c(
    palette = 'lopez',
    reverse = T
  )+
  coord_cartesian(expand = FALSE)+
  
  geom_vline(
    rbr_df %>% filter(site == 'paint.shallow'),
    mapping = aes(xintercept = yday),
    linewidth = 2,
    linetype = 'dotted',
    color = 'black',
    alpha = 0.2
  )+
  facet_wrap(
    ~year, 
    #scales = 'free', 
    ncol = 1
  )+
  theme_classic()