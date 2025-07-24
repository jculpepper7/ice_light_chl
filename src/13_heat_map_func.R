heat_map <- function(
    df, 
    xvar, 
    yvar, 
    map_var, #This is the color variable for the heat map e.g., 'temp_c'
    lake, #this is for the site id e.g., 'paint.shallow'
    facet #this should just be 'year'
  ) {
  
  ggplot() +
    geom_tile(
      df, 
      mapping = aes({{xvar}}, {{yvar}}, fill = {{map_var}})
    ) +
    scale_y_reverse() +
    scale_fill_wa_c(
      palette = 'lopez',
      reverse = T
    )+
    coord_cartesian(expand = FALSE)+
    xlab('')+
    ylab('')+
    geom_vline(
      rbr_df %>% filter(site == {{lake}}),
      mapping = aes(xintercept = {{xvar}}),
      linewidth = 2,
      #linetype = 'dotted',
      color = 'black',
      alpha = 0.2
    )+
    facet_wrap(
      ~year, 
      #scales = 'free', 
      ncol = 2
    )+
    theme_classic()+
    theme(
      legend.position = 'bottom'
    )
}



heat_map(
    df = pld_DO, 
    xvar = yday, 
    yvar = depth, 
    map_var = do_umol, 
    lake = 'paint.deep', 
    #facet = year
)


#save plot
ggsave(
  here(
    'output/data_viz/heat_maps/horz_orientation/pld_do_horz.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)
