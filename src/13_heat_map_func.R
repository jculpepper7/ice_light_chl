
# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(cmocean)
#library(cowplot)
library(patchwork)

# 2. Import data ----------------------------------------------------------

rbr <- read_csv(here('data/combined_data/interp_df.csv')) %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    yday_fac = as.factor(yday),
    do_mgL = (do_umol/1000)*31.998
  ) %>% 
  select(
    -do_umol
  )

#Add threshold depths for temp @ 4C, DO @ 8mg/L & 2mg/L, & chl-a @ 2 ug/L

rbr <- rbr %>% 
  na.omit() %>%
  mutate(
    t_diff = abs(temp - 4),         #Temp of max density (approx.)
    do_diff_high = abs(do_mgL - 8), #Stress threshold of some fish sp.
    do_diff_low = abs(do_mgL - 2),  #General hypoxia threshold
    chl_diff = abs(chl_a - 2)       #Oligotrophic threshold (approx.)
  ) %>% 
  group_by(
    site, yday_fac, year
  ) %>% 
  mutate(
    z_temp4 = depth[t_diff == min(t_diff)],
    z_do_high = depth[do_diff_high == min(do_diff_high)],
    z_do_low = depth[do_diff_low == min(do_diff_low)],
    z_chla = depth[chl_diff == min(chl_diff)]
  ) 


# 3. Heat map function ----------------------------------------------------
library(scales)
heat_map <- function(
    df, 
    xvar, 
    yvar, 
    # yvar_line,
    map_var,    #This var is the color variable for the heat map (e.g., 'temp_c')
    lake,       #This var is for the site id (e.g., 'paint.shallow')
    facet,      #This var should just be 'year'
    col_dir,
    yaxis,
    map_col,     #This var is a map color from the 'cmocean' pckg
    low,
    mid,          #The midpoint for the red to blue color scheme
    high
) {
  
  ggplot() +
    geom_tile(
      df %>% 
        filter(
          site == {{lake}}
        ), 
      mapping = aes({{xvar}}, {{yvar}}, fill = {{map_var}})
    ) +
    # geom_line(
    #   df %>% 
    #     filter(
    #       site == {{lake}}
    #     ), 
    #   mapping = aes({{xvar}}, {{yvar_line}}),
    #   color = 'black',
    #   linewidth = 1.5,
    #   alpha = 0.7
    # ) +
    scale_y_reverse() +
    
    # scale_fill_cmocean(name = {{map_col}}, direction = col_dir)+
    
    # scale_fill_gradient2(
    #   low = cmocean("balance")(100)[15],
    #   #mid = cmocean("balance")(100)[50],
    #   high = cmocean("balance")(100)[85],
    #   midpoint = {{mid}},
    # ) +
    
    scale_fill_gradientn(
      colors = c(
        #Cols for temp & chl-a
        cmocean("balance")(100)[15],
        cmocean("balance")(100)[50],
        cmocean("balance")(100)[85]
        #Cols for DO
        # cmocean("balance")(100)[85],
        # cmocean("balance")(100)[50],
        # cmocean("balance")(100)[15]
      ),
      values = rescale(
        c(
          {{low}},
          {{mid}},
          {{high}}
        )
      ),
      limits = c(
        {{low}},
        {{high}}
      )
    )+
    
    # scale_fill_gradientn(
    #   colours = c("darkblue","white","darkred"),
    #   values = 
    #     c(4, (4-0.4786835)/(5.926296-0.4786835), 5.93)
    #   ,
    #   guide = "colorbar",
    #   limits=c(0.46,5.93)
    # )+
    
    coord_cartesian(expand = FALSE)+
    xlab('Day of Year')+
    ylab({{yaxis}})+
    xlim(c(10, 90))+
    facet_wrap(
      ~year, 
      ncol = 2
    )+
    theme_classic()+
    theme(
      legend.position = 'right',
      legend.title = element_blank(),
      strip.text = element_blank()
    )+
    guides(
      fill = guide_colorbar(ticks.colour = NA)
    )
}

# 4. PLS heat maps --------------------------------------------------------


# **4a. PLS - Temperature -------------------------------------------------

pls_temp <- heat_map(
    df = rbr, 
    xvar = yday, 
    yvar = depth,
    # yvar_line = z_temp4,
    map_var = temp, 
    lake = 'Paint Lake - Shallow',
    col_dir = 1,
    yaxis = 'Temperature \u00b0C',
    # map_col = 'balance',
    low = min(rbr$temp),
    mid = 4,
    high = max(rbr$temp)
)
pls_temp
  
  
# save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pls_temp_horz_test2.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **4b. PLS - DO ----------------------------------------------------------

pls_do <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth,
  # yvar_line = z_do_high,
  map_var = do_mgL, 
  lake = 'Paint Lake - Shallow',
  col_dir = -1,
  yaxis = expression('Dissolved Oxygen  mg L'^-1),
  # map_col = 'thermal',
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pls_do_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **4c. PLS - Chl-a -------------------------------------------------------

pls_chla <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth,
  # yvar_line = z_chla,
  map_var = chl_a, 
  lake = 'Paint Lake - Shallow',
  col_dir = 1,
  yaxis = expression('Chlorophyll-a mg L'^-1),
  map_col = 'thermal',
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pls_chla_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )


# 5. PLD heat maps --------------------------------------------------------


# **5a. PLD - Temperature -------------------------------------------------

pld_temp <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = temp, 
  lake = 'Paint Lake - Deep',
  col_dir = 1,
  yaxis = 'Temperature \u00b0C',
  low = min(rbr$temp),
  mid = 4,
  high = max(rbr$temp)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pld_temp_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **5b. PLD - DO ----------------------------------------------------------

pld_do <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = do_mgL, 
  lake = 'Paint Lake - Deep',
  col_dir = -1,
  yaxis = expression('Dissolved Oxygen  mg L'^-1),
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pld_do_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **5c. PLD - Chl-a -------------------------------------------------------

pld_chla <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = chl_a, 
  lake = 'Paint Lake - Deep',
  col_dir = 1,
  yaxis = expression('Chlorophyll-a mg L'^-1),
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/pld_chla_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# 6. KB heat maps ---------------------------------------------------------


# **6a. KB -Temperature ---------------------------------------------------

kb_temp <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = temp, 
  lake = 'Kempenfelt Bay',
  col_dir = 1,
  yaxis = 'Temperature (\u00b0C)',
  low = min(rbr$temp),
  mid = 4,
  high = max(rbr$temp)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/kb_temp_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **6b. KB - DO -----------------------------------------------------------

kb_do <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = do_mgL, 
  lake = 'Kempenfelt Bay',
  col_dir = -1,
  yaxis = expression('Dissolved Oxygen  mg L'^-1),
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/kb_do_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# **6c. KB - Chl-a --------------------------------------------------------

kb_chla <- heat_map(
  df = rbr, 
  xvar = yday, 
  yvar = depth, 
  map_var = chl_a, 
  lake = 'Kempenfelt Bay',
  col_dir = 1,
  yaxis = expression('Chlorophyll-a mg L'^-1),
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a)
)

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/horz_orientation/kb_chla_horz.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )


# 7. Ice quality plot -----------------------------------------------------

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
    snow_avg_cm,
    iw_int,
    air
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
    yday = yday(date),
    perc_trans = (iw_int/air)*100
  )

# **7a. PLS -----------------------------------------------------------------

pls_ice <- ggplot(
  bar_plt_df %>% 
    filter(
      site == 'paint.shallow'
    )
)+
  geom_bar(
    aes(
      x = yday, 
      y = thickness_cm, 
      fill = factor(
        ice_qual, 
        levels = c('snow_avg_cm', 'wht_slush_cm', 'black_ice_cm')
      )
    ),
    stat = 'identity', 
    position = 'stack', 
    color = 'black'
  )+
  geom_line(
    aes(
      x = yday,
      #y = iw_int/4
      y = perc_trans/1.6667
    ),
    color = 'goldenrod',
    linewidth = 2,
    alpha = 0.7
  )+
  scale_y_continuous(
    name = 'Ice Thickness cm',
    sec.axis = sec_axis(
      ~.*1.6667/100, 
      # name=expression(mu * E * m^{-2} * s^{-1}),
      name = 'PAR Transmitted %',
      labels = scales::percent,
      breaks = seq(0,1,by = 0.2),
    )
  )+
  labs(fill = 'Ice Type')+
  xlab('')+
  #ylab('Ice Thickness cm')+
  xlim(c(10,90))+
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

# **7b. PLD --------------------------------------------------------------

pld_ice <- ggplot(
  bar_plt_df %>% 
    filter(
      site == 'paint.deep'
    )
)+
  geom_bar(
    aes(
      x = yday, 
      y = thickness_cm, 
      fill = factor(
        ice_qual, 
        levels = c('snow_avg_cm', 'wht_slush_cm', 'black_ice_cm')
      )
    ),
    stat = 'identity', 
    position = 'stack', 
    color = 'black'
  )+
  geom_line(
    aes(
      x = yday,
      #y = iw_int/2
      y = perc_trans/1.6667
    ),
    color = 'goldenrod',
    linewidth = 2,
    alpha = 0.7
  )+
  scale_y_continuous(
    name = 'Ice Thickness cm',
    #sec.axis = sec_axis(~.*2, name=expression(mu * E * m^{-2} * s^{-1}))
    sec.axis = sec_axis(
      ~.*1.6667/100, 
      #name=expression(mu * E * m^{-2} * s^{-1}),
      name = 'PAR Transmitted %',
      labels = scales::percent,
      breaks = seq(0,1,by = 0.2),
    )
    
  )+
  labs(fill = 'Ice Type')+
  xlab('')+
  #ylab('Ice Thickness cm')+
  xlim(c(10,90))+
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

# **7c. KB ----------------------------------------------------------------

kb_ice <- ggplot(
  bar_plt_df %>% 
    filter(
      site == 'simcoe.deep'
    )
)+
  geom_bar(
    aes(
      x = yday, 
      y = thickness_cm, 
      fill = factor(
        ice_qual, 
        levels = c('snow_avg_cm', 'wht_slush_cm', 'black_ice_cm')
      )
    ),
    stat = 'identity', 
    position = 'stack', 
    color = 'black'
  )+
  geom_line(
    aes(
      x = yday,
      # y = iw_int/10
      y = perc_trans/1.6667
    ),
    color = 'goldenrod',
    linewidth = 2,
    alpha = 0.7
  )+
  scale_y_continuous(
    name = 'Ice Thickness cm',
    # sec.axis = sec_axis(~.*10, name=expression(mu * E * m^{-2} * s^{-1})),
    sec.axis = sec_axis(
      ~.*1.6667/100, 
      #name=expression(mu * E * m^{-2} * s^{-1}),
      name = 'PAR Transmitted %',
      breaks = seq(0,1,by = 0.2),
      labels = scales::percent
    )
  )+
  labs(fill = 'Ice Type')+
  xlab('')+
  #ylab('Ice Thickness cm')+
  xlim(c(10,90))+
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

# 8. Combine plots --------------------------------------------------------


# **8a. PLS ---------------------------------------------------------------

pls_ice/pls_temp/pls_do/pls_chla + 
  plot_layout(
    axes = "collect"
  ) &
  theme(
    legend.justification = 'left'
  )

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/combined_plots/fig6.png'
#   ),
#   dpi = 300,
#   height = 8,
#   width = 7,
#   units = 'in'
# )

# **8b. PLD ---------------------------------------------------------------

pld_ice/pld_temp/pld_do/pld_chla + 
  plot_layout(
    axes = "collect"
  ) &
  theme(
    legend.justification = 'left'
  )

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/combined_plots/fig5.png'
#   ),
#   dpi = 300,
#   height = 8,
#   width = 7,
#   units = 'in'
# )

# **8c. KB ----------------------------------------------------------------

kb_ice/kb_temp/kb_do/kb_chla + 
  plot_layout(
    axes = "collect"
  ) &
  theme(
    legend.justification = 'left'
  )

# save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/combined_plots/fig4.png'
#   ),
#   dpi = 300,
#   height = 8,
#   width = 7,
#   units = 'in'
# )
