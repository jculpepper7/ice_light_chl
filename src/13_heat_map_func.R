
# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(cmocean)
#library(cowplot)
library(patchwork)
library(scales)
library(egg)

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
    high,
    leg.title,
    lake_name
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
      name = {{leg.title}},
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
      #NOTE: For strip.text commands below
      #Comment out lines 139-142 for PLD & PLS
      strip.text = element_text(
        size = 15,
        margin = margin (b = 10)
      ),
      #Comment out line 144 for KB
      # strip.text = element_blank(),
      strip.background = element_blank(),
      legend.title = element_text(
        angle = -90,
        margin = margin(l = 10),
        hjust = 0.5
      ),
      legend.direction = 'vertical'
    )+
    guides(
      fill = guide_colorbar(
        ticks.colour = NA,
        title.position = 'right'
      )
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
    yaxis = 'Depth (m)',
    # map_col = 'balance',
    low = min(rbr$temp),
    mid = 4,
    high = max(rbr$temp),
    leg.title = 'Temperature \u00b0C',
    lake_name = 'Paint Lake - Shallow'
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
  yaxis = 'Depth (m)',
  # map_col = 'thermal',
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL),
  leg.title = expression('Dissolved Oxygen  mg L'^-1)
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
  yaxis = 'Depth (m)',
  map_col = 'thermal',
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a),
  leg.title = expression('Chlorophyll-a \u03bcg L'^-1)
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
  yaxis = 'Depth (m)',
  low = min(rbr$temp),
  mid = 4,
  high = max(rbr$temp),
  leg.title = 'Temperature \u00b0C'
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
  yaxis = 'Depth (m)',
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL),
  leg.title = expression('Dissolved Oxygen  mg L'^-1)
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
  yaxis = 'Depth (m)',
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a),
  leg.title = expression('Chlorophyll-a \u03bcg L'^-1)
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
  yaxis = 'Depth (m)',
  low = min(rbr$temp),
  mid = 4,
  high = max(rbr$temp),
  leg.title = 'Temperature \u00b0C'
)
kb_temp
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
  yaxis = 'Depth (m)',
  low = min(rbr$do_mgL),
  mid = 8,
  high = max(rbr$do_mgL),
  leg.title = expression('Dissolved Oxygen  mg L'^-1)
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
  yaxis = 'Depth (m)',
  low = min(rbr$chl_a),
  mid = 2,
  high = max(rbr$chl_a),
  leg.title = expression('Chlorophyll-a \u03bcg L'^-1)
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

# **8d. TEMP --------------------------------------------------------------

kb_temp/pld_temp/pls_temp + 
  plot_annotation(
    tag_levels = 'a'
  )+
  plot_layout(
    axes = "collect",
    guides = 'collect'
  ) &
  theme(
    legend.justification = 'left'
  )

# save plot
ggsave(
  here(
    'output/data_viz/heat_maps/combined_plots/plot_by_var/fig4_temp.png'
  ),
  dpi = 300,
  height = 8,
  width = 7,
  units = 'in'
)

# **8e. DO ----------------------------------------------------------------

kb_do/pld_do/pls_do + 
  plot_annotation(
    tag_levels = 'a'
  )+
  plot_layout(
    axes = "collect",
    guides = 'collect'
  ) &
  theme(
    legend.justification = 'left'
  )

# save plot
ggsave(
  here(
    'output/data_viz/heat_maps/combined_plots/plot_by_var/fig5_do.png'
  ),
  dpi = 300,
  height = 8,
  width = 7,
  units = 'in'
)

# **8f. Chl-a -------------------------------------------------------------

kb_chla/pld_chla/pls_chla +
  plot_annotation(
    tag_levels = 'a'
  )+
  plot_layout(
    axes = "collect",
    guides = 'collect'
  ) &
  theme(
    legend.justification = 'left'
  )

# save plot
ggsave(
  here(
    'output/data_viz/heat_maps/combined_plots/plot_by_var/fig6_chla.png'
  ),
  dpi = 300,
  height = 8,
  width = 7,
  units = 'in'
)


# 9. Calculate daily temp, DO, can chl-a averages -------------------------


# **9a. Temp --------------------------------------------------------------


rbr_temp <- rbr %>% 
  mutate(
    date2 = as.factor(date)
  ) %>% 
  group_by(
    date2,
    site,
    year
  ) %>% 
  summarise(
    mean_temp = mean(temp, na.rm = T),
    min_temp = min(temp, na.rm = T),
    max_temp = max(temp, na.rm = T),
    # temp_z_low = depth[temp == min(temp)],
    # temp_z_high = depth[temp == max(temp)]
  ) %>% 
  arrange(
    site, year
  ) %>% 
  mutate(
    temp_diff = max_temp - min_temp,
    date = ymd(date2),
    yday = yday(date)
  )

rbr_temp_plt_df <- rbr_temp %>% 
  pivot_longer(
    cols = mean_temp:temp_diff,
    names_to = 'temp_var',
    values_to = 'temp_val'
  )
  
rbr_temp_ts_plt <- ggplot(data = rbr_temp_plt_df)+
  geom_line(
    mapping = aes(x = yday, y = temp_val, color = temp_var),
    size = 1.5,
    alpha = 0.6
  )+
  facet_wrap(
    ~site+year,
    ncol = 2,
    #scales = 'free'
  )+
  scale_color_viridis_d(
    labels = c(
      'Maximum',
      'Mean',
      'Minimum',
      'Difference'
    ),
    name = 'Temperature\nVariable'
  )+
  theme_bw()+
  ylab('Temperature \u00b0C')+
  xlab('Day of Year')+
  theme(
    text = element_text(size = 15),
    legend.box.margin = margin(r = 20)
  )

ggplotly(rbr_temp_ts_plt)

tag_facet(rbr_temp_ts_plt)+
  theme(
    text = element_text(size = 15),
    # legend.box.margin = margin(l = 25),
    strip.text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/heat_maps/combined_plots/plot_by_var/supp_to_fig4.png'),
  dpi = 300,
  width = 6.5,
  height = 8,
  units = 'in'
)

# **9b. Temp. summary table -----------------------------------------------

temp_sum <- rbr_temp %>% 
  group_by(site, year) %>% 
  summarise(
    mean = mean(mean_temp),
    mean_max = mean(max_temp),
    mean_min = mean(min_temp),
    mean_diff = mean(temp_diff)
  )

# write_csv(
#   temp_sum, 
#   here('output/data_viz/heat_maps/combined_plots/plot_by_var/temp_sum.csv')
# )
  
# **9c. DO --------------------------------------------------------------


rbr_DO <- rbr %>% 
  mutate(
    date2 = as.factor(date)
  ) %>% 
  group_by(
    date2,
    site,
    year
  ) %>% 
  summarise(
    mean_DO = mean(do_mgL, na.rm = T),
    min_DO = min(do_mgL, na.rm = T),
    max_DO = max(do_mgL, na.rm = T)
  ) %>% 
  arrange(
    site, year
  ) %>% 
  mutate(
    O2_diff = max_DO - min_DO,
    date = ymd(date2),
    yday = yday(date)
  )

rbr_do_plt_df <- rbr_DO %>% 
  pivot_longer(
    cols = mean_DO:O2_diff,
    names_to = 'DO_var',
    values_to = 'DO_val'
  )

rbr_DO_ts_plt <- ggplot(data = rbr_do_plt_df)+
  geom_line(
    mapping = aes(x = yday, y = DO_val, color = DO_var),
    size = 1.5,
    alpha = 0.6
  )+
  facet_wrap(
    ~site+year,
    ncol = 2,
    #scales = 'free'
  )+
  scale_color_viridis_d(
    labels = c(
      'Maximum',
      'Mean',
      'Minimum',
      'Difference'
    ),
    name = 'Dissovled\nOxygen'
  )+
  theme_bw()+
  ylab(expression(paste("Dissolved Oxygen (mg ", L^-1, ")")))+
  xlab('Day of Year')+
  theme(
    text = element_text(size = 15),
    legend.box.margin = margin(r = 20)
  )

ggplotly(rbr_DO_ts_plt)

tag_facet(rbr_DO_ts_plt)+
  theme(
    text = element_text(size = 15),
    # legend.box.margin = margin(l = 25),
    strip.text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/heat_maps/combined_plots/plot_by_var/supp_to_fig5.png'),
  dpi = 300,
  width = 6.5,
  height = 8,
  units = 'in'
)

# **9b. DO summary table -----------------------------------------------

DO_sum <- rbr_DO %>% 
  group_by(site, year) %>% 
  summarise(
    mean = mean(mean_DO),
    mean_max = mean(max_DO),
    mean_min = mean(min_DO),
    mean_diff = mean(O2_diff)
  )

write_csv(
  DO_sum,
  here('output/data_viz/heat_maps/combined_plots/plot_by_var/DO_sum.csv')
)

# **9d. Chl-a --------------------------------------------------------------


rbr_chla <- rbr %>% 
  mutate(
    date2 = as.factor(date)
  ) %>% 
  group_by(
    date2,
    site,
    year
  ) %>% 
  summarise(
    mean_chla = mean(chl_a, na.rm = T),
    min_chla = min(chl_a, na.rm = T),
    max_chla = max(chl_a, na.rm = T)
  ) %>% 
  arrange(
    site, year
  ) %>% 
  mutate(
    zchla_diff = max_chla - min_chla,
    date = ymd(date2),
    yday = yday(date)
  )

rbr_chla_plt_df <- rbr_chla %>% 
  pivot_longer(
    cols = mean_chla:zchla_diff,
    names_to = 'chla_var',
    values_to = 'chla_val'
  )

rbr_chla_ts_plt <- ggplot(data = rbr_chla_plt_df)+
  geom_line(
    mapping = aes(x = yday, y = chla_val, color = chla_var),
    size = 1.5,
    alpha = 0.6
  )+
  facet_wrap(
    ~site+year,
    ncol = 2,
    #scales = 'free'
  )+
  scale_color_viridis_d(
    labels = c(
      'Maximum',
      'Mean',
      'Minimum',
      'Difference'
    ),
    name = 'Chlorophyll-a'
  )+
  theme_bw()+
  ylab(expression(paste("Chlorophyll-a (", mu, 'g', L^-1, ")")))+
  xlab('Day of Year')+
  theme(
    text = element_text(size = 15),
    legend.box.margin = margin(r = 20)
  )

ggplotly(rbr_chla_ts_plt)

tag_facet(rbr_chla_ts_plt)+
  theme(
    text = element_text(size = 15),
    # legend.box.margin = margin(l = 25),
    strip.text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/heat_maps/combined_plots/plot_by_var/supp_to_fig6.png'),
  dpi = 300,
  width = 6.5,
  height = 8,
  units = 'in'
)

# **9b. DO summary table -----------------------------------------------

chla_sum <- rbr_chla %>% 
  group_by(site, year) %>% 
  summarise(
    mean = mean(mean_chla),
    mean_max = mean(max_chla),
    mean_min = mean(min_chla),
    mean_diff = mean(zchla_diff)
  )

write_csv(
  chla_sum,
  here('output/data_viz/heat_maps/combined_plots/plot_by_var/chla_sum.csv')
)
