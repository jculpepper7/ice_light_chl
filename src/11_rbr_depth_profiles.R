library(tidyverse)
library(here)
library(wacolors)

rbr <- read_csv(here::here('data/rbr/rbr_clean2.csv')) %>% 
  mutate(
    site = as.factor(site),
    position = as.factor(position),
    date2 = as.factor(date),
    year = as.factor(year),
    site_proper = as.factor(
      if_else(
        site == 'paint.deep', 'Paint - Deep', 
        if_else(
          site == 'paint.shallow', 'Paint - Shallow',
          'Kempenfelt Bay'
        )
      )
    )
  ) 



depth_intervals <- function(df, int, vars){
  df |>
    # Round depth up to nearest 0.1 m in a new column
    mutate(d_int =  plyr::round_any(depth, int)) |>
    # Calculate mean conductivity and sd for each depth interval
    dplyr::group_by(site, date, d_int) |>
    dplyr::summarize(
      dplyr::across(vars, mean)
    )
}

rbr1_int <- depth_intervals(rbr, 
                            int = 0.1, 
                            vars = c(
                              "temp_c", 
                              "chl_a", 
                              "do_mg_l", 
                              "do_sat", 
                              "par",
                              "pressure"
                            ))

rbr_df <- rbr1_int %>% 
  mutate(
    date2 = as.factor(date),
    year = as.factor(year(date)),
    yday = yday(date)
  ) %>% 
  filter(
    date2 != '2024-01-27'
  ) %>% 
  group_by(site, date, year) #%>% 
  # mutate(
  #   day_no = min_rank(date)
  # )


# Import ice quality data -------------------------------------------------

par_sum_wide <- read_csv(here('data/combined_data/par_ice.csv')) %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year)
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

# Temp plts ---------------------------------------------------------------


# kb ----------------------------------------------------------------------


kd_temp_plt <- ggplot(
  data = rbr_df %>% filter(site == 'simcoe.deep'), 
  aes(
    x = d_int, 
    y = temp_c, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'simcoe.deep' = 'Kempenfelt Bay'
                          ))
  ) +
  ylab("Temperature (°C)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
  # scale_color_wa_d(palette = 'olympic')

kd_temp_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/temp_profile_kb.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )


# ps ----------------------------------------------------------------------

ps_temp_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.shallow'), 
  aes(
    x = d_int, 
    y = temp_c, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.shallow' = 'Paint - Shallow'
                          ))
  ) +
  ylab("Temperature (°C)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

ps_temp_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/temp_profile_ps.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )

# ps ----------------------------------------------------------------------

pd_temp_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.deep'), 
  aes(
    x = d_int, 
    y = temp_c, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.deep' = 'Paint - Deep'
                          ))
  ) +
  ylab("Temperature (°C)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

pd_temp_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/temp_profile_pd.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )

# DO plts ---------------------------------------------------------------


# kb ----------------------------------------------------------------------


kd_do_plt <- ggplot(
  data = rbr_df %>% filter(site == 'simcoe.deep'), 
  aes(
    x = d_int, 
    y = do_mg_l, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'simcoe.deep' = 'Kempenfelt Bay'
                          ))
  ) +
  ylab("DO (\u00b5mol/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

kd_do_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/do_profile_kb.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )


# ps ----------------------------------------------------------------------

ps_do_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.shallow'), 
  aes(
    x = d_int, 
    y = do_mg_l, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.shallow' = 'Paint - Shallow'
                          ))
  ) +
  ylab("DO (\u00b5mol/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

ps_do_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/do_profile_ps.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )

# ps ----------------------------------------------------------------------

pd_do_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.deep'), 
  aes(
    x = d_int, 
    y = do_mg_l, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.deep' = 'Paint - Deep'
                          ))
  ) +
  ylab("DO (\u00b5mol/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

pd_do_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/do_profile_pd.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )

# Chla plts ---------------------------------------------------------------


# kb ----------------------------------------------------------------------


kd_chla_plt <- ggplot(
  data = rbr_df %>% filter(site == 'simcoe.deep'), 
  aes(
    x = d_int, 
    y = chl_a, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'simcoe.deep' = 'Kempenfelt Bay'
                          ))
  ) +
  ylab("Chl-a (mg/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

kd_chla_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/chla_profile_kb.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )


# ps ----------------------------------------------------------------------

ps_chla_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.shallow'), 
  aes(
    x = d_int, 
    y = chl_a, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.shallow' = 'Paint - Shallow'
                          ))
  ) +
  ylab("Chl-a (mg/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

ps_chla_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/chla_profile_ps.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )

# ps ----------------------------------------------------------------------

pd_chla_plt <- ggplot(
  data = rbr_df %>% filter(site == 'paint.deep'), 
  aes(
    x = d_int, 
    y = chl_a, 
    colour = date2
  )
) +
  geom_line(
    alpha = 0.5,
    linewidth = 2
  ) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(
    . ~ site+year, 
    #scales = "free",
    ncol = 1,
    labeller = labeller(site = 
                          c(
                            'paint.deep' = 'Paint - Deep'
                          ))
  ) +
  ylab("Chl-a (mg/L)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = 'none'
  )+
  scale_color_viridis_d()
# scale_color_wa_d(palette = 'olympic')

pd_chla_plt

# ggsave(
#   here::here('output/data_viz/rbr_viz/chla_profile_pd.png'),
#   dpi = 300,
#   width = 5,
#   height = 8,
#   units = 'in'
# )


#Heat map code----


# **Point depth profile ---------------------------------------------------

ggplot(
  # temp_interp_pls,
  # rbr_df,
  # aes(x = date, y = depth, colour = temp)
  rbr_df %>% filter(site == 'paint.shallow'),
  #rbr_df %>% filter(site == 'paint.deep'),
  #rbr_df %>% filter(site == 'simcoe.deep'),
  aes(x = date, y = d_int, colour = temp_c)
) +
  geom_point() +
  scale_y_reverse() +
  scale_color_gradient2(
    midpoint = 3,
    high = scales::muted("red"),
    low = scales::muted("blue")
  )+
  geom_point(shape = 1, color = 'black')+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()


# **Linear interpolation --------------------------------------------------

#First, I need to break up the df into individual sites
#Probably I don't need to do this, but I can't figure out a better way at the moment

pl_s <- rbr_df %>% 
  filter(
    site == 'paint.shallow'
  )

pl_d <- rbr_df %>% 
  filter(
    site == 'paint.deep',
    date != '2025-01-28'
  )

kb <- rbr_df %>% 
  filter(
    site == 'simcoe.deep'
  )


# 1a. Paint Lake Shallow - Temperature ------------------------------------



# **temp ------------------------------------------------------------------


#Below interpolates temperature for one date at a time
estimate_temp_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)

  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$temp_c,
    xout = target_depth
  )$y
}

#Test
#estimate_temp_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Alternate function for all variables
# estimate_params_by_date <- function(target_date, target_depth) {
#   data_for_date <- rbr_df %>%
#     group_by(site) %>% 
#     filter(date == target_date) %>%
#     arrange(d_int)
#   
#   # Interpolate temperature
#   temp <- approx(data_for_date$d_int, data_for_date$temp_c, xout = target_depth)$y
#   
#   # Interpolate dissolved oxygen
#   do <- approx(data_for_date$d_int, data_for_date$do_mg_l, xout = target_depth)$y
#   
#   # Interpolate chlorophyll-a
#   chl_a <- approx(data_for_date$d_int, data_for_date$chl_a, xout = target_depth)$y
#   
#   # Return as named list or vector
#   list(
#     temperature = temp,
#     dissolved_oxygen = do,
#     chlorophyll_a = chl_a
#   )
# }
# 
# estimate_params_by_date(ymd("2024-01-26"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
temp_interp_pls <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_s$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1, 3.1, length.out = 200))
) %>%
  group_by(date) %>%
  mutate(
    temp = estimate_temp_by_date(pl_s, date[1], depth),
    site = as.factor('paint.shallow'),
    year = as.factor(year(date))
  )


# Define the function
# temp_interp_depth <- function(df, min_depth = 1, max_depth = 20, n_points = 100) {
# 
#   # Create the grid of dates and depths
#   temp_interp_depth <- crossing(
#     tibble(date = unique(df$date)),
#     tibble(depth = seq(min_depth, max_depth, length.out = n_points))
#   ) %>%
#     group_by(site, year, date) %>%
#     mutate(temp = estimate_temp_by_date(date[1], depth))
# 
#   return(temp_interp_depth)
# }

ggplot(
  temp_interp_pls,
  aes(x = date, y = depth, colour = temp)
) +
  geom_point() +
  scale_y_reverse() +
  # scale_colour_gradient2(
  #   midpoint = 3,
  #   high = scales::muted("red"), 
  #   low = scales::muted("blue")
  # )+
  scale_color_wa_c(palette = 'lopez')+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_temp_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth(
  temp_interp_pls,
  target_depth = 1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pls_int_24 <- temp_interp_pls %>% 
  filter(
    year == 2024
  )

min(pls_int_24$date) #"2024-01-26"
max(pls_int_24$date) #"2024-03-06"

pls_int_25 <- temp_interp_pls %>% 
  filter(
    year == 2025
  )

min(pls_int_25$date) #"2024-01-15"
max(pls_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
temp_raster_pls_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(pls_int_24, depth[1], date))

temp_raster_pls_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(pls_int_25, depth[1], date))

#Join temp data
pls_temp <- temp_raster_pls_24 %>% 
  bind_rows(temp_raster_pls_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Shallow'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Temp heat map --------------------------------------------------------------


ggplot() +
  geom_tile(
    pls_temp, 
    mapping = aes(yday, depth, fill = temp)
  ) +
  scale_y_reverse() +
  # scale_fill_gradient2(
  #   midpoint = 4,
  #   high = scales::muted("red"),
  #   low = scales::muted("blue")
  # ) +
  # scale_fill_viridis_c()+
  scale_fill_wa_c(
    palette = 'lopez',
    reverse = T
  )+
  coord_cartesian(expand = FALSE)+
  # geom_point(
  #   rbr_df %>% filter(site == 'paint.shallow'),
  #   #rbr_df %>% filter(site == 'paint.deep'),
  #   #rbr_df %>% filter(site == 'simcoe.deep'),
  #   mapping = aes(x = yday, y = d_int, colour = temp_c)
  # )+
  # scale_color_wa_c(
  #   palette = 'lopez',
  #   reverse = T
  # )+
  # geom_point(
  #   rbr_df %>% filter(site == 'paint.shallow'),
  #   #rbr_df %>% filter(site == 'paint.deep'),
  #   #rbr_df %>% filter(site == 'simcoe.deep'),
  #   mapping = aes(x = yday, y = d_int),
  #   shape = 1,
  #   color = 'black'
  # )+
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

#save plot
# ggsave(
#   here(
#     'output/data_viz/heat_maps/pls_temp_vir.png'
#   ),
#   dpi = 300,
#   height = 3,
#   width = 5,
#   units = 'in'
# )

# 1b. Paint Lake Shallow - Dissolved Oxygen ------------------------------------


# **DO ------------------------------------------------------------------


#Below interpolates DO for one date at a time
estimate_DO_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$do_mg_l,
    xout = target_depth
  )$y
}

#Test
estimate_DO_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
DO_interp_pls <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_s$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1, 3, length.out = 200))
) %>%
  group_by(date) %>%
  mutate(
    do_umol = estimate_DO_by_date(pl_s, date[1], depth),
    site = as.factor('paint.shallow'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  DO_interp_pls,
  aes(x = date, y = depth, colour = do_umol)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =225, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_DO_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$do_umol, xout = target_date)$y
}

estimate_DO_by_depth(
  DO_interp_pls,
  target_depth = 1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pls_int_24 <- DO_interp_pls %>% 
  filter(
    year == 2024
  )

min(pls_int_24$date) #"2024-01-26"
max(pls_int_24$date) #"2024-03-06"

pls_int_25 <- DO_interp_pls %>% 
  filter(
    year == 2025
  )

min(pls_int_25$date) #"2024-01-15"
max(pls_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
DO_raster_pls_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(pls_int_24, depth[1], date))

DO_raster_pls_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(pls_int_25, depth[1], date))

#Join DO data
pls_DO <- DO_raster_pls_24 %>% 
  bind_rows(DO_raster_pls_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Shallow'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **DO heat map --------------------------------------------------------------


ggplot(pls_DO, aes(yday, depth, fill = do_umol)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 225,
    high = scales::muted("blue"),
    low = scales::muted("red")
  ) +
  # scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    #scales = 'free',
    ncol = 1
  )+
  theme_classic()

ggsave(
  here(
    'output/data_viz/heat_maps/pls_do_rb.png'
    #'output/data_viz/heat_maps/pls_do_vir.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)

# 1c. Paint Lake Shallow - Chlorophyll-a ------------------------------------


# **Chl-a ------------------------------------------------------------------


#Below interpolates DO for one date at a time
estimate_chl_a_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$chl_a,
    xout = target_depth
  )$y
}

#Test
estimate_chl_a_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
chl_a_interp_pls <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_s$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1, 3, length.out = 200))
) %>%
  group_by(date) %>%
  mutate(
    chl_a = estimate_chl_a_by_date(pl_s, date[1], depth),
    site = as.factor('paint.shallow'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  chl_a_interp_pls,
  aes(x = date, y = depth, colour = chl_a)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =2, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_chl_a_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$chl_a, xout = target_date)$y
}

estimate_chl_a_by_depth(
  chl_a_interp_pls,
  target_depth = 1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pls_int_24 <- chl_a_interp_pls %>% 
  filter(
    year == 2024
  )

min(pls_int_24$date) #"2024-01-26"
max(pls_int_24$date) #"2024-03-06"

pls_int_25 <- chl_a_interp_pls %>% 
  filter(
    year == 2025
  )

min(pls_int_25$date) #"2024-01-15"
max(pls_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
chl_a_raster_pls_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(pls_int_24, depth[1], date))

chl_a_raster_pls_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pls_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(pls_int_25, depth[1], date))

#Join chl-a data
pls_chl_a <- chl_a_raster_pls_24 %>% 
  bind_rows(chl_a_raster_pls_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Shallow'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Chl-a heat map --------------------------------------------------------------


ggplot(pls_chl_a, aes(yday, depth, fill = chl_a)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 2,
    high = scales::muted("blue"),
    low = scales::muted("red")
  ) +
  # scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    #scales = 'free',
    ncol = 1
  )+
  theme_classic()

ggsave(
  here(
    'output/data_viz/heat_maps/pls_chl_rb.png'
    #'output/data_viz/heat_maps/pls_chl_vir.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)


# 2a. Paint Lake Deep - Temperature ------------------------------------



# **temp ------------------------------------------------------------------


#Below interpolates temperature for one date at a time
estimate_temp_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$temp_c,
    xout = target_depth
  )$y
}

#Test
#estimate_temp_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
temp_interp_pld <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_d$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1.1, 13.3, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    temp = estimate_temp_by_date(pl_d, date[1], depth),
    site = as.factor('paint.deep'),
    year = as.factor(year(date))
  )


ggplot(
  temp_interp_pld,
  aes(x = date, y = depth, colour = temp)
) +
  geom_point() +
  scale_y_reverse() +
  # scale_colour_gradient2(
  #   midpoint = 2, 
  #   high = scales::muted("red"), 
  #   low = scales::muted("blue")
  # )+
  scale_colour_viridis_c()+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_temp_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth(
  temp_interp_pld,
  target_depth = 1.1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pld_int_24 <- temp_interp_pld %>% 
  filter(
    year == 2024
  )

min(pld_int_24$date) #"2024-01-26"
max(pld_int_24$date) #"2024-03-06"

pld_int_25 <- temp_interp_pld %>% 
  filter(
    year == 2025
  )

min(pld_int_25$date) #"2024-01-15"
max(pld_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
temp_raster_pld_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(pld_int_24, depth[1], date))

temp_raster_pld_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(pld_int_25, depth[1], date))

#Join temp data
pld_temp <- temp_raster_pld_24 %>% 
  bind_rows(temp_raster_pld_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Deep'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Temp heat map --------------------------------------------------------------


ggplot(pld_temp, aes(yday, depth, fill = temp)) +
  geom_tile() +
  scale_y_reverse() +
  # scale_fill_gradient2(
  #   midpoint = 2.2,
  #   high = scales::muted("red"),
  #   low = scales::muted("blue")
  # ) +
  scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    # scales = 'free',
    ncol = 1
  )+
  theme_classic()

#' ggsave(
#'   here(
#'     #'output/data_viz/heat_maps/pld_temp_rb.png'
#'     'output/data_viz/heat_maps/pld_temp_vir.png'
#'   ),
#'   dpi = 300,
#'   height = 3,
#'   width = 5,
#'   units = 'in'
#' )

# 2b. Paint Lake Deep - Dissolved Oxygen ------------------------------------


# **DO ------------------------------------------------------------------


#Below interpolates DO for one date at a time
estimate_DO_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$do_mg_l,
    xout = target_depth
  )$y
}

#Test
#estimate_DO_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
DO_interp_pld <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_d$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1.1, 13.3, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    do_umol = estimate_DO_by_date(pl_d, date[1], depth),
    site = as.factor('paint.deep'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  DO_interp_pld,
  aes(x = date, y = depth, colour = do_umol)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =250, 
    high = scales::muted("blue"), 
    low = scales::muted("red")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_DO_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$do_umol, xout = target_date)$y
}

estimate_DO_by_depth(
  DO_interp_pld,
  target_depth = 1.1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pld_int_24 <- DO_interp_pld %>% 
  filter(
    year == 2024
  )

min(pld_int_24$date) #"2024-01-26"
max(pld_int_24$date) #"2024-03-06"

pld_int_25 <- DO_interp_pld %>% 
  filter(
    year == 2025
  )

min(pld_int_25$date) #"2024-01-15"
max(pld_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
DO_raster_pld_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(pld_int_24, depth[1], date))

DO_raster_pld_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(pld_int_25, depth[1], date))

#Join temp data
pld_DO <- DO_raster_pld_24 %>% 
  bind_rows(DO_raster_pld_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Deep'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **DO heat map --------------------------------------------------------------


ggplot(pld_DO, aes(yday, depth, fill = do_umol)) +
  geom_tile() +
  scale_y_reverse() +
  # scale_fill_gradient2(
  #   midpoint = 250,
  #   high = scales::muted("blue"),
  #   low = scales::muted("red")
  # ) +
  scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    ncol = 1,
    #scales = 'free'
  )+
  theme_classic()

#' ggsave(
#'   here(
#'     #'output/data_viz/heat_maps/pld_do_rb.png'
#'     'output/data_viz/heat_maps/pld_do_vir.png'
#'   ),
#'   dpi = 300,
#'   height = 3,
#'   width = 5,
#'   units = 'in'
#' )

# 2c. Paint Lake Deep - Chlorophyll-a ------------------------------------


# **Chl-a ------------------------------------------------------------------

#Below interpolates DO for one date at a time
estimate_chl_a_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$chl_a,
    xout = target_depth
  )$y
}

#Test
#estimate_chl_a_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
chl_a_interp_pld <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(pl_d$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(1.1, 13.3, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    chl_a = estimate_chl_a_by_date(pl_d, date[1], depth),
    site = as.factor('paint.shallow'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  chl_a_interp_pld,
  aes(x = date, y = depth, colour = chl_a)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =2.25, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_chl_a_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$chl_a, xout = target_date)$y
}

estimate_chl_a_by_depth(
  chl_a_interp_pld,
  target_depth = 1.1, 
  target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
)

#split data into years
pld_int_24 <- chl_a_interp_pld %>% 
  filter(
    year == 2024
  )

min(pld_int_24$date) #"2024-01-26"
max(pld_int_24$date) #"2024-03-06"

pld_int_25 <- chl_a_interp_pld %>% 
  filter(
    year == 2025
  )

min(pld_int_25$date) #"2024-01-15"
max(pld_int_25$date) #"2024-03-25"


#Interpolate across dates by depth
chl_a_raster_pld_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-01-26"), ymd("2024-03-06"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(pld_int_24, depth[1], date))

chl_a_raster_pld_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-15"), ymd("2025-03-25"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pld_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(pld_int_25, depth[1], date))

#Join temp data
pld_chl_a <- chl_a_raster_pld_24 %>% 
  bind_rows(chl_a_raster_pld_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Deep'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Chlorophyll-a heat map --------------------------------------------------------------


ggplot(pld_chl_a, aes(yday, depth, fill = chl_a)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 2.25,
    high = scales::muted("blue"),
    low = scales::muted("red")
  ) +
  # scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    ncol = 1,
    # scales = 'free'
  )+
  theme_classic()

#' ggsave(
#'   here(
#'     'output/data_viz/heat_maps/pld_chl_rb.png'
#'     #'output/data_viz/heat_maps/pld_chl_vir.png'
#'   ),
#'   dpi = 300,
#'   height = 3,
#'   width = 5,
#'   units = 'in'
#' )

# 3a. Kempenfelt Bay - Temperature ------------------------------------



# **temp ------------------------------------------------------------------


#Below interpolates temperature for one date at a time
estimate_temp_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$temp_c,
    xout = target_depth
  )$y
}

#Test
#estimate_temp_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
temp_interp_kb <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(kb$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(0.85, 31.2, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    temp = estimate_temp_by_date(kb, date[1], depth),
    site = as.factor('kempenfelt.bay'),
    year = as.factor(year(date))
  )


ggplot(
  temp_interp_kb,
  aes(x = date, y = depth, colour = temp)
) +
  geom_point() +
  scale_y_reverse() +
  # scale_colour_gradient2(
  #   midpoint = 2, 
  #   high = scales::muted("red"), 
  #   low = scales::muted("blue")
  # )+
  scale_colour_viridis_c()+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_temp_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y
}

estimate_temp_by_depth(
  temp_interp_kb,
  target_depth = 0.85, 
  target_date = seq(ymd("2024-02-01"), ymd("2024-02-06"), by = 1)
)

#split data into years
kb_int_24 <- temp_interp_kb %>% 
  filter(
    year == 2024
  )

min(kb_int_24$date) #"2024-02-01"
max(kb_int_24$date) #"2024-02-20"

kb_int_25 <- temp_interp_kb %>% 
  filter(
    year == 2025
  )

min(kb_int_25$date) #"2024-01-27"
max(kb_int_25$date) #"2024-03-24"


#Interpolate across dates by depth
temp_raster_kb_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-02-01"), ymd("2024-02-20"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(kb_int_24, depth[1], date))

temp_raster_kb_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-27"), ymd("2025-03-24"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(kb_int_25, depth[1], date))

#Join temp data
kb_temp <- temp_raster_kb_24 %>% 
  bind_rows(temp_raster_kb_25) %>% 
  mutate(
    site = as.factor('Paint Lake - Deep'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Temp heat map --------------------------------------------------------------


ggplot(kb_temp, aes(yday, depth, fill = temp)) +
  geom_tile() +
  scale_y_reverse() +
  # scale_fill_gradient2(
  #   midpoint = 1,
  #   high = scales::muted("red"),
  #   low = scales::muted("blue")
  # ) +
  scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    # scales = 'free',
    ncol = 1
  )+
  theme_classic()

ggsave(
  here(
    # 'output/data_viz/heat_maps/kb_temp_rb.png'
    'output/data_viz/heat_maps/kb_temp_vir.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)

# 3b. Kempenfelt Bay - Dissolved Oxygen ------------------------------------


# **DO ------------------------------------------------------------------


#Below interpolates DO for one date at a time
estimate_DO_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$do_mg_l,
    xout = target_depth
  )$y
}

#Test
#estimate_DO_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
DO_interp_kb <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(kb$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(0.85, 31.2, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    do_umol = estimate_DO_by_date(kb, date[1], depth),
    site = as.factor('kempenfelt.bay'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  DO_interp_kb,
  aes(x = date, y = depth, colour = do_umol)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =250, 
    high = scales::muted("blue"), 
    low = scales::muted("red")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_DO_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$do_umol, xout = target_date)$y
}

estimate_DO_by_depth(
  DO_interp_kb,
  target_depth = 0.85, 
  target_date = seq(ymd("2024-02-01"), ymd("2024-02-06"), by = 1)
)

#split data into years
kb_int_24 <- DO_interp_kb %>% 
  filter(
    year == 2024
  )

min(kb_int_24$date) #"2024-02-01"
max(kb_int_24$date) #"2024-02-20"

kb_int_25 <- DO_interp_kb %>% 
  filter(
    year == 2025
  )

min(kb_int_25$date) #"2025-01-27"
max(kb_int_25$date) #"2025-03-24"


#Interpolate across dates by depth
DO_raster_kb_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-02-01"), ymd("2024-02-20"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(kb_int_24, depth[1], date))

DO_raster_kb_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-27"), ymd("2025-03-24"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(do_umol = estimate_DO_by_depth(kb_int_25, depth[1], date))

#Join temp data
kb_DO <- DO_raster_kb_24 %>% 
  bind_rows(DO_raster_kb_25) %>% 
  mutate(
    site = as.factor('Kempenfelt Bay'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **DO heat map --------------------------------------------------------------


ggplot(kb_DO, aes(yday, depth, fill = do_umol)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 270,
    high = scales::muted("blue"),
    low = scales::muted("red")
  ) +
  # scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    ncol = 1,
    #scales = 'free'
  )+
  theme_classic()

ggsave(
  here(
    'output/data_viz/heat_maps/kb_do_rb.png'
    # 'output/data_viz/heat_maps/kb_do_vir.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)

# 3c. Kempenfelt Bay - Chlorophyll-a ------------------------------------


# **Chl-a ------------------------------------------------------------------

#Below interpolates DO for one date at a time
estimate_chl_a_by_date <- function(df, target_date, target_depth) {
  data_for_date <- df %>%
    # filter(site == 'paint.deep') %>%
    filter(date == target_date) %>%
    arrange(d_int)
  
  # approx() is one way to do a linear interpolation
  approx(
    data_for_date$d_int, 
    data_for_date$chl_a,
    xout = target_depth
  )$y
}

#Test
#estimate_chl_a_by_date(kb, ymd("2024-02-01"), c(1,1.5,2,2.5,3,3.5,4,4.5,5))


#Here is the above function scaled up
chl_a_interp_kb <- crossing(
  # the same dates as rbr_df
  tibble(date = unique(kb$date)),
  # depths can now be any value
  #ps: min = 2, max = 3
  #pd: min = 2, max = 15
  #kb: min = 2, max = 32
  tibble(depth = seq(0.85, 31.2, length.out = 500))
) %>%
  group_by(date) %>%
  mutate(
    chl_a = estimate_chl_a_by_date(kb, date[1], depth),
    site = as.factor('kempenfelt.bay'),
    year = as.factor(year(date))
  )


#Test plot
ggplot(
  chl_a_interp_kb,
  aes(x = date, y = depth, colour = chl_a)
) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint =2.25, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )+
  facet_wrap(
    ~year,
    ncol = 1,
    scales = 'free'
  )+
  theme_classic()



# **Interpolate by date ---------------------------------------------------

# create a function that will, given a depth, estimate the temp on any given day
estimate_chl_a_by_depth <- function(df, target_depth, target_date) {
  data_for_depth <- df %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$chl_a, xout = target_date)$y
}

# estimate_chl_a_by_depth(
#   chl_a_interp_pld,
#   target_depth = 1.1, 
#   target_date = seq(ymd("2024-01-26"), ymd("2024-01-31"), by = 1)
# )

#split data into years
kb_int_24 <- chl_a_interp_kb %>% 
  filter(
    year == 2024
  )

min(kb_int_24$date) #"2024-02-01"
max(kb_int_24$date) #"2024-02-20"

kb_int_25 <- chl_a_interp_kb %>% 
  filter(
    year == 2025
  )

min(kb_int_25$date) #"2025-01-27"
max(kb_int_25$date) #"2025-03-24"


#Interpolate across dates by depth
chl_a_raster_kb_24 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2024-02-01"), ymd("2024-02-20"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_24$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(kb_int_24, depth[1], date))

chl_a_raster_kb_25 <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2025-01-27"), ymd("2025-03-24"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(kb_int_25$depth))
) %>%
  group_by(depth) %>%
  mutate(chl_a = estimate_chl_a_by_depth(kb_int_25, depth[1], date))

#Join temp data
kb_chl_a <- chl_a_raster_kb_24 %>% 
  bind_rows(chl_a_raster_kb_25) %>% 
  mutate(
    site = as.factor('Kempenfelt Bay'),
    year = as.factor(year(date)),
    yday = yday(date)
  )

# **Chlorophyll-a heat map --------------------------------------------------------------


ggplot(kb_chl_a, aes(yday, depth, fill = chl_a)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 1.8,
    high = scales::muted("blue"),
    low = scales::muted("red")
  ) +
  # scale_fill_viridis_c()+
  coord_cartesian(expand = FALSE)+
  facet_wrap(
    ~year, 
    ncol = 1,
    # scales = 'free'
  )+
  theme_classic()

ggsave(
  here(
    'output/data_viz/heat_maps/kb_chl_rb.png'
    # 'output/data_viz/heat_maps/kb_chl_vir.png'
  ),
  dpi = 300,
  height = 3,
  width = 5,
  units = 'in'
)
