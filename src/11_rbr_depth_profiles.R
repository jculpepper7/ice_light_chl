library(tidyverse)
library(here)
library(wacolors)

rbr <- read_csv(here::here('data/rbr/rbr_clean.csv')) %>% 
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
    mutate(d_int =  round_any(depth, int)) |>
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
    year = as.factor(year(date))
  ) %>% 
  filter(
    date2 != '2024-01-27'
  ) %>% 
  group_by(site, date, year) %>% 
  mutate(
    day_no = min_rank(date)
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

ggsave(
  here::here('output/data_viz/rbr_viz/temp_profile_kb.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)


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

ggsave(
  here::here('output/data_viz/rbr_viz/temp_profile_ps.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)

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

ggsave(
  here::here('output/data_viz/rbr_viz/temp_profile_pd.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)

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

ggsave(
  here::here('output/data_viz/rbr_viz/do_profile_kb.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)


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

ggsave(
  here::here('output/data_viz/rbr_viz/do_profile_ps.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)

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

ggsave(
  here::here('output/data_viz/rbr_viz/do_profile_pd.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)

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

ggsave(
  here::here('output/data_viz/rbr_viz/chla_profile_kb.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)


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

ggsave(
  here::here('output/data_viz/rbr_viz/chla_profile_ps.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)

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

ggsave(
  here::here('output/data_viz/rbr_viz/chla_profile_pd.png'),
  dpi = 300,
  width = 5,
  height = 8,
  units = 'in'
)
#Heat map code----
# ggplot(rbr_df %>% filter(site == 'simcoe.deep'), aes(date, d_int, fill = temp_c)) +
#   geom_raster() +
#   scale_y_reverse() +
#   scale_fill_gradient2(
#     midpoint = 1, 
#     high = scales::muted("red"), 
#     low = scales::muted("blue")
#   ) +
#   coord_cartesian(expand = FALSE)+
#   facet_wrap(~year, scales = 'free')
