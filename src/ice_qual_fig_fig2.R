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
    perc_trans = (iw_int/air)*100,
    #reorder in terms of depth for facet wrap
    site = fct_relevel(site, 'simcoe.deep', 'paint.deep', 'paint.shallow')
  )

bar_plt_df2 <- par_sum_wide %>% 
  select(
    site,
    date,
    year,
    blk_ratio,
    wht_ratio
  ) %>% 
  pivot_longer(
    cols = c('blk_ratio', 'wht_ratio'),
    names_to = 'ice_qual',
    values_to = 'thickness_cm'
  ) %>% 
  arrange(
    site, date
  ) %>% 
  mutate(
    yday = yday(date),
    #reorder in terms of depth for facet wrap
    site = fct_relevel(site, 'simcoe.deep', 'paint.deep', 'paint.shallow') 
  )


# Ice Quality function ----------------------------------------------------

ice_tot_func <- function(site){ggplot(
  bar_plt_df %>% 
    filter(
      site == {{site}}
    ) %>% 
    mutate(site_year = paste0(site, '-', year))
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
  labs(fill = 'Ice Type')+
  xlab('')+
  ylab('Ice Thickness cm')+
  xlim(c(10,90))+
  scale_fill_manual(
    values = c("snow_avg_cm" = "snow", 
               "wht_slush_cm" = "grey85", 
               "black_ice_cm" = "black"), 
    labels = c("Snow", "White Ice", "Black Ice"),
  )+
  facet_wrap(
    # ~site + year,
    ~site_year,
    ncol = 2,
    labeller = as_labeller(
      c(
        'simcoe.deep-2024' = '2024',
        'simcoe.deep-2025' = '2025'
      )
    )
  )+
  theme_classic()+
  theme(
    # strip.text = element_blank(),
    strip.text = element_text(size = 15),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = 'bottom'
  )
}


ice_qual_func <- function(site){ggplot(
  bar_plt_df2 %>% 
    filter(
      site == {{site}}
    ) %>% 
  mutate(site_year = paste0(site, '-', year))
)+
    geom_bar(
      aes(
        x = yday, 
        y = thickness_cm, 
        fill = factor(
          ice_qual, 
          levels = c('wht_ratio', 'blk_ratio')
        )
      ),
      stat = 'identity', 
      position = 'stack', 
      color = 'black'
    )+
    labs(fill = 'Ice Type')+
    xlab('')+
    ylab('Ice Ratio (%)')+
    scale_y_continuous(labels = scales::percent)+
    xlim(c(10,90))+
    scale_fill_manual(
      values = c( 
                 "wht_ratio" = "grey85", 
                 "blk_ratio" = "black"), 
      labels = c("White Ice", "Black Ice"),
    )+
    facet_wrap(
      # ~site + year,
      ~site_year,
      ncol = 2,
      labeller = as_labeller(
        c(
          'simcoe.deep-2024' = '2024',
          'simcoe.deep-2025' = '2025'
        )
      )
    )+
    theme_classic()+
    theme(
      # strip.text = element_blank(),
      strip.text = element_text(size = 15),
      strip.background = element_blank(),
      legend.title = element_blank(),
      # legend.position = 'none'
    )
}

# **7b. KB total -----------------------------------------------------------

kb_ice_tot <- ice_tot_func(
  site = 'simcoe.deep'
)

# **7c. PLD total -----------------------------------------------------------

pld_ice_tot <- ice_tot_func(
  site = 'paint.deep'
)

# **7c. PLS total -----------------------------------------------------------

pls_ice_tot <- ice_tot_func(
  site = 'paint.shallow'
)

# **7d. KB ratio -----------------------------------------------------------

kb_ice_qual <- ice_qual_func(
  site = 'simcoe.deep'
)

# **7e. PLD ratio -----------------------------------------------------------

pld_ice_qual <- ice_qual_func(
  site = 'paint.deep'
)

# **7f. PLS ratio -----------------------------------------------------------

pls_ice_qual <- ice_qual_func(
  site = 'paint.shallow'
)

# Combine plots -----------------------------------------------------------

#**ice tot subplot----
ice_tot_comb <- (kb_ice_tot / pld_ice_tot / pls_ice_tot) +
  plot_layout(
    axes = 'collect',
    guides = 'collect'
  )+
  plot_annotation(
    tag_levels = 'a'
  )&
  theme(
    legend.position = 'bottom'
  )
  
ice_tot_comb

#**ice tot subplot----

ice_qual_comb <- (kb_ice_qual / pld_ice_qual / pls_ice_qual) +
  plot_layout(
    axes = 'collect',
    guides = 'collect'
  )+
  plot_annotation(
    tag_levels = list(c('d','e','f'))
  )&
  theme(
    legend.position = 'none'
  )

ice_qual_comb

#**tot and qual combined----

(ice_tot_comb | ice_qual_comb)+
  plot_annotation(
    tag_levels = 'a'
  )+
  theme(
    legend.position = 'right'
  )

# ggsave(
#   here(
#     'output/data_viz/ice_viz/fig2/fig2.pdf'
#   ),
#   dpi = 300,
#   width = 10.5,
#   height = 8.5,
#   units = 'in'
# )
