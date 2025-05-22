library(here)
library(tidyverse)
library(janitor)

# 1. import data ----------------------------------------------------------

# climate <- read_csv(here('data/climate/Climate data up to April 2025.csv'))

# 2. Clean data -----------------------------------------------------------

# climate_clean <- climate %>%
#   clean_names() %>%
#   mutate(
#     station = as.factor(station_name)
#   ) %>%
#   group_by(
#     station, year, month, day
#   ) %>%
#   summarise(
#     temp_c = mean(temp_c, na.rm = T),
#     precip_mm = mean(precip_amount_mm, na.rm = T)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     season = if_else(
#       month == 11 | month == 12, as.factor('open'), as.factor('ice')
#     ),
#     snow = if_else(
#       temp_c <= 0, precip_mm, 0
#     ),
#     date = make_date(year = year, month = month, day = day)
#   )


# 1. Import met data ------------------------------------------------------


#First import all met data from Barrie-Oro (Kempenfelt Bay) and Beatrice (Paint Lake)
met_raw <- 
  list.files(
    path = here('data/climate'), #insert path to met data
    pattern = "\\.csv$",         #file type to read
    full.names = T
  ) %>% 
  map_df(
    ~read_delim(., col_types = cols(.default = 'c'), delim = ',') #Function to map to the listed files
  ) %>% 
  clean_names()


# 2. Clean dataframe ------------------------------------------------------

beatrice <- met_raw %>% 
  #Right now, Station name includes "BEATRICE CLIMATE" and "BEATRICE 2"
  #Need to condense the names to just beatrice
  filter(
    station_name != 'BARRIE-ORO'
  ) %>% 
  mutate(
    station_name = c('beatrice')
  )
  
barrie <- met_raw %>% 
  #Same as above, but reformatting "BARRIE-ORO to barrie
  filter(
    station_name == 'BARRIE-ORO'
  ) %>% 
  mutate(
    station_name = c('barrie')
  )

#Rebind rows
met_data_clean <- beatrice %>% 
  bind_rows(barrie) %>%
  mutate(
    station = as.factor(station_name),
    climate_id = as.factor(climate_id),
    date = ymd(date_time)
  ) %>%
  select(
    -station_name
  ) %>% 
  #Change the rest of the columns to numeric
  mutate_if(
    is.character, 
    as.numeric
  ) %>% 
  #Fix column types
  mutate(
    #year = as.numeric(year),
    w_year = if_else(
      month(date) >= 10, year+1, year
    ),
    total_precip_cm = total_precip_mm/10,
    total_rain_cm = total_rain_mm/10,
    rain_cm = if_else(
      mean_temp_c > 0, total_precip_cm, 0
    ),
    snow_cm = if_else(
      mean_temp_c <= 0, total_precip_cm, 0
    )
  ) %>%
  #Remove unnecessary columns
  select(
    -c(date_time, data_quality, 
       total_rain_mm, total_precip_mm,
       max_temp_flag, min_temp_flag, mean_temp_flag,
       heat_deg_days_flag, cool_deg_days_flag, 
       total_rain_flag, total_snow_flag, total_precip_flag,
       snow_on_grnd_flag, dir_of_max_gust_flag, spd_of_max_gust_flag)
  )


# **Write cleaned data to CSV ---------------------------------------------

 write_csv(met_data_clean, here('data/combined_data/met_clean.csv'))

# 3. Visualize daily data -------------------------------------------------

ggplot(data = met_data_clean)+
  geom_point(aes(x = date, y = snow_cm))+
  facet_wrap(~station, ncol = 1)

ggplot(data = met_data_clean)+
  #total snow doesn't seem to have sufficient data, 
  #so use snow on ground
  geom_point(aes(x = date, y = snow_on_grnd_cm))+ 
  facet_wrap(~station, ncol = 1)


# 4. Seasonal summaries ----------------------------------------------------

met_seasonal <- met_data_clean %>% 
  # filter(
  #   month == 9 | month == 10 | month == 11 | 
  #   month == 12 | month == 1 | month == 2 | 
  #   month == 3 | month == 4 | month == 5
  # ) %>% 
  mutate(
    season = if_else(
      month == 9 | month == 10 | month == 11, as.factor('fall'),
      if_else(
        month == 12 | month == 1 | month == 2, as.factor('winter'),
        if_else(
          month == 3 | month == 4 | month == 5, as.factor('spring'),
          as.factor('summer')
        )
      )
    )
  ) %>% 
  group_by(station, w_year, season) %>% 
  summarise(
    max_temp_c = mean(max_temp_c, na.rm = T),
    min_temp_c = mean(min_temp_c, na.rm = T),
    mean_temp_c = mean(mean_temp_c, na.rm = T),
    #heat_deg_days_c = mean(heat_deg_days_c, na.rm = T),
    #cool_deg_days_c = mean(cool_deg_days_c, na.rm = T),
    total_precip_cm = mean(total_precip_cm, na.rm = T),
    rain_sum_cm = sum(rain_cm),
    #rain_cm = mean(rain_cm, na.rm = T),
    snow_on_grnd_cm = mean(snow_on_grnd_cm, na.rm = T),
    snow_sum_cm = sum(snow_cm),
    wind_km_h = mean(spd_of_max_gust_km_h, na.rm = T)
  )

ggplot(data = met_seasonal %>% filter(season == 'fall'))+
  geom_point(
    aes(x = w_year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = w_year, y = mean_temp_c, color = station), 
    method = 'lm', 
    se = F
  )
  
ggplot(data = met_seasonal %>% filter(season == 'winter'))+
  geom_point(
    aes(x = w_year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = w_year, y = mean_temp_c, color = station), 
    method = 'lm', 
    se = F
  )

ggplot(data = met_seasonal %>% filter(season == 'spring'))+
  geom_point(
    aes(x = w_year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = w_year, y = mean_temp_c, color = station), 
    #method = 'lm', 
    se = F
  )

#Plot temp and precip variables
ggplot(data = met_seasonal)+
  geom_point(
    # aes(x = year, y = mean_temp_c, color = station)
    # aes(x = year, y = min_temp_c, color = station)
    # aes(x = year, y = max_temp_c, color = station)
    # aes(x = year, y = snow_on_grnd_cm, color = station)
    # aes(x = year, y = total_precip_mm, color = station) #Precip seems far too low at both stations given the snow on ground amounts (in cm)
    # aes(x = year, y = heat_deg_days_c, color = station)
    aes(x = w_year, y = cool_deg_days_c, color = station)
  )+
  geom_smooth(
    # aes(x = year, y = mean_temp_c, color = station),
    # aes(x = year, y = min_temp_c, color = station),
    # aes(x = year, y = max_temp_c, color = station),
    # aes(x = year, y = snow_on_grnd_cm, color = station),
    # aes(x = year, y = total_precip_mm, color = station),
    # aes(x = year, y = heat_deg_days_c, color = station),
    aes(x = w_year, y = cool_deg_days_c, color = station),
    method = 'lm', 
    se = F
  ) +
  facet_wrap(
    ~season, 
    ncol = 1,
    labeller = labeller(season = 
                          c(
                            'fall' = 'Fall',
                            'winter' = 'Winter',
                            'spring' = 'Spring'
                          ))
  )+
  theme_classic()+
  scale_color_manual(values = c('#E1BE6A','#40B0A6'))+
  xlab('')+
  #Change to match variable above
  ylab('Cooling Degree Days (\u00B0C)')+ #degree symbol = \u00B0
  theme(
    legend.title = element_blank()
  )
  
# ggsave(
#   here('output/data_viz/cooling_degree_days.pdf'),
#   dpi = 300,
#   width = 5,
#   height = 7,
#   units = 'in'
# )  


# 5. Seasonal anomalies ---------------------------------------------------

season_anom <- met_seasonal %>% 
  group_by(season, station) %>% 
  mutate(
    total_mean_temp_c = mean(mean_temp_c, na.rm = T),
    mean_anom_temp_c = mean_temp_c - total_mean_temp_c,
    col_1 = as.factor(if_else(mean_anom_temp_c > 0, 2,1)),
    total_min_temp_c = mean(min_temp_c, na.rm = T),
    min_anom_temp_c = min_temp_c - total_min_temp_c,
    col_2 = as.factor(if_else(min_anom_temp_c > 0, 2,1)),
    total_max_temp_c = mean(max_temp_c, na.rm = T),
    max_anom_temp_c = max_temp_c - total_max_temp_c,
    col_3 = as.factor(if_else(max_anom_temp_c > 0, 2,1)),
    total_snow_cm = mean(snow_on_grnd_cm, na.rm = T),
    snow_anom_cm = snow_on_grnd_cm - total_snow_cm,
    col_4 = as.factor(if_else(snow_anom_cm > 0, 2, 1)),
    total_rain_sum_cm = mean(rain_sum_cm, na.rm = T),
    rain_anom_cm = rain_sum_cm - total_rain_sum_cm,
    col_5 = as.factor(if_else(rain_anom_cm > 0, 2, 1)),
    total_wind_km_h = mean(wind_km_h, na.rm = T),
    wind_anom_km_h = wind_km_h - total_wind_km_h,
    col_6 = as.factor(if_else(wind_anom_km_h > 0, 2, 1))
  ) %>% 
  mutate(
    rain_anom_cm = round(rain_anom_cm, digits = 2),
    mean_anom_temp_c = round(mean_anom_temp_c, digits = 2),
    wind_anom_km_h = round(wind_anom_km_h, digits = 2),
    
  )
# %>% 
#   ungroup() %>% 
#   mutate(
#     across(
#       .cols = is.numeric, .fns = round
#     )
#   )
  

#Seasonal anomaly viz

ggplot(data = season_anom %>% filter(season == 'summer'))+
  geom_bar(
    # aes(x = w_year, y = mean_anom_temp_c, fill = col_1),
    # aes(x = w_year, y = min_anom_temp_c, fill = col_2),
    # aes(x = w_year, y = max_anom_temp_c, fill = col_3),
    # aes(x = w_year, y = snow_anom_cm, fill = col_4),
    # aes(x = w_year, y = rain_anom_cm, fill = col_5),
    aes(x = w_year, y = wind_anom_km_h, fill = col_6),
    
    stat = 'identity',
    show.legend = F,
    alpha = 0.5
  )+
  geom_text(
    aes(
      x = w_year, 
      y = wind_anom_km_h, 
      label = wind_anom_km_h,
      vjust = 0.5 - sign(wind_anom_km_h)/2
    ),
    #position = position_dodge(width = 0.9) 
  )+
  geom_hline(yintercept=0, size=0.5)+
  facet_wrap(
    ~station,
    ncol = 1,
    labeller = labeller(station = 
                          c(
                            'barrie' = 'Barrie',
                            'beatrice' = 'Beatrice'
                          ))
  )+
  # scale_fill_manual(values=c('red','blue'))+
  
  scale_fill_manual(values=c('blue','red'))+
  theme_classic()+
  xlab('')+
  #Change to match variable above
  ylab('Wind Anomaly (km/h)')+
  theme(
    text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/climate_viz/wind_anom_summer.png'),
  dpi = 300,
  width = 9,
  height = 7,
  units = 'in'
)  



# 6. Wilcoxon Rank Sum Test -----------------------------------------------

# Running this test for temp, snow, rain, 

met_24_25 <- met_data_clean %>% 
  mutate(
    w_year = as.factor(w_year)
  ) %>% 
  filter(
    w_year == 2024 | w_year == 2025
  )

wlcx_met_rslt <- met_24_25 %>% 
  group_by(station) %>% 
  summarise(
    #Snow on ground
    snow_p = wilcox.test(snow_on_grnd_cm~w_year, exact = F)$p.value,
    snow_w = wilcox.test(snow_on_grnd_cm~w_year, exact = F)$statistic,
    snow_n = n(),
    
    #Wind max gust
    wind_p = wilcox.test(spd_of_max_gust_km_h~w_year, exact = F)$p.value,
    wind_w = wilcox.test(spd_of_max_gust_km_h~w_year, exact = F)$statistic,
    wind_n = n()
  )
