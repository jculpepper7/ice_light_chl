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
    pattern = "\\.csv$",            #file type to read
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
  #Fix column types
  mutate(
    station = as.factor(station_name),
    climate_id = as.factor(climate_id),
    date = ymd(date_time)
  ) %>%
  #Remove unnecessary columns
  select(
    -c(station_name, date_time, data_quality, 
       max_temp_flag, min_temp_flag, mean_temp_flag,
       heat_deg_days_flag, cool_deg_days_flag, 
       total_rain_flag, total_snow_flag, total_precip_flag,
       snow_on_grnd_flag, dir_of_max_gust_flag, spd_of_max_gust_flag)
  ) %>% 
  #Change the rest of the columns to numeric
  mutate_if(
    is.character, 
    as.numeric
  ) 
 

# 3. Visualize daily data -------------------------------------------------

ggplot(data = met_data_clean)+
  geom_point(aes(x = date, y = mean_temp_c))+
  facet_wrap(~station, ncol = 1)

ggplot(data = met_data_clean)+
  #total snow doesn't seem to have sufficient data, 
  #so use snow on ground
  geom_point(aes(x = date, y = snow_on_grnd_cm))+ 
  facet_wrap(~station, ncol = 1)


# 4. Seaasonal summaries ----------------------------------------------------

met_seasonal <- met_data_clean %>% 
  filter(
    month == 9 | month == 10 | month == 11 | 
    month == 12 | month == 1 | month == 2 | 
    month == 3 | month == 4 | month == 5
  ) %>% 
  mutate(
    season = if_else(
      month == 9 | month == 10 | month == 11, as.factor('fall'),
      if_else(
        month == 12 | month == 1 | month == 2, as.factor('winter'),
        as.factor('spring')
      )
    )
  ) %>% 
  group_by(station, year, season) %>% 
  summarise(
    max_temp_c = mean(max_temp_c, na.rm = T),
    min_temp_c = mean(min_temp_c, na.rm = T),
    mean_temp_c = mean(mean_temp_c, na.rm = T),
    heat_deg_days_c = mean(heat_deg_days_c, na.rm = T),
    cool_deg_days_c = mean(cool_deg_days_c, na.rm = T),
    total_precip_mm = mean(total_precip_mm, na.rm = T),
    snow_on_grnd_cm = mean(snow_on_grnd_cm, na.rm = T),
    
  )

ggplot(data = met_seasonal %>% filter(season == 'fall'))+
  geom_point(
    aes(x = year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = year, y = mean_temp_c, color = station), 
    method = 'lm', 
    se = F
  )
  
ggplot(data = met_seasonal %>% filter(season == 'winter'))+
  geom_point(
    aes(x = year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = year, y = mean_temp_c, color = station), 
    method = 'lm', 
    se = F
  )

ggplot(data = met_seasonal %>% filter(season == 'spring'))+
  geom_point(
    aes(x = year, y = mean_temp_c, color = station)
  )+
  geom_smooth(
    aes(x = year, y = mean_temp_c, color = station), 
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
    aes(x = year, y = cool_deg_days_c, color = station)
  )+
  geom_smooth(
    # aes(x = year, y = mean_temp_c, color = station),
    # aes(x = year, y = min_temp_c, color = station),
    # aes(x = year, y = max_temp_c, color = station),
    # aes(x = year, y = snow_on_grnd_cm, color = station),
    # aes(x = year, y = total_precip_mm, color = station),
    # aes(x = year, y = heat_deg_days_c, color = station),
    aes(x = year, y = cool_deg_days_c, color = station),
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
  
ggsave(
  here('output/data_viz/cooling_degree_days.pdf'),
  dpi = 300,
  width = 5,
  height = 7,
  units = 'in'
)  


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
    col_4 = as.factor(if_else(snow_anom_cm > 0, 2,1))
  )

#Seasonal anomaly viz

ggplot(data = season_anom %>% filter(season == 'spring'))+
  geom_bar(
    aes(x = year, y = snow_anom_cm, fill = col_4),
    stat = 'identity',
    show.legend = F
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
  scale_fill_manual(values=c('red','blue'))+
  theme_classic()+
  xlab('')+
  #Change to match variable above
  ylab('Snow Anomaly (cm)')+
  theme(
    text = element_text(size = 15)
  )

ggsave(
  here('output/data_viz/snow_anom_spring.png'),
  dpi = 300,
  width = 6.5,
  height = 4.5,
  units = 'in'
)  

# 6. Seasonal trend analysis ----------------------------------------------

test <- season_anom %>% 
  filter(
    season == 'spring' | season == 'winter'
  ) %>% 
  select(station, year, mean_anom_temp_c, max_anom_temp_c, min_anom_temp_c, snow_anom_cm)
  