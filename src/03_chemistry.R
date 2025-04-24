
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)


# 1. Import data ----------------------------------------------------------

chem_24 <- read_csv(here('data/chemistry/combined_chemistry_2024.csv'))

chem_25 <- read_csv(here('data/chemistry/paint_chem_2025.csv'))


# 2. Clean data -----------------------------------------------------------


# **2024 ------------------------------------------------------------------


chem_24_clean <- chem_24 %>% 
  clean_names() %>% 
  select(-13) %>% 
  filter(
    depth_from_surface == 1
  ) %>% 
  mutate(
    lake_main_position = as.factor(lake_main_position),
    date = dmy(collect_date),
    analyte_name = as.factor(analyte_name)
  ) %>% 
  select(
    lake = lake_main_position,
    date,
    analyte_name,
    result = raw_result
  )
 

# **2025 ------------------------------------------------------------------

chem_25_clean <- chem_25 %>% 
  clean_names() %>% 
  select(
    lake = cust_sample_id,
    date = collect_date,
    analyte_name,
    result = final_result
  ) %>% 
  filter(
    lake == 'Paint East' | lake == 'Paint West'
  ) %>% 
  mutate(
    lake = as.factor(lake),
    date = mdy(date),
    analyte_name = as.factor(analyte_name)
  )
 

# 3. Combine data ---------------------------------------------------------

chem_df <- chem_24_clean %>% 
  bind_rows(chem_25_clean) %>% 
  mutate(
    year= as.factor(year(date))
  )



# 4. Quick data viz -------------------------------------------------------

ggplot(data = chem_df %>% filter(analyte_name == 'Nitrogen; total'))+
  geom_boxplot(aes(x = lake, y = result, fill = year))+
  geom_jitter(aes(x = lake, y = result), size = 2, alpha = 0.7)+
  theme_classic()+
  ylab('TN [ug/L]')+
  xlab('')+
  theme(
    text = element_text(size = 15),
    legend.title = element_blank()
  ) #+
  #facet_wrap(~year)
  
ggsave(
  here('output/data_viz/chem_tn.png'),
  dpi = 300,
  width = 6.5,
  height = 5,
  units = 'in'
)





