
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

ggplot(data = chem_df %>% filter(analyte_name == 'Carbon; dissolved organic'))+
  geom_boxplot(aes(x = lake, y = result, fill = year))+
  geom_jitter(aes(x = lake, y = result), size = 2, alpha = 0.7)+
  theme_classic()+
  ylab('DOC [ug/L]')+
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


# 5. MWU stat test  -------------------------------------------------------

chm_mwu_df <- chem_df %>%
  group_by(lake, date, analyte_name) %>% 
  summarize(result=mean(result)) %>% 
  # distinct(date) %>% 
  arrange(lake, date) %>% 
  # count(analyte_name, date, result, year) %>%
  # mutate(n = +(n > 0)) %>%
  pivot_wider(
    names_from = analyte_name, 
    values_from = result
  ) %>% 
  mutate(
    year = as.factor(year(date))
  )

chm_mwu_rslt <- chm_mwu_df %>% 
  group_by(lake) %>% 
  filter(lake != 'Lake Simcoe') %>% 
  summarise(
    #DIC
    dic_p = wilcox.test(`Carbon; dissolved inorganic` ~year, exact = F)$p.value,
    dic_w = wilcox.test(`Carbon; dissolved inorganic` ~year, exact = F)$statistic,
    dic_n = n(),
    #DOC
    doc_p = wilcox.test(`Carbon; dissolved organic` ~year, exact = F)$p.value,
    doc_w = wilcox.test(`Carbon; dissolved organic` ~year, exact = F)$statistic,
    doc_n = n(),
    #Total Nitrogen
    tn_p = wilcox.test(`Nitrogen; total`~year, exact = F)$p.value,
    tn_w = wilcox.test(`Nitrogen; total`~year, exact = F)$statistic,
    tn_n = n(),
    #Total Phosphorus
    tp_p = wilcox.test(`Phosphorus; total`~year, exact = F)$p.value,
    tp_W = wilcox.test(`Phosphorus; total`~year, exact = F)$statistic,
    tp_n = n()
  )

  