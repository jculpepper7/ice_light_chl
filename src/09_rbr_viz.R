#The goal of this script is to create intelligible, easy-to-read plots
#for RBR sonde data


# 1. Libraries -----------------------------------------------------------


library(tidyverse)
library(here)
library(ggridges)
library(ggstatsplot)


# 2. Import data ----------------------------------------------------------

rbr <- read_csv(here('data/rbr/rbr_clean.csv')) %>% 
  mutate(
    site = as.factor(site),
    position = as.factor(position),
    date2 = as.factor(date),
    year = as.factor(year)
  ) 

# 3. Ridgeline plot -------------------------------------------------------

ggplot(
  rbr %>% filter(position != 'mid'), 
  aes(x = temp_c, y = year, fill = position), 
  alpha = 0.5
)+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  #scale_fill_viridis_d()+
  labs(title = 'Water Column Temperature (C)') +
  theme_classic() +
  theme(
    legend.position="bottom",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )+
  facet_wrap(~site)


ggplot(rbr, aes(x = temp_c, y = date2, fill = year))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  #scale_fill_viridis_d()+
  labs(title = 'Water Column Temperature (C)') +
  theme_classic() +
  theme(
    #legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )+
  facet_wrap(~site+position)


set.seed(123)

ggbetweenstats(
  data  = rbr,
  x     = site,
  y     = temp_c,
  #title = "Distribution of sepal length across Iris species"
)


# 4. Between-lake CHL-A comparison plots ----------------------------------


# **4a. Whole water column comparison -------------------------------------


chl_all <- grouped_ggbetweenstats(
  data             = rbr,
  x                = site,
  y                = chl_a,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_all


# **4b. Top meter comparison ----------------------------------------------


chl_top <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'top'),
  x                = site,
  y                = chl_a,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_top


# **4c. Bottom meter comparison -------------------------------------------

chl_btm <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'bottom'),
  x                = site,
  y                = chl_a,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_btm


# 5. Between-lake TEMP comparison plots -----------------------------------


# **5a. Whole water column comparison -------------------------------------


temp_all <- grouped_ggbetweenstats(
  data             = rbr,
  x                = site,
  y                = temp_c,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Temperature (C)'
)
temp_all


# **5b. Top meter comparison ----------------------------------------------


temp_top <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'top'),
  x                = site,
  y                = temp_c,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Temperature (C)'
)

temp_top


# **5c. Bottom meter comparison -------------------------------------------

temp_btm <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'bottom'),
  x                = site,
  y                = temp_c,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Temperature (C)'
)
temp_btm

# 6. Between-lake DO comparison plots -----------------------------------

#NOT IN MG/L -- NEED TO FIX -- MUST BE MOLAR----

# **6a. Whole water column comparison -------------------------------------


temp_all <- grouped_ggbetweenstats(
  data             = rbr,
  x                = site,
  y                = do_mg_l,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'DO (mg/L)'
)
temp_all


# **6b. Top meter comparison ----------------------------------------------


temp_top <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'top'),
  x                = site,
  y                = do_mg_l,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'DO (mg/L)'
)

temp_top


# **6c. Bottom meter comparison -------------------------------------------

temp_btm <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'bottom'),
  x                = site,
  y                = do_mg_l,
  grouping.var     = year,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = 'DO (mg/L)'
)
temp_btm


# 7. Between year CHL-A comparison plots ----------------------------------

# **7a. Whole water column comparison -------------------------------------


chl_all <- grouped_ggbetweenstats(
  data             = rbr,
  x                = year,
  y                = chl_a,
  grouping.var     = site,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_all

ggsave(
  here('output/data_viz/compare_plts/rbr_chla_by_year.png'),
  dpi = 300,
  width = 20,
  height = 6,
  units = 'in'
)

# **4b. Top meter comparison ----------------------------------------------


chl_top <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'top'),
  x                = year,
  y                = chl_a,
  grouping.var     = site,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_top


# **4c. Bottom meter comparison -------------------------------------------

chl_btm <- grouped_ggbetweenstats(
  data             = rbr %>% filter(position == 'bottom'),
  x                = year,
  y                = chl_a,
  grouping.var     = site,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  xlab             = '',
  ylab             = 'Chlorophyll-a (mg/L)'
)
chl_btm

#KEEP GOING WITH THIS!
#STILL NEED TO DO TEMP AND DO
#####
#THEN IT'D BE GOOD TO DO THIS WITH THE IN SITU VARS
#I.E.: PAR, ICE QUALITY
