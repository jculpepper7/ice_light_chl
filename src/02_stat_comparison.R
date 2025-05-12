# The goal of this script is to take the aggregated data from script 01 
# and run statistical comparisons of the independent samples.
# These comparisons will be between lakes (i.e., Simcoe and Paint) within  
# the same year and between years for the same lake (i.e., cold and warm years)


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(ggtext)
library(car)
library(onewaytests)
library(CGPfunctions)


# 2. Import data ----------------------------------------------------------

tbl <- read_csv(here('data/combined_data/comparison_table.csv'))

# 3. Clean data -----------------------------------------------------------

tbl <- tbl %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year)
  )

# 3. Wilcoxon test across years -------------------------------------------


wlcx_rslt <- tbl %>% 
  group_by(site) %>% 
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = wilcox.test(ice_sheet_cm~year, exact = F)$p.value,
    ice_sheet_w = wilcox.test(ice_sheet_cm~year, exact = F)$statistic,
    ice_sheet_n = n(),
    #Total ice without slush
    total_ice_p = wilcox.test(total_ice_cm~year, exact = F)$p.value,
    total_ice_w = wilcox.test(total_ice_cm~year, exact = F)$statistic,
    total_ice_n = n(),
    #Total black ice thickness
    black_ice_p = wilcox.test(black_ice_cm~year, exact = F)$p.value,
    black_ice_w = wilcox.test(black_ice_cm~year, exact = F)$statistic,
    black_ice_n = n(),
    #White ice without slush
    white_ice_p = wilcox.test(white_ice_cm~year, exact = F)$p.value,
    white_ice_w = wilcox.test(white_ice_cm~year, exact = F)$statistic,
    white_ice_n = n(),
    #White ice with slush
    wht_slush_p = wilcox.test(wht_slush_cm~year, exact = F)$p.value,
    wht_slush_w = wilcox.test(wht_slush_cm~year, exact = F)$statistic,
    wht_slush_n = n(),
    #number of white layers
    n_white_p = wilcox.test(n_white~year, exact = F)$p.value,
    n_white_w = wilcox.test(n_white~year, exact = F)$statistic,
    n_white_n = n(),
    #number of white and slush layers
    n_slush_p = wilcox.test(n_slush~year, exact = F)$p.value,
    n_slush_w = wilcox.test(n_slush~year, exact = F)$statistic,
    n_slush_n = n(),
    #Snow depth
    snow_p = wilcox.test(snow_avg_cm~year, exact = F)$p.value,
    snow_w = wilcox.test(snow_avg_cm~year, exact = F)$statistic,
    snow_n = n(),
    #Black ice ratio to ice thickness
    perc_blk_tot_p = wilcox.test(perc_blk_tot~year, exact = F)$p.value,
    perc_blk_tot_w = wilcox.test(perc_blk_tot~year, exact = F)$statistic,
    perc_blk_tot_n = n(),
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = wilcox.test(perc_blk_sheet~year, exact = F)$p.value,
    perc_blk_sheet_w = wilcox.test(perc_blk_sheet~year, exact = F)$statistic,
    perc_blk_sheet_n = wilcox.test(perc_blk_sheet~year, exact = F)$n,
    #White ice ratio to ice thickness
    perc_wht_tot_p = wilcox.test(perc_wht_tot~year, exact = F)$p.value,
    perc_wht_tot_w = wilcox.test(perc_wht_tot~year, exact = F)$statistic,
    perc_wht_tot_p = n(),
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = wilcox.test(perc_wht_sheet~year, exact = F)$p.value,
    perc_wht_sheet_w = wilcox.test(perc_wht_sheet~year, exact = F)$statistic,
    perc_wht_sheet_n = n(),
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = wilcox.test(perc_wht_slush~year, exact = F)$p.value,
    perc_wht_slush_w = wilcox.test(perc_wht_slush~year, exact = F)$statistic,
    perc_wht_slush_n = n(),
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = wilcox.test(perc_trans_air~year, exact = F)$p.value,
    perc_trans_air_w = wilcox.test(perc_trans_air~year, exact = F)$statistic,
    perc_trans_air_n = n(),
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = wilcox.test(perc_trans_surf~year, exact = F)$p.value,
    perc_trans_surf_w = wilcox.test(perc_trans_surf~year, exact = F)$statistic,
    perc_trans_surf_n = n(),
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = wilcox.test(perc_par_no_snow~year, exact = F)$p.value,
    perc_par_no_snow_w = wilcox.test(perc_par_no_snow~year, exact = F)$statistic,
    perc_par_no_snow_n = n()
  )

#write_csv(wlcx_rslt, here('output/stat_test/wilcox_rslt.csv'))


# 4. Levene's test --------------------------------------------------------

#Test for equality of variance (i.e., are the variances different between years)
lev_rslt <- tbl %>% 
  group_by(site) %>%
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = leveneTest(ice_sheet_cm~year)[1,3],
    #Total ice without slush
    total_ice_p = leveneTest(total_ice_cm~year)[1,3],
    #Total black ice thickness
    black_ice_p = leveneTest(black_ice_cm~year)[1,3],
    #White ice without slush
    white_ice_p = leveneTest(white_ice_cm~year)[1,3],
    #White ice with slush
    wht_slush_p = leveneTest(wht_slush_cm~year)[1,3],
    #number of white layers
    n_white_p = leveneTest(n_white~year)[1,3],
    #number of white and slush layers
    n_slush_p = leveneTest(n_slush~year)[1,3],
    #Snow depth
    snow_p = leveneTest(snow_avg_cm~year)[1,3],
    #Black ice ratio to ice thickness
    perc_blk_tot_p = leveneTest(perc_blk_tot~year)[1,3],
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = leveneTest(perc_blk_sheet~year)[1,3],
    #White ice ratio to ice thickness
    perc_wht_tot_p = leveneTest(perc_wht_tot~year)[1,3],
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = leveneTest(perc_wht_sheet~year)[1,3],
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = leveneTest(perc_wht_slush~year)[1,3],
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = leveneTest(perc_trans_air~year)[1,3],
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = leveneTest(perc_trans_surf~year)[1,3],
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = leveneTest(perc_par_no_snow~year)[1,3],
  )

#write_csv(lev_rslt, here('output/stat_test/lev_rslt.csv'))

# 5. Brown-Forsythe test --------------------------------------------------


# **FB - Paint Deep (West) ------------------------------------------------


tbl_pd <- tbl %>% 
  filter(
    site == 'paint.deep'
  )

bf_rslt_pd <- tbl %>% filter(site == 'paint.deep') %>% 
  # group_by(site) %>%
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = bf.test(data = tbl_pd, ice_sheet_cm~year)$p.value,
    #Total ice without slush
    total_ice_p = bf.test(data = tbl_pd, total_ice_cm~year)$p.value,
    #Total black ice thickness
    black_ice_p = bf.test(data = tbl_pd, black_ice_cm~year)$p.value,
    #White ice without slush
    white_ice_p = bf.test(data = tbl_pd, white_ice_cm~year)$p.value,
    #White ice with slush
    wht_slush_p = bf.test(data = tbl_pd, wht_slush_cm~year)$p.value,
    #number of white layers
    n_white_p = bf.test(data = tbl_pd, n_white~year)$p.value,
    #number of white and slush layers
    n_slush_p = bf.test(data = tbl_pd, n_slush~year)$p.value,
    #Snow depth
    snow_p = bf.test(data = tbl_pd, snow_avg_cm~year)$p.value,
    #Black ice ratio to ice thickness
    perc_blk_tot_p = bf.test(data = tbl_pd, perc_blk_tot~year)$p.value,
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = bf.test(data = tbl_pd, perc_blk_sheet~year)$p.value,
    #White ice ratio to ice thickness
    perc_wht_tot_p = bf.test(data = tbl_pd, perc_wht_tot~year)$p.value,
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = bf.test(data = tbl_pd, perc_wht_sheet~year)$p.value,
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = bf.test(data = tbl_pd, perc_wht_slush~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = bf.test(data = tbl_pd, perc_trans_air~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = bf.test(data = tbl_pd, perc_trans_surf~year)$p.value,
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = bf.test(data = tbl_pd, perc_par_no_snow~year)$p.value,
  ) %>% 
  mutate(
    site = 'paint.deep'
  ) %>% 
  select(17,1:16)


# **FB - Paint Shallow (East) ---------------------------------------------

tbl_ps <- tbl %>% 
  filter(
    site == 'paint.shallow'
  )

bf_rslt_ps <- tbl %>% filter(site == 'paint.shallow') %>% 
  # group_by(site) %>%
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = bf.test(data = tbl_ps, ice_sheet_cm~year)$p.value,
    #Total ice without slush
    total_ice_p = bf.test(data = tbl_ps, total_ice_cm~year)$p.value,
    #Total black ice thickness
    black_ice_p = bf.test(data = tbl_ps, black_ice_cm~year)$p.value,
    #White ice without slush
    white_ice_p = bf.test(data = tbl_ps, white_ice_cm~year)$p.value,
    #White ice with slush
    wht_slush_p = bf.test(data = tbl_ps, wht_slush_cm~year)$p.value,
    #number of white layers
    n_white_p = bf.test(data = tbl_ps, n_white~year)$p.value,
    #number of white and slush layers
    n_slush_p = bf.test(data = tbl_ps, n_slush~year)$p.value,
    #Snow depth
    snow_p = bf.test(data = tbl_ps, snow_avg_cm~year)$p.value,
    #Black ice ratio to ice thickness
    perc_blk_tot_p = bf.test(data = tbl_ps, perc_blk_tot~year)$p.value,
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = bf.test(data = tbl_ps, perc_blk_sheet~year)$p.value,
    #White ice ratio to ice thickness
    perc_wht_tot_p = bf.test(data = tbl_ps, perc_wht_tot~year)$p.value,
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = bf.test(data = tbl_ps, perc_wht_sheet~year)$p.value,
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = bf.test(data = tbl_ps, perc_wht_slush~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = bf.test(data = tbl_ps, perc_trans_air~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = bf.test(data = tbl_ps, perc_trans_surf~year)$p.value,
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = bf.test(data = tbl_ps, perc_par_no_snow~year)$p.value,
  ) %>% 
  mutate(
    site = 'paint.shallow'
  ) %>% 
  select(17,1:16)  


# **FB - Simcoe -----------------------------------------------------------

tbl_sd <- tbl %>% 
  filter(
    site == 'simcoe.deep'
  )

bf_rslt_sd <- tbl %>% filter(site == 'simcoe.deep') %>% 
  # group_by(site) %>%
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = bf.test(data = tbl_sd, ice_sheet_cm~year)$p.value,
    #Total ice without slush
    total_ice_p = bf.test(data = tbl_sd, total_ice_cm~year)$p.value,
    #Total black ice thickness
    black_ice_p = bf.test(data = tbl_sd, black_ice_cm~year)$p.value,
    #White ice without slush
    white_ice_p = bf.test(data = tbl_sd, white_ice_cm~year)$p.value,
    #White ice with slush
    wht_slush_p = bf.test(data = tbl_sd, wht_slush_cm~year)$p.value,
    #number of white layers
    n_white_p = bf.test(data = tbl_sd, n_white~year)$p.value,
    #number of white and slush layers
    n_slush_p = bf.test(data = tbl_sd, n_slush~year)$p.value,
    #Snow depth
    snow_p = bf.test(data = tbl_sd, snow_avg_cm~year)$p.value,
    #Black ice ratio to ice thickness
    perc_blk_tot_p = bf.test(data = tbl_sd, perc_blk_tot~year)$p.value,
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = bf.test(data = tbl_sd, perc_blk_sheet~year)$p.value,
    #White ice ratio to ice thickness
    perc_wht_tot_p = bf.test(data = tbl_sd, perc_wht_tot~year)$p.value,
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = bf.test(data = tbl_sd, perc_wht_sheet~year)$p.value,
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = bf.test(data = tbl_sd, perc_wht_slush~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = bf.test(data = tbl_sd, perc_trans_air~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = bf.test(data = tbl_sd, perc_trans_surf~year)$p.value,
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = bf.test(data = tbl_sd, perc_par_no_snow~year)$p.value,
  ) %>% 
  mutate(
    site = 'simcoe'
  ) %>% 
  select(17,1:16) 


# **Combine BF results ----------------------------------------------------


bf_rslt <- bf_rslt_pd %>% 
  bind_rows(bf_rslt_ps, bf_rslt_sd)

write_csv(bf_rslt, here('output/stat_test/bf_icepar__rslt.csv'))


# 6. Kolmogorov-Smirnov test ----------------------------------------------

ks_rslt <- tbl %>% 
  group_by(site) %>%
  summarise(
    #Total ice cover thickness (black, white, slush)
    ice_sheet_p = ks.test(ice_sheet_cm~year)$p.value,
    #Total ice without slush
    total_ice_p = ks.test(total_ice_cm~year)$p.value,
    #Total black ice thickness
    black_ice_p = ks.test(black_ice_cm~year)$p.value,
    #White ice without slush
    white_ice_p = ks.test(white_ice_cm~year)$p.value,
    #White ice with slush
    wht_slush_p = ks.test(wht_slush_cm~year)$p.value,
    #number of white layers
    n_white_p = ks.test(n_white~year)$p.value,
    #number of white and slush layers
    n_slush_p = ks.test(n_slush~year)$p.value,
    #Snow depth
    snow_p = ks.test(snow_avg_cm~year)$p.value,
    #Black ice ratio to ice thickness
    perc_blk_tot_p = ks.test(perc_blk_tot~year)$p.value,
    #Black ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_blk_sheet_p = ks.test(perc_blk_sheet~year)$p.value,
    #White ice ratio to ice thickness
    perc_wht_tot_p = ks.test(perc_wht_tot~year)$p.value,
    #White ice ratio to total thickness of ice sheet (i.e., with slush)
    perc_wht_sheet_p = ks.test(perc_wht_sheet~year)$p.value,
    #Ratio of white ice and slush to to total ice sheet
    perc_wht_slush_p = ks.test(perc_wht_slush~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor at knee height
    perc_trans_air_p = ks.test(perc_trans_air~year)$p.value,
    #Percent PAR transmitted through ice sheet from PAR sensor on snow/ice surface
    perc_trans_surf_p = ks.test(perc_trans_surf~year)$p.value,
    #Percent PAR transmitted through ice with snow removed
    perc_par_no_snow_p = ks.test(perc_par_no_snow~year)$p.value,
  )

#write_csv(ks_rslt, here('output/stat_test/ks_rslt.csv'))

# 7. KW test and Dunn post hoc --------------------------------------------

test <- kruskal.test(data = tbl %>% filter(year == 2025), black_ice_cm~site)$p.value
pairwise.wilcox.test(data = tbl,  p.adjust.method = 'bonferroni')
test

kw_rslt <- tbl %>% 
  group_by(year) %>% 
  summarise(
    ice_sheet_kw = kruskal.test(ice_sheet_cm~site)$p.value,
    ice_sheet_kw = kruskal.test(ice_sheet_cm~site)$p.value,
    total_ice_kw = kruskal.test(total_ice_cm~site)$p.value,
    black_ice_kw = kruskal.test(black_ice_cm~site)$p.value,
    white_ice_kw = kruskal.test(white_ice_cm~site)$p.value,
    wht_slush_kw = kruskal.test(wht_slush_cm~site)$p.value,
    n_white_kw = kruskal.test(n_white~site)$p.value,
    n_slush_kw = kruskal.test(n_slush~site)$p.value,
    snow_kw = kruskal.test(snow_avg_cm~site)$p.value,
    perc_blk_tot_kw = kruskal.test(perc_blk_tot~site)$p.value,
    perc_blk_sheet_kw = kruskal.test(perc_blk_sheet~site)$p.value,
    perc_wht_tot_kw = kruskal.test(perc_wht_tot~site)$p.value,
    perc_wht_sheet_kw = kruskal.test(perc_wht_sheet~site)$p.value,
    perc_wht_slush_kw = kruskal.test(perc_wht_slush~site)$p.value,
    perc_trans_air_kw = kruskal.test(perc_trans_air~site)$p.value,
    perc_trans_surf_kw = kruskal.test(perc_trans_surf~site)$p.value,
    perc_par_no_snow_kw = kruskal.test(perc_par_no_snow~site)$p.value
  )

write_csv(kw_rslt, here('data/kw_result.csv'))

library(FSA)

dunnTest(
  data = tbl %>% filter(year == 2025),
  perc_par_no_snow~site,
  method = 'bonferroni'
)

# library(rstatix)
# test <- tbl %>% 
#   group_by(site, year) %>% 
#   get_summary_stats(type = 'median_iqr')
# tst
# test
