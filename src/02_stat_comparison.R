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


# 2. Import data ----------------------------------------------------------

tbl <- read_csv(here('data/comparison_table.csv'))


# 3. Wilcoxon test across years -------------------------------------------


wlcx_rslt <- tbl %>% 
  group_by(site) %>% 
  summarise(
    ice_sheet_wt = wilcox.test(ice_sheet_cm~year, exact = F)$p.value,
    ice_sheet_wt = wilcox.test(ice_sheet_cm),
    total_ice_wt = wilcox.test(total_ice_cm~year, exact = F)$p.value,
    black_ice_wt = wilcox.test(black_ice_cm~year, exact = F)$p.value,
    white_ice_wt = wilcox.test(white_ice_cm~year, exact = F)$p.value,
    wht_slush_wt = wilcox.test(wht_slush_cm~year, exact = F)$p.value,
    n_white_wt = wilcox.test(n_white~year, exact = F)$p.value,
    n_slush_wt = wilcox.test(n_slush~year, exact = F)$p.value,
    snow_wt = wilcox.test(snow_avg_cm~year, exact = F)$p.value,
    perc_blk_tot_wt = wilcox.test(perc_blk_tot~year, exact = F)$p.value,
    perc_blk_sheet_wt = wilcox.test(perc_blk_sheet~year, exact = F)$p.value,
    perc_wht_tot_wt = wilcox.test(perc_wht_tot~year, exact = F)$p.value,
    perc_wht_sheet_wt = wilcox.test(perc_wht_sheet~year, exact = F)$p.value,
    perc_wht_slush_wt = wilcox.test(perc_wht_slush~year, exact = F)$p.value,
    perc_trans_air_wt = wilcox.test(perc_trans_air~year, exact = F)$p.value,
    perc_trans_surf_wt = wilcox.test(perc_trans_surf~year, exact = F)$p.value,
    perc_par_no_snow_wt = wilcox.test(perc_par_no_snow~year, exact = F)$p.value
  )

#write_csv(wlcx_rslt, here('data/wilcox_rslt.csv'))


# 4. KW test and Dunn post hoc --------------------------------------------

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

dunnTest(data = tbl %>% filter(year == 2025),
         perc_par_no_snow~site,
                     method = 'bonferroni')

library(rstatix)
test <- tbl %>% 
  group_by(site, year) %>% 
  get_summary_stats(type = 'median_iqr')
tst
test
