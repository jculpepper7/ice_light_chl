

max(par_sum_wide$wht_ratio)
test <- par_sum_wide %>% 
  filter(
    wht_ratio >=0.5
  )

mean(test$par_trans_no_snow, na.rm = T)

range(test$par_trans_no_snow)

#0-27.9
#mean = 13%
#range = 0 - 78%

#1-27.9
#mean = 6%
#range = 0 - 31%

#2-27.9
#mean = 4%
#range = 0 - 29%

#3-27.9
#mean = 4%
#range = 0 - 29%

#4-27.9
#mean = 4%
#range = 0 - 29%


ggplot()+
  geom_point(
    data = par_sum_wide, 
    mapping = aes(x = snow_avg_cm, y = par_trans),
  )+
  geom_smooth(
    data = par_sum_wide, 
    mapping = aes(x = snow_avg_cm, y = par_trans),
    se = FALSE, 
    method = "gam", 
    # formula = par_trans ~ s(snow_avg_cm)
  )



