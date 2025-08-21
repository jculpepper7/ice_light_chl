target <- 4

test2 <- test %>% 
  mutate(
    date2 = as.factor(date)
  ) %>% 
  group_by(date, site, year) %>% 
  summarise(
    # min = min(temp)
    depth_4C = which.min(abs(temp-target))
  )
  

test3 <- test %>%
  na.omit() %>% 
  mutate(
    diff = abs(temp - target),
    yday_fac = as.factor(yday)
  ) %>% 
  group_by(
    site, yday_fac, year
  ) %>% 
  filter(diff == min(diff))

ggplot(test3, mapping = aes(x = yday, y = depth))+
  geom_line()+
  scale_y_reverse()+
  facet_wrap(~year)
  
rbr_test <- rbr %>%
  na.omit() %>%
  mutate(
    diff = abs(temp - 4),
    yday_fac = as.factor(yday)
  ) %>% 
  group_by(
    site, yday_fac, year
  ) %>% 
  mutate(
    z_temp4c = depth[diff == min(diff)]
  ) 
