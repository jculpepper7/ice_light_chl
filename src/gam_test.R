test <- par_sum_wide %>% 
  filter(
    site == 'paint.shallow',
    #year == 2025
)
mean(test$par_trans)
mean(test$snow_avg_cm)
range(test$par_trans)

mean(par_sum_wide$par_trans)
n(par_sum_wide$par_trans)

ggplot(data = par_sum_wide)+
  geom_point(
    mapping = aes(y = ice_sheet_cm, x = wht_slush_cm, color = site, shape = year)
  )+
  geom_smooth(
    mapping = aes(y = ice_sheet_cm, x = wht_slush_cm),
    method = 'gam'
  )

#Linear model----
lm.fit <- lm(data = par_sum_wide , formula = ice_sheet_cm~wht_slush_cm) #Probably not appropriate. Will test resid below.
summary(lm.fit)

#GAM total ice and white ice----
library(mgcv) #for GAM

gam.fit <- gam(data = par_sum_wide, formula = ice_sheet_cm~s(wht_slush_cm))
summary(gam.fit)

gam.thick <- gam(data = par_sum_wide, formula = par_trans~s(ice_sheet_cm))
summary(gam.thick)

gam.snow <- gam(data = par_sum_wide, formula = par_trans~s(snow_avg_cm))
gam.wht <- gam(data = par_sum_wide, formula = par_trans~s(wht_ratio))
gam.wht_snow <- gam(data = par_sum_wide, formula = par_trans~s(wht_ratio)+s(snow_avg_cm))

summary(gam.snow)
summary(gam.wht)
summary(gam.wht_snow)

anova(gam.snow)
anova(gam.wht)
anova(gam.wht_snow)

# plot(gam.wht)
# plot(gam.thick)

#Test resid of LM for normality----
plot(lm.fit) #QQ plot looks off, deviation at ends and in center
shapiro.test(resid(lm.fit)) #p.val < 0.05, thus resid not normal. lm not appropriate.

#Test LM against GAM
anova(lm.fit, gam.fit, test = 'F') #GAM offers better model. LM inappropriate anyway.


x <- ggplot(data = par_sum_wide)+
  geom_point(
    mapping = aes(y = ice_sheet_cm, x = wht_ratio, color = site, shape = year),
    size = 5,
    alpha = 0.7
  )+
  theme_classic()+
  # scale_x_log10()+
  # scale_y_log10()+
  facet_wrap(
    ~site+year, 
    scales = 'free',
    ncol = 2
  )
x
ggplotly(x)
  geom_smooth(
    mapping = aes(x = snow_avg_cm, y = par_trans),
    formula = par_trans~snow_avg_cm,
    method = 'lm'
  )
par_sum_wide$par_trans


test <- par_sum_wide %>% 
  select(
    site, year, date,
    snow_avg_cm,
    wht_ratio, blk_ratio,
    par_trans, par_trans_no_snow
  ) %>% 
  filter(
    wht_ratio > 0.10
  ) %>% 
  arrange(site,year, desc(par_trans_no_snow)) 
  

mean(test$par_trans_no_snow)

x <- ggplot(data = test)+ 
  geom_point(
    mapping = aes(x = wht_ratio, y = par_trans_no_snow, color = site, shape = year)
  )
ggplotly(x)
