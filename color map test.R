##############
#KB24----
##############

test <- rbr %>% 
  filter(
    site == 'Kempenfelt Bay',
    year == 2024
  )

max(test$temp) #1.24
min(test$temp) #0.545

######################################

max(test$do_mgL) #10.6
min(test$do_mgL) #9.83

######################################

max(test$chl_a) #1.76
min(test$chl_a) #0.370


######################################
#KB25----
test <- rbr %>% 
  filter(
    site == 'Kempenfelt Bay',
    year == 2025
  )

max(test$temp) #2.29
min(test$temp) #0.0239

######################################

max(test$do_mgL) #10.5
min(test$do_mgL) #6.78

######################################

max(test$chl_a) #3.03
min(test$chl_a) #0.232


######################################

################
#PLD24----
################

test <- rbr %>% 
  filter(
    site == 'Paint Lake - Deep',
    year == 2024
  )

max(test$temp) #2.71
min(test$temp) #1.10

######################################

max(test$do_mgL) #10.6
min(test$do_mgL) #6.48

######################################

max(test$chl_a) #4.77
min(test$chl_a) #1.00


######################################
#PLD25----
test <- rbr %>% 
  filter(
    site == 'Paint Lake - Deep',
    year == 2025
  )

max(test$temp) #3.73
min(test$temp) #0.908

######################################

max(test$do_mgL) #11.2
min(test$do_mgL) #5.19

######################################

max(test$chl_a) #3.20
min(test$chl_a) #0.838


######################################

################
#PLS24----
################

test <- rbr %>% 
  filter(
    site == 'Paint Lake - Shallow',
    year == 2024
  )

max(test$temp) #5.00
min(test$temp) #1.28

######################################

max(test$do_mgL) #9.43
min(test$do_mgL) #4.60

######################################

max(test$chl_a) #3.77
min(test$chl_a) #1.09


######################################

test <- rbr %>% 
  filter(
    site == 'Paint Lake - Shallow',
    year == 2025
  )

max(test$temp) #5.93
min(test$temp) #0.479

######################################

max(test$do_mgL) #10.8
min(test$do_mgL) #3.41

######################################

max(test$chl_a) #3.12
min(test$chl_a) #0.735


######################################


library(ggplot2)
mtcars$qsec <- mtcars$qsec-21
sp2<-ggplot(mtcars, aes(x=wt, y=mpg, color=qsec)) + geom_point()
midpoint <- 0
sp2+scale_color_gradientn( colours = c("red","white","blue"),
                           values=c(1.0, (0-min(mtcars$qsec))/(max(mtcars$qsec)-min(mtcars$qsec)),0))
