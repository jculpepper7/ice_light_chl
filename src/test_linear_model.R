dt <- par_sum_wide %>% 
  mutate(
    par_log = log(par_trans),
    snow_log = log(snow_avg_cm),
    no_snow_log = log(par_trans_no_snow),
    wht_perc_log = log(wht_ratio),
    wht_log = log(wht_slush_cm)
  ) %>% 
  filter(
    par_log != -Inf,
    snow_log != -Inf,
    no_snow_log != -Inf,
    wht_perc_log != -Inf,
    wht_log != -Inf
  )

fit1 <- lm(data = dt, log(par_trans)~log(snow_avg_cm)) 
fit2 <- lm(data = dt, log(par_trans)~log(snow_avg_cm)+log(ice_sheet_cm))
fit3 <- lm(data = dt, log(par_trans)~log(snow_avg_cm)+log(ice_sheet_cm)+log(wht_ratio))

summary(fit1) #35%, p<0.01
summary(fit2) #42%, p<0.01
summary(fit3) #40%, p<0.01

#These tests are probably moot because none of the data are normal (by Shapiro-Wilk)
#Need to test with a non-parametric

ggdensity(dt$par_log)

# Fit the linear model
# fit <- lm(log_y ~ x, data = dt)

# Extract coefficients
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

# Calculate original parameters
a <- exp(intercept)
b <- -slope

# Print the results
print(paste("Estimated a:", a))
print(paste("Estimated b:", b))

# Create a function for the fitted exponential curve
fitted_curve <- function(x) a * exp(-b * x)

# Visualize the results
plot(dt$snow_avg_cm, dt$par_trans, main = "Exponential Decay Fit", xlab = "x", ylab = "y")
curve(fitted_curve, from = min(dt$snow_avg_cm), to = max(dt$par_trans), add = TRUE, col = "red")

# Optionally, add the equation to the plot
equation <- paste("y =", round(a, 2), "* exp(-", round(b, 2), "* x)")
text(min(dt$snow_avg_cm), max(dt$par_trans) * 0.9, equation, pos = 4, col = "red")
