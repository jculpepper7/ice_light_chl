# Figures for June 26 2025 Lake Simcoe meeting  on
# Picnocline in Paint Lake East

library(plyr)
library(tidyverse)
library(lubridate)
library(ggh4x)
library(rLakeAnalyzer)
library(janitor)
library(viridis)
library(ggpubr)
library(RColorBrewer)
library(zoo)

# Function to isolate downcast data
rbr_cleanup <- function(df, vars){
  df |>
    # Remove soak period: prior to start of downcast (depth < 0)
    subset(depth > 0.5) |>
    # Add a date column: first one for plotting with just the calendar date
    mutate(
      date = paste(month(time),"/",day(time), sep = ""),
      # Then one with the full date including year
      full_date = paste(month(time), day(time), year(time), sep = "-")) |>
    # Remove upcast data
    group_by(site, date) |>
    mutate(cast = ifelse(time > time[which.max(depth)], "upcast", "downcast")) |>
    subset(cast == "downcast") |>
    # Remove extraneous variables
    select(site, date, full_date, depth, all_of(vars)) |>
    # Recode the site names 
    mutate(site = recode(site,
                         paint.shallow = "paint_east",
                         paint.deep = "paint_west",
                         simcoe.deep = "kempenfelt_bay"))
}

# Function to calculate averages of depth intervals
depth_intervals <- function(df, int, vars){
  df |>
    # Round depth up to nearest 0.1 m in a new column
    mutate(d_int =  round_any(depth, int)) |>
    # Calculate mean conductivity and sd for each depth interval
    group_by(site, full_date, d_int) |>
    summarize(
      across(vars, mean))
}

# Depth profiles figures

# Import data
rbr1_data_raw <- read_csv("rbr_25a.csv")
rbr2_data_raw <- read_csv("rbr_25b.csv")
rbr24_data_raw <- read_csv("rbr.csv")

# Isolate the downcasts and rename some columns
rbr24_clean <- rbr24_data_raw |>
  rbr_cleanup(vars = c("temp1","chl.a","do.conc","do.sat","par","sea.pressure")) |>
  rename("temp" = temp1,
         "chl_a" = chl.a,
         "do_mgl" = do.conc,
         "do_sat" = do.sat,
         "pressure" = sea.pressure)
rbr1_clean <- rbr1_data_raw |>
  rbr_cleanup(vars = c("temp1","chl.a","do.conc","do.sat","par","sea.pressure")) |>
  rename("temp" = temp1,
         "chl_a" = chl.a,
         "do_mgl" = do.conc,
         "do_sat" = do.sat,
         "pressure" = sea.pressure)
rbr2_clean <- rbr2_data_raw |>
  rbr_cleanup(vars = c("spec.cond")) |>
  rename("con" = spec.cond)


# Calculate depth intervals
rbr24_int <- depth_intervals(rbr24_clean,
                             int = 0.1,
                             vars = c("temp","chl_a","do_mgl","do_sat","par","pressure"))
rbr1_int <- depth_intervals(rbr1_clean, 
                            int = 0.1, 
                            vars = c("temp", "chl_a", "do_mgl", "do_sat", "par","pressure"))
rbr2_int <- depth_intervals(rbr2_clean,
                            int = 0.1,
                            vars = c("con"))

# Join the data sets
rbr_all_both_years <- inner_join(
  x = rbr1_int,
  y = rbr2_int,
  by = c("site", "full_date", "d_int")) |>
  rbind(rbr24_int)

# Temperature profile figures
rbr_all_both_years |>
  filter(site != "kempenfelt_bay") |>
  filter(site != "simcoe.shallow") |>
  filter(year(mdy(full_date)) == 2024) |>
  select(c(site, full_date, d_int, temp)) |>
  mutate(site = recode(site,
                       paint_east = "B) Paint East",
                       paint_west = "A) Paint West")) |>
  ggplot(aes(x = d_int, y = temp, colour = full_date)) +
  geom_point(alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  facet_grid(. ~ site, scales = "free_y") +
  ylab("Temperature (°C)") +
  xlab("Depth (m)") +
  labs(colour = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal(base_size = 20) +
  theme(panel.border = element_rect(fill = NA),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"))


ggsave("simcoe_meeting_figures/fig_1_depth_profiles.png",
       dpi = 300,
       width = 17,
       height = 12,
       units = "cm")