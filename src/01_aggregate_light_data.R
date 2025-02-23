#The goal of this script is to aggregate light data for analysis


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)

# 1. Import data ----------------------------------------------------------

light <- readxl::read_xlsx(here('data/light/light_2024.xlsx'), sheet = 4)
