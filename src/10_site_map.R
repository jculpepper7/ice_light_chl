library(tidyverse)
library(here)
library(patchwork)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

#Require the developer version to get the ne_states() function to work.
devtools::install_github("ropenscilabs/rnaturalearth")
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")


#Load map data
na <- rnaturalearth::ne_states(
  returnclass = "sf") 

#Create df with lat/long for Paint and Kempenfelt Bay

lake <- c('Paint Lake', 'Kempenfelt Bay')
lat <- c(45.216048, 44.39815)
lon <- c(-78.944673, -79.57249)

lakes <- tibble(
  lake, lat, lon
)
# Map ---------------------------------------------------------------------

ggplot() +
  ggplot2::geom_sf(data = na) +
  coord_sf(
    xlim = c(-75, -85), 
    ylim = c(41, 48), 
    expand = FALSE
  )+
  geom_point(
    data = lakes, 
    aes(x = lon, y = lat), 
    size = 8, 
    pch =21, 
    stroke = 0.5,
    alpha = 0.6,
    fill = 'red'
  )+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()+
  theme(
    text = element_text(
      size = 50
    )
  )

ggsave(
  here('output/data_viz/ice_qual_maps.2025.06.22.png'),
  dpi = 300,
  width = 10,
  height = 10,
  units = 'in'
)
