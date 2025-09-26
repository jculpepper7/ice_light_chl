# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(wql)
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(patchwork)

# 2. Create a data set ----------------------------------------------------


# **2a. KB and PL lat/longs -----------------------------------------------

#Create a tibble consisting of the sampling points for each site

sample_pts <- tibble(
  lake = c(
    as.factor('kempenfelt_bay'),  #i.e., KB
    as.factor('paint_lake_s'), #i.e., PL
    as.factor('paint_lake_d')
  ),
  latitude = c(
    as.numeric(44.39911),
    as.numeric(45.22475),
    as.numeric(45.21507)
  ),
  longitude = c(
    as.numeric(-79.57023),
    as.numeric(-78.92809),
    as.numeric(-78.94580)
  ),
)

#Convert to an SF object for map plotting
lake_pts <- st_as_sf(
  sample_pts,
  coords = c('longitude', 'latitude'),
  crs = 4326,
  agr = 'constant'
) 

# **2b. HydroLAKES polygons for lake shapes -------------------------------

#Create a bounding box to reduce the size of HydroLAKES
#Otherwise, it's a bit too large of a file to work with easily

# Bounding box for southern Ontario
poly_wkt <- 'POLYGON ((
  -75.0 44.0,
  -85.0 44.0,
  -85.0 46.0,
  -75.0 46.0,
  -75.0 44.0
))'

#Read in data from only within the bounding box to limit memory use
test <- st_read(
  dsn = here('data/lake_polys/HydroLAKES_polys_v10.shp'),
  wkt_filter = poly_wkt
) %>% 
  filter(
    Hylak_id == 109230 | Hylak_id == 759 | Hylak_id == 7 | Hylak_id == 8
  ) %>% 
  mutate(
    Lake_name = if_else(
      is.na(Lake_name), 'Paint', Lake_name
    )
  )

# test <- filt_test %>% 
#   filter(
#     Hylak_id == 109230 | Hylak_id == 759 | Hylak_id == 7 | Hylak_id == 8
#   ) %>% 
#   mutate(
#     Lake_name = if_else(
#       is.na(Lake_name), 'Paint', Lake_name
#     )
#   )
# sf_object <- st_read(here('data/lake_polys/HydroLAKES_polys_v10.shp'))

# can_lakes <- sf_object %>% 
#   filter(
#     Country == 'Canada'
#   )
#Filter to just lakes in southern Ontario

# on_lakes <- can_lakes %>% 
#   filter(
#     Pour_long < -75 & Pour_long > -85,
#     Pour_lat > 40 & Pour_lat < 50
#   )

# 3. Map ------------------------------------------------------------------

#Create a map layered with:
#1. North American map
#2. HydroLAKES of sites (w/ Great Lakes for reference)
#3. Boxes around the sites for a zoom in of the sampling sites

# **3a. Set up map of North America ---------------------------------------

na <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf") %>%
  select(name, continent, geometry) %>%
  filter(continent == 'North America')


# **3b. Create boxes for zoom in ------------------------------------------

#KB rectanlge coords
xmin <-  -79.75 
xmax <-  -79.45 
ymin <-  44.35 
ymax <-  44.45

# Create a coordinate matrix for the polygon
rect_kb <- matrix(c(
  xmin, ymin,
  xmax, ymin,
  xmax, ymax,
  xmin, ymax,
  xmin, ymin # Close the polygon
), ncol = 2, byrow = TRUE)

#Create a simple feature geometry for KB
sfg_kb <- st_polygon(list(rect_kb)) 

# Assign the CRS from the NA object (i.e., WGS84 - EPSG:4326)
sfc_kb <- st_sfc(sfg_kb, crs = 4326)


#PL rectanlge coords
xmin <-  -78.98 
xmax <-  -78.9 
ymin <-  45.2 
ymax <-  45.24

# Create a coordinate matrix for the polygon
rect_pl <- matrix(c(
  xmin, ymin,
  xmax, ymin,
  xmax, ymax,
  xmin, ymax,
  xmin, ymin # Close the polygon
), ncol = 2, byrow = TRUE)

#Create a simple feature geometry for KB
sfg_pl <- st_polygon(list(rect_pl)) 

# Assign the CRS from the NA object (i.e., WGS84 - EPSG:4326)
sfc_pl <- st_sfc(sfg_pl, crs = 4326)

# **3c. Create large map --------------------------------------------------


full_map <- ggplot() +
  ggplot2::geom_sf(data = na, fill = 'forestgreen', alpha = 0.5) +
  ggplot2::geom_sf(data = test, fill = 'blue', color = 'grey25', alpha = 0.5) +
  ggplot2::geom_sf(data = sfc_kb, color = 'red', fill = NA, linewidth = 1)+
  ggplot2::geom_sf(data = sfc_pl, color = 'red', fill = NA, linewidth = 1)+
  theme_minimal()+
  coord_sf(xlim = c(-78.5, -81), ylim = c(44, 45.4), expand = FALSE)+
  annotation_north_arrow(
    location = 'br',
    which_north = 'true',
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering,
    pad_y = unit(0.7, 'cm'),
    pad_x = unit(0.9, 'cm')
  )+
  annotation_scale(
    location = "br", # "bl" for bottom left, other options: "tl", "tr", "br"
    width_hint = 0.2 # Scale bar width as a proportion of the map width
  )+
  scale_x_continuous(
    breaks = c(-80.5, -80.0, -79.5, -79.0)
  )+
  scale_y_continuous(
    breaks = c(44.2, 44.4, 44.6, 44.8, 45.0, 45.2)
  )

# full_map

#Save map
# ggsave(
#   here('output/data_viz/map/fig1.pdf'),
#   dpi = 300,
#   width = 6.5,
#   height = 5.5,
#   units = 'in',
#   bg = 'white'
# )


# **3d. Create KB map -----------------------------------------------------

kb_map <- ggplot() +
  ggplot2::geom_sf(data = na, fill = 'forestgreen', alpha = 0.5) +
  ggplot2::geom_sf(data = test, fill = 'blue', color = 'grey25', alpha = 0.5) +
  geom_sf(
    data = lake_pts, 
    colour = 'red',
    size = 5,
    alpha = 0.8, 
    shape = 17
  )+
  theme_minimal()+
  coord_sf(xlim = c(-79.54, -79.7), ylim = c(44.35, 44.45), expand = FALSE)+
  scale_x_continuous(
    breaks = seq(from = -79.68, to = -79.56, by = 0.04)
  )

# kb_map

#Save map
# ggsave(
#   here('output/data_viz/map/fig1a.pdf'),
#   dpi = 300,
#   width = 2.75,
#   height = 2.35,
#   units = 'in',
#   bg = 'white'
# )

# **3e. Create PL map -----------------------------------------------------

pl_map <- ggplot() +
  ggplot2::geom_sf(data = na, fill = 'forestgreen', alpha = 0.5) +
  ggplot2::geom_sf(data = test, fill = 'blue', color = 'grey25', alpha = 0.5) +
  geom_sf(
    data = lake_pts, 
    colour = 'red',
    size = 2,
    alpha = 0.8, 
    shape = 17
  )+
  theme_minimal()+
  coord_sf(xlim = c(-78.96, -78.92), ylim = c(45.205, 45.230), expand = FALSE)+
  scale_y_continuous(
    breaks = c(45.210, 45.215, 45.22, 45.225)
  )+
  scale_x_continuous(
    breaks = c(-78.955, -78.945, -78.935, -78.925)
  )

# pl_map

#Save map
# ggsave(
#   here('output/data_viz/map/fig1b.pdf'),
#   dpi = 300,
#   width = 2.75,
#   height = 2.35,
#   units = 'in',
#   bg = 'white'
# )
