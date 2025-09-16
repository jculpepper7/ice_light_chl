#The goal of this script is to combine data
#convert data to z-scores
#run a PCA


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(vegan)


# 2. Import data ----------------------------------------------------------

ice_par_chl <- read_csv(here('data/combined_data/chla_par_clean.csv'))

chem <- read_csv(here('data/combined_data/chem_clean.csv'))

rbr <- read_csv(here('data/combined_data/rbr_processed.csv'))

# 3. Modify data to join --------------------------------------------------


# **3a. Clean Ice, PAR, chl-a data ----------------------------------------

#IPC = Ice, PAR, chl-a combined data
ipc_clean <- ice_par_chl %>% 
  #Change site and year to factors in order to join the dataframes
  mutate(
    site = as.factor(site),
    year = as.factor(year)
  ) %>% 
  #eliminate the individual depth measurements
  select(
    #Original values selected
    # 1:22, 
    # 72:77,
    #Updated version - 2025.09.14
    1:8,
    10,11,13,15,
    18:21,
    72, 74:77,
    #rename total chl-a to give measurement
    chla_conc_ugL = total_conc_ug_l
  )

# write_csv(ipc_clean, here('data/clean_data/ipc_clean.csv'))

# **3b. Clean chem data --------------------------------------------------

chem_clean <- chem %>% 
  select(
    1:24
  ) %>% 
  mutate(
    site = as.factor(
      if_else(
        lake == 'Lake Simcoe', 'simcoe.deep',
        if_else(
          lake == 'Paint West', 'paint.deep', 'paint.shallow'
        )
      )
    ),
    year = as.factor(
      year(date)
    )
  ) %>% 
  select(
    -lake
  )

# write_csv(chem_clean, here('data/clean_data/chem_clean.csv'))


# **3c. Clean RBR data  ---------------------------------------------------

rbr_clean <- rbr %>% 
  mutate(
    site = as.factor(site),
    year = as.factor(year),
    position = as.factor(position)
  ) %>% 
  group_by(site, date) %>% 
  pivot_wider(
    names_from = c(position),
    values_from = c(
      mean_temp,
      mean_do,
      mean_do_sat,
      mean_par_up,
      mean_chl_a
    )
  )

# write_csv(rbr_clean, here('data/clean_data/rbr_clean.csv'))


# 4. Filter clean data for PCA --------------------------------------------


# **4a. IPC PCA -----------------------------------------------------------

ipc_pca_df <- ipc_clean %>% 
  select(
    date,
    site,
    year,
    ice_sheet_cm, 
    snow_avg_cm,
    par_trans,
    # par_trans_no_snow,
    blk_ratio,
    wht_ratio,
    #chla_conc_ugL,
    max_chl_depth
  )


# **4b. Chem PCA ----------------------------------------------------------

chem_pca <- chem_clean %>% 
  select(
    site,
    date,
    year,
    carbon_dissolved_inorganic,
    carbon_dissolved_organic,
    nitrogen_total,
    phosphorus_total
  )


# **4c. RBR PCA -----------------------------------------------------------

#Isolate the average full water column values
rbr_pca_full <- rbr_clean %>% 
  select(
    site,
    date,
    year,
    mean_temp_full,
    mean_do_full,
    mean_do_sat_full,
    #mean_par_up_full,
    #ADD RBR chl-a?
    mean_chl_a_full
  )

# #Isolate surface measurments
# rbr_pca_top <- rbr_clean %>% 
#   select(
#     site,
#     date,
#     year,
#     mean_temp_top,
#     mean_do_top,
#     #mean_do_sat_top,
#     mean_par_up_top
#   )
# 
# 
# #Isolate the bottom water values
# rbr_pca_bottom <- rbr_clean %>% 
#   select(
#     site,
#     date,
#     year,
#     mean_temp_bottom,
#     mean_do_bottom,
#     #mean_do_sat_bottom,
#     mean_par_up_bottom
#   )

# 5. Combine dataframes ---------------------------------------------------

#BELOW IS INITIAL PCA THAT WORKED, SO I'M SAVING IT FOR NOW.----
# pca_prep <- ipc_clean %>% 
#   full_join(chem_clean) %>% 
#   full_join(rbr_clean) %>% 
#   arrange(site, date) %>% 
#   #There are so many missing values in the chem data
#   #that I need to eliminate them for the PCA
#   #because the PCA can't handle NAs
#   #I'll figure out how to fix them later.
#   select(
#     #Need to remove bio variables
#     1:2,4:28
#   ) %>% 
#   na.omit() %>% 
#   #Need to remove non-numeric columns
#   #Combining site name and date, then moving them to row names
#   unite(
#     site_date, c(site, date), sep = '_', remove = F
#   ) %>% 
#   distinct(
#     site_date, .keep_all = T
#   ) 

#**PCA Prep new----
pca_prep <- ipc_pca_df %>% 
  #full_join(chem_pca) %>% #LEAVING OUT FOR NOW. 
                           #TOO MANY MISSING DATA POINTS FOR SIMCOE
  full_join(rbr_pca_full) %>% 
  arrange(site, date) %>% 
  na.omit() %>% 
  #Need to remove non-numeric columns
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(site, date), sep = '_', remove = F
  ) %>% 
  distinct(
    site_date, .keep_all = T
  ) 

pca_df <- pca_prep %>% 
  #Note: Including year within the PCA
  mutate(
    year = as.numeric(year)
  ) %>% 
  select(
    -c(site, date) #, year
  ) %>% 
  column_to_rownames(var = 'site_date')

# Normalize the variables
df_standard <- decostand(pca_df,"standardize")
# Calculate the PCA
ipc_pca <- rda(df_standard)
summary(ipc_pca)  
plot(ipc_pca)

# Plots using biplot.rda
# Crappy plots in both scalings
# Not for presentation but can be used to quickly evaluate the nicer plots prepared below.
dev.new(width = 12,
        height = 6,
        title = "PCA biplots - Physical Variables - biplot.rda", 
        noRStudioGD = FALSE
)
par(mfrow = c(1, 2))
biplot(ipc_pca, scaling = 1, main = "PCA - scaling 1")
biplot(ipc_pca, main = "PCA - scaling 2")

# Extract site scores on 1st two PC axes along with metadata
# Remember to specify scaling (1 for variables, 2 for sites)
site_scores <- tibble(as.data.frame(
  scores(ipc_pca,
         choices=c(1,2),
         display="sites", 
         scaling = 2))[,1:2]) |>
  mutate(site_date = row.names(pca_df)) |>
  left_join(select(
    pca_prep, site_date, site, date)) |>
    #all_data, sample_id, collection_site, collect_date, layer)) |>
  unique() %>% 
  mutate(
    year = as.factor(year(date))
  )

par_scores <- tibble(as.data.frame(
  scores(ipc_pca,
         choices=c(1,2),
         display="species", 
         scaling = 2))[,1:2]) |>
  mutate(variable = colnames(pca_df))

ggplot(par_scores, aes(x = PC1, y = PC2)) +
  # Draw axes along the zero intercept
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  # Draw arrows representing the variables
  geom_segment(aes(
    x = 0,
    y = 0,
    xend = PC1,
    yend = PC2),
    arrow = arrow(length = unit(0.1, "inches"))) +
  # Label the arrows
  # geom_text(aes(label = variable),
  #           nudge_x = 0.08,
  #           nudge_y = 0.1) +
  geom_text_repel(
    aes(x = PC1, y = PC2, label = variable),
    box.padding = unit(0.5, 'lines'),
    point.padding = unit(0.5, 'lines'),
    segment.color = NA
  )+
  # Draw points representing the sites
  geom_point(data = site_scores,
             aes(x = PC1,
                 y = PC2,
                 colour = site,
                 shape = year),
             size = 3,
             alpha = 0.7) +
  scale_color_wa_d(
    palette = 'rainier',
    which = c('lake', 'lodge', 'ground'),
    labels = c('Paint Lake (Deep)','Paint Lake (Shallow)','Kempenfelt Bay')
  )+
  theme_classic() +
  theme(
    axis.line = element_blank(),
    legend.title = element_blank(),
    legend.position = 'bottom'
  ) +
  # ggtitle("Principal Component Analysis, scaling 2")+
  xlab(paste0("PCA1 (51%)"))+ 
  ylab(paste0("PCA2 (22%)"))

ggsave(
  here('output/data_viz/pca_wyear_2025.09.14.pdf'),
  dpi = 300,
  width = 8,
  height = 8,
  units = 'in'
)
