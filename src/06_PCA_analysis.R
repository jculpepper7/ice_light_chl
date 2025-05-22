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
    1:22, 
    72:77,
    #rename total chl-a to give measurement
    chla_conc_ugL = total_conc_ug_l
  )


# #**3b. Clean chem data --------------------------------------------------

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

# 3. Combine dataframes ---------------------------------------------------


pca_prep <- ipc_clean %>% 
  full_join(chem_clean) %>%
  arrange(site, date) %>% 
  #There are so many missing values in the chem data
  #that I need to eliminate them for the PCA
  #because the PCA can't handle NAs
  #I'll figure out how to fix them later.
  select(
    #Need to remove bio variables
    1:2,4:28
  ) %>% 
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
  select(
    -c(site, date)
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
  geom_text(aes(label = variable),
            nudge_x = -0.08,
            nudge_y = -0.1) +
  # Draw points representing the sites
  geom_point(data = site_scores,
             aes(x = PC1,
                 y = PC2,
                 colour = site,
                 shape = year),
             size = 3,
             alpha = 0.7) +
  theme_classic() +
  theme(
    axis.line = element_blank()) +
  ggtitle("Principal Component Analysis, scaling 2")

ggsave(
  here('output/data_viz/pca.png'),
  dpi = 300,
  width = 8,
  height = 8,
  units = 'in'
)
