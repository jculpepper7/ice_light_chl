#The goal of this script is to combine clean data
#test for collinearity
#combine data for RDA
#run a RDA
#use forward selection to improve model


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(vegan)


# 2. Import data ----------------------------------------------------------

ipc_clean <- read_csv(here('data/clean_data/ipc_clean.csv'))

chem_clean <- read_csv(here('data/clean_data/chem_clean.csv'))

rbr_clean <- read_csv(here('data/clean_data/rbr_clean.csv'))


# 3. Prepare clean data for RDA -------------------------------------------

# **3a. IPC RDA -----------------------------------------------------------

ipc_rda_prep <- ipc_clean %>% 
  select(
    date,
    site,
    year,
    ice_sheet_cm, 
    snow_avg_cm,
    par_trans,
    par_trans_no_snow,
    blk_ratio,
    wht_ratio,
    black_ice_cm,
    wht_slush_cm,
    chla_conc_ugL,
    max_chl_depth,
    #Include the species?
    # green_ug_l,
    # bluegreen_ug_l,
    # diatom_ug_l,
    # cryptophyta_ug_l
  ) %>% 
  na.omit() %>%
  #Need to remove non-numeric columns
  arrange(
    site, date
  ) 



# **3b. Chl-a df ----------------------------------------------------------

chl <- ipc_rda_prep %>% 
  select(
    site, date,
    chla_conc_ugL, max_chl_depth,
    #Include the species?
    # green_ug_l,
    # bluegreen_ug_l,
    # diatom_ug_l,
    # cryptophyta_ug_l
  ) %>% 
  arrange(
    site, date
  ) %>% 
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(site, date), sep = '_', remove = F
  ) %>%
  distinct(
    site_date, .keep_all = T
  )%>%
  select(
    -c(site, date)
  ) 


# **3c. Ice df ------------------------------------------------------------

ice <- ipc_rda_prep %>% 
  select(
    -c(chla_conc_ugL, max_chl_depth),
    #Include the species?
    #-c(green_ug_l,
      # bluegreen_ug_l,
      # diatom_ug_l,
      # cryptophyta_ug_l)
  ) %>% 
  arrange(
    site, date
  ) %>% 
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(site, date), sep = '_', remove = F
  ) %>%
  distinct(
    site_date, .keep_all = T
  )%>%
  select(
    -c(site, date, year)
  ) 


# **3d. Chem PCA ----------------------------------------------------------

chem_rda <- chem_clean %>% 
  select(
    site,
    date,
    year,
    carbon_dissolved_inorganic,
    carbon_dissolved_organic,
    nitrogen_total,
    phosphorus_total
  )


# **3e. RBR phys df -------------------------------------------------------

#Isolate the average full water column values
rbr_phys_prep <- rbr_clean %>% 
  select(
    site,
    date,
    year,
    mean_temp_full,
    mean_do_full,
    #mean_par_up_full, #exclude b/c column has negative values
    #mean_chl_a_full
  ) %>% 
  na.omit() %>%
  #Need to remove non-numeric columns
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(site, date), sep = '_', remove = F
  ) %>%
  distinct(
    site_date, .keep_all = T
  )%>%
  select(
    -c(site, date, year)
  )


# **3f. RBR chl-a df ------------------------------------------------------

rbr_chl_prep <- rbr_clean %>% 
  select(
    site,
    date,
    year,
    mean_chl_a_full
  ) %>% 
  na.omit() %>%
  #Need to remove non-numeric columns
  #Combining site name and date, then moving them to row names
  unite(
    site_date, c(site, date), sep = '_', remove = F
  ) %>%
  distinct(
    site_date, .keep_all = T
  )%>%
  select(
    -c(site, date, year)
  )

# 4. Combine dataframes ---------------------------------------------------


# **4a. Combine phys dfs --------------------------------------------------

rbr_phys <- rbr_phys_prep %>% 
  filter(site_date %in% ice$site_date)

phys_rda <- ice %>% 
  inner_join(rbr_phys)%>%
  column_to_rownames(var = 'site_date')
  

# **4b. Combine chl dfs ---------------------------------------------------

rbr_chl <- rbr_chl_prep %>% 
  filter(site_date %in% chl$site_date)

chl_rda <- chl %>% 
  inner_join(rbr_chl)%>%
  column_to_rownames(var = 'site_date')


# 5. Test for collinearity ------------------------------------------------

heatmap(abs(cor(phys_rda)), 
        # Compute pearson correlation (note they are absolute values)
        col = rev(heat.colors(6)), 
        Colv = NA, Rowv = NA)
legend("topright", 
       title = "Absolute Pearson R",
       legend =  round(seq(0,1, length.out = 6),1),
       inset=c(-0.2,0),
       y.intersp = 0.7, bty = "n",
       fill = rev(heat.colors(6)))

#Seems like the only strong multicollinearity is the black and white ice ratios
#I'm going to leave them in for now because they're of ecological significance.
#But maybe using the actual thickness measurements will be better? I'm uncertain for now.


# 6. Detrended correspondence analysis ------------------------------------


# **6a. DCA bio variables -------------------------------------------------

dca_chl <- decorana(chl_rda)
print(dca_chl)

#Axis length of DCA1 < 3, so trends are likely linear


# **6b. DCA phys variables ------------------------------------------------

dca_phys <- decorana(phys_rda)
print(dca_phys)

#Axis length of DCA1 < 3, so trends are likely linear

# 7. Normalize the variables ----------------------------------------------

phys_std <- decostand(phys_rda,'standardize')

chl_std <- decostand(chl_rda, 'standardize')

# 8. RDA ------------------------------------------------------------------

rda <- rda(chl_std ~ ., phys_std)
summary(rda)
#Results:
#Constrained: 0.7354 - ~74% variance explained
#Unconstrained: 0.2646 - ~26% unexpained
#i.e., The included environmental variables explain ~74% of the
#      variation in the chl-a concentration and peak depth across sites


# 9. Forward selection ---------------------------------------------------

#Using forward selection to simplify model (if possible)



# **9a. Fwd. sel. process ------------------------------------------------

fwd.sel <- ordiR2step(
  rda(chl_std ~ 1, phys_std), # lower model limit (simple)
  scope = formula(rda), # upper model limit (the "full" model)
  direction = "forward",
  R2scope = TRUE, # can't surpass the "full" model's R2
  pstep = 1000,
  trace = FALSE # change to TRUE to see the selection process
) 

#Check the new model with the forward selection process
fwd.sel$call
#New Model:
#chl_std ~ blk_ratio + wht_slush_cm + par_trans + ice_sheet_cm,
#data = phys_std


# **9b. New model ---------------------------------------------------------

#Write new model
chl_rda_signif <- rda(
  chl_std ~ blk_ratio + wht_slush_cm + par_trans + ice_sheet_cm, 
  data = phys_std
)


# **9c. Check var. exp. and significance ----------------------------------

#Variance explained

#Check the adjusted R2 (corrected for the number of
#explanatory variables)
RsquareAdj(chl_rda_signif) #Adj R2 = 0.516925 or ~ 52% variation

#Significance of forward selected model

anova.cca(chl_rda_signif, step = 1000) # p<0.001***

#Significance of each term within forward selected model

anova.cca(chl_rda_signif, step = 1000, by = 'term')

#Signif. by term:
#black ice ratio (%) (i.e., 'blk_ratio'): p<0.001***
#white ice and slush (cm) (i.e., 'wht_slush_cm'): p<0.002**
#PAR transmitted (%) (i.e., 'par_trans'): p<0.001***
#Ice sheet thickness (cm) (i.e., 'ice_sheet_cm'): p<0.004**


# 10. Basic visualization -------------------------------------------------

plot(chl_rda_signif,
     scaling = 2,
     display = c("sp","wa","bp"),
     type = "points")

ordiplot(chl_rda_signif, scaling = 1, type = "text")
ordiplot(chl_rda_signif, scaling = 2, type = "text")



# 11. Viz with ggplot -----------------------------------------------------

# Plot in ggplot by extracting scores using tidy_scores function

# Set the scaling factor (1 or 2)
scale_factor <- 2

## extract % explained by the first 2 axes
perc <- round(100*(summary(chl_rda_signif)$cont$importance[2, 1:2]), 2)

# Extract the conversion factor for environmental variables for plotting
# This adjustment was automatically performed in the vegan plotting function
# To remove it just set plot_factor to 1.
rda_scores <- scores(chl_rda_signif, display = "all", scaling = scale_factor)
plot_factor <- attr(rda_scores,"const")

# Extract the scores
tidy_scores <- scores(chl_rda_signif,
                      scaling = scale_factor,
                      tidy = TRUE)

# Split up the scores to be plotted
# Also here set up any factors for colour/shapes
site_scores <- tidy_scores |>
  filter(score == "sites") |>
  separate_wider_delim(
    cols = 'label', delim = '_', names = c('label1', 'label2')
  ) |>
  mutate(
    site = ifelse(grepl("simcoe.deep",label1),"Kempenfelt Bay",
                  ifelse(grepl("paint.shallow",label1),"Paint Shallow",
                         ifelse(grepl("paint.deep",label1),"Paint Deep",NA))),
    year = ifelse(grepl("2024",label2),"2024","2025"))
chl_scores <- tidy_scores |>
  filter(score == "species")
phys_scores <- tidy_scores |>
  filter(score == "biplot")

ggplot(tidy_scores, aes(x = RDA1, y = RDA2)) +
  geom_hline(yintercept = 0, col = "grey50") +
  geom_vline(xintercept = 0, col = "grey50") +
  geom_segment(phys_scores,
               mapping = aes(x = 0,
                             y = 0,
                             xend = RDA1*plot_factor,
                             yend = RDA2*plot_factor),
               arrow = arrow(length = unit(0.1, "inches")),
               colour = "#117733",
               linewidth = 1.5,
               alpha = 1) +
  geom_point(site_scores,
             mapping = aes(x = RDA1,
                           y = RDA2,
                           colour = site,
                           shape = year),
             size = 5,
             alpha = 0.7) +
  scale_color_manual(
    values = c('#44AA99','#DDCC77','#CC6677')
  ) +
  geom_text(chl_scores,
            mapping = aes(x = RDA1,
                          y = RDA2,
                          label = label),
            colour = "#332288") +
  geom_text(phys_scores,
            mapping = aes(label = label,
                          x = RDA1*plot_factor + ifelse(RDA1>0,0.2,-0.2),
                          y = RDA2*plot_factor + ifelse(RDA2>0,0.2,-0.2)),
            colour = "#117733") +
  theme_classic()+
  xlab(paste0("RDA1 (", perc[1], "%)"))+ 
  ylab(paste0("RDA2 (", perc[2], "%)"))+
  theme(
    legend.title = element_blank()
  )

# ggsave(
#   here('output/rda_viz/rda_2025.06.06.png'),
#   dpi = 300,
#   width = 8,
#   height = 5,
#   units = 'in'
# )


