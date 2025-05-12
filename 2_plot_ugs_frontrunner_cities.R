# Plot the "frontrunners" or "best cities" for urban green space by climate zone and subregion 
rm(list=ls(all=TRUE)) # Removes all previously created variables

# Working directory [RStudio] -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
library(tidyverse)
# Source helper files and functions -----------------------------------------------
source("URGED/support/fcts_labelers_colors.R")
source("URGED/support/fcts_helpers_debug.R")
# Directories and settings ----------------------------
## Input
# path_data_citymean <- "results/scenarios/dfscenarios.rds" # These are the scenarios generated in 
path_ghsnames <- "results/ghs_subregion_Cls.rds" # Needed for merging with path_data_citymean
path_results <- "ugs/"
path_highestobs <- "ugs/dfhighestobs.rds"

# Load data
# df0 <- read_rds(path_data_citymean)
dfhighestobs <- read_rds(path_highestobs)
ghsnames <- read_rds(path_ghsnames)
# # Export to vector file (gpkg) so that results can be investigated in QGIS etc
# ## Convert to sf object
# sf_object <- sf::st_as_sf(dffrontrunners, coords = c("x", "y"), crs = 4326) # WGS84 coordinate system
# ## Export to a shapefile
# sf::st_write(sf_object, "ugs/frontrunners_10.gpkg", append = FALSE)

# Plot thhis dataframe
dfplot <- dfhighestobs %>%
  mutate(Clsmain = Cls_short,
         lcz = lcz_filter_v3)
dfplot <- fct_labeler_lcz(dfplot)
dfplot <- fct_labeler_Clsmain(dfplot)

# Plot for highest values
ggplot(data = dfplot,
       aes(x = Clsmain, y = lcz, fill = boundupr, label = round(boundupr,1))) +
  geom_tile() +
  # geom_text(aes(label = round(out_b_maxmean10, 2)), size = 2) +
  scale_fill_gradientn(colours = rev(terrain.colors(7)), limits = c(0,35), oob = scales::squish) +
  geom_text(size = 1.75) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "KGC Climate Zone",
       y = "LCZ",
       fill = "GVI\n90% perc.")
outname <- "results/scenarios/bestcities_byLCZ_KGC_90percentile.png"
ggsave(outname, width = 8, height = 7, units = "cm", bg = "white", dpi = 300)

# Plot for lowest values
ggplot(data = dfplot,
       aes(x = Clsmain, y = lcz, fill = boundlwr, label = round(boundlwr,1))) +
  geom_tile() +
  # geom_text(aes(label = round(out_b_maxmean10, 2)), size = 2) +
  scale_fill_gradientn(colours = rev(terrain.colors(7)), limits = c(0,35), oob = scales::squish) +
  geom_text(size = 1.75) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  labs(x = "KGC Climate Zone",
       y = "LCZ",
       fill = "GVI\n10% perc.")
outname <- "results/scenarios/bestcities_byLCZ_KGC_10percentile.png"
ggsave(outname, width = 8, height = 7, units = "cm", bg = "white", dpi = 300)

# May 2025: From here, the code now breaks. It is operating using df0, which is an old data.frame (dfscenarios.rds). Needs to be redone to redo these figures.
# Plot the names of frontrunners by KGC.
df_topcls <- merge(df0, ghsnames,
                   by.x = c("city", "country"),
                   by.y = c("UC_NM_MN", "CTR_MN_NM"), all.x = TRUE)
# # Test again. The merging worked!
# test <- df_topcls %>%
#   dplyr::select(city, country, CTR_MN_ISO, GRGN_L2) %>% distinct()

# Write this to a file for use in "2d_project_future_ugs.R
# write_csv(df_topcls, file = paste0(path_results, "df_topcls.csv"))
saveRDS(df_topcls, file = paste0(path_results, "df_topcls.rds"))


# Now constrain the projections by the "highest observed value" by Clsmain and LCZ
## The data for this is saved in dfhighestobs, which is the average of the 10 strongest points in each Clsmaina and lcz.
## We want to see whether in the scenarios any of the projections hit this maximum value


# Now plot the top cities by KGC and subregion using their name -------------------
df_topcls <- df_topcls %>%
  dplyr::filter(stats == "mean") %>%
  dplyr::select(city, GRGN_L1, GRGN_L2,  GVIm, Cls) %>%
  distinct() %>%
  drop_na(GVIm) %>%
  dplyr::group_by(Cls) %>%
  mutate(rankgvi_Cls = rank(-GVIm)) %>%
  dplyr::group_by(GRGN_L2) %>%
  mutate(rankgvi_L2 = rank(-GVIm)) %>%
  ungroup()

dfplot <- df_topcls %>%
  dplyr::filter(rankgvi_Cls <= 3 | rankgvi_L2 <= 3) # Keep the top three
dfplot <- fct_labeler_Cls(dfplot)

ggplot(data = dfplot, aes(x = GRGN_L2, y = GVIm, color = Cls)) +
  geom_text(aes(label = city), nudge_y = 0.1) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 3 cities by Sub-region",
       y = "Green View Index",
       color = "KGC Climate Zone")
ggsave(paste0("results/scenarios/top3cities_bySubregion.png"), width = 30, height = 20, units = "cm", bg = "white", dpi = 300)

ggplot(data = dfplot, aes(x = Cls, y = GVIm, color = GRGN_L2)) +
  geom_text(aes(label = city)) +
  theme_minimal(base_size = 9) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 3 cities by KGC climate zone",
       x = "KGC Climate Zone",
       y = "Green View Index",
       color = "Sub-region")
ggsave(paste0("results/scenarios/top3cities_byKGC.png"), width = 25, height = 14, units = "cm", bg = "white", dpi = 300)
