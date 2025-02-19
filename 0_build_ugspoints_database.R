# Build a database for a datframe containing ugs points for all 180 cities
# Determine the "frontrunners" or "best cities" for urban green space by climate zone and subregion 
rm(list=ls(all=TRUE)) # Removes all previously created variables
# Working directory [RStudio] -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
# Libraries etc ----------------------------
library(rstudioapi)
library(conflicted)
library(tidyverse)
library(terra) # For the LCZ data inclusion
# Directories and settings ----------------------------
## Input
path_ugs_citynames <- "ugs/after_points_030624_citynames.rds" # Citynames added in 1b_
path_ghsnames <- "results/ghs_subregion_Cls.rds"
path_lcz <- "climate/lcz/lcz_filter_v3.tif"
## Output
pathout <- "ugs/after_points_030624_completedatabase.rds"

# Load data
dfugs <- read_rds(path_ugs_citynames)
# Add names from GHS and KGC Cls
ghsnames <- read_rds(path_ghsnames)

lcz <- terra::rast(path_lcz)
# Get the LCZ for each point -------------
## Convert the dfugs to a SpatVector
dfugspoints <- terra::vect(dfugs, geom = c("x", "y"), crs = crs(lcz))
# Extract land use for each point
lcz_per_point <- terra::extract(lcz, dfugspoints)

# Merge the extracted land use with the original data.frame
dfpointslcz <- cbind(dfugs, lcz_per_point)
dfpointslcz <- dfpointslcz %>%
  dplyr::filter(city != "N/A")

# Now add GHS names (dataframe ghsnames which also has KGC). Merge by cityname AND country!!!
## If merging only by cityname, 19 duplicates exist. In that case, dfpoints has 8,518,383 entries, but the merged version has 9,319,305 entries. Suzhou is duplicated in GHS (there are two cities with that name, but also in dfpoints, the one around 120-31 degrees, close to Shanghai)! We therefore remove the "wrong" Suzhou from dfpoints.
dfpoints <- merge(dfpointslcz, ghsnames %>% dplyr::filter(ID_HDC_G0 != 11632),
                  by.x = c("city", "country"),
                  by.y = c("UC_NM_MN", "CTR_MN_NM"),
                  all.x = TRUE)

# Save the merged data
saveRDS(dfpoints, pathout)