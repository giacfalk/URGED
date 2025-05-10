
# Build a database for a datframe containing ugs points for all 180 cities
# Determine the "frontrunners" or "best cities" for urban green space by climate zone and subregion 
rm(list=ls(all=TRUE)) # Removes all previously created variables
# Working directory [RStudio] -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Define a variable with the current WD

# Libraries etc ----------------------------
library(rstudioapi)
library(conflicted)
library(tidyverse)
library(terra) # For the LCZ data inclusion
# Directories and settings ----------------------------
## Input
path_ugs_citynames <- "ugs/after_points_predict_100425.Rds" # Citynames added in 1b_

out_ndvi_m <- read_rds(path_ugs_citynames)

###

setwd("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking")

cities <- read_sf("data/validation/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")
cities <- cities %>% group_by(GRGN_L1) %>% slice_max(P15, n = 50)

cities_provide <- read_sf("data/validation/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

dict <- basename(list.dirs("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/climatechange/future_deltas", recursive = F))

cities_provide <- read_sf("data/validation/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

library(stringdist)

# Compute the string distance matrix
dist_matrix <- stringdistmatrix(dict, cities_provide$UC_NM_MN, method = "lv")  # "lv" = Levenshtein distance

# Find the closest match for each string in vec1
closest_matches <- apply(dist_matrix, 1, function(row) cities_provide$UC_NM_MN[which.min(row)])

# Combine results
matches <- data.frame(original = dict, closest_match = closest_matches)

matches$closest_match[3] <- "Alacant / Alicante"
matches$closest_match[50] <- "Ho Chi Minh City"
matches$closest_match[53] <- "Rawalpindi [Islamabad]"
matches$closest_match[111] <- "Rotterdam [The Hague]"
matches <- matches[-c(67,133),] 

cities_provide <- dplyr::filter(cities_provide, UC_NM_MN %in% matches$closest_match)
cities_provide <- group_by(cities_provide, UC_NM_MN) %>% slice_max(P15, n = 1, with_ties = FALSE) %>% 
  ungroup()

cities <- bind_rows(cities, cities_provide)
cities <- unique(cities)

out_ndvi_m$city <- cities$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]

###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location

write_rds(out_ndvi_m, paste0(stub0, "ugs/after_points_100425_citynames.rds"))

