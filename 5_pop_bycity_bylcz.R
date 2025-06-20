
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub <- paste0(getwd(), "/")
library(tidyverse)
# Source helper files and functions ---------------------------------------
source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found
### 

library(exactextractr)
library(terra)
library(sf)

sf::sf_use_s2(F)

pop <- rast("climate/lcz/lcz_filter_v3.tif")

# Load data
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

# Prepare a list to store results
city_lcz_polygons <- list()

# Loop over each city
for (i in (1:nrow(cities))[-c(261, 281)]) {
  
  print(cities$UC_NM_MN[i])
  
  city <- cities[i,]
  
  # Crop raster to city extent with a small buffer
  cropped_raster <- crop(pop, vect(city))
  masked_raster <- mask(cropped_raster, vect(city))
  
  # Convert raster to polygons
  poly_lcz <- as.polygons(masked_raster)
  poly_lcz <- st_as_sf(poly_lcz)
  
  # Intersect with city to clip precisely to boundary
  poly_lcz_clipped <- st_intersection(poly_lcz, city)
  
  # Add city ID or name
  poly_lcz_clipped$city_id <- city$UC_NM_MN  # or another identifier
  
  # Store result
  city_lcz_polygons[[i]] <- poly_lcz_clipped
}

# Combine all into a single sf object
all_lcz_polygons <- bind_rows(city_lcz_polygons)

# Optional: clean up
all_lcz_polygons <- all_lcz_polygons %>%
  rename(lcz = lcz_filter_v3)  # or adjust based on raster layer name

###############

setwd(stub)

pop <- rast("socioecon/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

all_lcz_polygons$pop_2020 <- exact_extract(pop, all_lcz_polygons, "sum")

pop_2030 <- rast("socioecon/GHS_POP_E2030_GLOBE_R2023A_4326_3ss_V1_0.tif")

all_lcz_polygons$pop_2030 <- exact_extract(pop_2030, all_lcz_polygons, "sum")

###

all_lcz_polygons$gr_rate_2050 <- all_lcz_polygons$pop_2030 / all_lcz_polygons$pop_2020

all_lcz_polygons$gr_rate_2050[is.na(all_lcz_polygons$gr_rate_2050)] <- 1
all_lcz_polygons$gr_rate_2050[is.infinite(all_lcz_polygons$gr_rate_2050)] <- 1
all_lcz_polygons$gr_rate_2050[all_lcz_polygons$gr_rate_2050>3] <- 3

all_lcz_polygons$pop_2050 <- all_lcz_polygons$pop_2020 * all_lcz_polygons$gr_rate_2050^3

###

all_lcz_polygons$geometry <- NULL

all_lcz_polygons <- all_lcz_polygons %>% dplyr::select(UC_NM_MN, lcz, pop_2020, pop_2030, pop_2050, gr_rate_2050)

##

all_lcz_polygons <- all_lcz_polygons %>% dplyr::group_by(UC_NM_MN, lcz) %>% dplyr::summarise(pop_2020=sum(pop_2020, na.rm=T), pop_2030=sum(pop_2030, na.rm=T), pop_2050=sum(pop_2050, na.rm=T))

write.csv(all_lcz_polygons, "cities/cities_pop_bycity_bylcz_2020_2050.csv")

