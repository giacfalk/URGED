# Create csv files with
## a) all 180 cities in the analysis;
## b) the 142 PROVIDE cities
## Other quantities such as elevation is added here.
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] --------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
library(tidyverse)
library(sf)

# Input paths
path_dictionary <- "cities/dictionary_GHS-provide.csv" # A dictionary for names for provide and GHS
path_list_cities_provide <- "climate/provide_urban_climate_data/cities_provide.csv" # List of PROVIDE cities

# Get the GHS data set
ghs <- read_sf(paste0("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"))
ghs <- sf::st_drop_geometry(ghs)
ghs_subregion <- readRDS("results/ghs_subregion_Cls.rds")
list_cities_provide <- read_csv(path_list_cities_provide)
dfpoints <- read_rds("ugs/after_points_100425_completedatabase.rds")
# We want to create a data.frame that contains the GHS IDs for the provide cities, so that in future we can easily grab variables from the GHS data set
# For this, we use the dfpoints data set, which contains the GHS ID for each point, and we strip it off the points information
dfpoints <- dfpoints %>%
  dplyr::select(-out_b, -x, -y, -lcz_filter_v3, -ID, -year) %>% # ID is the point ID
  distinct()

elevation <- ghs %>%
  dplyr::select(ID_HDC_G0, EL_AV_ALS) %>%
  dplyr::filter(ID_HDC_G0 %in% dfpoints$ID_HDC_G0)

cities_urged <- merge(dfpoints, elevation, by = "ID_HDC_G0", all = TRUE)

# Export to .csv
write_csv(cities_urged, "cities/cities_urged+GHS.csv")
