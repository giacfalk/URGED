rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
library(tidyverse)
library(sf)
library(terra)

path_cities <- paste0(stub0, "results/cities_database_climatezones.gpkg")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

ghs <- read_sf(path_ghs_urbcentres) # Cities database
cities_cls <- read_sf(path_cities) # Cities database


# Read city data from WUP
df <- read_rds("data_provide_cdh_gvi_143cities_withcovariates.rds")
cols <- colnames(df)
cities <- df %>%
  select(city, x_s, y_s, Clsmain) %>%
  distinct()
