rm(list=ls(all=TRUE)) # Removes all previously created variables 
gc()
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(pbapply)
library(matrixStats)
library(lubridate)
library(terra)
library(rstudioapi)

##
# Working directory -------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Paths -------------------------------------------------------------------
path_cities <- paste0(stub0, "results/cities_database_climatezones.gpkg")
path_provide <- paste0(stub0, "climate/provide_urban_climate_data/")
path_gvi <- paste0(stub0, "ugs/after_points_100425.Rdata")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

out_ndvi_m <- read_rds(paste0(stub0, "implementation/climate/provide_urban_climate_data/data_provide_cdh_gvi_143cities.rds"))

#

out_ndvi_m <- filter(out_ndvi_m, city=="Athens")

out_ndvi_m <- st_as_sf(out_ndvi_m, coords = c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(15) %>% st_transform(4326)

#

pop <- rast("implementation/hrsl_pop/grc_general_2020.tif")

out_ndvi_m$pop <- exact_extract(pop, out_ndvi_m, "mean")

###

out_ndvi_m <- filter(out_ndvi_m, pop>summary( out_ndvi_m$pop)[5] &  (pop < (summary(out_ndvi_m$pop)[5] + 0.1*(summary(out_ndvi_m$pop)[5]))))

# Calculate the 1st quantile
quantile_value <- quantile(out_ndvi_m$out_b_mean, probs = 0.25)

# Find observations at the 1st quantile boundary
observations_at_quantile <- out_ndvi_m[which.min(abs(out_ndvi_m$out_b_mean - quantile_value)), ]

library(googleway)

key <- "AIzaSyABAy4zgNg6P41_RlFIEa0JBo9W8o4866g"

google_streetview(location = c(observations_at_quantile$y, observations_at_quantile$x),
                  size = c(1200,1200),
                  output = "plot",
                  heading = 180,
                  fov = 120,
                  pitch = 0,
                  response_check = FALSE,
                  key = key)


dev.print(pdf, paste0('athens_q1_gvi_', round(observations_at_quantile$out_b_mean, 0), '.pdf'))

###

# Calculate the 1st quantile
quantile_value <- quantile(out_ndvi_m$out_b_mean, probs = 0.75)

# Find observations at the 1st quantile boundary
observations_at_quantile <- out_ndvi_m[which.min(abs(out_ndvi_m$out_b_mean - quantile_value)), ]

library(googleway)

key <- "AIzaSyABAy4zgNg6P41_RlFIEa0JBo9W8o4866g"

google_streetview(location = c(observations_at_quantile$y, observations_at_quantile$x),
                  size = c(1200,1200),
                  output = "plot",
                  heading = 180,
                  fov = 120,
                  pitch = 0,
                  response_check = FALSE,
                  key = key)



dev.print(pdf, paste0('athens_q3_gvi_', round(observations_at_quantile$out_b_mean, 0), '.pdf'))

####

setwd(paste0(stub0, "/URGED"))

