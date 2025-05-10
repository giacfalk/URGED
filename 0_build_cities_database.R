rm(list=ls(all=TRUE)) # Removes all previously created variables
# Set required packages
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move WD one up
stub0 <- paste0(getwd(), "/") # Define a variable with the current WD
res_dir <- paste0(stub0, 'results/', sep ='') # Set path for results directory
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(kgc)
library(sf)
sf::sf_use_s2(F)
library(terra)


###
# First load city names
city_d = tolower(read.csv("climate/provide_urban_climate_data/cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .) # Cities from provide

ghs <- read_sf(paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")) # Cities database from GHS
ghs_coors = as.data.frame(st_coordinates(st_centroid(ghs)))
ghs = bind_cols(ghs, ghs_coors)

###

cl <- climatezones # Read climate zones from the kgc package

ghs$x_s <- RoundCoordinates(ghs$X) # Round coordinates in GHS
ghs$y_s <- RoundCoordinates(ghs$Y) # Round coordinates in GHS

ghs <- merge(ghs, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))

#A (tropical), B (arid), C (temperate), D (continental), and E (polar)
ghs$Cls_short <- substr(as.character(ghs$Cls), 1, 1)

###
# check with additional covariates

# Local climate zone(s)
# https://doi.org/10.1175/BAMS-D-11-00019.1

lcz <- rast("climate/lcz/lcz_filter_v3.tif")

ghs$lcz <- exact_extract(lcz, ghs, "majority")

###
###

library(fuzzyjoin)

city_d = as.data.frame(city_d)
colnames(city_d) = "UC_NM_MN"

data_c_sp <- stringdist_join(ghs, city_d, 
                             by = "UC_NM_MN",
                             mode = "left",
                             ignore_case = TRUE, 
                             method = "jw", 
                             max_dist = 99, 
                             distance_col = "dist")

data_c_sp = data_c_sp %>%
  group_by(UC_NM_MN.y) %>%
  slice_min(order_by = dist, n = 1)

data_c_sp = data_c_sp %>%
  group_by(UC_NM_MN.x) %>%
  slice_max(order_by = P15, n = 1)

data_c_sp = dplyr::select(data_c_sp, UC_NM_MN.x, UC_NM_MN.y, P15, lcz, Cls, Cls_short, X, Y, geometry)
colnames(data_c_sp) = c("UC_NM_MN", "name_PROVIDE", "population2015", "lcz", "kg_cl", "kg_cl_1", "x", "y", "geometry")

data_c_sp = st_as_sf(data_c_sp)
mapview::mapview(data_c_sp["kg_cl"])

write_sf(data_c_sp, "results/cities_database_climatezones.gpkg")

# Export the Region and Subregion name from the GHS database
ghs_export <- ghs %>%
  st_drop_geometry() %>%
  select(ID_HDC_G0, CTR_MN_NM, CTR_MN_ISO, GRGN_L1, GRGN_L2, UC_NM_MN, UC_NM_LST, EL_AV_ALS, Cls, Cls_short)
write_rds(ghs_export, "results/ghs_subregion_Cls.rds")

####

setwd(paste0(stub0, "/URGED"))
