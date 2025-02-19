# This file writes a global gridded output template for climate zone-specific, local-climate zone specific CEs.
# Set required packages -------------------------------------------------
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
library(terra)
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Paths -------------------------------------------------------------------
path_cities <- paste0(stub0, "results/cities_database_climatezones.gpkg")
path_provide <- paste0(stub0, "climate/provide_urban_climate_data/")
path_gvi <- paste0(stub0, "ugs/after_points_030624.Rdata")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

################

library(stars)

template <- read_stars("F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/accreu/data/output/cdd/ukesm1-0-ll_r1i1p1f2_w5e5_ssp370_tas_global_daily_2091_2100_cdd24.tif")

template = st_as_sf(template)

template$id = 1:nrow(template)

template_coords = st_coordinates(st_centroid(template$geometry))

template$geometry = NULL

template_df = bind_cols(template, template_coords)

#################

library(kgc)

cl <- climatezones

template_df$x_s <- RoundCoordinates(template_df$X)
template_df$y_s <- RoundCoordinates(template_df$Y)


out_ndvi_m <- merge(template_df, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"), all.x=T)
out_ndvi_m$Cls_short <- substr(as.character(out_ndvi_m$Cls), 1, 1) # Add the full and main KGC [A (tropical), B (arid), C (temperate), D (continental), and E (polar)]

###
# check with additional covariates

# Local climate zone(s)
# https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml
setwd(stub0)
lcz <- raster(path_lcz)

###

# pop <- raster("")

###

library(stars)

template <- read_stars("F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/accreu/data/output/cdd/ukesm1-0-ll_r1i1p1f2_w5e5_ssp370_tas_global_daily_2091_2100_cdd24.tif")

template = st_as_sf(template)

template$id = 1:nrow(template)

landcov_fracs <- exact_extract(lcz, template, function(df) {
  df %>%
    dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    dplyr::group_by(id, value) %>%
    dplyr::summarize(freq = sum(frac_total, na.rm=T), .groups = 'keep')
}, summarize_df = TRUE, include_cols = 'id', progress = T)

landcov_fracs = pivot_wider(landcov_fracs, id_cols=id, names_from = value,
                            values_from = freq)

write.csv(landcov_fracs, "results/lcz_fracs.csv")

##

landcov_fracs = read.csv("results/lcz_fracs.csv")

landcov_fracs[is.na(landcov_fracs)] <- 0

out_ndvi_m = merge(out_ndvi_m, landcov_fracs, "id", all=T)

out_ndvi_m = dplyr::select(out_ndvi_m, c(14:17, 19:35))

colnames(out_ndvi_m)[1] = "X"

out_ndvi_m[is.na(out_ndvi_m)] <- 0


write_rds(out_ndvi_m, "results/output_template_ce_globalgrid.rds")

###
###
###

data_c_sp <- read_sf( "results/cities_database_climatezones.gpkg")

lcz <- raster(path_lcz)

###

library(stars)

data_c_sp$id = 1:nrow(data_c_sp)

landcov_fracs <- exact_extract(lcz, data_c_sp, function(df) {
  df %>%
    dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
    dplyr::group_by(id, value) %>%
    dplyr::summarize(freq = sum(frac_total, na.rm=T), .groups = 'keep')
}, summarize_df = TRUE, include_cols = 'id', progress = T)

landcov_fracs = pivot_wider(landcov_fracs, id_cols=id, names_from = value,
                            values_from = freq)

landcov_fracs[is.na(landcov_fracs)] <- 0

out_ndvi_m = merge(data_c_sp, landcov_fracs, "id", all=T)

out_ndvi_m$lcz = NULL
out_ndvi_m$id = NULL

out_ndvi_m[is.na(out_ndvi_m)] <- 0

write_rds(out_ndvi_m, "results/output_template_ce_cities.rds")


