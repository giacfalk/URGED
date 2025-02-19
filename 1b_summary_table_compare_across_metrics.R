
# script that produces comparative table

library(tidyverse)
library(stargazer)

##
# Working directory -------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Paths -------------------------------------------------------------------
path_cities <- paste0(stub0, "results/cities_database_climatezones.gpkg")
path_provide <- paste0(stub0, "climate/provide_urban_climate_data/")
path_gvi <- paste0(stub0, "ugs/after_points_030624.Rdata")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

###

fls <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/results/", pattern="monthly_coefs_", full.names = T)

fls_read <- lapply(fls, read.csv)
names(fls_read) <- basename(fls)
fls_read <- bind_rows(fls_read, .id="source")

fls_read$source <- gsub("_mean.csv", "_mean_Ta.csv", fls_read$source)
fls_read$source <- gsub("_min.csv", "_min_Ta.csv", fls_read$source)
fls_read$source <- gsub("_max.csv", "_max_Ta.csv", fls_read$source)

###

fls_read$X <- NULL

dff <- as.data.frame(matrix(unlist(str_split(fls_read$source, pattern = "_")), ncol=5, byrow = T))
dff <- dff[,c(3:5)]
colnames(dff) <- c("city", "var", "metric")

###

fls_read_m <- bind_cols(dff, fls_read)
fls_read_m$source <- NULL
fls_read_m$metric <- gsub(".csv", "", fls_read_m$metric)

###

library(sf)
kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

fls_read_m <- merge(fls_read_m, kg, by.x="city", by.y="UC_NM_MN")


###

library(modelsummary)
datasummary( Factor(var) * Factor(metric)  ~ Factor(lcz) * Estimate_d * mean, data = fls_read_m)

datasummary( Factor(var) * Factor(metric)  ~ Factor(lcz) * Estimate_d * mean, data = fls_read_m,  output = "results/compare_metrics_lcz.tex")

###

library(modelsummary)
datasummary( Factor(var) * Factor(metric)  ~ Factor(kg_cl_1) * Estimate_d * mean, data = fls_read_m)

datasummary( Factor(var) * Factor(metric)  ~ Factor(kg_cl_1) * Estimate_d * mean, data = fls_read_m,  output = "results/compare_metrics_kgc.tex")

