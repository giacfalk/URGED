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
path_gvi <- paste0(stub0, "ugs/after_points_100425.Rdata")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

###

coefs1 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmax_raw_gvi.csv")
coefs2 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmin_raw_gvi.csv")
coefs3 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmean_raw_gvi.csv")
coefs4 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmax_raw_gvi.csv")
coefs5 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmin_raw_gvi.csv")
coefs6 <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmean_raw_gvi.csv")

coefs1$metric <- "WBGT"
coefs2$metric <- "WBGT"
coefs3$metric <- "WBGT"
coefs4$metric <- "T2M"
coefs5$metric <- "T2M"
coefs6$metric <- "T2M"

coefs1$var <- "max"
coefs2$var <- "min"
coefs3$var <- "mean"
coefs4$var <- "max"
coefs5$var <- "min"
coefs6$var <- "mean"

fls_read <- bind_rows(coefs1, coefs2, coefs3, coefs4, coefs5, coefs6)

###

fls_read_m <- fls_read

###

library(sf)
kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

#

library(stringdist)

closest <- sapply(unique(kg$UC_NM_MN), function(x) {
  unique(fls_read_m$city)[which.min(stringdist(x, unique(fls_read_m$city), method = "jw"))]
})

kg$UC_NM_MN <- closest[match(kg$UC_NM_MN, names(closest))]


#

fls_read_m <- merge(fls_read_m, kg, by.x="city", by.y="UC_NM_MN")


###

library(modelsummary)
datasummary( Factor(var) * Factor(metric)  ~ Factor(lcz) * coef * mean, data = fls_read_m, fmt = fmt_significant(1))

datasummary( Factor(var) * Factor(metric)  ~ Factor(lcz) * coef * mean, data = fls_read_m,  output = "results/compare_metrics_lcz.tex", fmt = fmt_significant(1))

###

library(modelsummary)
datasummary( Factor(var) * Factor(metric)  ~ Factor(kg_cl_1) * coef * mean, data = fls_read_m, fmt = fmt_significant(1))

datasummary( Factor(var) * Factor(metric)  ~ Factor(kg_cl_1) * coef * mean, data = fls_read_m,  output = "results/compare_metrics_kgc.tex", fmt = fmt_significant(1))

####

setwd(paste0(stub0, "/URGED"))

