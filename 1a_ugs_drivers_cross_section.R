
##############################################################################

# This Rscript: 

#   1) build a cross sectional model to explain globally observed differences in city-level UGS density

##############################################################################

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
require(data.table)
require(tidyverse)
require(fixest)
require(haven)
require(fabricatr)
require(texreg)
require(xtable)
require(stargazer)
require(effects)
require(marginaleffects)
library(sf)
library(raster)
library(exactextractr)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

########
########

sf <- read_sf("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database

# import city-level estimated UGS from Falchetta and Hammad's paper

load("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/after_points_090524.Rdata")
out_ndvi_m <- out_ndvi_m[out_ndvi_m$year==2016,]
out_ndvi_m$city <- sf$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]
out_ndvi_m <- out_ndvi_m %>% group_by(city, country) %>% dplyr::summarise(out_b = median(out_b, na.rm=T), out_b_mean = mean(out_b, na.rm=T))
out_ndvi_m$country <- countrycode::countrycode(out_ndvi_m$country, 'iso2c', 'iso3c')

# parse with driver variables

cdd <- brick("climate/cdd18_global_hist_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
             varname="variable")
cdd <- stackApply(cdd, 1, mean, na.rm = T)

hdd <- brick("climate/hdd18_global_hist_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
             varname="variable")
hdd <- stackApply(hdd, 1, mean, na.rm = T)

pop <- raster("socioecon/GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

sf$cdd <- exact_extract(cdd, sf, "weighted_mean", weights=pop)
sf$hdd <- exact_extract(hdd, sf, "weighted_mean", weights=pop)

#


sf$pop <- exact_extract(pop, sf, "sum")
sf$popdens <- sf$pop / sf$AREA

# gdp downscaled (SSPS)
gdp_ssps <- list.files(path="F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/supporting_data/gdp_downscaled_ssps", recursive = T, pattern="tif", full.names = T)[18]

gdp_ssps_data <- raster(gdp_ssps)

sf$gdp <- exact_extract(gdp_ssps_data, sf, "sum")
sf$gdp_capita <- sf$gdp / sf$pop
 
sf$geometry <- NULL

sf <- st_make_valid(sf)
sf_c <- as_tibble(st_coordinates(st_centroid(sf)) )

cart2polar <- function(x, y) {
  data.frame(r = sqrt(x^2 + y^2), theta = atan2(y, x))
}

sf_c <- cart2polar(sf_c$X, sf_c$Y) 

sf <- bind_cols(sf, sf_c)

out_ndvi_m <- merge(out_ndvi_m, sf, by.x="city", by.y="UC_NM_MN")
out_ndvi_m$geom <- NULL

# estimate feols cross-sectional model

library(fixest)

model <- feglm(out_b/100 ~ cdd + hdd + log(popdens) + log(gdp_capita) + r + theta , data=out_ndvi_m, family = quasibinomial('logit'))
summary(model)

write_rds(model, paste0(res_dir, "ugs_drivers_model.rds"))

