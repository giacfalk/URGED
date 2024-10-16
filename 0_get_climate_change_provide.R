
###

# Process the climate change scenarios versions

###


rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'
res_dir <- paste0(stub, 'results/', sep ='')

###

stub2 <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub2)

###

city_d=tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)

indicator=c("urbclim-cooling-degree-hours", "urbclim-T2M-daily-mean-max", "urbclim-T2M-daily-mean-min")
reference=c("absolute")
spatial="area"
time="annual"
year=c("2030", "2050", "2070", "2100")
scenario=c("curpol", "gs", "sp")
indicator_value="25" 

gg <- expand.grid(city_d, indicator, reference, spatial, time, year, scenario, indicator_value)
colnames(gg) <- c("city_d", "indicator", "reference", "spatial", "time", "year", "scenario", "indicator_value")


library(purrr)

purrr::map(1:nrow(gg), function(i){print(i);download.file(paste0("https://provide-api-sharing.climateanalytics.org/api/impact-geo/?geography=", gg$city_d[i], "&indicator=", gg$indicator[i], "&reference=", gg$reference[i], ifelse(gg$indicator[i] %in% indicator[3:4], paste0("&indicator_value=", gg$indicator_value[i]), ""), "&spatial=", gg$spatial[i], "&time=", gg$time[i], "&displayOption=side-by-side&year=", gg$year[i], "&scenario=", gg$scenario[i], "&resolution=100m&format=geotiff"), destfile = paste0(paste(gg$city_d[i], gg$indicator[i], gg$year[i], gg$scenario[i], gg$reference[i], sep = "_"), ".tif"), mode = "wb")})

  ###
  
  indicator=c("urbclim-cooling-degree-hours", "urbclim-T2M-daily-mean-max", "urbclim-T2M-daily-mean-min")
  reference=c("present-day")
  spatial="area"
  time="annual"
  year=c("2030")
  scenario=c("curpol")
  indicator_value="25" 
  
  gg <- expand.grid(city_d, indicator, reference, spatial, time, year, scenario, indicator_value)
  colnames(gg) <- c("city_d", "indicator", "reference", "spatial", "time", "year", "scenario", "indicator_value")
  
  library(purrr)
  
  purrr::map(1:nrow(gg), function(i){print(i);download.file(paste0("https://provide-api-sharing.climateanalytics.org/api/impact-geo/?geography=", gg$city_d[i], "&indicator=", gg$indicator[i], "&reference=", gg$reference[i], ifelse(gg$indicator[i] %in% indicator[3:4], paste0("&indicator_value=", gg$indicator_value[i]), ""), "&spatial=", gg$spatial[i], "&time=", gg$time[i], "&displayOption=side-by-side&year=", gg$year[i], "&scenario=", gg$scenario[i], "&resolution=100m&format=geotiff"), destfile = paste0(paste(gg$city_d[i], gg$indicator[i], gg$year[i], gg$scenario[i], gg$reference[i], sep = "_"), ".tif"), mode = "wb")})

###
###
  
v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "r1_temp.vrt", overwrite=T)

v <- list.files(pattern = "present-day")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v2 <- vrt(v, "r2_temp.vrt", overwrite=T)


v <- rast("r1_temp.vrt")
v2 <- rast("r2_temp.vrt")

v3 <- v - v2

writeRaster(v3, "my_cooling-degree-hours_curpol_2020.tif", overwrite=T)


####

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_cooling-degree-hours_curpol_2030.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_cooling-degree-hours_curpol_2050.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_cooling-degree-hours_curpol_2070.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_cooling-degree-hours_curpol_2100.vrt", overwrite=T)

##########

my_cooling_degree_hours_curpol_2020 <- rast("my_cooling-degree-hours_curpol_2020.tif")
my_cooling_degree_hours_curpol_2030 <- rast("my_cooling-degree-hours_curpol_2030.vrt")
my_cooling_degree_hours_curpol_2050 <- rast("my_cooling-degree-hours_curpol_2050.vrt")
my_cooling_degree_hours_curpol_2070 <- rast("my_cooling-degree-hours_curpol_2070.vrt")
my_cooling_degree_hours_curpol_2100 <- rast("my_cooling-degree-hours_curpol_2100.vrt")

###
###

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "r1_temp.vrt", overwrite=T)

v <- list.files(pattern = "present-day")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v2 <- vrt(v, "r2_temp.vrt", overwrite=T)


v <- rast("r1_temp.vrt")
v2 <- rast("r2_temp.vrt")

v3 <- v - v2

writeRaster(v3, "my_urbclim-T2M-daily-mean-max_curpol_2020.tif", overwrite=T)


####

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-max_curpol_2030.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-max_curpol_2050.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-max_curpol_2070.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-max_curpol_2100.vrt", overwrite=T)

##########

my_urbclim_T2M_daily_mean_max_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2020.tif")
my_urbclim_T2M_daily_mean_max_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2100.vrt")

###
###

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "r1_temp.vrt", overwrite=T)

v <- list.files(pattern = "present-day")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v2 <- vrt(v, "r2_temp.vrt", overwrite=T)


v <- rast("r1_temp.vrt")
v2 <- rast("r2_temp.vrt")

v3 <- v - v2

writeRaster(v3, "my_urbclim-T2M-daily-mean-min_curpol_2020.tif", overwrite=T)


####

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-min_curpol_2030.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-min_curpol_2050.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-min_curpol_2070.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_urbclim-T2M-daily-mean-min_curpol_2100.vrt", overwrite=T)

##########

my_urbclim_T2M_daily_mean_min_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2020.tif")
my_urbclim_T2M_daily_mean_min_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2100.vrt")

# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_cooling-degree-hours_gs_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_cooling-degree-hours_gs_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_cooling-degree-hours_gs_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_cooling-degree-hours_gs_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_cooling-degree-hours_gs_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_cooling_degree_hours_gs_2020 <- rast("my_cooling-degree-hours_gs_2020.tif")
# my_cooling_degree_hours_gs_2030 <- rast("my_cooling-degree-hours_gs_2030.vrt")
# my_cooling_degree_hours_gs_2050 <- rast("my_cooling-degree-hours_gs_2050.vrt")
# my_cooling_degree_hours_gs_2070 <- rast("my_cooling-degree-hours_gs_2070.vrt")
# my_cooling_degree_hours_gs_2100 <- rast("my_cooling-degree-hours_gs_2100.vrt")
# 
# ###
# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_urbclim-T2M-daily-mean-max_gs_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_gs_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_gs_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_gs_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_gs_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_urbclim_T2M_daily_mean_max_gs_2020 <- rast("my_urbclim-T2M-daily-mean-max_gs_2020.tif")
# my_urbclim_T2M_daily_mean_max_gs_2030 <- rast("my_urbclim-T2M-daily-mean-max_gs_2030.vrt")
# my_urbclim_T2M_daily_mean_max_gs_2050 <- rast("my_urbclim-T2M-daily-mean-max_gs_2050.vrt")
# my_urbclim_T2M_daily_mean_max_gs_2070 <- rast("my_urbclim-T2M-daily-mean-max_gs_2070.vrt")
# my_urbclim_T2M_daily_mean_max_gs_2100 <- rast("my_urbclim-T2M-daily-mean-max_gs_2100.vrt")
# 
# ###
# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_urbclim-T2M-daily-mean-min_gs_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_gs_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_gs_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_gs_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("gs", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_gs_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_urbclim_T2M_daily_mean_min_gs_2020 <- rast("my_urbclim-T2M-daily-mean-min_gs_2020.tif")
# my_urbclim_T2M_daily_mean_min_gs_2030 <- rast("my_urbclim-T2M-daily-mean-min_gs_2030.vrt")
# my_urbclim_T2M_daily_mean_min_gs_2050 <- rast("my_urbclim-T2M-daily-mean-min_gs_2050.vrt")
# my_urbclim_T2M_daily_mean_min_gs_2070 <- rast("my_urbclim-T2M-daily-mean-min_gs_2070.vrt")
# my_urbclim_T2M_daily_mean_min_gs_2100 <- rast("my_urbclim-T2M-daily-mean-min_gs_2100.vrt")
# 
# ###
# 
# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_cooling-degree-hours_sp_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_cooling-degree-hours_sp_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_cooling-degree-hours_sp_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_cooling-degree-hours_sp_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("cooling-degree-hours", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_cooling-degree-hours_sp_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_cooling_degree_hours_sp_2020 <- rast("my_cooling-degree-hours_sp_2020.tif")
# my_cooling_degree_hours_sp_2030 <- rast("my_cooling-degree-hours_sp_2030.vrt")
# my_cooling_degree_hours_sp_2050 <- rast("my_cooling-degree-hours_sp_2050.vrt")
# my_cooling_degree_hours_sp_2070 <- rast("my_cooling-degree-hours_sp_2070.vrt")
# my_cooling_degree_hours_sp_2100 <- rast("my_cooling-degree-hours_sp_2100.vrt")
# 
# ###
# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_urbclim-T2M-daily-mean-max_sp_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_sp_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_sp_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_sp_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-max", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-max_sp_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_urbclim_T2M_daily_mean_max_sp_2020 <- rast("my_urbclim-T2M-daily-mean-max_sp_2020.tif")
# my_urbclim_T2M_daily_mean_max_sp_2030 <- rast("my_urbclim-T2M-daily-mean-max_sp_2030.vrt")
# my_urbclim_T2M_daily_mean_max_sp_2050 <- rast("my_urbclim-T2M-daily-mean-max_sp_2050.vrt")
# my_urbclim_T2M_daily_mean_max_sp_2070 <- rast("my_urbclim-T2M-daily-mean-max_sp_2070.vrt")
# my_urbclim_T2M_daily_mean_max_sp_2100 <- rast("my_urbclim-T2M-daily-mean-max_sp_2100.vrt")
# 
# ###
# ###
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "r1_temp.vrt", overwrite=T)
# 
# v <- list.files(pattern = "present-day")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v2 <- vrt(v, "r2_temp.vrt", overwrite=T)
# 
# 
# v <- rast("r1_temp.vrt")
# v2 <- rast("r2_temp.vrt")
# 
# v3 <- v - v2
# 
# writeRaster(v3, "my_urbclim-T2M-daily-mean-min_sp_2020.tif", overwrite=T)
# 
# 
# ####
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2030", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_sp_2030.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2050", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_sp_2050.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2070", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_sp_2070.vrt", overwrite=T)
# 
# v <- list.files(pattern = "absolute")
# v <- v[grepl("2100", v)]
# v <- v[grepl(paste0(city_d, collapse = "|"), v)]
# v <- v[grepl("urbclim-T2M-daily-mean-min", v)]
# v <- v[grepl("sp", v)]
# v <- vrt(v, "my_urbclim-T2M-daily-mean-min_sp_2100.vrt", overwrite=T)
# 
# ##########
# 
# my_urbclim_T2M_daily_mean_min_sp_2020 <- rast("my_urbclim-T2M-daily-mean-min_sp_2020.tif")
# my_urbclim_T2M_daily_mean_min_sp_2030 <- rast("my_urbclim-T2M-daily-mean-min_sp_2030.vrt")
# my_urbclim_T2M_daily_mean_min_sp_2050 <- rast("my_urbclim-T2M-daily-mean-min_sp_2050.vrt")
# my_urbclim_T2M_daily_mean_min_sp_2070 <- rast("my_urbclim-T2M-daily-mean-min_sp_2070.vrt")
# my_urbclim_T2M_daily_mean_min_sp_2100 <- rast("my_urbclim-T2M-daily-mean-min_sp_2100.vrt")
# 
