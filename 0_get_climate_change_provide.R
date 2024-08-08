
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

indicator=c("urbclim-cooling-degree-hours", "urbclim-WBGT-hourover25")
reference=c("absolute")
spatial="area"
time="annual"
year=c("2030", "2050", "2070", "2100")
scenario=c("curpol", "gs", "sp")

gg <- expand.grid(city_d, indicator, reference, spatial, time, year, scenario)
colnames(gg) <- c("city_d", "indicator", "reference", "spatial", "time", "year", "scenario")

library(purrr)

purrr::map(1:nrow(gg), function(i){print(i/nrow(gg));tryCatch(download.file(paste0("https://provide-api-sharing.climateanalytics.org/api/impact-geo/?geography=", gg$city_d[i], "&indicator=", gg$indicator[i], "&reference=", gg$reference[i], "&spatial=", gg$spatial[i], "&time=", gg$time[i], "&displayOption=side-by-side&year=", gg$year[i], "&scenario=", gg$scenario[i], "&resolution=100m&format=geotiff"), destfile = paste0(paste(gg$city_d[i], gg$indicator[i], gg$year[i], gg$scenario[i], gg$reference[i], sep = "_"), ".tif"), mode = "wb"),
                                     error = function(e) print(paste(i, 'did not work out')))})

###

v <- list.files(pattern = "absolute")
v <- v[grepl("2020", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- vrt(v, "my.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2030.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2050.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2070.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2100.vrt", overwrite=T)

##########

my_curpol_2020 <- rast("my.vrt")
my_curpol_2030 <- rast("my_curpol_2030.vrt")
my_curpol_2050 <- rast("my_curpol_2050.vrt")
my_curpol_2070 <- rast("my_curpol_2070.vrt")
my_curpol_2100 <- rast("my_curpol_2100.vrt")

