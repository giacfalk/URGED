
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

stub <- 'C:/Users/Utente/OneDrive - IIASA/IBGF_2024/implementation/'
res_dir <- paste0(stub, 'results/', sep ='')

###

stub <- "C:/Users/Utente/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub)

##########

d <- read_csv("C:/Users/Utente/Downloads/waqi-covid19-airqualitydata-2024.csv", skip = 4)
d_city <- paste(unique(d$City), unique(d$Country))
library(googleway) 
set_key("AIzaSyABAy4zgNg6P41_RlFIEa0JBo9W8o4866g")
d_city <- unique(d$City)
res <- lapply(d_city, function(x){res <- google_geocode(x); return(res$results$geometry$bounds$southwest[1,])}) 
names(res) <- d_city
res_b <- bind_rows(res, .id="City")
d <- d %>% group_by(City, Country) %>% dplyr::summarise(min=mean(min, na.rm=T), max=mean(max, na.rm=T), median=mean(median, na.rm=T))
d <- merge(d, res_b, by="City")
d <- st_as_sf(d, coords=c("lng", "lat"), crs=4326, remove=F)

##########

load("C:/Users/Utente/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/after_points_030624.Rdata")

sf <- read_sf("C:/Users/Utente/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
sf_c <- sf %>% group_by(GRGN_L2) %>% slice_max(P15, n = 10)

###

out_ndvi_m$city <- sf_c$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]

out_ndvi_m <- dplyr::select(out_ndvi_m, city, year, out_b, x, y, country)

###

out_ndvi_m <- out_ndvi_m %>% group_by(city) %>% dplyr::summarise(out_b=mean(out_b, na.rm=T))
out_ndvi_m <- merge(out_ndvi_m, sf_c, by.x="city", by.y="UC_NM_MN")
out_ndvi_m <- st_as_sf(out_ndvi_m)

library(nngeo)
sf::sf_use_s2(F)

out_ndvi_m <- st_join(out_ndvi_m, d, st_nn, k=1. , maxdist = 25000)

out_ndvi_m <- filter(out_ndvi_m, !is.na(max))
out_ndvi_m_bk <- out_ndvi_m
out_ndvi_m$geometry <- NULL

###

write_rds(out_ndvi_m, "data_provide_aqci_gvi_143cities.rds")

out_ndvi_m <- read_rds("data_provide_aqci_gvi_143cities.rds")

library(fixest)

m1 <- feols(max ~ out_b | Country, data=out_ndvi_m)
summary(m1, "cluster")

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab1.png"))
