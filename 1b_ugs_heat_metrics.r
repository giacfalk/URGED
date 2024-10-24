# This file collects all considered variables from various (geospatial and non-geospatial) databases that may have an explanatory role for explaining the number of cooling degree days. The output is written into a file `data_provide_cdh_gvi_143cities_withcovariates.rds`. This file will be accessed by other scripts for further analysis.
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

# Output file:
outname <- paste0(stub0, "data_provide_cdh_gvi_143cities_withcovariates.rds")

# First load city names -------------------------------------------------------------
setwd(path_provide)
city_d = tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)
city_d_c <- gsub("_", " ", city_d)
indicator = c("urbclim-cooling-degree-hours", "urbclim-WBGT-hourover25")
reference = c("absolute", "present-day")
spatial = "area"
time = "annual"
year = "2020"
scenario = "curpol"

#### Main Script
# Get data:
# PROVIDE climate data ------------------------------------------------
# In case the raster files for the PROVIDE data do not exist, they can be created using the code in the following script:
# source(paste0(stub, "URGED/0_get_climate_change_provide.R"))
# Otherwise, load the existing files for the years 2020, 2030, 2050, 2070, and 2100
my_cooling_degree_hours_curpol_2020 <- rast("my_cooling-degree-hours_curpol_2020.tif")
my_cooling_degree_hours_curpol_2030 <- rast("my_cooling-degree-hours_curpol_2030.vrt")
my_cooling_degree_hours_curpol_2050 <- rast("my_cooling-degree-hours_curpol_2050.vrt")
my_cooling_degree_hours_curpol_2070 <- rast("my_cooling-degree-hours_curpol_2070.vrt")
my_cooling_degree_hours_curpol_2100 <- rast("my_cooling-degree-hours_curpol_2100.vrt")

# urbclim data --------------------------------------------------------
# Load the maximum mean T2 from urbclim for 2020, 2030, 2050, 2070, and 2100.
my_urbclim_T2M_daily_mean_max_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2020.tif")
my_urbclim_T2M_daily_mean_max_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2100.vrt")
# Load the minimum mean T2 from urbclim for 2020, 2030, 2050, 2070, and 2100.
my_urbclim_T2M_daily_mean_min_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2020.tif")
my_urbclim_T2M_daily_mean_min_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2100.vrt")
# GVI data --------------------------------------------------------
load(path_gvi) # Variable is called 'out_ndvi_m'.
# GHS data ------------------------------------------------------------
ghs <- read_sf(path_ghs_urbcentres) # Cities database
ghs_top10 <- ghs %>% # Filter by 10 largest cities per region
  group_by(GRGN_L2) %>% # Group by Geographical Region (UNDESA, 2018b)
  slice_max(P15, n = 10) %>% # Filter for the largest 10 cities each using P15, which is population in 2015
  ungroup()

# Process the data
# Assign city names -------------------------------------------------------
# Assignment is based on the first part of 'id' column, which has been previously mapped to lat/lon
out_ndvi_m$city <- ghs_top10$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]
# Assign country names. Assignment is based on the first part of 'id' column, which has been previously mapped to lat/lon.
out_ndvi_m$country <- ghs_top10$CTR_MN_NM[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]
# Add to dataset
out_ndvi_m <- dplyr::select(out_ndvi_m, city, year, out_b, x, y, country)
# Add city names from PROVIDE data set
out_ndvi_m <- out_ndvi_m[grep(paste(city_d_c, collapse="|"), out_ndvi_m$city, ignore.case=T),]

# Summary statistics -------------------------------------------------------
out_ndvi_m <- out_ndvi_m %>%
  group_by(x, y, city, country) %>%
  summarise(out_b_mean = mean(out_b, na.rm=T),
            out_b_min = min(out_b, na.rm=T),
            out_b_max = max(out_b, na.rm=T),
            out_b_sd = sd(out_b, na.rm=T)) %>%
  ungroup()

# Write as sf spatial data frame --------------------------------------------------------
out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% # Transform out_ndvi_m to sf object
  st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)


# Extract PROVIDE CDH and add to df -----------------------------------------------
out_ndvi_m$t <- exact_extract(my_cooling_degree_hours_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_2030 <- exact_extract(my_cooling_degree_hours_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_2050 <- exact_extract(my_cooling_degree_hours_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_2070 <- exact_extract(my_cooling_degree_hours_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_2100 <- exact_extract(my_cooling_degree_hours_curpol_2100, out_ndvi_m, "mean")
# Extract urbclim T2max and add to df -----------------------------------------------
out_ndvi_m$t_max <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_max_2030 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_max_2050 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_max_2070 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_max_2100 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2100, out_ndvi_m, "mean")
# Extract urbclim T2min and add to df -----------------------------------------------
out_ndvi_m$t_min <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_min_2030 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_min_2050 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_min_2070 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_min_2100 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2100, out_ndvi_m, "mean")


# df filtering and cleaning -----------------------------------------------
out_ndvi_m <- dplyr::filter(out_ndvi_m, !is.na(t))
out_ndvi_m_bk <- out_ndvi_m # Backup variable
out_ndvi_m$geometry <- NULL # Add a column "geometry"

# Summary statistics -------------------------------------------------------
# out_ndvi_m <- group_by(out_ndvi_m, x, y, city) %>%
#   dplyr::summarise(out_b_mean=mean(out_b, na.rm=T),
#                    out_b_max=max(out_b, na.rm=T),
#                    out_b_sd=sd(out_b, na.rm=T),
#                    t=mean(t, na.rm=T),
#                    t_2030=mean(t_2030, na.rm=T),
#                    t_2050=mean(t_2050, na.rm=T),
#                    t_2100=mean(t_2100, na.rm=T),
#                    t_max=mean(t_max, na.rm=T),
#                    t_max_2030=mean(t_max_2030, na.rm=T),
#                    t_max_2050=mean(t_max_2050, na.rm=T),
#                    t_max_2100=mean(t_max_2100, na.rm=T),
#                    t_min=mean(t_min, na.rm=T), t_min_2030=mean(t_min_2030, na.rm=T),
#                    t_min_2050=mean(t_min_2050, na.rm=T),
#                    t_min_2100=mean(t_min_2100, na.rm=T))
# Test plots -------------------------------------------------------
ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=out_b_mean))+
  scale_colour_distiller(palette = "YlGn", direction = 1)+
  ggtitle("Street green space density in Berlin")

ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=out_b_mean))+
  scale_colour_distiller(palette = "YlGn", direction = 1)+
  ggtitle("Street green space density in Athens")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t_2050-t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin (2050 w.r.t. 2020)")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t_2100-t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin (2100 w.r.t. 2020)")

###

summary(out_ndvi_m$t)
summary(out_ndvi_m$out_b_mean)

###

write_rds(out_ndvi_m, "data_provide_cdh_gvi_143cities.rds")

# Backup step: In case something went wrong
# out_ndvi_m <- read_rds("data_provide_cdh_gvi_143cities.rds")

out_ndvi_m <- filter(out_ndvi_m, t>0)

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

# out_ndvi_m <- out_ndvi_m %>% filter(city=="Rome" | city=="Milan")

###

library(kgc)

cl <- climatezones

out_ndvi_m$x_s <- RoundCoordinates(out_ndvi_m$x)
out_ndvi_m$y_s <- RoundCoordinates(out_ndvi_m$y)

out_ndvi_m <- merge(out_ndvi_m, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))
out_ndvi_m$Cls <- substr(as.character(out_ndvi_m$Cls), 1, 1) # Add the main KGC [A (tropical), B (arid), C (temperate), D (continental), and E (polar)]

###
# check with additional covariates

# Local climate zone(s)
# https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml
setwd(stub0)
lcz <- rast("climate/lcz/lcz_filter_v3.tif")

out_ndvi_m$lcz <- exact_extract(lcz, out_ndvi_m, "majority")


######
# Additional covariates for the urban environment
# Buildings ------------------------------------------------------------
## Building Density/Height/Volume from GHS 
build_v <- rast("socioecon/GHS_BUILT_V_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")
build_h <- rast("socioecon/GHS_BUILT_H_AGBH_E2018_GLOBE_R2023A_4326_3ss_V1_0.tif")
out_ndvi_m$build_v <- exact_extract(build_v, out_ndvi_m, "mean")
out_ndvi_m$build_h <- exact_extract(build_h, out_ndvi_m, "mean")
# Socio-economic covariate(s) ------------------------------------------------------------
## Population density
pop_dens <- rast("socioecon/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif") # Pop density
out_ndvi_m$pop_dens <- exact_extract(pop_dens, out_ndvi_m, "mean")
# Make into quantiles
out_ndvi_m$pop_dens_d <- raster::cut(out_ndvi_m$pop_dens, quantile(out_ndvi_m$pop_dens, seq(0, 1, 0.2)))

## Poverty 
pov_ind <- rast("socioecon/povmap-grdi-v1.tif") # Poverty indicator GRDI
out_ndvi_m$pov_ind <- exact_extract(pov_ind, out_ndvi_m, "mean")

# Water Bodies ------------------------------------------------------------
# options(timeout = 300)
# download.file("https://globalland.cls.fr/webResources/catalogTree/netcdf/water_bodies/wb_300m_v2_monthly/2023/20230601/c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", mode="wb", destfile = "c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", time=120)
#https://land.copernicus.eu/en/products/water-bodies/water-bodies-global-v2-0-300m#general_info
water <- rast("c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", lyrs="WB")
out_ndvi_m$water <- exact_extract(water, out_ndvi_m, "max")
out_ndvi_m$water  <- ifelse(is.na(out_ndvi_m$water), 0, out_ndvi_m$water)

# Altitude ----------------------------------------------------------------
library(elevatr) #elevatr v0.99.0 NOTE: Version 0.99.0 of 'elevatr' uses 'sf' and 'terra'.  Use of the 'sp', 'raster', and underlying 'rgdal' packages by 'elevatr' is being deprecated; however, get_elev_raster continues to return a RasterLayer.  This will be dropped in future versions, so please plan accordingly.
sf_temp <- out_ndvi_m
out_ndvi_ms <- list()

for(cityy in unique(out_ndvi_m$city)){
  print(cityy)
  out_ndvi_ms[[as.character(cityy)]] <- get_elev_point(sf_temp %>% filter(city==cityy) %>% st_centroid(.), prj = 4326, src = "aws", z = 10)
}

out_ndvi_m <- bind_rows(out_ndvi_ms)

write_rds(out_ndvi_m, outname)
###########