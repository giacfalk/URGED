
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(raster)
library(exactextractr)
library(sf)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

#############

df <- read_rds("energy/processed_data.rds")
df_s <- df %>% group_by(comune) %>% dplyr::summarise(id=1)
df_s$comune_m <- df_s$comune

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

formap <- merge(df_s, comuni, by.x="comune", by.y="COMUNE")
formap$geometry <- NULL

###

library(data.table)

f1 <- fread("climate/hourly_tas_bycomune_2020.csv")
f2 <- fread("climate/hourly_tas_bycomune_2021.csv")
f3 <- fread("climate/hourly_tas_bycomune_2022.csv")

f1 <- bind_rows(f1, f2, f3)

library(lubridate)

f1$variable <- as.POSIXct(f1$variable)
f1$year <- year(f1$variable)
f1$month <-  month(f1$variable)

f1$cdh <- ifelse(f1$value>25, f1$value-25, 0)
  
#############

era5_tmax_daily_comuni_2011_2023_s <- group_by(f1, comune, year, month) %>% dplyr::summarise(cdh_month=sum(cdh, na.rm=T)) %>% ungroup() 

era5_tmax_daily_comuni_2011_2023_s <- era5_tmax_daily_comuni_2011_2023_s %>% dplyr::group_by(comune, year) %>% dplyr::mutate(cdh=sum(cdh_month, na.rm=T))

write_rds(era5_tmax_daily_comuni_2011_2023_s, "climate/cdh_2020_2022_comuni.rds")

