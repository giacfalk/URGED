
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
res_dir <- paste0(stub, '/results/', sep ='')

###

user <- 'gf'
#user <- 'edc'

if (user=='gf') {
  stub <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data'
}

if (user=='edc') {
  stub <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data'
}

setwd(stub)

cdh <- read_rds("cdh_2020_2022_comuni.rds")

cdh <- dplyr::group_by(cdh, comune, year) %>% dplyr::summarise(cdh=sum(cdh, na.rm=T)) %>% ungroup() %>% group_by(comune) %>%  dplyr::summarise(cdh=mean(cdh, na.rm=T))

####

stub2 <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub2)

out_ndvi_m <- read_rds("data_provide_cdh_gvi_ITA_cities.rds")

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(1000) %>% st_transform(4326)

pop_dens <- rast("H:/ECIP/Falchetta/era5/daily/new_2016_2022/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

out_ndvi_m$pop_dens <- exact_extract(pop_dens, out_ndvi_m, "sum")

out_ndvi_m$geometry <- NULL

out_ndvi_m <- dplyr::group_by(out_ndvi_m, COMUNE) %>% dplyr::summarise(t=weighted.mean(t, pop_dens, na.rm=T))

df <- merge(out_ndvi_m, cdh, by.x="COMUNE", by.y="comune")

library(ggplot2)
library(ggpmisc)

ggplot(df, aes(x=t, cdh))+
  theme_classic()+
  geom_point()+
  scale_x_continuous(limits = c(0, 8000))+
  scale_y_continuous(limits = c(0, 8000))+
  geom_abline()+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))+ 
  coord_cartesian(xlim=c(500, 8000), ylim=c(500, 8000))+
  xlab("PROVIDE granular data")+
  ylab("ERA5-Land")
  

ggsave(paste0(res_dir, "comparison_provide_era_cdh.png"), height = 5, width = 5, scale=1.1)

####
####
####


