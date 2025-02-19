library(sf)
library(tidyverse)
library(raster)
library(doBy)
library(pbapply)

rasterOptions(tmpdir = "H:/ECIP/Falchetta/temp_raster")

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation")

###########

eu_sf <- read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson") %>% dplyr::filter(LEVL_CODE == 3)

gridded_pop_data <- raster("socioecon/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

gridded_pop <- list()

for (i in 1:nrow(eu_sf)){
  print(i)
  gridded_pop[[i]] <- rgis::mask_raster_to_polygon(gridded_pop_data, eu_sf[i,])
}

###

output_items <- list()

for (i in 1:nrow(eu_sf)){
  print(i)
  outs <- which.maxn(values(gridded_pop[[i]]), n=1000)
  output_items[[i]] <- as.data.frame(xyFromCell(gridded_pop[[i]], outs))
  output_items[[i]]$CNTR_CODE <- eu_sf$CNTR_CODE[i]
  output_items[[i]]$NUTS_ID <- eu_sf$NUTS_ID[i]
}

output_items <- pblapply(1:nrow(eu_sf), function(i){output_items[[i]] %>%  st_as_sf(coords=c("x", "y"), remove=F, crs=4326) %>% st_transform(3395) %>% st_buffer(10) %>% st_transform(4326)})

names(output_items) <- unlist(pblapply(1:length(output_items), function(X){first(substr(output_items[[X]]$NUTS_ID, 1, 3))}))

output_items <- tapply(output_items, names(output_items), dplyr::bind_rows)

####

# save(output_items, file = "results/bk_gvi_nuts.Rdata")

library(rgee)
email = "giacomo.falchetta@gmail.com"
ee_Initialize(email, drive=T, gcs=T)

# load("results/bk_gvi_nuts.Rdata")

for (i in 1:length(output_items)){
  print(i)
  write_sf(output_items[[i]] , paste0("ugs/shp/gv_jus_", i, ".shp"))
  sf_as_ee(output_items[[i]], via="gcs_to_asset", bucket = "accessibility_jack", assetId=paste0("users/giacomofalchetta/gv_jus_", i, ".shp"), overwrite = T, monitoring=F)
}

