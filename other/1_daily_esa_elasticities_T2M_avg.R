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
library(pbapply)
library(matrixStats)
library(lubridate)
library(terra)
library(rstudioapi)
library(paisaje)   # helper to download ESA WorldCover :contentReference[oaicite:1]{index=1}

##
# Working directory -------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Paths -------------------------------------------------------------------
path_cities <- paste0(stub0, "results/cities_database_climatezones.gpkg")
path_provide <- paste0(stub0, "climate/provide_urban_climate_data/")
path_gvi <- paste0(stub0, "ugs/after_points_100425_citynames.rds")
path_ghs_urbcentres <- paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # GHS Urban Centre Database (R2019A) https://human-settlement.emergency.copernicus.eu/download.php?ds=ucdb
path_lcz <- paste0(stub0, "climate/lcz/lcz_filter_v3.tif") # https://zenodo.org/records/8419340

# Output file:
outname <- paste0(stub0, "data_provide_tas_qv_monthly_cities.rds")

# First load city names -------------------------------------------------------------
setwd(path_provide)
city_d = tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)
city_d_c <- gsub("_", " ", city_d)

##

for(min_max_avg_sel in c("max")){
  
  print(min_max_avg_sel)
  
  min_max_avg <- min_max_avg_sel
  
  # rowCustoms <- ifelse(min_max_avg=="mean", rowMeans, ifelse(min_max_avg=="max", rowMaxs, rowMins))
  rowCustoms <- rowMeans
  
  # GHS data ------------------------------------------------------------
  ghs <- read_sf(path_ghs_urbcentres) # Cities database
  
  ff = list.files(pattern=paste0("T2M_year_daily_", min_max_avg, "_"), path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.name=T, recursive=T)
  ff_c = list.dirs( path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.names = F, recursive = F)
  
  for(city_n in 1:length(ff_c)){
    
    print(city_n)
    
    ff_filter <- ff[grepl(ff_c[city_n], ff)]
    
    proj_city =lapply(ff_filter, rast)
    proj_city <- rast(proj_city)
    city = ncdf4::nc_open(ff_filter[1])
    
    crs_city <- (parse_number(capture.output(print(city))[which(grepl("EPSG", capture.output(print(city))))]))
    
    crs(proj_city) = paste0("EPSG:", crs_city)
    
    ncdf4::nc_close(city)
    
    names(proj_city) <- paste0("tas_", 1:nlyr(proj_city))
    
    ###
    
    bbox <- st_as_sfc( st_bbox(project(proj_city[[1]], "epsg:4326")))
    bbox <- st_as_sf(bbox)
    
    # coefficients || GVI need to be updates to some other metric ||  OR urbclim variation is too limited in space and time
    
    out_dir <- paste0("F:/.shortcut-targets-by-id/1pinMP3eFVcLzSAMscqgGCJ3v3ujjfAeR/adaptation_infrastructure_database/data/get_green_data_cities/worldcover_", ff_c[city_n])
    dir.create(out_dir, showWarnings = FALSE)
    
    wc <- get_esa_10m(
      aoi_sf        = bbox,   # AOI as sf polygon
      year          = 2021,   # 2020 or 2021 currently available
      output_folder = out_dir
    )
    
    wc <- list.files(paste0("F:/.shortcut-targets-by-id/1pinMP3eFVcLzSAMscqgGCJ3v3ujjfAeR/adaptation_infrastructure_database/data/get_green_data_cities/worldcover_", ff_c[city_n]), full.names = TRUE, pattern = ".tif$")
    
    # 2) Read them in as a SpatRaster
    wc <- vrt(wc)
    
    # Mask to bbox boundary first
    wc_bbox <- crop(wc, vect(bbox)) |> mask(vect(bbox))
    
    # choose which classes count as green
    veg_classes_strict  <- c(10, 60)       # exclude cropland
    
    green_strict <- classify(wc_bbox,
                             rbind(cbind(veg_classes_strict, 1)),
                             others = 0)
    
    green_density_100m <- aggregate(
      green_strict,
      fact = 10,         # 10 * 10 m = 100 m
      fun  = "mean",
      na.rm = TRUE
    )
    
    proj_city
    
    rast_to_sf <- function(rast){
      df <- as.data.frame(rast, xy=TRUE, na.rm=TRUE)
      sf::st_as_sf(df, coords=c("x", "y"), crs=crs(rast), remove = F) %>% st_transform(3395) %>% st_buffer(1000) %>% st_transform(4326)
    }
    
    out_ndvi_m_f <- rast_to_sf(proj_city[[1]]) 
    
    out_ndvi_m_f <- sample_n(out_ndvi_m_f, 10000)
    
    # Extract PROVIDE CDH and add to df -----------------------------------------------
    
    out_ndvi_m_f$tas_1 <- NULL
    out_ndvi_m_f$green_density_100m <- exact_extract(green_density_100m, out_ndvi_m_f, "mean")
    
    terra::gdalCache(9000)
    
    tmax_out <- exact_extract(proj_city, out_ndvi_m_f, "mean")
    tmax_out <- bind_cols(tmax_out) 
    colnames(tmax_out) <- time(proj_city)
    
    ###
    
    colnames(tmax_out) <- paste0("d_", lubridate::yday(colnames(tmax_out)))
    
    nms <- unique(names(tmax_out))
    
    # Average columns with the same name
    tmax_out <- sapply(nms, function(x) rowMeans(tmax_out[names(tmax_out) %in% x], na.rm = T))
    tmax_out <- as.data.frame(tmax_out)
    
    tmax_out <- tmax_out[,1:365]
    
    colnames(tmax_out) <- time(proj_city)[1:365]
    
    ###
    
    out_ndvi_m_f = st_transform(out_ndvi_m_f, 4326)
    
    # collapse to mean of daily max for each month
    
    ## rowMedians
    tmax_out_m1_m <- pblapply(1:12, function(X){rowCustoms(as.matrix(tmax_out[,lubridate::month(unique(as.Date(time(proj_city)[1:365])))==X]), na.rm=T)})
    tmax_out_m1_m <- bind_cols(tmax_out_m1_m)
    tmax_out_m1_m <- tmax_out_m1_m - 273.15
    names(tmax_out_m1_m) <- paste0("mean.T2M_m", 1:12)
    
    ###
    
    out_ndvi_m_f <- bind_cols(out_ndvi_m_f,tmax_out_m1_m)
    
    # df filtering and cleaning -----------------------------------------------
    out_ndvi_m_bk <- out_ndvi_m_f # Backup variable
    
    ###
    
    library(kgc)
    
    out_ndvi_m_f$x <- NULL
    out_ndvi_m_f$y <- NULL
    
    out_ndvi_m_f <- st_coordinates(st_centroid(out_ndvi_m_f)) %>% as.data.frame() %>% bind_cols(out_ndvi_m_f)
    
    out_ndvi_m_f <- st_as_sf(out_ndvi_m_f)
    
    colnames(out_ndvi_m_f)[1:2] <- c("x", "y")
    
    cl <- climatezones
    
    out_ndvi_m_f$x_s <- RoundCoordinates(out_ndvi_m_f$x)
    out_ndvi_m_f$y_s <- RoundCoordinates(out_ndvi_m_f$y)
    
    out_ndvi_m_f <- merge(out_ndvi_m_f, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))
    out_ndvi_m_f$Clsmain <- substr(as.character(out_ndvi_m_f$Cls), 1, 1) # Add the main KGC [A (tropical), B (arid), C (temperate), D (continental), and E (polar)]
    
    ###
    # check with additional covariates
    
    # Local climate zone(s)
    # https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml
    setwd(stub0)
    lcz <- rast(path_lcz)
    
    out_ndvi_m_f$lcz <- exact_extract(lcz, out_ndvi_m_f, "majority")
    
    
    ######
    # Additional covariates for the urban environment
    # Buildings ------------------------------------------------------------
    ## Building Density/Height/Volume from GHS 
    build_v <- rast("socioecon/GHS_BUILT_V_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")
    build_h <- rast("socioecon/GHS_BUILT_H_AGBH_E2018_GLOBE_R2023A_4326_3ss_V1_0.tif")
    out_ndvi_m_f$build_v <- exact_extract(build_v, out_ndvi_m_f, "mean")
    out_ndvi_m_f$build_h <- exact_extract(build_h, out_ndvi_m_f, "mean")
    # Socio-economic covariate(s) ------------------------------------------------------------
    ## Population density
    pop_dens <- rast("socioecon/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif") # Pop density
    out_ndvi_m_f$pop_dens <- exact_extract(pop_dens, out_ndvi_m_f, "mean")
    # Make into quantiles
    # out_ndvi_m_f$pop_dens_d <- raster::cut(out_ndvi_m_f$pop_dens, quantile(out_ndvi_m_f$pop_dens, seq(0, 1, 0.2)))
    
    
    # Water Bodies ------------------------------------------------------------
    # options(timeout = 300)
    # download.file("https://globalland.cls.fr/webResources/catalogTree/netcdf/water_bodies/wb_300m_v2_monthly/2023/20230601/c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", mode="wb", destfile = "c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", time=120)
    #https://land.copernicus.eu/en/products/water-bodies/water-bodies-global-v2-0-300m#general_info
    water <- rast("old/c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", lyrs="WB")
    out_ndvi_m_f$water <- exact_extract(water, out_ndvi_m_f, "max")
    out_ndvi_m_f$water  <- ifelse(is.na(out_ndvi_m_f$water), 0, out_ndvi_m_f$water)
    
    # Altitude ----------------------------------------------------------------
    library(elevatr) #elevatr v0.99.0 NOTE: Version 0.99.0 of 'elevatr' uses 'sf' and 'terra'.  Use of the 'sp', 'raster', and underlying 'rgdal' packages by 'elevatr' is being deprecated; however, get_elev_raster continues to return a RasterLayer.  This will be dropped in future versions, so please plan accordingly.
    sf_temp <- out_ndvi_m_f
    
    out_ndvi_m_f <- get_elev_point(sf_temp %>% st_centroid(.), prj = 4326, src = "aws", z = 10)
    
    ###
    
    # tapply(out_ndvi_m_f$`mean.tas_2008-01-01`, out_ndvi_m_f$lcz, summary)
    
    ###
    
    colnames(out_ndvi_m_f)[5] <- "out_b"
    
    out_ndvi_m_monthly = dplyr::select(out_ndvi_m_f, x, y, out_b, Cls, build_h , build_v , pop_dens , water , elevation , lcz, contains("mean.T2M_m"))
    out_ndvi_m_monthly$geometry = NULL
    
    out_ndvi_m_monthly = dplyr::filter(out_ndvi_m_monthly, lcz<10)
    
    out_ndvi_m_monthly = reshape2::melt(out_ndvi_m_monthly, c(1:10))
    
    out_ndvi_m_monthly$variable <- parse_number(unlist(str_split(out_ndvi_m_monthly$variable, pattern = "_"))[c(FALSE, TRUE)])
    out_ndvi_m_monthly$variable <- factor(as.character(out_ndvi_m_monthly$variable), levels=c(1:12))
    
    out_ndvi_m_monthly$lcz <- factor(as.character(out_ndvi_m_monthly$lcz), levels=c(1:9))
    
    ##
    
    out_ndvi_m_monthly <- dplyr::filter(out_ndvi_m_monthly, is.finite(out_ndvi_m_monthly$value))
    
    ##
    
    m1 <- lapply(unique(out_ndvi_m_monthly$lcz), function(lcz_sel){
      
      print(lcz_sel)
      
      if(length(unique(out_ndvi_m_monthly %>% filter(lcz==lcz_sel) %>% pull(out_b)))>1){
        
        m1 <- feols(value ~  out_b:as.factor(variable) + build_h + build_v + elevation + water | variable, data=out_ndvi_m_monthly %>% filter(lcz==lcz_sel))
        return(coef(m1)[grepl("out_b", names(coef(m1)))]) } else{
          
          m1 <- c(rep(NA, 12))
          names(m1) <- paste0("out_b:as.factor(month)", 1:12)
          return(m1)
        }
      
      
    })
    
    names(m1) <- as.character(unique(out_ndvi_m_monthly$lcz))
    
    m1 <- bind_rows(m1)
    m1$lcz <- unique(out_ndvi_m_monthly$lcz)
    m1 <- reshape2::melt(m1, ncol(m1))
    m1$variable <- gsub("out\\_b\\:as\\.factor\\(variable\\)", "", m1$variable)
    
    m1$city <- ff_c[city_n]
    
    saveRDS(m1, paste0(stub0, "results/elasticities/daily/elasticities_ugs_t2m_daily_", min_max_avg, "_", ff_c[city_n], ".rds"))
    
  }
  
}

