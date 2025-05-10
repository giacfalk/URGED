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
outname <- paste0(stub0, "data_provide_tmax_qv_monthly_cities.rds")

# First load city names -------------------------------------------------------------
setwd(path_provide)
city_d = tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)
city_d_c <- gsub("_", " ", city_d)

##

for(min_max_avg_sel in c("mean")){
  
  print(min_max_avg_sel)
  
  min_max_avg <- min_max_avg_sel
  
  # rowCustoms <- ifelse(min_max_avg=="mean", rowMeans, ifelse(min_max_avg=="max", rowMaxs, rowMins))
  rowCustoms <- rowMeans
  
  ##
  
  # GHS data ------------------------------------------------------------
  ghs <- read_sf(path_ghs_urbcentres) # Cities database
  
  # Process the data
  # Assign city names -------------------------------------------------------
  # Assignment is based on the first part of 'id' column, which has been previously mapped to lat/lon
  
  out_ndvi_m <- read_rds(path_gvi)
  
  # ######
  # 
  # out_ndvi_m_ff <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)
  # 
  # lcz <- rast(path_lcz)
  # out_ndvi_m_ff$lcz <- exact_extract(lcz, out_ndvi_m_ff, "majority")
  # 
  # ###
  # 
  # ggplot(out_ndvi_m_ff)+
  #   geom_bar(aes(x=city, fill=as.factor(lcz)), position="fill")+
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # 
  # ggsave(paste0(stub0, "results/lcz_share_bycity.png"), height=7, width=14)
  # 
  # ##
  # 
  # out_ndvi_m_ff$geometry = NULL
  # 
  # out_ndvi_m_ff <- dplyr::select(out_ndvi_m_ff, city, lcz)
  # out_ndvi_m_ff <- na.omit(out_ndvi_m_ff)
  # 
  # out_ndvi_m_ff_s <- out_ndvi_m_ff %>%
  #   arrange(city, lcz) %>% 
  #   group_by(city, lcz) %>%
  #   summarise(Count = n()) %>%
  #   group_by(city) %>%
  #   mutate(RelativeFrequency = Count / sum(Count)) %>%
  #   arrange(city, lcz)
  # 
  # out_ndvi_m_ff_s$Count = NULL
  # out_ndvi_m_ff_s$RelativeFrequency = out_ndvi_m_ff_s$RelativeFrequency*100
  # 
  # out_ndvi_m_ff_s <- out_ndvi_m_ff_s %>%
  #   pivot_wider(names_from = lcz, values_from = RelativeFrequency, values_fill = 0)
  # 
  # colnames(out_ndvi_m_ff_s)[2:18] <- paste0("LCZ_", colnames(out_ndvi_m_ff_s)[2:18])
  # 
  # out_ndvi_m_ff_s <- as.data.frame(out_ndvi_m_ff_s)
  # 
  # stargazer::stargazer(out_ndvi_m_ff_s, summary = F, out =  paste0(stub0, "results/table_countries.tex"))
  # 
  # stargazer::stargazer(out_ndvi_m_ff_s, summary = F, out =  paste0(stub0, "results/table_countries.html"), type="html")
  
  #############
  
  cdb = read_sf(paste0(stub0, "results/cities_database_climatezones.gpkg"))
  cdb = dplyr::select(cdb, 1:2)
  cdb$geom = NULL
  
  out_ndvi_m = merge(out_ndvi_m, cdb, by.x="city", by.y="UC_NM_MN")
  out_ndvi_m = dplyr::filter(out_ndvi_m, city!="Newcastle")
  
  ##
  ##
  
  ff = list.files(pattern=paste0("QV2M_year_daily_", min_max_avg, "_"), path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.name=T, recursive=T)
  ff_c = list.dirs( path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.names = F, recursive = F)
  
  
  ###
  
  nearest_word_match <- function(vec1, vec2, method = "lv") {
    library(stringdist)
    
    dist_matrix <- stringdistmatrix(vec1, vec2, method = method)
    nearest_indices <- apply(dist_matrix, 1, which.min)
    
    data.frame(
      original = vec1,
      closest_match = vec2[nearest_indices],
      distance = dist_matrix[cbind(1:length(vec1), nearest_indices)],
      stringsAsFactors = FALSE
    )
  }
  
  nnn <- nearest_word_match(unique(out_ndvi_m$city), ff_c)
  nnn <- dplyr::filter(nnn, original!="Nassau")
  nnn$distance <- NULL
  colnames(nnn) <- c("city", "city_provide")
  
  out_ndvi_m <- merge(out_ndvi_m, nnn, "city")
  
  
  # hh = list.files(pattern="QV2M_year_daily_mean_", path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.name=T, recursive=T)
  # ww = list.files(pattern="WS2M_year_daily_mean_", path="H:/ECIP/Falchetta/Downloads/compressed_daily", full.name=T, recursive=T)
  
  for(city_n in which(ff_c %in% out_ndvi_m$city_provide)){
    
    print(city_n)
    
    ff_filter <- ff[grepl(ff_c[city_n], ff)]
    
    proj_city =rast(ff_filter)
    city = ncdf4::nc_open(ff_filter[1])
    
    crs_city <- (parse_number(capture.output(print(city))[which(grepl("EPSG", capture.output(print(city))))]))
    
    crs(proj_city) = paste0("EPSG:", crs_city)
    
    ncdf4::nc_close(city)
    
    names(proj_city) <- paste0("QV2M_", 1:nlyr(proj_city))
    
    # GVI data --------------------------------------------------------
    
    out_ndvi_m_f = out_ndvi_m %>% dplyr::filter(name_PROVIDE==tolower(ff_c)[city_n])
    
    if(city_n==135){
      
      out_ndvi_m_f <- dplyr::filter(out_ndvi_m_f, country=="ES")
      
    }
    
    out_ndvi_m_f = dplyr::group_by(out_ndvi_m_f, x, y, city, country) %>% dplyr::summarise(out_b=mean(out_b, na.rm=T))
    
    ############
    
    # Write as sf spatial data frame --------------------------------------------------------
    out_ndvi_m_f <- st_as_sf(out_ndvi_m_f, coords=c("x", "y"), crs=4326, remove = F) %>% # Transform out_ndvi_m_f to sf object
      st_transform(crs_city) %>% st_buffer(50) 
    
    # Extract PROVIDE CDH and add to df -----------------------------------------------
    
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
    tas_out_m1_m <- pblapply(1:12, function(X){rowCustoms(as.matrix(tmax_out[,lubridate::month(unique(as.Date(time(proj_city)[1:365])))==X]), na.rm=T)})
    tas_out_m1_m <- bind_cols(tas_out_m1_m)
    tas_out_m1_m <- tas_out_m1_m
    names(tas_out_m1_m) <- paste0("mean.QV2M_m", 1:12)
    
    ###
    
    out_ndvi_m_f <- bind_cols(out_ndvi_m_f,tas_out_m1_m)
    
    # df filtering and cleaning -----------------------------------------------
    out_ndvi_m_bk <- out_ndvi_m_f # Backup variable
    out_ndvi_m_f$geometry <- NULL # Add a column "geometry"
    
    out_ndvi_m_f <- st_as_sf(out_ndvi_m_f, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)
    
    ###
    
    library(kgc)
    
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
    out_ndvi_m_f$pop_dens_d <- raster::cut(out_ndvi_m_f$pop_dens, quantile(out_ndvi_m_f$pop_dens, seq(0, 1, 0.2)))
    
    ## Poverty 
    pov_ind <- rast("socioecon/povmap-grdi-v1.tif") # Poverty indicator GRDI
    out_ndvi_m_f$pov_ind <- exact_extract(pov_ind, out_ndvi_m_f, "mean")
    
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
    out_ndvi_ms <- list()
    
    for(cityy in unique(out_ndvi_m_f$city)){
      print(cityy)
      out_ndvi_ms[[as.character(cityy)]] <- get_elev_point(sf_temp %>% dplyr::filter(city==cityy) %>% st_centroid(.), prj = 4326, src = "aws", z = 10)
    }
    
    out_ndvi_m_f <- bind_rows(out_ndvi_ms)
    
    ###
    
    tapply(out_ndvi_m_f$mean.QV2M_m6, out_ndvi_m_f$lcz, summary)
    
    ###
    
    out_ndvi_m_monthly = dplyr::select(out_ndvi_m_f, x, y, out_b, Cls, build_h , build_v , pop_dens , water , elevation , city , lcz, contains("mean.QV2M_m"))
    out_ndvi_m_monthly$geometry = NULL
    
    out_ndvi_m_monthly = dplyr::filter(out_ndvi_m_monthly, lcz<10)
    
    out_ndvi_m_monthly = reshape2::melt(out_ndvi_m_monthly, c(1:11))
    
    out_ndvi_m_monthly$variable <- parse_number(unlist(str_split(out_ndvi_m_monthly$variable, pattern = "_"))[c(FALSE, TRUE)])
    out_ndvi_m_monthly$variable <- factor(as.character(out_ndvi_m_monthly$variable), levels=c(1:12))
    
    out_ndvi_m_monthly$lcz <- factor(as.character(out_ndvi_m_monthly$lcz), levels=c(1:9))
    
    ##
    
    out_ndvi_m_monthly <- dplyr::filter(out_ndvi_m_monthly, is.finite(out_ndvi_m_monthly$value))
    
    ##
    
    out_ndvi_m_monthly_stats <- out_ndvi_m_monthly %>% group_by(city, lcz, variable) %>% dplyr::summarise(value=mean(value, na.rm=T))
    
    write.csv(out_ndvi_m_monthly_stats, paste0("results/URBCLIM_historical/qv_/qv_stats_", min_max_avg, "_", ff_c[city_n], ".csv"))
    
    #
    
    write.csv(out_ndvi_m_monthly %>% dplyr::select(city, lcz, variable, value), paste0("results/URBCLIM_historical/qv_/qv_data_", min_max_avg, "_", ff_c[city_n], ".csv"))
    
  }
  
}

####

setwd(paste0(stub0, "/URGED"))
