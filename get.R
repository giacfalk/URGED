
library(tidyverse)

city_d=read.csv("H:/ECIP/Falchetta/Downloads/compressed_daily/cities.csv", header = F)
city_d <- city_d$V1 

year = c(2008:2017)

lapply(city_d, function(cc){dir.create(paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", cc))})

gg <- expand.grid(city_d, year)
colnames(gg) <- c("city_d", "year")

options(timeout = max(600, getOption("timeout")))

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/WBGT_year_daily_max_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "WBGT_year_daily_max_", gg$year[X], ".nc"))})

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/WBGT_year_daily_min_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "WBGT_year_daily_min_", gg$year[X], ".nc"))})

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/WBGT_year_daily_mean_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "WBGT_year_daily_mean_", gg$year[X], ".nc"))})

###
###
###

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/T2M_year_daily_mean_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "T2M_year_daily_mean_", gg$year[X], ".nc"))})

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/T2M_year_daily_min_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "T2M_year_daily_min_", gg$year[X], ".nc"))})

lapply(1:nrow(gg), function(X){download.file(paste0("https://provide.marvin.vito.be/ftp/compressed_daily/", gg$city_d[X], "/T2M_year_daily_max_", gg$year[X], ".nc"), mode = "wb", destfile = paste0("H:/ECIP/Falchetta/Downloads/compressed_daily/", gg$city_d[X], "/", "T2M_year_daily_max_", gg$year[X], ".nc"))})

