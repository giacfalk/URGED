# This file writes a global gridded output template for climate zone-specific, local-climate zone specific CEs.
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

###

# cities_db

cities <- read_sf("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")
cities <- dplyr::group_by(cities, UC_NM_MN) %>% slice_max((P15))
cities <- dplyr::select(cities, UC_NM_MN, CTR_MN_ISO, GRGN_L2)
cities$geom <- NULL

#########

# share of LCZs by city

outer <- read_rds("results/output_template_ce_cities.rds")

outer <- dplyr::select(outer, -c(2:5))
outer$geometry <- NULL
colnames(outer)[4:21] <- paste0("lcz_frac_", colnames(outer)[4:21])

outer <- merge(outer, cities, "UC_NM_MN")

write.csv(outer, "output_data_ALPS/outer_ALPS.csv")

###
# Coefficient of GVI on temperature

outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[!grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("mean", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_mean_ALPS.csv")

#


outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[!grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("max", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_max_ALPS.csv")

###

outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[!grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("min", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_min_ALPS.csv")


###

# Coefficient of GVI on WBGT

outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("mean", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_wbgt_mean_ALPS.csv")

#


outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("max", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_wbgt_max_ALPS.csv")

###

outer_2n = list.files(path="results", full.names = T, pattern="monthly_coefs", recursive = T)
outer_2n = outer_2n[grepl("wbgt", outer_2n)]
outer_2n = outer_2n[grepl("min", outer_2n)]
outer_2s = bind_rows(lapply(outer_2n, read.csv), .id="city")
outer_2s$city =gsub("_mean", "", gsub(".csv", "", gsub("monthly_coefs_", "", basename(outer_2n)))[as.numeric(outer_2s$city)])
outer_2s$conf_high =  outer_2s$Estimate_d + 1.96*outer_2s$Std..Error_d
outer_2s$conf_low = outer_2s$Estimate_d - 1.96*outer_2s$Std..Error_d

outer_2s$conf_high_abs =  outer_2s$Estimate + 1.96*outer_2s$Std..Error
outer_2s$conf_low_abs = outer_2s$Estimate - 1.96*outer_2s$Std..Error

# outer_2s$Estimate_d[outer_2s$conf_high - outer_2s$conf_low >  1 ] = 0

outer_2 = dplyr::select(outer_2s, city, variable, lcz, Estimate_d, Estimate)
colnames(outer_2) = c("UC_NM_MN", "month", "lcz", "coef", "coef_abs")
outer_2 <- na.omit(outer_2)
#

# vv <- setdiff(outer$UC_NM_MN, outer_2$UC_NM_MN)
# vv <- expand.grid(UC_NM_MN=vv, month=1:12, lcz=unique(outer_2$lcz), coef=NA, coef_abs=NA)
# 
# outer_2 <- bind_rows(outer_2, vv)
# 
# outer_2 <- merge(outer_2, cities, "UC_NM_MN")
# 
# outer_2 <- dplyr::group_by(outer_2, GRGN_L2, month, lcz) %>% dplyr::mutate(coef=ifelse(is.na(coef), mean(coef, na.rm=T), coef), coef_abs=ifelse(is.na(coef_abs), mean(coef_abs, na.rm=T), coef_abs))
# 
# outer_2 <- na.omit(outer_2)

outer_2 = arrange(outer_2, UC_NM_MN, lcz, month)

write.csv(outer_2, "output_data_ALPS/outer_2_wbgt_min_ALPS.csv")

###
# Projection of GVI scenarios in the future

data <- read_rds(paste0(stub0, "/results/scenarios_ALPS/dfscen_pointlevel_ALPS.rds")) # This reads the file which is made in script 2f
outer_3 <- data %>%
  dplyr::select(1:6, ID_HDC_G0, starts_with("GRGRN_"), CTR_MN_ISO, lczshare_s, out_b_mean_s, starts_with("GVI_")) %>%
  arrange(city, lcz_filter_v3, year)
outer_3_long <- outer_3 %>% # Make first long with regards to GVI values
  pivot_longer(cols = c(out_b_mean_s, starts_with("GVI_")), 
               names_to = "scen_SGS", 
               values_to = "SGS")

outer_3_long <- outer_3_long %>%
  dplyr::rename(UC_NM_MN = city, lcz = lcz_filter_v3) %>%
  drop_na(SGS)

outer_3_long <- dplyr::filter(outer_3_long, UC_NM_MN %in% gsub("_min_wbgt", "", outer_2$UC_NM_MN))

outer_3_long <- dplyr::select(outer_3_long, UC_NM_MN, lcz, year, scen_SGS, SGS)

outer_3_long <- merge(outer_3_long, cities, "UC_NM_MN")
 
# outer_3_long <- dplyr::group_by(outer_3_long, UC_NM_MN, lcz, year, scen_SGS) %>%
#   dplyr::summarise(SGS=mean(SGS, na.rm=T))

write.csv(outer_3_long, "output_data_ALPS/outer_3_ALPS.csv")

###
# Output telling how much currently the GVI is contributing to decreasing the temperature compared to a counterfactual where GVI is 0 

outer_3_long_filt <- outer_3_long %>% filter(scen_SGS=="out_b_mean_s")  %>% mutate(lcz=factor(lcz, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, NA),
                                                                                                 labels = c("Compact high-rise", "Compact midrise", "Compact lowrise",
                                                                                                            "Open high-rise", "Open midrise", "Open lowrise",
                                                                                                            "Lightweight lowrise", "Large lowrise", "Sparsely built",
                                                                                                            "Heavy ind", "Dense trees", "Scattered trees",
                                                                                                            "Bush, scrub", "Low plants", "Bare rock", "Bare soil",
                                                                                                            "Water"))) %>% group_by(UC_NM_MN, lcz, scen_SGS, CTR_MN_ISO, GRGN_L2) %>% dplyr::summarise(SGS=mean(SGS, na.rm=T))


outer_merge <- merge(outer_2, outer_3_long_filt, by=c("UC_NM_MN", "lcz"))

outer_merge$delta <- outer_merge$SGS * outer_merge$coef
outer_merge <- dplyr::select(outer_merge, UC_NM_MN, CTR_MN_ISO, GRGN_L2, lcz, month, delta)

outer_merge = arrange(outer_merge, UC_NM_MN, month, lcz, month)

write.csv(outer_merge, "output_data_ALPS/outer_4_ALPS.csv")

####

setwd(paste0(stub0, "/URGED"))

