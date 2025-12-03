
# Build a database for a datframe containing ugs points for all 180 cities
# Determine the "frontrunners" or "best cities" for urban green space by climate zone and subregion 
rm(list=ls(all=TRUE)) # Removes all previously created variables
# Working directory [RStudio] -------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Define a variable with the current WD

# Libraries etc ----------------------------
library(rstudioapi)
library(conflicted)
library(tidyverse)
library(terra) # For the LCZ data inclusion

out_ndvi_m <- read_rds(paste0(stub0, "ugs/after_points_100425_completedatabase.rds"))

r <- read.csv("cities/dictionary_GHS-provide.csv")
out_ndvi_m <- dplyr::filter(out_ndvi_m, city %in% r$city.GHS)

####

library(modelsummary)

out_ndvi_m$lcz_filter_v3 <- factor(out_ndvi_m$lcz_filter_v3, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

out_ndvi_m$city <- factor(out_ndvi_m$city)

#

out_ndvi_m <- out_ndvi_m %>% group_by(city, lcz_filter_v3, x, y) %>% dplyr::summarize(out_b=mean(out_b, na.rm=T))

#

datasummary(
  city * lcz_filter_v3 ~ out_b * (Mean + SD + Min + Max),
  data = out_ndvi_m,
  output = "sgs_levels_desc.tex",        # writes directly to file
  fmt = 2,                     # optional: decimals
  title = "Distribution of current SGS by city and LCZ (2017-2023 average)"
)


