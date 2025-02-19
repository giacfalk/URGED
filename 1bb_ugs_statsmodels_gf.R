# Fixed effects models to explore the relationship between GVI and climate vars (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
# - City-Level Analysis
# - Visualize the results
# - map plots
rm(list=ls(all=TRUE)) # Removes all previously created variables
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
rm(list=ls(all=TRUE)) # Removes all previously created variables
library(tidyverse)
library(fixest)
library(sf)
library(pdftools) # For printing tables from Latex into pdf. Requires latex installation
library(marginaleffects) # For marginal effects package
library(modelsummary) # For model summary
library(gtsummary)


# Source helper files and functions ---------------------------------------
source("URGED/fcts_labelers_colors.R")
source("URGED/fcts_helpers_debug.R")

# Directories and settings ----------------------------
path_data <- "data_provide_cdh_gvi_143cities_withcovariates.rds"
path_results <- "results/regtab/"
## For additional data
path_subregions <- "wup/country-subregions_adapted.csv" # Path to citylist from WUP2018 F22, adapated to country names in URGED 

# Data Preparation ----------------------------
df <- read_rds(path_data) # The main data frame created in 1b_ugs_heat_metrics.R
df <- dplyr::filter(df, t > 0) # We remove t <= 0 as we use log(t) later on and there are no negative days. It appears though that all t > 0.

df <- filter(df, lcz<10)

# Add some more quantities:
## Add factor labels and levels
df <- fct_factorlabels(df)

# Debug: Make a list of cities to see how many are in each climate and subregion
fct_count_Cls_SR(df)

##

tapply(df$t_max, df$city, summary)

##

m12 <- feols(t ~ out_b_mean:lcz + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "twoway"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))

##

# BOTOTM LINE: very difficult to find a big effect with the yearly avg data!

m12 <- feols(t_max ~ out_b_mean:lcz + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "twoway"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))

##

m12 <- feols(t_min ~ out_b_mean:lcz + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "twoway"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))

##

m12 <- feols((t_max+t_min)/2 ~ out_b_mean:lcz + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "twoway"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))


##
##

m12 <- feols(t ~ out_b_mean:Cls + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "cluster"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))

m12 <- feols(t_max ~ out_b_mean:Cls + water + elevation + build_h + build_v + pop_dens | city + lcz + city^lcz, data=df, combine.quick = F)
summary(m12, "twoway")

coefplot(summary(m12, "cluster"), drop=c("water", "elevation", "build_h", "build_v", "pop_dens"))


