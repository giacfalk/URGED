
##############################################################################

# This Rscript: 

#   1) invoke the different scripts to run the different parts of the analysis

##############################################################################

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
require(data.table)
require(tidyverse)
require(fixest)
require(haven)
require(fabricatr)
require(texreg)
require(xtable)
require(stargazer)
require(effects)
require(marginaleffects)
library(sf)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
stub <- paste0(getwd(), "/")

# Set directories
hbs_dir <- paste0(stub, 'hbs_data/', sep ='')

setwd(paste0(stub, "/URGED"))

#################
#################

file.edit("0_get_climate_change_provide.R") # obtain city-level climate data and projections from PROVIDE, combine them and write them to raster files
file.edit("0c_process_climate_data_comuni.R") # read and process ERA5 data for Italian municipalities
file.edit("0_comparison_cdh_era5_10comuni.R") # compare PROVIDE climate data with ERA5 data for Italian municipalities
file.edit("0_show_ugs_policy_meaning.R") # useful for understanding what a given GVI value corresponds to visually (from Google Street View)
file.edit("0_get_pollution_data.R") # function to obtain pollution data from AQI API

file.edit("1a_ugs_drivers_cross_section.R") # try to understand what socio-economic and climate drivers determine a given GVI value
file.edit("1b_ugs_heat_metrics.r") # estimate cooling capacity/efficiency of GVI in different urban areas using PROVIDE climate data
file.edit("1b_ugs_heat_metrics_comuni.r") # estimate cooling capacity/efficiency of GVI in different urban areas using PROVIDE climate data in Italian municipalities
file.edit("1c_ugs_ely_elasticity.R") # estimate GVI impact as a mediator in temperature-electricity demand relation in Italian municipalities
file.edit("1d_ugs_heat_mortality.R") # estimate GVI impact as a mediator in temperature-mortality relation in EU NUTS regions
file.edit("1e_ugs_pollution_level.R") # estimate GVI impact as a determinant of local pollution levels

file.edit("2a_ugs_project_citylevel.R") # attempt to project GVI at the city level 
file.edit("2b_ely_ugs_project.R") # attempt to project electricity use at the city level 
file.edit("2c_project_mortality_ugs.R")  # attempt to project mortality at the city level 

file.edit("3a_policy_simulation_climate_change_ugs_heat_metrics.r") # perform policy simulation to see effect on heat metrics from GVI policy
file.edit("3b_policy_simulation_climate_change_ugs_electricity.r") # perform policy simulation to see effect on electricity use from GVI policy

