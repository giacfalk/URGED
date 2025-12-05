##############################################################################

# This Rscript: 

#   1) invoke the different scripts to run the different parts of the analysis
# Also chech the information contained in README.md
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
library(conflicted)
conflicts_prefer(dplyr::filter) # Always use the filter command from dplyr, even if stats is loaded
conflicts_prefer(dplyr::select) # Always use the select command from dplyr, even if MASS is loaded
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
stub <- paste0(getwd(), "/")

# Set directories
hbs_dir <- paste0(stub, 'hbs_data/', sep ='')

setwd(paste0(stub, "/URGED"))

# Get helper files
source("support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("support/fcts_helpers_debug.R")
source("support/fct_scenarios.R") # Here the "filtering" function can be found

#################
#################

source("0_build_cities_database.R") # write a database of city boundaries with climate zone information
source("0_build_ugspoints_citynames.R") # parse city name to ugs points dataframe and write it to "after_points_100425_citynames.rds" file
source("0_build_ugspoints_database.R") # write a database of within-city units of analysis with climate/local climate zone information
source("0_citynames_harmonization.R") # write a database of city boundaries with climate zone information

###

source("0_output_template.R") # This file writes a global gridded output template for climate zone-specific, local-climate zone specific CEs.
source("0_show_ugs_policy_meaning.R") # useful for understanding what a given GVI value corresponds to visually (from Google Street View)

########
########

source("1_monthly_ugs_elasticities.R") # estimate GVI impact as a determinant of local mean, maximum, and minimum air temperature in each city
source("1_monthly_ugs_elasticities_WBGT.R") # estimate GVI impact as a determinant of local mean, maximum, and minimum WBGT in each city
source("1_summary_table_compare_across_metrics.R") # produce a table comparing the estimated impact of GVI across the climate metrics considered

source("0_tstats_city_lcz_urbclim.R") # This file writes a global gridded output templatefor climate zone-specific, local-climate zone specific CEs.
source("0_calculate_climate_change_markups.R") # create a dataframe of the deltas of climate variables in each city in future years according to CMIP6 models
source("4_process_humidity_data.R")
source("4_process_lst_data.R")
source("0_wbt_wbgt.R") # This script computes an estimation of future wet bulb globe temperature based on the data provided

########
########
source("2_ugs_frontrunner_cities.R")  # determine lower and upper bounds of SGS by main KGC as well as LCZ
source("2_project_future_ugs_pointwise.R") # Project GVI
# source("2ALPS_project_future_ugs_pointwise.R") # attempt to project GVI at the city level (ALPS project variant)
########
########

source("3_write_output_chilled.R") # write outputs from empirical and projection analysis, to be used to run policy simulations and produce results summary plots and tables
# source("3_write_output_chilled_ALPS.R") # write outputs from empirical and projection analysis, to be used to run policy simulations and produce results summary plots and tables (ALPS project variant)

########
########

source("4_policy_simulation_climate_change_ugs_heat_metrics.r") # perform policy simulation to see effect on heat metrics from GVI policy

########
# Paper scripts

source("figures_scripts/fig_1.R")
source("figures_scripts/regression_boxplots.R")
source("figures_scripts/table_scenarios.R")
source("figures_scripts/21_project_future_ugs_pointwise_plotting.R")
source("figures_scripts/plot_scenarios.R")
source("figures_scripts/fig_4_new.r")
source("figures_scripts/map_counterbalancing_wbgt_mean.R")
source("figures_scripts/map_counterbalancing_tas_min.R")
source("figures_scripts/map_counterbalancing_wbgt_max.R")
