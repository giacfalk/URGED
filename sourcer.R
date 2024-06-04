
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

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation'

setwd(stub)

# Set directories
hbs_dir <- paste0(stub, 'hbs_data/', sep ='')

#################
#################

source("1a_ugs_drivers_cross_section.R")
source("1b_ugs_ely_elasticity.R")
source("1c_ugs_heat_mortality.R")

source("2a_ugs_project_citylevel.R")
source("2b_ely_ugs_project.R")
source("2c_project_mortality_ugs.R")

