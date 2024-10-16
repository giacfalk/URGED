
##############################################################################

# This Rscript: 

#   1) estimate the elasticity of electricity consumption to hot temperatures

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

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

########

df <- read_rds("energy/processed_data.rds")
df <- df[df$cons_month>50,]

####

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

###

# set hot months categorical variable

df$hotmonths <- as.factor(ifelse(as.numeric(df$month)>5 & as.numeric(df$month)<9, 1, 0))

df$quartile_reddito <- cut(df$reddito_medio, quantile(df$reddito_medio, by=seq(0, 1, 0.2), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"))

################

reg_def2 <- feols(log(cons_month) ~ tmax:hotmonths + tmax:log(gvi_mean):hotmonths |  pod + comune + year + month + region^year, data=df)
summary(reg_def2, cluster = ~comune+pod)

reg_def2_lev <- feols(log(cons_month) ~ tmax:hotmonths + tmax:gvi_mean:hotmonths |  pod + comune + year + month + region^year, data=df, combine.quick = F)
summary(reg_def2_lev, cluster = ~comune+pod)

etable(reg_def2_lev, cluster = ~comune+pod, export = "results/regtab2.png")

coef(reg_def2)[4]*100

write_rds(reg_def2, paste0(res_dir, "ugs_ely_elasticity_model.rds"))

