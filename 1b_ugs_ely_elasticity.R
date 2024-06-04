
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
########

df <- read_rds("energy/processed_data.rds")
df <- df[df$cons_month>50,]

# set hot months categorical variable

df$hotmonths <- as.factor(ifelse(as.numeric(df$month)>5 & as.numeric(df$month)<9, 1, 0))

df$quartile_reddito <- cut(df$reddito_medio, quantile(df$reddito_medio, by=seq(0, 1, 0.2), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"))

reg_def2 <- feols(log(cons_month) ~ log(tmax):hotmonths + log(tmax):log(gvi_mean):hotmonths |  pod + comune + year + month + month^year, data=df)
summary(reg_def2, cluster = ~comune+pod)

write_rds(reg_def2, paste0(res_dir, "ugs_ely_elasticity_model.rds"))

