
##############################################################################

# This Rscript: 

#   1) estimate the elasticity of mortality rates to hot temperatures

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

# Load mortality data
mort <- readRDS('socioecon/mort_era5_weekly_nuts2.rds')

# Mortality rates in 100,000s
mort <- mort %>% mutate(mort_rate = value/pop*100000,
                        ln_mort_rate = log(mort_rate),
                        covid = ifelse(year >= 2020, 1, 0), ISO3 = substr(NUTS_ID, 1, 2)) %>% rename(age_group=age)

# Weight
tpop <- mort %>% drop_na(pop) %>% group_by(ISO3, age_group, year) %>% summarise(totpop = sum(pop))
mort <- merge(mort, tpop, by = c("year", "ISO3", "age_group"))
mort <- mort %>% mutate(weight = pop/totpop)

###

m1 <- feols(log(mort_rate) ~ CDD:age_group + HDD:age_group | NUTS_ID + year + NUTS_ID^year + week + as.factor(covid), data=mort, weights = ~ weight, combine.quick = F)
summary(m1, cluster=c("NUTS_ID", "week"))

library(marginaleffects)

p_1 <- plot_predictions(m1, condition = c("CDD", "age_group")) 
p_2 <-plot_predictions(m1, condition = c("HDD", "age_group")) 

library(patchwork)

p_1 + p_2

preferred_specification <- m1

###########################################################################################################

# write the model objects

write_rds(preferred_specification, paste0(res_dir, "mortality_reg_weekly.rds"))



