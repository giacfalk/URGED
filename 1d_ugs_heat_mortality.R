
##############################################################################

# This Rscript: 

#   1) estimate the elasticity of mortality rates to hot temperatures and the mediating role of UGS

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

###
# Merge with UGS data

load("ugs/nuts_calculation/after_points.Rdata"); rm(gvs); gc()

sf <- read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson")
sf <- filter(sf, NUTS_ID %in% mort$NUTS_ID)

out_ndvi_m <- st_as_sf(out_ndvi_m, coords = c("x", "y"), crs=4326, remove = F)

out_ndvi_mm <- st_join(out_ndvi_m, sf %>% dplyr::select(NUTS_ID), st_intersects)
out_ndvi_mm$geometry <- NULL
out_ndvi_mm <- out_ndvi_mm %>% group_by(NUTS_ID, year) %>% dplyr::summarise(out_b=mean(out_b, na.rm=T))

mort <- merge(mort, out_ndvi_mm, by=c("NUTS_ID", "year"))

###

unique(mort$NUTS_ID)
unique(mort$year)

# Mortality rates in 100,000s
mort <- mort %>% dplyr::mutate(mort_rate = value/pop*100000,
                        ln_mort_rate = log(mort_rate),
                        covid = ifelse(year >= 2020, 1, 0), ISO3 = substr(NUTS_ID, 1, 2)) %>% dplyr::rename(age_group=age)

# Weight
tpop <- mort %>% drop_na(pop) %>% group_by(ISO3, age_group, year) %>% dplyr::summarise(totpop = sum(pop))
mort <- merge(mort, tpop, by = c("year", "ISO3", "age_group"))
mort <- mort %>% mutate(weight = pop/totpop)

###

m1 <- feols(log(mort_rate) ~ CDD:age_group + HDD:age_group | NUTS_ID + year + NUTS_ID^year + week + as.factor(covid), data=mort, weights = ~ weight, combine.quick = F)
summary(m1, cluster=c("NUTS_ID", "week"))

m2 <- feols(mort_rate ~ CDD:age_group + HDD:age_group + CDD:age_group:out_b | NUTS_ID + year + NUTS_ID^year + week + as.factor(covid), data=mort, weights = ~ weight, combine.quick = F)
summary(m2, cluster=c("NUTS_ID", "week"))

m3 <- feols(mort_rate ~ CDD + HDD + CDD:out_b +  HDD:out_b | ISO3 + NUTS_ID + year + NUTS_ID^year + week + as.factor(covid) + age_group, data=mort, weights = ~ weight, combine.quick = F)
summary(m3, cluster=c("NUTS_ID", "week"))

plot_predictions(m3, condition = c("CDD", "out_b")) 
plot_predictions(m3, condition = c("HDD", "out_b")) 


library(marginaleffects)

p_1 <- plot_predictions(m2, condition = c("CDD", "out_b", "age_group")) 
p_2 <-plot_predictions(m2, condition = c("HDD", "out_b", "age_group")) 

library(patchwork)

p_1 + p_2

plot_predictions(m1, condition = c("CDD", "out_b", "age_group"))+
  scale_colour_brewer(palette="Greens")+
  scale_fill_brewer(palette="Greens")

preferred_specification <- m1

###########################################################################################################

# write the model objects

write_rds(preferred_specification, paste0(res_dir, "ugs_mortality_model.rds"))
