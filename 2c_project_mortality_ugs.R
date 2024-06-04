
##############################################################################

# This Rscript: 

#   1) project future mortality rates

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

################

# Set directories
hbs_dir <- paste0(stub, 'hbs_data/', sep ='')
clmt_dir <- paste0(stub, 'climate_data/', sep ='')
popl_dir <- paste0(stub, 'population_data/', sep ='')
resl_dir <- paste0(stub, 'results/', sep ='')
mort_dir <- paste0(stub, 'mortality_data/', sep ='')

# extract regression coefficients to calculate impacts

reg <- read_rds(paste0(resl_dir, "mortality_reg_weekly.rds"))
coefs_reg <- coef(reg)

# produce impacts at the grid-cell level

cmip6_data_mortality <- read_rds(paste0(clmt_dir, "cmip6_data_mortality.rds"))

rm(hbs); gc()

###

cmip6_data_mortality$pr_his_65 <- coefs_reg[3] * cmip6_data_mortality$CDD_hist + coefs_reg[6] * cmip6_data_mortality$HDD_hist

cmip6_data_mortality$pr_245_65 <- coefs_reg[3] * cmip6_data_mortality$CDD_245 + coefs_reg[6] * cmip6_data_mortality$HDD_245

cmip6_data_mortality$shock_pctg_245_65 <- exp(cmip6_data_mortality$pr_245_65) - exp(cmip6_data_mortality$pr_his_65)
cmip6_data_mortality$shock_pctg_245_65 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_245_65) | is.na(cmip6_data_mortality$shock_pctg_245_65), NA, cmip6_data_mortality$shock_pctg_245_65)

cmip6_data_mortality$pr_370_65 <- coefs_reg[3] * cmip6_data_mortality$CDD_370 + coefs_reg[6] * cmip6_data_mortality$HDD_370

cmip6_data_mortality$shock_pctg_370_65 <- exp(cmip6_data_mortality$pr_370_65) - exp(cmip6_data_mortality$pr_his_65)
cmip6_data_mortality$shock_pctg_370_65 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_370_65) | is.na(cmip6_data_mortality$shock_pctg_370_65), NA, cmip6_data_mortality$shock_pctg_370_65)

cmip6_data_mortality$pr_585_65 <- coefs_reg[3] * cmip6_data_mortality$CDD_585 + coefs_reg[6] * cmip6_data_mortality$HDD_585

cmip6_data_mortality$shock_pctg_585_65 <- exp(cmip6_data_mortality$pr_585_65) - exp(cmip6_data_mortality$pr_his_65)
cmip6_data_mortality$shock_pctg_585_65 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_585_65) | is.na(cmip6_data_mortality$shock_pctg_585_65), NA, cmip6_data_mortality$shock_pctg_585_65)
cmip6_data_mortality <- st_as_sf(cmip6_data_mortality, coords=c("x", "y"), crs=4326)

cmip6_data_mortality$pr_his_020 <- coefs_reg[1] * cmip6_data_mortality$CDD_hist + coefs_reg[4] * cmip6_data_mortality$HDD_hist

cmip6_data_mortality$pr_245_020 <- coefs_reg[1] * cmip6_data_mortality$CDD_245 + coefs_reg[4] * cmip6_data_mortality$HDD_245

cmip6_data_mortality$shock_pctg_245_020 <- exp(cmip6_data_mortality$pr_245_020) - exp(cmip6_data_mortality$pr_his_020)
cmip6_data_mortality$shock_pctg_245_020 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_245_020) | is.na(cmip6_data_mortality$shock_pctg_245_020), NA, cmip6_data_mortality$shock_pctg_245_020)

cmip6_data_mortality$pr_370_020 <- coefs_reg[1] * cmip6_data_mortality$CDD_370 + coefs_reg[4] * cmip6_data_mortality$HDD_370

cmip6_data_mortality$shock_pctg_370_020 <- exp(cmip6_data_mortality$pr_370_020) - exp(cmip6_data_mortality$pr_his_020)
cmip6_data_mortality$shock_pctg_370_020 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_370_020) | is.na(cmip6_data_mortality$shock_pctg_370_020), NA, cmip6_data_mortality$shock_pctg_370_020)

cmip6_data_mortality$pr_585_020 <- coefs_reg[1] * cmip6_data_mortality$CDD_585 + coefs_reg[4] * cmip6_data_mortality$HDD_585

cmip6_data_mortality$shock_pctg_585_020 <- exp(cmip6_data_mortality$pr_585_020) - exp(cmip6_data_mortality$pr_his_020)
cmip6_data_mortality$shock_pctg_585_020 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_585_020) | is.na(cmip6_data_mortality$shock_pctg_585_020), NA, cmip6_data_mortality$shock_pctg_585_020)
cmip6_data_mortality <- st_as_sf(cmip6_data_mortality, coords=c("x", "y"), crs=4326)

###

cmip6_data_mortality$pr_his_2065 <- coefs_reg[2] * cmip6_data_mortality$CDD_hist + coefs_reg[5] * cmip6_data_mortality$HDD_hist

cmip6_data_mortality$pr_245_2065 <- coefs_reg[2] * cmip6_data_mortality$CDD_245 + coefs_reg[5] * cmip6_data_mortality$HDD_245

cmip6_data_mortality$shock_pctg_245_2065 <- exp(cmip6_data_mortality$pr_245_2065) - exp(cmip6_data_mortality$pr_his_2065)
cmip6_data_mortality$shock_pctg_245_2065 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_245_2065) | is.na(cmip6_data_mortality$shock_pctg_245_2065), NA, cmip6_data_mortality$shock_pctg_245_2065)

cmip6_data_mortality$pr_370_2065 <- coefs_reg[2] * cmip6_data_mortality$CDD_370 + coefs_reg[5] * cmip6_data_mortality$HDD_370

cmip6_data_mortality$shock_pctg_370_2065 <- exp(cmip6_data_mortality$pr_370_2065) - exp(cmip6_data_mortality$pr_his_2065)
cmip6_data_mortality$shock_pctg_370_2065 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_370_2065) | is.na(cmip6_data_mortality$shock_pctg_370_2065), NA, cmip6_data_mortality$shock_pctg_370_2065)

cmip6_data_mortality$pr_585_2065 <- coefs_reg[2] * cmip6_data_mortality$CDD_585 + coefs_reg[5] * cmip6_data_mortality$HDD_585

cmip6_data_mortality$shock_pctg_585_2065 <- exp(cmip6_data_mortality$pr_585_2065) - exp(cmip6_data_mortality$pr_his_2065)
cmip6_data_mortality$shock_pctg_585_2065 <- ifelse(is.infinite(cmip6_data_mortality$shock_pctg_585_2065) | is.na(cmip6_data_mortality$shock_pctg_585_2065), NA, cmip6_data_mortality$shock_pctg_585_2065)
cmip6_data_mortality <- st_as_sf(cmip6_data_mortality, coords=c("x", "y"), crs=4326)

# plot shock for 65+

cmip6_data_mortality$binned_c <- ((exp(cmip6_data_mortality$pr_585_65)/exp(cmip6_data_mortality$pr_his_65))-1)*100
cmip6_data_mortality$binned_c <- ifelse(is.infinite(cmip6_data_mortality$binned_c) | is.na(cmip6_data_mortality$binned_c), 0, cmip6_data_mortality$binned_c)

cmip6_data_mortality$binned <- cut(cmip6_data_mortality$binned_c, breaks = c(-Inf, -100, -50, -25, -5, 0,  5, 25, 50, 100, Inf))

library(sf)

shape <- read_sf(paste0(mort_dir, "NUTS_RG_60M_2021_4326.geojson"))

ggplot()+
  theme_void()+
  geom_sf(data=cmip6_data_mortality %>% filter(week==30), aes(colour=(binned)))+
  geom_sf(data= shape,fill="transparent", lwd=0.001)+
  scale_colour_brewer(palette = "RdBu", name="%", na.value = "grey50", direction = -1, drop = FALSE)+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("Change in the weekly elderly mortality rate for individuals >65 in mid-July, SSP370 w.r.t. historical climate")

####

city_sel <- "Palermo"

d_sel <- cmip6_data_mortality %>% filter(NAME_LATN==city_sel) %>% group_by(week) %>% dplyr::summarise_if(is.numeric, mean, na.rm=T)

ggplot()+
  theme_classic()+
  geom_line(data=d_sel, aes(x=week, y=exp(pr_585_65), colour="585"))+
  geom_line(data=d_sel, aes(x=week, y=exp(pr_370_65), colour="370"))+
  geom_line(data=d_sel, aes(x=week, y=exp(pr_245_65), colour="245"))+
  geom_line(data=d_sel, aes(x=week, y=exp(pr_his_65), colour="historical"))+
  ggtitle("Contribution of climate on the weekly elderly mortality rate")

# aggregate impacts at the NUTS-level

cmip6_data_mortality$geometry <- NULL

cmip6_data_mortality_nuts <- cmip6_data_mortality %>%  group_by(NUTS_ID, week) %>% dplyr::summarise_at(colnames(cmip6_data_mortality)[20:40], mean, na.rm=T)

cmip6_data_mortality_nuts_mort_rates <- cmip6_data_mortality_nuts %>% dplyr::select(-contains("shock"))
cmip6_data_mortality_nuts <- cmip6_data_mortality_nuts %>% dplyr::select(-contains("pr_"))

cmip6_data_mortality_nuts <- reshape2::melt(cmip6_data_mortality_nuts, c(1,2))
cmip6_data_mortality_nuts$variable <- as.character(cmip6_data_mortality_nuts$variable)

cmip6_data_mortality_nuts$scenario <- NA
cmip6_data_mortality_nuts$scenario[grepl("245", cmip6_data_mortality_nuts$variable)] <- "SSP2"
cmip6_data_mortality_nuts$scenario[grepl("370", cmip6_data_mortality_nuts$variable)] <- "SSP3"
cmip6_data_mortality_nuts$scenario[grepl("585", cmip6_data_mortality_nuts$variable)] <- "SSP5"

cmip6_data_mortality_nuts$agegr <- NA
cmip6_data_mortality_nuts$agegr[grepl("_020", cmip6_data_mortality_nuts$variable)] <- "0-19"
cmip6_data_mortality_nuts$agegr[grepl("_2065", cmip6_data_mortality_nuts$variable)] <- "19-69"
cmip6_data_mortality_nuts$agegr[grepl("_65", cmip6_data_mortality_nuts$variable)] <- "69+"

cmip6_data_mortality_nuts$variable <- NULL

pop <- read_rds(paste0(popl_dir, "pop_projections_age_stratified_nuts1.rds"))

cmip6_data_mortality_nuts <- merge(cmip6_data_mortality_nuts, pop, by.x=c("NUTS_ID", "scenario", "agegr"), by.y=c("NUTS_ID", "SCENARIO", "AGEGR"))

cmip6_data_mortality_nuts$climate_shock_2050 <- (cmip6_data_mortality_nuts$P_2050/100000 * (cmip6_data_mortality_nuts$value))

cmip6_data_mortality_nuts <- cmip6_data_mortality_nuts %>% dplyr::group_by(NUTS_ID, scenario, agegr) %>% dplyr::summarise(P_2050=mean(P_2050, na.rm=T), climate_shock_2050=sum(climate_shock_2050, na.rm=T))

cmip6_data_mortality_nuts$pop_with_impact_2050 <- cmip6_data_mortality_nuts$P_2050 - cmip6_data_mortality_nuts$climate_shock_2050

write_rds(cmip6_data_mortality_nuts, paste0(resl_dir, "cmip6_mortality_impacts_nuts.rds"))

###

cmip6_data_mortality_ctr <- cmip6_data_mortality_nuts %>% mutate(NUTS_ID=substr(NUTS_ID, 1, 2)) %>%  group_by(NUTS_ID, scenario, agegr) %>% dplyr::summarise_if(is.numeric, sum, na.rm=T)

cmip6_data_mortality_ctr$climate_shock_2050_pctg <- ((cmip6_data_mortality_ctr$pop_with_impact_2050 / cmip6_data_mortality_ctr$P_2050) - 1)

write_rds(cmip6_data_mortality_ctr, paste0(resl_dir, "cmip6_mortality_impacts_country.rds"))

(cmip6_data_mortality_ctr %>% dplyr::group_by(scenario, agegr) %>% dplyr::summarise(climate_shock_2050_pos=sum(climate_shock_2050[climate_shock_2050>0], na.rm=T), climate_shock_2050_neg=sum(climate_shock_2050[climate_shock_2050<0], na.rm=T)))

###

cmip6_data_mortality_nuts_mort_rates <- reshape2::melt(cmip6_data_mortality_nuts_mort_rates, c(1,2))
cmip6_data_mortality_nuts_mort_rates$variable <- as.character(cmip6_data_mortality_nuts_mort_rates$variable)

cmip6_data_mortality_nuts_mort_rates$scenario <- NA
cmip6_data_mortality_nuts_mort_rates$scenario[grepl("his", cmip6_data_mortality_nuts_mort_rates$variable)] <- "his"
cmip6_data_mortality_nuts_mort_rates$scenario[grepl("245", cmip6_data_mortality_nuts_mort_rates$variable)] <- "SSP2"
cmip6_data_mortality_nuts_mort_rates$scenario[grepl("370", cmip6_data_mortality_nuts_mort_rates$variable)] <- "SSP3"
cmip6_data_mortality_nuts_mort_rates$scenario[grepl("585", cmip6_data_mortality_nuts_mort_rates$variable)] <- "SSP5"

cmip6_data_mortality_nuts_mort_rates$agegr <- NA
cmip6_data_mortality_nuts_mort_rates$agegr[grepl("_020", cmip6_data_mortality_nuts_mort_rates$variable)] <- "0-19"
cmip6_data_mortality_nuts_mort_rates$agegr[grepl("_2065", cmip6_data_mortality_nuts_mort_rates$variable)] <- "19-69"
cmip6_data_mortality_nuts_mort_rates$agegr[grepl("_65", cmip6_data_mortality_nuts_mort_rates$variable)] <- "69+"

cmip6_data_mortality_nuts_mort_rates$variable <- NULL

cmip6_data_mortality_nuts_mort_rates$value <- exp(cmip6_data_mortality_nuts_mort_rates$value)

pop <- read_rds(paste0(popl_dir, "pop_projections_age_stratified_nuts1.rds"))
pop_2050 <- pop %>% dplyr::select(-P_2020) %>% rename(pop=P_2050)
pop_2020 <- pop %>% dplyr::select(P_2050) %>% rename(pop=P_2050) %>% dplyr::group_by(NUTS_ID, AGEGR) %>% dplyr::summarise(pop=mean(pop, na.rm=T)) %>% mutate(SCENARIO="his")
pop_2050$Region <- NULL
pop <- bind_rows(pop_2020, pop_2050)

cmip6_data_mortality_nuts_mort_rates <- merge(cmip6_data_mortality_nuts_mort_rates, pop, by.x=c("NUTS_ID", "scenario", "agegr"), by.y=c("NUTS_ID", "SCENARIO", "AGEGR"))

cmip6_data_mortality_mort_rates_ctr <- cmip6_data_mortality_nuts_mort_rates %>% mutate(NUTS_ID=substr(NUTS_ID, 1, 2)) %>%  group_by(NUTS_ID, scenario, agegr, week) %>% dplyr::mutate(value_s=value*pop) 

cmip6_data_mortality_mort_rates_ctr <- cmip6_data_mortality_mort_rates_ctr %>%  group_by(NUTS_ID, scenario, agegr) %>% dplyr::summarise(pop=sum(pop, na.rm=T), value_s=sum(value_s, na.rm=T))

cmip6_data_mortality_mort_rates_ctr$mr = cmip6_data_mortality_mort_rates_ctr$value_s/cmip6_data_mortality_mort_rates_ctr$pop
cmip6_data_mortality_mort_rates_ctr$value_s <- NULL
cmip6_data_mortality_mort_rates_ctr$pop <- NULL

cmip6_data_mortality_mort_rates_ctr <- pivot_wider(cmip6_data_mortality_mort_rates_ctr, id_cols = c(NUTS_ID, agegr), names_from = scenario, values_from = mr)

cmip6_data_mortality_mort_rates_ctr <- cmip6_data_mortality_mort_rates_ctr[,c(1,2,6, 3:5)]
