
##############################################################################

# This Rscript: 

#   1) project impact on energy demand with climate change / UGS changes

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

grid2 <- expand.grid(c("SSP2", "SSP5"), c(2020, 2030, 2040, 2050), c(" baseline", "5%"))

#

joined_projs_2 <- joined_projs %>% filter(variable==2020)
joined_projs_2 <- merge(outer, joined_projs_2, by.x=c("Var1", "scenario"), by.y=c("iso3c", "Scenario"))
joined_projs_2$variable <- NULL
joined_projs_2$Var2 <- NULL
joined_projs_2$geometry <- NULL
colnames(joined_projs_2) <- c("iso3c", "scenario", "out_b", "year", "policy", "gdp", "urbanisation", "cdd", "hdd", "IMAGE24")

#

baseline_ssp2 <- as.numeric(predict(model_ely_resid, joined_projs_2 %>% filter(scenario=="SSP2", year==2020, policy==" baseline")))
baseline_ssp5 <- as.numeric(predict(model_ely_resid, joined_projs_2 %>% filter(scenario=="SSP5", year==2020, policy==" baseline")))
outer2 <- mapply(function(X, Y, P){predict(model_ely_resid, joined_projs_2 %>% filter(scenario==X, year==Y, policy==P))}, grid2$Var1, grid2$Var2, grid2$Var3)
colnames(outer2) <- paste0(grid$Var1, "_", grid$Var2, "_", grid2$Var3)
outer2[,grep("SSP2", colnames(outer2))] <- outer2[,grep("SSP2", colnames(outer2))] / baseline_ssp2
outer2[,grep("SSP5", colnames(outer2))] <- outer2[,grep("SSP5", colnames(outer2))] / baseline_ssp5

###

outer2 <- reshape2::melt(outer2)
outer2$scenario <- substr(outer2$Var2, 1, 4)
outer2$year  <- substr(outer2$Var2, 6, 9)
outer2$policy  <- substr(outer2$Var2, 11, 18)
outer2$Var1 <- rep(unique(joined_projs_2$iso3c), nrow(grid2))

# this final change is the *elasticity curve of energy demand* to urban green space

deltas <- as.data.frame(outer2)

# show the projected deltas (variation of energy demand with respect to baseline (constant UGS))

ggplot(deltas %>% filter(Var1 %in% countriestoplot), aes(x=year, y=(value - 1)*100, colour=scenario, group=interaction(scenario, policy), linetype=policy))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(Var1), scales="free_y", nrow=5)+
  ylab("projected % change in residential energy demand due to UGS change")

ggsave("plots/ene_impact_predicted_resid.png", height=7, width=5, scale=1.2, bg="white")

####

grid2 <- expand.grid(c("SSP2", "SSP5"), c(2020, 2030, 2040, 2050), c(" baseline", "5%"))

#

joined_projs_2 <- joined_projs %>% filter(variable==2020)
joined_projs_2 <- merge(outer, joined_projs_2, by.x=c("Var1", "scenario"), by.y=c("iso3c", "Scenario"))
joined_projs_2$variable <- NULL
joined_projs_2$Var2 <- NULL
joined_projs_2$geometry <- NULL
colnames(joined_projs_2) <- c("iso3c", "scenario", "out_b", "year", "policy", "gdp", "urbanisation", "cdd", "hdd", "IMAGE24")

#

baseline_ssp2 <- as.numeric(predict(model_ely_compub, joined_projs_2 %>% filter(scenario=="SSP2", year==2020, policy==" baseline")))
baseline_ssp5 <- as.numeric(predict(model_ely_compub, joined_projs_2 %>% filter(scenario=="SSP5", year==2020, policy==" baseline")))
outer2 <- mapply(function(X, Y, P){predict(model_ely_compub, joined_projs_2 %>% filter(scenario==X, year==Y, policy==P))}, grid2$Var1, grid2$Var2, grid2$Var3)
colnames(outer2) <- paste0(grid$Var1, "_", grid$Var2, "_", grid2$Var3)
outer2[,grep("SSP2", colnames(outer2))] <- outer2[,grep("SSP2", colnames(outer2))] / baseline_ssp2
outer2[,grep("SSP5", colnames(outer2))] <- outer2[,grep("SSP5", colnames(outer2))] / baseline_ssp5

###

outer2 <- reshape2::melt(outer2)
outer2$scenario <- substr(outer2$Var2, 1, 4)
outer2$year  <- substr(outer2$Var2, 6, 9)
outer2$policy  <- substr(outer2$Var2, 11, 18)
outer2$Var1 <- rep(unique(joined_projs_2$iso3c), nrow(grid2))

# this final change is the *elasticity curve of energy demand* to urban green space

deltas <- as.data.frame(outer2)

# show the projected deltas (variation of energy demand with respect to baseline (constant UGS))

ggplot(deltas %>% filter(Var1 %in% countriestoplot), aes(x=year, y=(value - 1)*100, colour=scenario, group=interaction(scenario, policy), linetype=policy))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(Var1), scales="free_y", nrow=5)+
  ylab("projected % change in commercial & public energy demand due to UGS change")

ggsave("plots/ene_impact_predicted_compub.png", height=7, width=5, scale=1.2, bg="white")
