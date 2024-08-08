
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

plot_cities <- c("Rome", "Madrid", "Liverpool", "Cairo", "Beijing")

################

proj <- readRDS(paste0(res_dir, "projections_gvi_citylevel.rds"))
ely_model <- readRDS(paste0(res_dir, "ugs_ely_elasticity_model.rds"))

proj <- filter(proj, Var1 %in% plot_cities)

################

proj <- proj %>% group_by(Var1) %>% mutate(impact=coef(ely_model)[4] * ((value/mean(value[Var2=="SSP2_2020"]))-1))

# show the projected deltas (variation of energy demand with respect to baseline (constant UGS))

ggplot(proj)+
  geom_line(aes(x=year, y=impact*100, colour=interaction(scenario, policy), group=interaction(scenario, policy)))+
  facet_wrap(vars(Var1), nrow = 1)+
  ylab("projected % change in residential energy demand due to UGS change")+
  geom_hline(yintercept = 0)

ggsave("plots/ene_impact_predicted_resid.png", height=7, width=5, scale=1.2, bg="white")
