
##############################################################################

# This Rscript: 

#   1) project future mortality rates

##############################################################################

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
require(mort.table)
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
clmt_dir <- paste0(stub, 'climate/', sep ='')
popl_dir <- paste0(stub, 'population_data/', sep ='')
resl_dir <- paste0(stub, 'results/', sep ='')
mort_dir <- paste0(stub, 'mortality_data/', sep ='')

# extract regression coefficients to calculate impacts
mort <- read_rds(paste0(resl_dir, "ugs_mortality_data.rds"))
reg <- read_rds(paste0(resl_dir, "ugs_mortality_model.rds"))
coefs_reg <- coef(reg)

# produce impacts at the grid-cell level

cmip6_data_mortality <- read_rds(paste0(clmt_dir, "cmip6_data_mortality.rds"))
cmip6_data_mortality = cmip6_data_mortality %>% group_by(NUTS_ID, week) %>% dplyr::summarise_if(is.numeric, mean, na.rm=T)

data_m = merge(mort, cmip6_data_mortality, by=c("NUTS_ID", "week"))

##################
#################

output <- list()

# loop for all ssps and time-steps

for (ssp in c("hist", "245", "370", "585")){
  
    orig_data <- data_m
    
    orig_data$CDD = orig_data[,paste0("CDD_", ssp)]
    orig_data$HDD = orig_data[,paste0("HDD_", ssp)]
    
    projected <- predict(reg, orig_data)
    
    output[[as.character(ssp)]] <- projected
    
  }
  

###

# bind results together
output <- as.data.frame(do.call("cbind", output))
colnames(output) = paste0("mort_", colnames(output))
output = bind_cols(data_m, output)

##########

nuts = read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson")

output_s = dplyr::group_by(output, NUTS_ID)  %>% dplyr::summarise_if(is.numeric, sum, na.rm=T)

output_s = merge(output_s, nuts, "NUTS_ID")

output_s = st_as_sf(output_s)

ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
    geom_sf(data=output_s, aes(fill=((mort_585/mort_hist)-1)*100))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen")+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))

##################
#################

output <- list()

# loop for all ssps and time-steps

for (ssp in c("hist", "245", "370", "585")){
  for (out_b_policy in c(F, 20, 25, 30)){
    
    orig_data <- data_m
    
    orig_data$CDD = orig_data[,paste0("CDD_", ssp)]
    orig_data$HDD = orig_data[,paste0("HDD_", ssp)]
    orig_data$out_b = ifelse(out_b_policy!=F & orig_data$out_b<out_b_policy, out_b_policy, orig_data$out_b)
    
    projected <- predict(reg, orig_data)
    
    output[[paste0(as.character(ssp), "_", as.character(out_b_policy))]] <- projected
    
  }}
  

###

# bind results together
output <- as.data.frame(do.call("cbind", output))
colnames(output) = paste0("mort_", colnames(output))
output = bind_cols(data_m, output)

##########

nuts = read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson")

output_s = dplyr::group_by(output, NUTS_ID)  %>% dplyr::summarise_if(is.numeric, sum, na.rm=T)

output_s = merge(output_s, nuts, "NUTS_ID")

output_s = st_as_sf(output_s)

output_s$heh = ((output_s$mort_585_0/output_s$mort_hist_0)-1)*100

plot_a = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
    geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 5))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with current GVI")


output_s$heh = ((output_s$mort_585_20/output_s$mort_hist_20)-1)*100

plot_b = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 5))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>20 policy")

#

output_s$heh = ((output_s$mort_585_25/output_s$mort_hist_25)-1)*100

plot_c = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 5))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>25 policy")


output_s$heh = ((output_s$mort_585_30/output_s$mort_hist_30)-1)*100

plot_d = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 5))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>30 policy")

library(patchwork)

plot_a + plot_b + plot_c + plot_d + plot_layout(guides="collect") + plot_annotation(title="Climate change impact on yearly 65+ mortality rate")

ggsave("results/mortality_project_ugs_policy_climatechange.png", height=8, width=8, scale=1.2, bg="white")

##########

output <- list()

# loop for all ssps and time-steps

for (ssp in c("hist", "245", "370", "585")){
  
  orig_data <- data_m
  
  orig_data$CDD = orig_data[,paste0("CDD_", ssp)]
  orig_data$HDD = orig_data[,paste0("HDD_", ssp)]
  
  projected <- predict(reg, orig_data)
  
  output[[as.character(ssp)]] <- projected
  
}


###

# bind results together
output <- as.data.frame(do.call("cbind", output))
colnames(output) = paste0("mort_", colnames(output))
output = bind_cols(data_m, output)

##########

nuts = read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson")

output_s = dplyr::group_by(output, NUTS_ID)  %>% filter(week==28) %>%  dplyr::summarise_if(is.numeric, mean, na.rm=T)

output_s = merge(output_s, nuts, "NUTS_ID")

output_s = st_as_sf(output_s)

ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=((mort_585/mort_hist)-1)*100))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen")+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))

##################
#################

output <- list()

# loop for all ssps and time-steps

for (ssp in c("hist", "245", "370", "585")){
  for (out_b_policy in c(F, 20, 25, 30)){
    
    orig_data <- data_m
    
    orig_data$CDD = orig_data[,paste0("CDD_", ssp)]
    orig_data$HDD = orig_data[,paste0("HDD_", ssp)]
    orig_data$out_b = ifelse(out_b_policy!=F & orig_data$out_b<out_b_policy, out_b_policy, orig_data$out_b)
    
    projected <- predict(reg, orig_data)
    
    output[[paste0(as.character(ssp), "_", as.character(out_b_policy))]] <- projected
    
  }}


###

# bind results together
output <- as.data.frame(do.call("cbind", output))
colnames(output) = paste0("mort_", colnames(output))
output = bind_cols(data_m, output)

##########

nuts = read_sf("boundaries/NUTS_RG_60M_2021_4326.geojson")

output_s = dplyr::group_by(output, NUTS_ID)  %>% filter(week==28) %>%  dplyr::summarise_if(is.numeric, mean, na.rm=T)

output_s = merge(output_s, nuts, "NUTS_ID")

output_s = st_as_sf(output_s)

output_s$heh = ((output_s$mort_585_0/output_s$mort_hist_0)-1)*100

plot_a = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 25))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with current GVI")


output_s$heh = ((output_s$mort_585_20/output_s$mort_hist_20)-1)*100

plot_b = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 25))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>20 policy")

#

output_s$heh = ((output_s$mort_585_25/output_s$mort_hist_25)-1)*100

plot_c = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 25))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>25 policy")


output_s$heh = ((output_s$mort_585_30/output_s$mort_hist_30)-1)*100

plot_d = ggplot()+
  theme_void()+
  geom_sf(data= nuts,fill="grey50", lwd=0.001)+
  geom_sf(data=output_s, aes(fill=heh))+
  scale_fill_gradient2(midpoint = 0, name="%", na.value = "grey50", high = "red", low="forestgreen", limits=c(-5, 25))+
  coord_sf(xlim=c(-20, 40), ylim=c(30, 80))+
  theme(aspect.ratio = 1, axis.text.x=element_blank(),  axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.text = element_text(size = 15), strip.text = element_text(size = 15) ,legend.title=element_text(size=15))+
  ggtitle("SSP585, around 2050 with GVI>30 policy")

library(patchwork)

plot_a + plot_b + plot_c + plot_d + plot_layout(guides="collect") + plot_annotation(title="Climate change impact on yearly 65+ mortality rate")

ggsave("results/mortality_project_ugs_policy_climatechange_midjuly.png", height=8, width=8, scale=1.2, bg="white")
