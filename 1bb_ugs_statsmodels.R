# Fixed effects models to explore the relationship between GVI and climate vars (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
# - City-Level Analysis
# - Visualize the results
# - map plots
# Libraries etc ----------------------------
rm(list=ls(all=TRUE)) # Removes all previously created variables 
gc()
library(tidyverse)
library(fixest)
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory

# Directories and settings ----------------------------
path_data <- "data_provide_cdh_gvi_143cities_withcovariates.rds"

# Data Preparation ----------------------------
df <- read_rds(path_data) # This will be an 
# df <- filter(df, t>0) # Remove missing values; Somehow doesnt work (! `x` must be a vector, not a <sfc_POINT/sfc> object.
)


# Analysis: First ----------------------------
m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data=df)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100

m2 <- feols(log(t) ~ out_b_max + build_h + pop_dens + pov_ind + water + elevation  | city, data=out_ndvi_m)
summary(m2, "cluster")
(exp(coef(m2)[1])-1)*100

m3 <- feols(log(t) ~ out_b_sd + build_h + pop_dens + pov_ind + water +elevation | city, data=out_ndvi_m)
summary(m3, "cluster")
(exp(coef(m3)[1])-1)*100

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab1.png"))


m1 <- feols(log(t_max) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data=out_ndvi_m)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100

m2 <- feols(log(t_max) ~ out_b_max + build_h + pop_dens + pov_ind + water + elevation  | city, data=out_ndvi_m)
summary(m2, "cluster")
(exp(coef(m2)[1])-1)*100

m3 <- feols(log(t_max) ~ out_b_sd + build_h + pop_dens + pov_ind + water +elevation | city, data=out_ndvi_m)
summary(m3, "cluster")
(exp(coef(m3)[1])-1)*100

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab2.png"))

m1 <- feols(log(t_min) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data=out_ndvi_m)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100

m2 <- feols(log(t_min) ~ out_b_max + build_h + pop_dens + pov_ind + water + elevation  | city, data=out_ndvi_m)
summary(m2, "cluster")
(exp(coef(m2)[1])-1)*100

m3 <- feols(log(t_min) ~ out_b_sd + build_h + pop_dens + pov_ind + water +elevation | city, data=out_ndvi_m)
summary(m3, "cluster")
(exp(coef(m3)[1])-1)*100

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab3.png"))


####

#city-level cooling capacity

ddd <- list()

for(cc in unique(out_ndvi_m$city)){
  
  m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + water + elevation, data=out_ndvi_m %>% filter(city==cc))
  summary(m1)
  (exp(coef(m1)[2])-1)*100
  
  ddd[[cc]] <- data.frame(cc=(exp(coef(m1)[2])-1)*100, c.l=(exp(coef(m1)[2] - 1.96 * m1$coeftable$`Std. Error`[2])-1)*100, c.u=(exp(coef(m1)[2] + 1.96 * m1$coeftable$`Std. Error`[2])-1)*100)
  
}

ddd <- bind_rows(ddd, .id="city")

ddd$sig = ifelse(sign(ddd$c.l)==sign(ddd$c.u), 1, 0)

ggplot(ddd)+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_errorbar(aes(x=match(city, ddd$city), ymin=c.l, ymax=c.u, colour=as.factor(sig)))+
  theme_classic()+
  theme(
    strip.background = element_blank(),
  )+
  xlab("GVI")+
  ylab("CDH")+
  scale_x_continuous(breaks = c(1:60))

###

sf <- read_sf(paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")) # Cities database

ddd = merge(ddd, sf, by.x="city", by.y="UC_NM_MN")

ddd <- st_as_sf(ddd)
ddd <- st_transform(ddd, "ESRI:54009")

####

require(maptools)

data(wrld_simpl)
wrld_simpl_sf <- st_as_sf(wrld_simpl)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)

ddd$cc_d = cut(ddd$cc, c(-3, -2, -1, -0.5, -0.25, 0.25, 1))

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="black", lwd=0.25)+
  stat_sf_coordinates(data=st_centroid(ddd), colour="black", size=3.5)+
  stat_sf_coordinates(data=st_centroid(ddd), aes(colour=cc_d), size=2.8)+
  scale_color_manual(values=c("darkblue", "blue", "skyblue", "lightblue", "white", "red"), name="")+
  theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1.25, "cm"))+
  xlab("")+
  ylab("")+
  ggtitle("Estimated city-level cooling capacity of GVI")+
  theme(plot.title = element_text(hjust = 0.5))

###
### Robustness checks and sensitivities
###
# check with original GVI data to see there is no model bias w/temperature

sf <- read_sf("ugs/all_cities_green_index.shp")

sf <- sf %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

sf$t <- exact_extract(r, sf, "mean")

sf <- filter(sf, t>0)

library(fixest)

m1_sf <- feols(log(t) ~ GreenView | city, data=sf)
summary(m1_sf, "cluster")
(exp(coef(m1_sf)[1])-1)*100

###############

out_ndvi_m_t <- filter(out_ndvi_m, city %in% c("London", "Los Angeles", "Vancouver", "São Paulo", "Toronto", "Miami", "Tampa", "Singapore", "Sydney", "Johannesburg", "Tel Aviv", "Amsterdam", "Turin"))

m2_sf <- feols(log(t) ~ out_b_mean | city, data=out_ndvi_m_t)
summary(m2_sf, "cluster")
(exp(coef(m2_sf)[1])-1)*100

etable(m1_sf, m2_sf, vcov="cluster")

### 

# random effects models for robustness

library(plm)
random <- plm(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation, data=out_ndvi_m, index=c("city"), model="random")  #random model
summary(random)

library(lme4)
library(sjPlot)
model_rand_int <- lmer(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation + (1 | city), data = out_ndvi_m)
summary(model_rand_int)

model_rand_int <- lmer(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation + (out_b_mean | city), data = out_ndvi_m)
summary(model_rand_int)

write_rds(m1, paste0(res_dir, "ugs_cdh_general_model.rds"))

###

library(broom)

fitted_models <- out_ndvi_m %>% nest(data = -city) %>% mutate(model = map(data, ~feols(log(t)~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation, data = .)), tidied = map(model, tidy)) %>% unnest(tidied)
fitted_models <- filter(fitted_models, term=="out_b_mean") %>% dplyr::select(city, estimate, std.error, p.value)
fitted_models <- arrange(fitted_models, (estimate))

fitted_models

write_rds(fitted_models, paste0(res_dir, "ugs_cdh_cityspecific_model.rds"))