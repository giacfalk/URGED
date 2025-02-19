# Fixed effects models to explore the relationship between GVI and climate vars (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
# - City-Level Analysis
# - Visualize the results
# - map plots
rm(list=ls(all=TRUE)) # Removes all previously created variables
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
library(tidyverse)
library(fixest)
library(sf)
library(pdftools) # For printing tables from Latex into pdf. Requires latex installation
library(marginaleffects) # For marginal effects package
library(modelsummary) # For model summary
library(gtsummary)

# Source helper files and functions ---------------------------------------
source("URGED/fcts_labelers_colors.R")
source("URGED/fcts_helpers_debug.R")

# Directories and settings ----------------------------
path_data <- "data_provide_cdh_gvi_143cities_withcovariates.rds"
path_results <- "results/regtab/"
## For additional data
path_subregions <- "wup/country-subregions_adapted.csv" # Path to citylist from WUP2018 F22, adapated to country names in URGED 

# Data Preparation ----------------------------
df <- read_rds(path_data) # The main data frame created in 1b_ugs_heat_metrics.R
df <- dplyr::filter(df, t > 0) # We remove t <= 0 as we use log(t) later on and there are no negative days. It appears though that all t > 0.

# Add some more quantities:
## Add factor labels and levels
df <- fct_factorlabels(df)

# Descriptive Statistics of LCZ classes ----------------------------
descrstats_lcz <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::select(city, x, y, lcz) %>%
  group_by(lcz) %>%
  summarise(count = n()) %>%
  ungroup()
## Make a small plot
ggplot(data = descrstats_lcz, aes(x = lcz, y = count)) + geom_bar(stat = "identity") + theme_minimal(base_size = 9) + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank(), axis.title.x = element_blank()) + labs(y = "Number of points")
outname <- "gis/lcz_counts.png"
ggsave(outname, width = 12, height = 7, units = "cm", dpi = 300, bg = "white")

# Descriptive statistics of Köppen Geiger Climates ----------------------------
descrstats_cls <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::select(city, x, y, Cls) %>%
  group_by(Cls) %>%
  summarise(count = n()) %>%
  mutate(Cls = factor(Cls,
                      levels = c("Af", "Aw", "BSh", "BSk", "BWh", "BWk", "Cfa", "Cfb", "Csa", "Csb", "Cwb", "Dfb"),
                      labels = c("Af (Eq. hum.)", "Aw (Eq. wint.dry)", "BSh (Arid step.hot.arid)", "BSk (Arid step.cold.arid)", "BWh (Arid des.hot.arid)", "BWk (Temp. des.cold.arid)", "Cfa (Temp. hum.hotsum)", "Cfb (Temp. hum.warmsum)", "Csa (Temp. sum.dry.hotsum)", "Csb (Temp. sum.dry.warmsum)", "Cwb (Temp. wint.dry.warmsum)", "Dfb (Snow hum.warmsum)"))) %>%
  ungroup()

# descrstats_cls <- descrstats_cls %>% dplyr::select(Cls) %>% distinct()
## Make a small plot
ggplot(data = descrstats_cls, aes(x = Cls, y = count)) + geom_bar(stat = "identity") + theme_minimal(base_size = 9) + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank(), axis.title.x = element_blank()) + labs(y = "Number of points")
outname <- "gis/cls_counts.png"
ggsave(outname, width = 12, height = 8, units = "cm", dpi = 300, bg = "white")

# Debug: Export cities for investigation of "LCZ == informal settlement" ----------------------------
infset <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(lcz == "Lightweight lowrise")
write_csv(infset, paste0(stub0, "gis/lightweight-lowrise.csv"))

# Debug: Export cities for investigation of "LCZ == informal settlement" ----------------------------
infset <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(lcz == "Compact high-rise")
write_csv(infset, paste0(stub0, "gis/compact-highrise.csv"))

## Debug: Make a list of cities to see how many are in each climate and subregion
fct_count_Cls_SR(df)



# Data cleansing ----------------------------
## Change lcz == "Lightweight lowrise" to "Compact low-rise"
df$lcz <- fct_recode(df$lcz, "Compact low-rise" = "Lightweight lowrise")
## Remove all data points with "natural" lcz from df: Bush, scrub, grassland, bare rock, sand, water
df <- df %>%
  dplyr::filter(!lcz %in% c("Dense trees", "Scattered trees", "Bush, scrub", "Low plants", "Bare rock", "Bare soil", "Water", "NA"))
test <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::select(lcz) %>% distinct()



# Analysis 0: Physical quantities of urban morphology----------------------------
## As fixed effects
m1 <- feols(pop_dens ~ build_h + build_v + lcz | city, data=df)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100
# No fixed effects
m2 <- feols(pop_dens ~ build_h + build_v + lcz, data=df)
summary(m2)
(exp(coef(m1)[1])-1)*100
etable(m1, m2, file = paste0(path_results, "regtab_urbanform.tex"), replace = T) # Export as .tex
etable(m1, m2, export = paste0(path_results, "regtab_urbanform.png"), replace = T) # Export as .tex


# CDH (variable t) and Cls ----------------------------
m1 <- feols(t ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m11 <- feols(t ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city^lcz, data=df, combine.quick = FALSE)
m12 <- feols(t ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation + lcz | city, data=df)

m2 <- feols(t ~ out_b_min:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz, data=df)
m21 <- feols(t ~ out_b_min:Cls + build_h + build_v + pop_dens + water + elevation | city^lcz, data=df, combine.quick = FALSE)
m22 <- feols(t ~ out_b_min:Cls + build_h + build_v + pop_dens + water + elevation + lcz | city, data=df)

m3 <- feols(t ~ out_b_max:Cls + build_h + build_v + pop_dens + water + elevation| city + lcz, data=df)
m31 <- feols(t ~ out_b_max:Cls + build_h + build_v + pop_dens + water + elevation | city^lcz, data=df)
m32 <- feols(t ~ out_b_max:Cls + build_h + build_v + pop_dens + water + elevation + lcz | city, data=df)

etable(m1, m11, m2, m21, m3, m31, vcov="cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH-GVI_FE-city+lcz.png"), replace = T)

etable(m1, vcov="cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH-GVI_FE-city+lcz--lcz.png"), replace = T)

# CDH, Tmax, Tmin and LCZ ----------------------------
m1_t <- feols(t ~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df, combine.quick=FALSE)
m1_tmax <- feols(t_max~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m1_tmin <- feols(t_min ~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)

etable(m1_t, m1_tmax, m1_tmin, vcov="cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH--lcz-GVI_FE-city+lcz--lcz.png"), replace = T)

## Slope
mfx_gvimean <- slopes(m1, variables = "out_b_mean")
summary(mfx_gvimean)
### Plot Predictions ----------------------------
plot_predictions(m1_t, c("out_b_mean", "lcz")) 
  theme_minimal(base_size = 11) +
  geom_line(linewidth = 2.5) +
  labs(
    title = expression("City as FE, GVI"["mean"]),
    x = expression("GVI"["mean"]),
    y = "Cooling Degree Hours (log)",
    color = "Climate Zone", fill = "Climate Zone"
  ) +
  theme(
    text = element_text(size = 11),
    legend.position = "top"
  )#, interval = "confidence", geom = "line", newdata = subset(Cls == "Trop.")) # Plot the marginal effect
outname <- paste0(stub0, "/", path_results, "/slopes/test_slopes.png")
ggsave(outname, width = 14, height = 14, units = "cm", dpi = 300, bg = "white")

# CDH, Tmax, Tmin and Cls ----------------------------
m1_t <- feols(t ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m1_tmax <- feols(t_max ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m1_tmin <- feols(t_min ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)

etable(m1_t, m1_tmax, m1_tmin, vcov="cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH--Cls-GVI_FE-city+lcz--lcz.png"), replace = T)

df_Clsmain <- df %>%
  sf::st_drop_geometry() %>%
  dplyr::select(city, Clsmain) %>% distinct()
write_csv(df_Clsmain, paste0(stub0, path_results, "city_clsmain.csv"))

# CDH, Tmax, Tmin and Clsmain ----------------------------
m1_t <- feols(t ~ out_b_mean:Clsmain + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m1_tmax <- feols(t_max ~ out_b_mean:Clsmain + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
m1_tmin <- feols(t_min ~ out_b_mean:Clsmain + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)

etable(m1_t, m1_tmax, m1_tmin, vcov="cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH--Clsmain-GVI_FE-city+lcz--lcz.png"), replace = T)


# CDH, Tmax, Tmin and LCZ using Cls as fixed ----------------------------
m1_t <- feols(t ~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz + Cls, data=df)
m1_tmax <- feols(t_max~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz+ Cls, data=df)
m1_tmin <- feols(t_min ~ out_b_mean:lcz + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz + Cls, data=df)

etable(m1_t, m1_tmax, m1_tmin, vcov = "cluster",
       dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_CDH--lcz-GVI_FE-Cls-city+lcz--lcz.png"), replace = T)


# # T2max and Cls ----------------------------
# m1 <- feols(t_max ~ out_b_mean:Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
# etable(m1, vcov="cluster",
#        dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_tmax-GVI_FE-city+lcz--lcz.png"), replace = T)
# 
# # T2max and LCZ ----------------------------
# m1 <- feols(t_max ~ out_b_mean:lcz + Cls + build_h + build_v + pop_dens + water + elevation | city + lcz + city^lcz, data=df)
# etable(m1, vcov="cluster",
#        dict=dictstats, export = paste0(stub0, "/", path_results, "/regtab_tmax--lcz-GVI_FE-city+lcz--lcz.png"), replace = T)
# 
# 
# # Analysis 3: T2min and physical quantities ----------------------------
# m1 <- feols(log(t_min) ~ out_b_mean + build_h + pop_dens + water + elevation | city, data=df)
# summary(m1, "cluster")
# (exp(coef(m1)[1])-1)*100
# 
# m2 <- feols(log(t_min) ~ out_b_max + build_h + pop_dens + water + elevation  | city, data=df)
# summary(m2, "cluster")
# (exp(coef(m2)[1])-1)*100
# 
# m3 <- feols(log(t_min) ~ out_b_sd + build_h + pop_dens + water +elevation | city, data=df)
# summary(m3, "cluster")
# (exp(coef(m3)[1])-1)*100
# 
# etable(m1, m2, m3, vcov="cluster")
# etable(m1, m2, m3, vcov="cluster", export = paste0(path_results, "regtab3.png"), replace = T)
# etable(m1, m2, m3, vcov="cluster", file = paste0(path_results, "regtab3.tex"), replace = T) # Export as .tex
# 
# 
# # City-level cooling capacity ----------------------------
# 
# ddd <- list()
# 
# for(cc in unique(df$city)){
#   
#   m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + water + elevation, data=df %>% filter(city==cc))
#   summary(m1)
#   (exp(coef(m1)[2])-1)*100
#   
#   ddd[[cc]] <- data.frame(cc=(exp(coef(m1)[2])-1)*100, c.l=(exp(coef(m1)[2] - 1.96 * m1$coeftable$`Std. Error`[2])-1)*100, c.u=(exp(coef(m1)[2] + 1.96 * m1$coeftable$`Std. Error`[2])-1)*100)
#   
# }
# 
# ddd <- bind_rows(ddd, .id="city")
# 
# ddd$sig = ifelse(sign(ddd$c.l)==sign(ddd$c.u), 1, 0)
# 
# ggplot(ddd)+
#   geom_hline(yintercept = 0, linetype="dashed")+
#   geom_errorbar(aes(x=match(city, ddd$city), ymin=c.l, ymax=c.u, colour=as.factor(sig)))+
#   theme_classic()+
#   theme(
#     strip.background = element_blank(),
#   )+
#   xlab("GVI")+
#   ylab("CDH")+
#   scale_x_continuous(breaks = c(1:60))
# 
# ###
# 
# sf <- read_sf(paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")) # Cities database
# 
# ddd = merge(ddd, sf, by.x="city", by.y="UC_NM_MN")
# 
# ddd <- st_as_sf(ddd)
# ddd <- st_transform(ddd, "ESRI:54009")
# 
# ####
# 
# require(maptools)
# 
# data(wrld_simpl)
# wrld_simpl_sf <- st_as_sf(wrld_simpl)
# wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
# wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")
# 
# load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# 
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)
# 
# ddd$cc_d = cut(ddd$cc, c(-3, -2, -1, -0.5, -0.25, 0.25, 1))
# 
# ggplot()+
#   theme_void()+
#   geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="black", lwd=0.25)+
#   stat_sf_coordinates(data=st_centroid(ddd), colour="black", size=3.5)+
#   stat_sf_coordinates(data=st_centroid(ddd), aes(colour=cc_d), size=2.8)+
#   scale_color_manual(values=c("darkblue", "blue", "skyblue", "lightblue", "white", "red"), name="")+
#   theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1.25, "cm"))+
#   xlab("")+
#   ylab("")+
#   ggtitle("Estimated city-level cooling capacity of GVI")+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ###
# ### Robustness checks and sensitivities
# ###
# # check with original GVI data to see there is no model bias w/temperature
# 
# sf <- read_sf("ugs/all_cities_green_index.shp")
# 
# sf <- sf %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)
# 
# sf$t <- exact_extract(r, sf, "mean")
# 
# sf <- filter(sf, t>0)
# 
# library(fixest)
# 
# m1_sf <- feols(log(t) ~ GreenView | city, data=sf)
# summary(m1_sf, "cluster")
# (exp(coef(m1_sf)[1])-1)*100
# 
# 
# m1 <- feols(log(t_min) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data=df)
# summary(m1, "cluster")
# (exp(coef(m1)[1])-1)*100
# 
# m2 <- feols(log(t_min) ~ out_b_max + build_h + pop_dens + pov_ind + water + elevation  | city, data=df)
# summary(m2, "cluster")
# (exp(coef(m2)[1])-1)*100
# 
# m3 <- feols(log(t_min) ~ out_b_sd + build_h + pop_dens + pov_ind + water +elevation | city, data=df)
# summary(m3, "cluster")
# (exp(coef(m3)[1])-1)*100
# 
# etable(m1, m2, m3, vcov="cluster")
# etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab3.png"))
# etable(m1, m2, m3, vcov="cluster", export = paste0(res_dir, "regtab3.png"))
# 
# 
# ####
# 
# #city-level cooling capacity
# 
# ddd <- list()
# 
# for(cc in unique(df$city)){
#   
#   m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + water + elevation, data=df %>% filter(city==cc))
#   summary(m1)
#   (exp(coef(m1)[2])-1)*100
#   
#   ddd[[cc]] <- data.frame(cc=(exp(coef(m1)[2])-1)*100, c.l=(exp(coef(m1)[2] - 1.96 * m1$coeftable$`Std. Error`[2])-1)*100, c.u=(exp(coef(m1)[2] + 1.96 * m1$coeftable$`Std. Error`[2])-1)*100)
#   
# }
# 
# ddd <- bind_rows(ddd, .id="city")
# 
# ddd$sig = ifelse(sign(ddd$c.l)==sign(ddd$c.u), 1, 0)
# 
# ggplot(ddd)+
#   geom_hline(yintercept = 0, linetype="dashed")+
#   geom_errorbar(aes(x=match(city, ddd$city), ymin=c.l, ymax=c.u, colour=as.factor(sig)))+
#   theme_classic()+
#   theme(
#     strip.background = element_blank(),
#   )+
#   xlab("GVI")+
#   ylab("CDH")+
#   scale_x_continuous(breaks = c(1:60))
# 
# ###
# 
# sf <- read_sf(paste0(stub0, "boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")) # Cities database
# 
# ddd = merge(ddd, sf, by.x="city", by.y="UC_NM_MN")
# 
# ddd <- st_as_sf(ddd)
# ddd <- st_transform(ddd, "ESRI:54009")
# 
# ####
# 
# require(maptools)
# 
# data(wrld_simpl)
# wrld_simpl_sf <- st_as_sf(wrld_simpl)
# wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
# wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")
# 
# load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# 
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
# NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)
# 
# ddd$cc_d = cut(ddd$cc, c(-3, -2, -1, -0.5, -0.25, 0.25, 1))
# 
# ggplot()+
#   theme_void()+
#   geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="black", lwd=0.25)+
#   stat_sf_coordinates(data=st_centroid(ddd), colour="black", size=3.5)+
#   stat_sf_coordinates(data=st_centroid(ddd), aes(colour=cc_d), size=2.8)+
#   scale_color_manual(values=c("darkblue", "blue", "skyblue", "lightblue", "white", "red"), name="")+
#   theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1.25, "cm"))+
#   xlab("")+
#   ylab("")+
#   ggtitle("Estimated city-level cooling capacity of GVI")+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ###
# ### Robustness checks and sensitivities
# ###
# # check with original GVI data to see there is no model bias w/temperature
# 
# sf <- read_sf("ugs/all_cities_green_index.shp")
# 
# sf <- sf %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)
# 
# sf$t <- exact_extract(r, sf, "mean")
# 
# sf <- filter(sf, t>0)
# 
# library(fixest)
# 
# m1_sf <- feols(log(t) ~ GreenView | city, data=sf)
# summary(m1_sf, "cluster")
# (exp(coef(m1_sf)[1])-1)*100
# 
# df_t <- filter(df, city %in% c("London", "Los Angeles", "Vancouver", "São Paulo", "Toronto", "Miami", "Tampa", "Singapore", "Sydney", "Johannesburg", "Tel Aviv", "Amsterdam", "Turin"))
# 
# m2_sf <- feols(log(t) ~ out_b_mean | city, data=df_t)
# summary(m2_sf, "cluster")
# (exp(coef(m2_sf)[1])-1)*100
# 
# etable(m1_sf, m2_sf, vcov="cluster")
# 
# ### 
# 
# # random effects models for robustness
# 
# library(plm)
# random <- plm(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation, data=df, index=c("city"), model="random")  #random model
# summary(random)
# 
# library(lme4)
# library(sjPlot)
# model_rand_int <- lmer(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation + (1 | city), data = df)
# summary(model_rand_int)
# 
# model_rand_int <- lmer(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation + (out_b_mean | city), data = df)
# summary(model_rand_int)
# 
# write_rds(m1, paste0(res_dir, "ugs_cdh_general_model.rds"))
# 
# ###
# 
# # city-specific models
# 
# library(broom)
# 
# fitted_models <- df %>% nest(data = -city) %>% mutate(model = map(data, ~feols(log(t)~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation, data = .)), tidied = map(model, tidy)) %>% unnest(tidied)
# fitted_models <- filter(fitted_models, term=="out_b_mean") %>% dplyr::select(city, estimate, std.error, p.value)
# fitted_models <- arrange(fitted_models, (estimate))
# 
# fitted_models
# 
# write_rds(fitted_models, paste0(res_dir, "ugs_cdh_cityspecific_model.rds"))
# 
# ###
# 
# # climate zone-specific models
# 
# library(broom)
# 
# fitted_models <- df %>% nest(data = -Cls) %>% mutate(model = map(data, ~feols(log(t)~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data = .)), tidied = map(model, tidy)) %>% unnest(tidied)
# fitted_models <- filter(fitted_models, term=="out_b_mean") %>% dplyr::select(Cls, estimate, std.error, p.value)
# fitted_models <- arrange(fitted_models, (estimate))
# 
# fitted_models
# 
# write_rds(fitted_models, paste0(res_dir, "ugs_cdh_clsspecific_model.rds"))
# 
# 
# # local climate zone-specific models
# 
# library(broom)
# 
# fitted_models <- df %>% nest(data = -lcz) %>% mutate(model = map(data, ~feols(log(t)~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data = .)), tidied = map(model, tidy)) %>% unnest(tidied)
# fitted_models <- filter(fitted_models, term=="out_b_mean") %>% dplyr::select(lcz, estimate, std.error, p.value)
# fitted_models <- arrange(fitted_models, (estimate))
# 
# fitted_models
# 
# write_rds(fitted_models, paste0(res_dir, "ugs_cdh_lczspecific_model.rds"))