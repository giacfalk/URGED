 
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'
res_dir <- paste0(stub, 'results/', sep ='')

###

stub <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub)

###

city_d=tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)
indicator=c("urbclim-cooling-degree-hours", "urbclim-WBGT-hourover25")
reference=c("absolute", "present-day")
spatial="area"
time="annual"
year="2020"
scenario="curpol"

####

# source(paste0(stub, "URGED/0_get_climate_change_provide.R"))

# current climate and climate change

my_cooling_degree_hours_curpol_2020 <- rast("my_cooling-degree-hours_curpol_2020.tif")
my_cooling_degree_hours_curpol_2030 <- rast("my_cooling-degree-hours_curpol_2030.vrt")
my_cooling_degree_hours_curpol_2050 <- rast("my_cooling-degree-hours_curpol_2050.vrt")
my_cooling_degree_hours_curpol_2070 <- rast("my_cooling-degree-hours_curpol_2070.vrt")
my_cooling_degree_hours_curpol_2100 <- rast("my_cooling-degree-hours_curpol_2100.vrt")


my_urbclim_T2M_daily_mean_max_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2020.tif")
my_urbclim_T2M_daily_mean_max_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_max_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-max_curpol_2100.vrt")

my_urbclim_T2M_daily_mean_min_curpol_2020 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2020.tif")
my_urbclim_T2M_daily_mean_min_curpol_2030 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2030.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2050 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2050.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2070 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2070.vrt")
my_urbclim_T2M_daily_mean_min_curpol_2100 <- rast("my_urbclim-T2M-daily-mean-min_curpol_2100.vrt")

load("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/after_points_030624.Rdata")

sf <- read_sf("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
sf_c <- sf %>% group_by(GRGN_L2) %>% slice_max(P15, n = 10)

###

out_ndvi_m$city <- sf_c$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]

out_ndvi_m <- dplyr::select(out_ndvi_m, city, year, out_b, x, y, country)
rm(gvs); gc()

###

city_d_c <- gsub("_", " ", city_d)

out_ndvi_m <- out_ndvi_m[grep(paste(city_d_c, collapse="|"), out_ndvi_m$city, ignore.case=T),]

# out_ndvi_m <- filter(out_ndvi_m, city=="Berlin")

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

out_ndvi_m$t <- exact_extract(my_cooling_degree_hours_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_2030 <- exact_extract(my_cooling_degree_hours_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_2050 <- exact_extract(my_cooling_degree_hours_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_2070 <- exact_extract(my_cooling_degree_hours_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_2100 <- exact_extract(my_cooling_degree_hours_curpol_2100, out_ndvi_m, "mean")

out_ndvi_m$t_max <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_max_2030 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_max_2050 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_max_2070 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_max_2100 <- exact_extract(my_urbclim_T2M_daily_mean_max_curpol_2100, out_ndvi_m, "mean")

out_ndvi_m$t_min <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2020, out_ndvi_m, "mean")
out_ndvi_m$t_min_2030 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_min_2050 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_min_2070 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_min_2100 <- exact_extract(my_urbclim_T2M_daily_mean_min_curpol_2100, out_ndvi_m, "mean")

out_ndvi_m <- filter(out_ndvi_m, !is.na(t))

out_ndvi_m_bk <- out_ndvi_m
out_ndvi_m$geometry <- NULL

out_ndvi_m <- group_by(out_ndvi_m, x, y, city) %>% dplyr::summarise(out_b_mean=mean(out_b, na.rm=T), out_b_max=max(out_b, na.rm=T), out_b_sd=sd(out_b, na.rm=T), t=mean(t, na.rm=T), t_2030=mean(t_2030, na.rm=T), t_2050=mean(t_2050, na.rm=T), t_2100=mean(t_2100, na.rm=T), t_max=mean(t_max, na.rm=T), t_max_2030=mean(t_max_2030, na.rm=T), t_max_2050=mean(t_max_2050, na.rm=T), t_max_2100=mean(t_max_2100, na.rm=T), t_min=mean(t_min, na.rm=T), t_min_2030=mean(t_min_2030, na.rm=T), t_min_2050=mean(t_min_2050, na.rm=T), t_min_2100=mean(t_min_2100, na.rm=T))

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=out_b_mean))+
  scale_colour_distiller(palette = "YlGn", direction = 1)+
  ggtitle("Street green space density in Berlin")

ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=out_b_mean))+
  scale_colour_distiller(palette = "YlGn", direction = 1)+
  ggtitle("Street green space density in Athens")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t_2050-t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin (2050 w.r.t. 2020)")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=t_2100-t))+
  scale_colour_distiller(palette = "YlOrRd", direction = 1)+
  ggtitle("Cooling degree hours in Berlin (2100 w.r.t. 2020)")

###

summary(out_ndvi_m$t)
summary(out_ndvi_m$out_b_mean)

###

write_rds(out_ndvi_m, "data_provide_cdh_gvi_143cities.rds")

out_ndvi_m <- read_rds("data_provide_cdh_gvi_143cities.rds")

out_ndvi_m <- filter(out_ndvi_m, t>0)

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

# out_ndvi_m <- out_ndvi_m %>% filter(city=="Rome" | city=="Milan")

###

library(kgc)

cl <- climatezones

out_ndvi_m$x_s <- RoundCoordinates(out_ndvi_m$x)
out_ndvi_m$y_s <- RoundCoordinates(out_ndvi_m$y)

out_ndvi_m <- merge(out_ndvi_m, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))

#A (tropical), B (arid), C (temperate), D (continental), and E (polar)
out_ndvi_m$Cls <- substr(as.character(out_ndvi_m$Cls), 1, 1)

###
# check with additional covariates

# Local climate zone(s)
# https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml

lcz <- rast("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/lcz/lcz_filter_v3.tif")

out_ndvi_m$lcz <- exact_extract(lcz, out_ndvi_m, "majority")


######

# Building Density/Height/Volume

build_v <- rast("H:/ECIP/Falchetta/era5/daily/new_2016_2022/GHS_BUILT_V_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")
build_h <- rast("H:/ECIP/Falchetta/era5/daily/new_2016_2022/GHS_BUILT_H_AGBH_E2018_GLOBE_R2023A_4326_3ss_V1_0.tif")

out_ndvi_m$build_v <- exact_extract(build_v, out_ndvi_m, "mean")
out_ndvi_m$build_h <- exact_extract(build_h, out_ndvi_m, "mean")

######

# Socio-economic covariate(s)

pop_dens <- rast("H:/ECIP/Falchetta/era5/daily/new_2016_2022/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

out_ndvi_m$pop_dens <- exact_extract(pop_dens, out_ndvi_m, "mean")

out_ndvi_m$pop_dens_d <- cut(out_ndvi_m$pop_dens, quantile(out_ndvi_m$pop_dens, seq(0, 1, 0.2)))

pov_ind <- rast("H:/ECIP/Falchetta/era5/daily/new_2016_2022/povmap-grdi-v1.tif")

out_ndvi_m$pov_ind <- exact_extract(pov_ind, out_ndvi_m, "mean")

############
# Water bodies
# options(timeout = 300)
# download.file("https://globalland.cls.fr/webResources/catalogTree/netcdf/water_bodies/wb_300m_v2_monthly/2023/20230601/c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", mode="wb", destfile = "c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", time=120)

#https://land.copernicus.eu/en/products/water-bodies/water-bodies-global-v2-0-300m#general_info
water <- rast("c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", lyrs="WB")

out_ndvi_m$water <- exact_extract(water, out_ndvi_m, "max")
out_ndvi_m$water  <- ifelse(is.na(out_ndvi_m$water), 0, out_ndvi_m$water)

######

# altitude
library(elevatr)

sf <- out_ndvi_m

out_ndvi_ms <- list()

for(cityy in unique(out_ndvi_m$city)){
  print(cityy)
  out_ndvi_ms[[as.character(cityy)]] <- get_elev_point(sf %>% filter(city==cityy) %>% st_centroid(.), prj = 4326, src = "aws", z = 10, tmp_dir ="H:/ECIP/Falchetta/elevatr")
}

out_ndvi_m <- bind_rows(out_ndvi_ms)

write_rds(out_ndvi_m, "data_provide_cdh_gvi_143cities_withcovariates.rds")

###########

out_ndvi_m <- read_rds("data_provide_cdh_gvi_143cities_withcovariates.rds")
out_ndvi_m <- filter(out_ndvi_m, t>0)

library(fixest)

m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + pov_ind + water + elevation | city, data=out_ndvi_m)
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
  
m1 <- feols(log(t) ~ out_b_mean + out_b_mean^2 + + out_b_mean^3 + build_h + pop_dens + pov_ind + water + elevation, data=out_ndvi_m %>% filter(city==cc))
summary(m1)
(exp(coef(m1)[2])-1)*100

rrange <- seq(5, 30, 5)

dd <- predictions(m1, variables=list(out_b_mean=rrange), transform = exp, vcov = T)

ddd[[cc]] <- dd %>% group_by(out_b_mean) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T), conf.high=mean(conf.high, na.rm=T), conf.low=mean(conf.low, na.rm=T))

}

ddd <- bind_rows(ddd, .id="city")

ggplot(ddd)+
  geom_line(aes(x=out_b_mean, y=estimate))+
  # geom_ribbon(aes(x=out_b_mean, ymin=conf.low, ymax=conf.high), alpha=0.1)+
  scale_colour_brewer(palette = "Greens", name="GVI")+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  facet_wrap(vars(city), scales = "free")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
  )+
  xlab("GVI")+
  ylab("CDH")

###

out_ndvi_m$out_b_mean_d <- cut(out_ndvi_m$out_b_mean, c(-Inf, 5, 10, 15, 20, 25, 30, 35, Inf), labels=c(0, 5, 10, 15, 20, 25, 30, 35))

ddd <- list()

for(cc in unique(out_ndvi_m$city)){
  
  m1 <- feols(log(t) ~ out_b_mean_d + build_h + pop_dens + pov_ind + water + elevation, data=out_ndvi_m %>% filter(city==cc))
  summary(m1)
  
  ddd[[cc]] <- data.frame(name=names(coef(m1)), coef=coef(m1))
  
}

ddd <- bind_rows(ddd, .id="city")
ddd <- filter(ddd, grepl("out_b_mean_d", name))
ddd$name <- gsub("out_b_mean_d", "", ddd$name)

ggplot(ddd)+
  geom_line(aes(x=as.numeric(as.character(name)), y=coef, group=1))+
  # geom_ribbon(aes(x=out_b_mean, ymin=conf.low, ymax=conf.high), alpha=0.1)+
  facet_wrap(vars(city), scales = "free")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
  )+
  xlab("GVI")+
  ylab("CDH")


###
###
# check with original GVI data to see there is no model bias w/temperature

sf <- read_sf("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/all_cities_green_index.shp")

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
