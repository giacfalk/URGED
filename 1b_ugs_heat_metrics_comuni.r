
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
res_dir <- paste0(stub, '/results/', sep ='')

###

stub2 <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub2)

###

city_d=tolower(read.csv("cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)

city_d <- city_d[grep(paste0(c("Bari", "Bologna", "Genoa", "Milan", "Naples", "Padua", "Palermo", "Rome", "Trieste", "Turin"), collapse="|"), city_d, ignore.case = T)]

indicator=c("urbclim-cooling-degree-hours", "urbclim-T2M-daily-mean-max")
reference=c("absolute", "present-day")
spatial="area"
time="annual"
year="2020"
scenario="curpol"

gg <- expand.grid(city_d, indicator, reference, spatial, time, year, scenario)
colnames(gg) <- c("city_d", "indicator", "reference", "spatial", "time", "year", "scenario")

#

v <- list.files(pattern = "absolute")
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("2020", v)]
v <- v[grepl("curpol", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- vrt(v, "my_10comuni.vrt", overwrite=T)
r <- rast("my_10comuni.vrt")

v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2030_10comuni.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2050_10comuni.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2070_10comuni.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("cooling-degree-hours", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2100_10comuni.vrt", overwrite=T)

###

my_curpol_2030 <- rast("my_curpol_2030_10comuni.vrt")
my_curpol_2050 <- rast("my_curpol_2050_10comuni.vrt")
my_curpol_2070 <- rast("my_curpol_2070_10comuni.vrt")
my_curpol_2100 <- rast("my_curpol_2100_10comuni.vrt")

###


v <- list.files(pattern = "absolute")
v <- v[grepl("2030", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2030_10comuni_tmax.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2050", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2050_10comuni_tmax.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2070", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2070_10comuni_tmax.vrt", overwrite=T)

v <- list.files(pattern = "absolute")
v <- v[grepl("2100", v)]
v <- v[grepl(paste0(city_d, collapse = "|"), v)]
v <- v[grepl("T2M-daily-mean-max", v)]
v <- v[grepl("curpol", v)]
v <- vrt(v, "my_curpol_2100_10comuni_tmax.vrt", overwrite=T)

###

my_curpol_2030_tmax <- rast("my_curpol_2030_10comuni_tmax.vrt")
my_curpol_2050_tmax <- rast("my_curpol_2050_10comuni_tmax.vrt")
my_curpol_2070_tmax <- rast("my_curpol_2070_10comuni_tmax.vrt")
my_curpol_2100_tmax <- rast("my_curpol_2100_10comuni_tmax.vrt")

###

setwd(stub)
out_ndvi_m<-readRDS("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/gvi_extract_10cities/GVI_comuni_10cities_2016_2023.rds")

###

comuni = read_sf("boundaries/Com01012023_g_WGS84.shp")
comuni$geometry <- NULL
comuni <- dplyr::select(comuni, PRO_COM, COMUNE)
merger <- merge(out_ndvi_m, comuni, "PRO_COM")


###

out_ndvi_m <- merger[merger$COMUNE %in% c("Bari", "Bologna", "Genova", "Milano", "Napoli", "Padova", "Palermo", "Roma", "Trieste", "Torino"),]

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

out_ndvi_m$t <- exact_extract(r, out_ndvi_m, "mean")

out_ndvi_m$t_2030 <- exact_extract(my_curpol_2030, out_ndvi_m, "mean")
out_ndvi_m$t_2050 <- exact_extract(my_curpol_2050, out_ndvi_m, "mean")
out_ndvi_m$t_2070 <- exact_extract(my_curpol_2070, out_ndvi_m, "mean")
out_ndvi_m$t_2100 <- exact_extract(my_curpol_2100, out_ndvi_m, "mean")

###

out_ndvi_m$tmax_2030 <- exact_extract(my_curpol_2030_tmax, out_ndvi_m, "mean")
out_ndvi_m$tmax_2050 <- exact_extract(my_curpol_2050_tmax, out_ndvi_m, "mean")
out_ndvi_m$tmax_2070 <- exact_extract(my_curpol_2070_tmax, out_ndvi_m, "mean")
out_ndvi_m$tmax_2100 <- exact_extract(my_curpol_2100_tmax, out_ndvi_m, "mean")

###

out_ndvi_m <- filter(out_ndvi_m, !is.na(t))

out_ndvi_m_bk <- out_ndvi_m
out_ndvi_m$geometry <- NULL

out_ndvi_m <- group_by(out_ndvi_m, x, y, COMUNE) %>% dplyr::summarise(out_b_mean=mean(GVI, na.rm=T), out_b_max=max(GVI, na.rm=T), out_b_sd=sd(GVI, na.rm=T), t=mean(t, na.rm=T), tmax=mean(tmax_2030, na.rm=T))

###

write_rds(out_ndvi_m, "data_provide_cdh_gvi_ITA_cities.rds")

out_ndvi_m <- read_rds("data_provide_cdh_gvi_ITA_cities.rds")

###

# out_ndvi_m_plot = out_ndvi_m
# out_ndvi_m_plot = out_ndvi_m_plot[-which(out_ndvi_m_plot$COMUNE=="Genova" & out_ndvi_m_plot$y<44.44),]

b <- pblapply(unique(out_ndvi_m_plot$COMUNE), function(ctry){ ggplot(out_ndvi_m_plot %>% filter(COMUNE==ctry))+
    theme_classic()+
    geom_point(aes(x=x, y=y, colour=out_b_mean))+
    scale_colour_distiller(palette = "YlGn", direction = 1, limits=c(5, 40))+
    ggtitle(ctry)})

library(patchwork)

plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}


b <- plot_a_list(b, 2, 5) + plot_layout(guides = "collect") & theme(legend.direction = "horizontal", legend.position = "bottom")

ggsave("results/gvi_ita_comuni.png", b, height = 2, width = 3.5, scale=3, bg="white")

###

c <- pblapply(unique(out_ndvi_m$COMUNE), function(ctry){ ggplot(out_ndvi_m %>% filter(COMUNE==ctry))+
    theme_classic()+
    geom_point(aes(x=x, y=y, colour=t))+
    scale_colour_distiller(palette = "YlOrRd", direction = 1, limits=c(0, 10000))+
    ggtitle(ctry)})


library(patchwork)

c <- plot_a_list(c, 2, 5) + plot_layout(guides = "collect") & theme(legend.direction = "horizontal", legend.position = "bottom")

setwd(stub)

ggsave("results/cdh_ita_comuni.png", c, height = 2, width = 3.5, scale=3, bg="white")

###

out_ndvi_m <- filter(out_ndvi_m, t>0)


library(kgc)

cl <- climatezones

out_ndvi_m$x_s <- RoundCoordinates(out_ndvi_m$x)
out_ndvi_m$y_s <- RoundCoordinates(out_ndvi_m$y)

out_ndvi_m <- merge(out_ndvi_m, cl, by.x=c("x_s", "y_s"), by.y=c("Lon", "Lat"))

#A (tropical), B (arid), C (temperate), D (continental), and E (polar)
out_ndvi_m$Cls <- substr(as.character(out_ndvi_m$Cls), 1, 1)

lcz <- rast("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/lcz/lcz_filter_v3.tif")

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

out_ndvi_m$lcz <- exact_extract(lcz, out_ndvi_m, "majority")

build_v <- rast("socioecon/GHS_BUILT_V_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")
build_h <- rast("socioecon/GHS_BUILT_H_AGBH_E2018_GLOBE_R2023A_4326_3ss_V1_0.tif")

out_ndvi_m$build_v <- exact_extract(build_v, out_ndvi_m, "mean")
out_ndvi_m$build_h <- exact_extract(build_h, out_ndvi_m, "mean")

pop_dens <- rast("socioecon/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

out_ndvi_m$pop_dens <- exact_extract(pop_dens, out_ndvi_m, "mean")

out_ndvi_m$pop_dens_d <- cut(out_ndvi_m$pop_dens, quantile(out_ndvi_m$pop_dens, seq(0, 1, 0.2)))

pov_ind <- rast("socioecon/povmap-grdi-v1.tif")

out_ndvi_m$pov_ind <- exact_extract(pov_ind, out_ndvi_m, "mean")

# options(timeout = 300)
# download.file("https://globalland.cls.fr/webResources/catalogTree/netcdf/water_bodies/wb_300m_v2_monthly/2023/20230601/c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", mode="wb", destfile = "c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", time=120)

water <- rast("c_gls_WB300_202306010000_GLOBE_S2_V2.0.1.nc", lyrs="WB")

out_ndvi_m$water <- exact_extract(water, out_ndvi_m, "max")
out_ndvi_m$water  <- ifelse(is.na(out_ndvi_m$water), 0, out_ndvi_m$water)

library(elevatr)

out_ndvi_ms <- list()

for(cityy in unique(out_ndvi_m$COMUNE)){
  print(cityy)
  out_ndvi_ms[[as.character(cityy)]] <- get_elev_point(out_ndvi_m %>% filter(COMUNE==cityy) %>% st_centroid(.), prj = 4326, src = "aws", z = 10, tmp_dir ="H:/ECIP/Falchetta/elevatr")
}

out_ndvi_m <- bind_rows(out_ndvi_ms)

write_rds(out_ndvi_m, "data_provide_cdh_gvi_ITA_cities_withcovariates.rds")

out_ndvi_m <- read_rds("data_provide_cdh_gvi_ITA_cities_withcovariates.rds")

###

library(stargazer)

ms <- as.data.frame(out_ndvi_m %>% dplyr::select(t, out_b_mean, out_b_max, out_b_sd, build_h, pop_dens, water, elevation))

stargazer(ms, summary = T, out="results/sum_tab_points.tex")

###

out_ndvi_m$out_b_mean_bins <- cut(out_ndvi_m$out_b_mean, quantile(out_ndvi_m$out_b_mean, seq(0, 1, 0.1)))
out_ndvi_m <- within(out_ndvi_m, out_b_mean_bins <- relevel(out_b_mean_bins, ref = 3))

library(fixest)

m1 <- feols(log(t) ~ out_b_mean + build_h + pop_dens + water + elevation | COMUNE, data=out_ndvi_m)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100

m1_q <- feols(log(t) ~ out_b_mean + out_b_mean^2 + build_h + pop_dens + water + elevation | COMUNE, data=out_ndvi_m)
summary(m1_q, "cluster")
# (exp(coef(m1_q)[1])-1)*100

m1_b <- feols(log(t) ~ out_b_mean_bins + build_h + pop_dens + water + elevation | COMUNE, data=out_ndvi_m)
summary(m1_b, "cluster")
# coefplot(m1_b, 1:2)

m2 <- feols(log(t) ~ out_b_max + build_h + pop_dens + water + elevation  | COMUNE, data=out_ndvi_m)
summary(m2, "cluster")
(exp(coef(m2)[1])-1)*100

m3 <- feols(log(t) ~ out_b_sd + build_h + pop_dens + water +elevation | COMUNE, data=out_ndvi_m)
summary(m3, "cluster")
(exp(coef(m3)[1])-1)*100

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", file = paste0(res_dir, "regtab1_ITA.tex"), tex = T, replace = T)
etable(m1, m1_q, m1_b, vcov="cluster", file = paste0(res_dir, "regtab1_ITA_extra.tex"), tex = T, replace = T)

write_rds(m1, paste0(res_dir, "ugs_cdh_general_model_ITA.rds"))

###

m1 <- feols(log(tmax) ~ out_b_mean + build_h + pop_dens + water + elevation | COMUNE, data=out_ndvi_m)
summary(m1, "cluster")
(exp(coef(m1)[1])-1)*100

m2 <- feols(log(tmax) ~ out_b_max + build_h + pop_dens + water + elevation  | COMUNE, data=out_ndvi_m)
summary(m2, "cluster")
(exp(coef(m2)[1])-1)*100

m3 <- feols(log(tmax) ~ out_b_sd + build_h + pop_dens + water +elevation | COMUNE, data=out_ndvi_m)
summary(m3, "cluster")
(exp(coef(m3)[1])-1)*100

etable(m1, m2, m3, vcov="cluster")
etable(m1, m2, m3, vcov="cluster", file = paste0(res_dir, "regtab1_ITA_tmax.tex"), tex = T, replace = T, label = "tab:tmax_10cities_si")

###

library(broom)

fitted_models <- out_ndvi_m %>% filter(t>0) %>%  nest(data = -COMUNE) %>% mutate(model = map(data, ~feols(log(t)~out_b_mean + build_h + pop_dens + pov_ind + elevation, data = .)), tidied = map(model, tidy)) %>% unnest(tidied)
fitted_models <- filter(fitted_models, term=="out_b_mean") %>% dplyr::select(COMUNE, estimate, std.error, p.value)
fitted_models <- arrange(fitted_models, (estimate))

fitted_models

write_rds(fitted_models, paste0(res_dir, "ugs_cdh_cityspecific_model_ITA.rds"))


###############
###############

# climate change

setwd(stub2)

r <- rast("my_10comuni.vrt")

my_curpol_2030 <- rast("my_curpol_2030_10comuni.vrt")
my_curpol_2050 <- rast("my_curpol_2050_10comuni.vrt")
my_curpol_2070 <- rast("my_curpol_2070_10comuni.vrt")
my_curpol_2100 <- rast("my_curpol_2100_10comuni.vrt")

