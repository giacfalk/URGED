
##############################################################################

# This Rscript: 

#   1)  project future evolution of UGS with changes in drivers

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
library(rpart)
library(caret)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

plot_cities <- c("Rome", "Madrid", "Liverpool", "Cairo", "Beijing")

################

load(paste0(res_dir, "/ugs_drivers_model.rds"))

sf <- read_sf("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") %>% group_by(GRGN_L2) %>% slice_max(P15, n = 10) %>% ungroup()


# update to future cdd / hdd

cdd_245 <- stack("climate/cdd18_global_mid_245_model_ensemble_median_yearly.nc")

hdd_245 <- stack("climate/hdd18_global_mid_245_model_ensemble_median_yearly.nc")

cdd_585 <- stack("climate/cdd18_global_mid_585_model_ensemble_median_yearly.nc")

hdd_585 <- stack("climate/hdd18_global_mid_585_model_ensemble_median_yearly.nc")

cdd_fut_245 <- exact_extract(cdd_245, sf, "mean")
hdd_fut_245 <- exact_extract(hdd_245, sf, "mean")

cdd_fut_245$iso3c <- sf$UC_NM_MN
hdd_fut_245$iso3c <- sf$UC_NM_MN

cdd_fut_585 <- exact_extract(cdd_585, sf, "mean")
hdd_fut_585 <- exact_extract(hdd_585, sf, "mean")

cdd_fut_585$iso3c <- sf$UC_NM_MN
hdd_fut_585$iso3c <- sf$UC_NM_MN

#####

cdd_fut_585$cdd_2020_ssp585 <- rowMeans(cdd_fut_585[,c(4:8)], na.rm=T)
cdd_fut_585$cdd_2030_ssp585 <- rowMeans(cdd_fut_585[,c(14:18)], na.rm=T)
cdd_fut_585$cdd_2040_ssp585 <- rowMeans(cdd_fut_585[,c(24:28)], na.rm=T)
cdd_fut_585$cdd_2050_ssp585 <- rowMeans(cdd_fut_585[,c(34:36)], na.rm=T)

cdd_fut_245$cdd_2020_ssp245 <- rowMeans(cdd_fut_245[,c(4:8)], na.rm=T)
cdd_fut_245$cdd_2030_ssp245 <- rowMeans(cdd_fut_245[,c(14:18)], na.rm=T)
cdd_fut_245$cdd_2040_ssp245 <- rowMeans(cdd_fut_245[,c(24:28)], na.rm=T)
cdd_fut_245$cdd_2050_ssp245 <- rowMeans(cdd_fut_245[,c(34:36)], na.rm=T)

hdd_fut_585$hdd_2020_ssp585 <- rowMeans(hdd_fut_585[,c(4:8)], na.rm=T)
hdd_fut_585$hdd_2030_ssp585 <- rowMeans(hdd_fut_585[,c(14:18)], na.rm=T)
hdd_fut_585$hdd_2040_ssp585 <- rowMeans(hdd_fut_585[,c(24:28)], na.rm=T)
hdd_fut_585$hdd_2050_ssp585 <- rowMeans(hdd_fut_585[,c(34:36)], na.rm=T)

hdd_fut_245$hdd_2020_ssp245 <- rowMeans(hdd_fut_245[,c(4:8)], na.rm=T)
hdd_fut_245$hdd_2030_ssp245 <- rowMeans(hdd_fut_245[,c(14:18)], na.rm=T)
hdd_fut_245$hdd_2040_ssp245 <- rowMeans(hdd_fut_245[,c(24:28)], na.rm=T)
hdd_fut_245$hdd_2050_ssp245 <- rowMeans(hdd_fut_245[,c(34:36)], na.rm=T)

cdd_fut_585 <- dplyr::select(cdd_fut_585, starts_with("cdd"))
cdd_fut_245 <- dplyr::select(cdd_fut_245, iso3c, starts_with("cdd"))

hdd_fut_585 <- dplyr::select(hdd_fut_585, starts_with("hdd"))
hdd_fut_245 <- dplyr::select(hdd_fut_245, iso3c, starts_with("hdd"))

cdd <- bind_cols(cdd_fut_245, cdd_fut_585)

cdd <- reshape2::melt(cdd, 1)
cdd$Scenario <- toupper(substr(cdd$variable, 10, 13))
cdd$year <-  substr(cdd$variable, 5, 8)
cdd$variable <- NULL
colnames(cdd)[2] <- "cdd"

hdd <- bind_cols(hdd_fut_245, hdd_fut_585)

hdd <- reshape2::melt(hdd, 1)
hdd$Scenario <-  toupper(substr(hdd$variable, 10, 13))
hdd$year <- substr(hdd$variable, 5, 8)
hdd$variable <- NULL
colnames(hdd)[2] <- "hdd"

#####

# gdp downscaled (SSPS)
gdp_ssps <- list.files(path="F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/supporting_data/gdp_downscaled_ssps", recursive = T, pattern="tif", full.names = T)

gdp_ssps <- gdp_ssps[grepl("2010|2020|2030|2040|2050", gdp_ssps)]

gdp_ssps_data <- lapply(gdp_ssps, raster)
gdp_ssps_data <- split(gdp_ssps_data, rep(1:5, each=5))
gdp_ssps_data <- lapply(gdp_ssps_data, stack)
names(gdp_ssps_data[[1]]) <- paste0("GDP_SSP1_", seq(2010, 2050, by=10))
names(gdp_ssps_data[[2]]) <- paste0("GDP_SSP2_", seq(2010, 2050, by=10))
names(gdp_ssps_data[[3]]) <- paste0("GDP_SSP3_", seq(2010, 2050, by=10))
names(gdp_ssps_data[[4]]) <- paste0("GDP_SSP4_", seq(2010, 2050, by=10))
names(gdp_ssps_data[[5]]) <- paste0("GDP_SSP5_", seq(2010, 2050, by=10))

# pop downscaled (SSPS)
pop_ssps <- list.files(path="F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/supporting_data/pop_downscaled_spps", recursive = T, pattern="nc", full.names = T)
pop_ssps_data <- lapply(pop_ssps, stack)

for (i in 1:5){
  pop_ssps_data[[i]] <- raster::subset(pop_ssps_data[[i]], c(5+c(10*c(0:4))))
  
}

names(pop_ssps_data[[1]]) <- paste0("POP_SSP1_", seq(2010, 2050, by=10))
names(pop_ssps_data[[2]]) <- paste0("POP_SSP2_", seq(2010, 2050, by=10))
names(pop_ssps_data[[3]]) <- paste0("POP_SSP3_", seq(2010, 2050, by=10))
names(pop_ssps_data[[4]]) <- paste0("POP_SSP4_", seq(2010, 2050, by=10))
names(pop_ssps_data[[5]]) <- paste0("POP_SSP5_", seq(2010, 2050, by=10))

pop_ssps_data <- stack(brick(pop_ssps_data))

gdp_ssps_data_ex <- exact_extract(stack(gdp_ssps_data), sf, "sum", max_cells_in_memory = 93355200 )
pop_ssps_data_ex <- exact_extract(pop_ssps_data, sf, "sum", max_cells_in_memory  = 93355200 )

gdp_ssps_data_ex$iso3 <- sf$UC_NM_MN
pop_ssps_data_ex$iso3 <- sf$UC_NM_MN

gdp_ssps_data_ex <- reshape2::melt(gdp_ssps_data_ex, 26)
gdp_ssps_data_ex$Scenario <- toupper(substr(gdp_ssps_data_ex$variable, 9, 12))
gdp_ssps_data_ex$year <-  substr(gdp_ssps_data_ex$variable, 14, 17)
gdp_ssps_data_ex$variable <- NULL
colnames(gdp_ssps_data_ex)[2] <- "gdp"

pop_ssps_data_ex <- reshape2::melt(pop_ssps_data_ex, 26)
pop_ssps_data_ex$Scenario <- toupper(substr(pop_ssps_data_ex$variable, 9, 12))
pop_ssps_data_ex$year <-  substr(pop_ssps_data_ex$variable, 14, 17)
pop_ssps_data_ex$variable <- NULL
colnames(pop_ssps_data_ex)[2] <- "pop"

##########

sf <- st_make_valid(sf)
sf_c <- as_tibble(st_coordinates(st_centroid(sf)) )

cart2polar <- function(x, y) {
  data.frame(r = sqrt(x^2 + y^2), theta = atan2(y, x))
}

sf_c <- cart2polar(sf_c$X, sf_c$Y) 

sf <- bind_cols(sf, sf_c)

sf$geom <- NULL
sf$geometry <- NULL

###

sf <- merge(sf, cdd, by.x="UC_NM_MN", by.y="iso3c")
sf <- merge(sf, hdd, by.x=c("UC_NM_MN", "Scenario", "year"), by.y=c("iso3c", "Scenario", "year"))
sf <- merge(sf, gdp_ssps_data_ex, by.x=c("UC_NM_MN", "Scenario", "year"), by.y=c("iso3", "Scenario", "year"))
sf <- merge(sf, pop_ssps_data_ex, by.x=c("UC_NM_MN", "Scenario", "year"), by.y=c("iso3", "Scenario", "year"))

sf$popdens <- sf$pop / sf$AREA
sf$gdp_capita <- sf$gdp / sf$pop

###
# make projections of UGS using trained model and ssps gdp evolution

grid <- expand.grid(c("SSP2", "SSP5"), c(2020, 2030, 2040, 2050))

##
# summary(model$data[-1])
# summary(sf %>% dplyr::select(colnames(model$data)[-1]))

outer <- mapply(function(X, Y){predict(model, newdata= as.data.frame(sf %>% filter(Scenario==X, year==Y)))}, grid$Var1, grid$Var2)

colnames(outer) <- paste0(grid$Var1, "_", grid$Var2)

outer <- reshape2::melt(outer)
outer$scenario <- substr(outer$Var2, 1, 4)
outer$year  <- substr(outer$Var2, 6, 10)
outer$Var1 <- sf  %>% filter(Scenario %in% c("SSP2", "SSP5") & year %in% c(2020, 2030, 2040, 2050)) %>% arrange(Scenario, year, UC_NM_MN) %>% pull(UC_NM_MN)

outer$value <- outer$value

# show the projected trajectories (global and main sf/countries)

ggplot(outer %>% filter(Var1 %in% plot_cities), aes(x=year, y=value, colour=scenario, group=scenario))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(Var1), nrow = 1)+
  ylab("Predicted UGS, baseline trends")

ggsave("results/ugs_predicted.png", height=7, width=5, scale=1.2, bg="white")

##################

# design policy scenarios

### S1

outer_p1 <- outer
outer_p1$value_s <- outer_p1$value

for (ctr in unique(outer_p1$Var1)){
  for (scn in unique(outer_p1$scenario)){
    for(i in 1:(nrow(outer_p1[outer_p1$Var1==ctr & outer_p1$scenario==scn,])-1)){
      outer_p1$value_s[outer_p1$Var1==ctr & outer_p1$scenario==scn][i+1] <- ((outer_p1$value[outer_p1$Var1==ctr & outer_p1$scenario==scn][i+1] - outer_p1$value[outer_p1$Var1==ctr & outer_p1$scenario==scn][i]) + (outer_p1$value_s[outer_p1$Var1==ctr & outer_p1$scenario==scn][i]* 1.025))
    }}}

ggplot(outer_p1 %>% filter(Var1 %in% plot_cities))+
  geom_point(aes(x=year, y=value, colour=scenario, group=scenario))+
  geom_line(aes(x=year, y=value, colour=scenario, group=scenario, linetype="baseline"))+
  geom_point(aes(x=year, y=value_s, colour=scenario, group=scenario))+
  geom_line(aes(x=year, y=value_s, colour=scenario, group=scenario, linetype="policy"))+
  facet_wrap(vars(Var1), nrow = 1)+
  ylab("Predicted UGS, baseline trends")

##################

outer$policy <- " baseline"
outer_p1$policy <- "2.5%"

outer_p1$value <- outer_p1$value_s
outer_p1$value_s <- NULL

outer <- bind_rows(outer, outer_p1)

write_rds(outer, file="results/projections_gvi_citylevel.rds")
