
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

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

################

model <- read_rds(paste0(res_dir, "/ugs_drivers_model.rds"))

# update to future cdd / hdd

cdd_245 <- brick("cdd18_global_mid_245_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
                 varname="variable")
cdd_245 <- rgis::mask_raster_to_polygon(cdd_245, regions)

hdd_245 <- brick("hdd18_global_mid_245_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
                 varname="variable")
hdd_245 <- rgis::mask_raster_to_polygon(hdd_245, regions)

cdd_585 <- brick("cdd18_global_mid_585_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
                 varname="variable")
cdd_585 <- rgis::mask_raster_to_polygon(cdd_585, regions)

hdd_585 <- brick("hdd18_global_mid_585_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
                 varname="variable")
hdd_585 <- rgis::mask_raster_to_polygon(hdd_585, regions)

sf <- st_as_sf(rworldmap::countriesLow) %>% dplyr::select(ISO3, IMAGE24)

cdd_fut_245 <- exact_extract(cdd_245, sf, "weighted_mean", weights=pop)
hdd_fut_245 <- exact_extract(hdd_245, sf, "weighted_mean", weights=pop)

cdd_fut_245$iso3c <- sf$ISO3
hdd_fut_245$iso3c <- sf$ISO3

cdd_fut_585 <- exact_extract(cdd_585, sf, "weighted_mean", weights=pop)
hdd_fut_585 <- exact_extract(hdd_585, sf, "weighted_mean", weights=pop)

cdd_fut_585$iso3c <- sf$ISO3
hdd_fut_585$iso3c <- sf$ISO3

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

joined_projs <- merge(gdp_cap, cdd, by.x=c("iso3c", "Scenario", "variable"), by.y=c("iso3c", "Scenario", "year"))

joined_projs <- merge(joined_projs, hdd, by.x=c("iso3c", "Scenario", "variable"), by.y=c("iso3c", "Scenario", "year"))

#####

joined_projs <- merge(joined_projs, urban, by.x=c("iso3c", "Scenario", "variable"), by.y=c("iso3c", "scenario", "year"))
colnames(joined_projs)[5] <- "gdp"
joined_projs <- na.omit(joined_projs)

joined_projs <- group_by(joined_projs, iso3c, Scenario, variable) %>% dplyr::summarise(gdp=mean(gdp, na.rm=T), urbanisation=mean(urbanisation, na.rm=T), cdd=mean(cdd, na.rm=T), hdd=mean(hdd, na.rm=T))

joined_projs <- merge(joined_projs, sf %>% dplyr::select(ISO3, IMAGE24), by.x="iso3c", by.y="ISO3")

######

# make projections of UGS using trained model and ssps gdp evolution

grid <- expand.grid(c("SSP2", "SSP5"), c(2020, 2030, 2040, 2050))

outer <- mapply(function(X, Y){predict(model, joined_projs %>% filter(Scenario==X, variable==Y))}, grid$Var1, grid$Var2)

colnames(outer) <- paste0(grid$Var1, "_", grid$Var2)

outer <- reshape2::melt(outer)
outer$scenario <- substr(outer$Var2, 1, 4)
outer$year  <- substr(outer$Var2, 6, 10)
outer$Var1 <- rep(unique(joined_projs$iso3c), nrow(grid))

# show the projected trajectories (global and main regions/countries)

countriestoplot <- c("CHN", "USA", "IND", "RUS", "JPN", "CAN", "BRA", "DEU", "IRN", "KOR")

ggplot(outer %>% filter(Var1 %in% countriestoplot), aes(x=year, y=value, colour=scenario, group=scenario))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(Var1), scales = "free", nrow = 5)+
  ylab("Predicted UGS, baseline trends")

ggsave("plots/ugs_predicted.png", height=7, width=5, scale=1.2, bg="white")

##################

# design policy scenarios

### S1

outer_p1 <- outer
outer_p1$value_s <- outer_p1$value

for (ctr in unique(outer_p1$Var1)){
  for (scn in unique(outer_p1$scenario)){
    for(i in 1:(nrow(outer_p1[outer_p1$Var1==ctr & outer_p1$scenario==scn,])-1)){
      outer_p1$value_s[outer_p1$Var1==ctr & outer_p1$scenario==scn][i+1] <- ((outer_p1$value[outer_p1$Var1==ctr & outer_p1$scenario==scn][i+1] - outer_p1$value[outer_p1$Var1==ctr & outer_p1$scenario==scn][i]) + (outer_p1$value_s[outer_p1$Var1==ctr & outer_p1$scenario==scn][i]* 1.05))
    }}}

countriestoplot <- c("CHN", "USA", "IND", "RUS", "JPN", "CAN", "BRA", "DEU", "IRN", "KOR")

ggplot(outer_p1 %>% filter(Var1 %in% countriestoplot))+
  geom_point(aes(x=year, y=value, colour=scenario, group=scenario))+
  geom_line(aes(x=year, y=value, colour=scenario, group=scenario, linetype="baseline"))+
  geom_point(aes(x=year, y=value_s, colour=scenario, group=scenario))+
  geom_line(aes(x=year, y=value_s, colour=scenario, group=scenario, linetype="policy"))+
  facet_wrap(vars(Var1), scales = "free", nrow = 5)+
  ylab("Predicted UGS, baseline trends")

##################

outer$policy <- " baseline"
outer_p1$policy <- "5%"

outer_p1$value <- outer_p1$value_s
outer_p1$value_s <- NULL

outer <- bind_rows(outer, outer_p1)
