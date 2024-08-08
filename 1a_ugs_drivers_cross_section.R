
##############################################################################

# This Rscript: 

#   1) build a cross sectional model to explain globally observed differences in city-level UGS density

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
library(raster)
library(exactextractr)
library(rpart)
library(caret)  # For data splitting
library(caret)

# Set the random seed for reproducibility (optional)
set.seed(123)


stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

########
########

sf <- read_sf("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database
sf <- sf %>% group_by(GRGN_L2) %>% slice_max(P15, n = 10)

# import city-level estimated UGS from Falchetta and Hammad's paper

load("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/urban_green_space_mapping_and_tracking/data/validation/after_points_090524.Rdata")
out_ndvi_m$city <- sf$UC_NM_MN[as.numeric((sapply(strsplit(out_ndvi_m$id,"_"), `[`, 1)))]
out_ndvi_m <- out_ndvi_m %>% group_by(city, country) %>% dplyr::summarise(out_b = median(out_b, na.rm=T), out_b_mean = mean(out_b, na.rm=T), out_b_max = max(out_b, na.rm=T) , out_b_min = min(out_b, na.rm=T))
out_ndvi_m$country <- countrycode::countrycode(out_ndvi_m$country, 'iso2c', 'iso3c')

# parse with driver variables

cdd <- brick("climate/cdd18_global_hist_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
             varname="variable")
cdd <- stackApply(cdd, 1, mean, na.rm = T)

hdd <- brick("climate/hdd18_global_hist_model_ensemble_median_yearly.nc", lvar=3, values=TRUE, level=1, 
             varname="variable")
hdd <- stackApply(hdd, 1, mean, na.rm = T)

pop_ssps <- list.files(path="F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/supporting_data/pop_downscaled_spps", recursive = T, pattern="nc", full.names = T)
pop_ssps_data <- lapply(pop_ssps, stack)

for (i in 1:5){
  pop_ssps_data[[i]] <- raster::subset(pop_ssps_data[[i]], c(5+c(10*c(0:4))))
  
}
names(pop_ssps_data[[1]]) <- paste0("POP_SSP1_", seq(2010, 2050, by=10))

sf$cdd <- exact_extract(cdd, sf, "mean")
sf$hdd <- exact_extract(hdd, sf, "mean")

#


sf$pop <- exact_extract(brick(pop_ssps_data[[1]][[1]]), sf, "sum")
sf$popdens <- sf$pop / sf$AREA

# gdp downscaled (SSPS)
gdp_ssps <- list.files(path="F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/rscripts/global_spline/supporting_data/gdp_downscaled_ssps", recursive = T, pattern="tif", full.names = T)[18]

gdp_ssps_data <- raster(gdp_ssps)

sf$gdp <- exact_extract(gdp_ssps_data, sf, "sum")
sf$gdp_capita <- sf$gdp / sf$pop

sf$geometry <- NULL

sf <- st_make_valid(sf)
sf_c <- as_tibble(st_coordinates(st_centroid(sf)) )

cart2polar <- function(x, y) {
  data.frame(r = sqrt(x^2 + y^2), theta = atan2(y, x))
}

sf_c <- cart2polar(sf_c$X, sf_c$Y) 

sf <- bind_cols(sf, sf_c)

out_ndvi_mm <- merge(out_ndvi_m, sf, by.x=c("city", "country"), by.y=c("UC_NM_MN", "CTR_MN_ISO"))
out_ndvi_mm$geom <- NULL

# estimate feols cross-sectional model

out_ndvi_mm <- dplyr::select(out_ndvi_mm, out_b, out_b_mean, out_b_max, out_b_min, cdd, hdd, popdens, gdp_capita, r, theta, country, GRGN_L2, GRGN_L1, pop)

library(fixest)

out_ndvi_mm <- filter(out_ndvi_mm, gdp_capita<1e5)

library(caret)

out_ndvi_mm <- dplyr::select(out_ndvi_mm, out_b_mean, cdd, hdd, popdens, gdp_capita, r, theta, GRGN_L2, GRGN_L1, pop)

rrfFit <- train(out_b_mean ~ ., 
                data = as.data.frame(out_ndvi_mm),
                method = 'ranger',
                tuneLength = 10,
                importance = "permutation", na.action = na.omit)

max(rrfFit$results$Rsquared)

varImp(rrfFit)

df <- data.frame(x=out_ndvi_mm$out_b, y=predict(rrfFit,  as.data.frame(out_ndvi_mm)))
cor(df$x, df$y)^2

ggplot(df, aes(x=x, y=y))+
  geom_point()+
  geom_abline()

###

pdp1 <- pdp::partial(rrfFit, 
                     pred.var = c("gdp_capita", "cdd"), 
                     trim.outliers = TRUE, chull = FALSE, parallel = TRUE,
                     grid.resolution = 10,  prob=T, paropts = list(.packages = "ranger"))

ggplot(pdp1)+
  geom_tile(aes(x=gdp_capita, y=cdd, fill=yhat))+
  scale_fill_distiller(palette = "Greens", direction = 1)+
  scale_y_continuous(limits = c(100, 2500))+
  scale_x_continuous(limits=c(1000, 75000))

rrfFit -> model

###

gdata::keep(res_dir, model, out_ndvi_mm, sure=T)

save.image(paste0(res_dir, "ugs_drivers_model.rds"))

