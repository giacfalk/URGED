
library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# max T

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("max", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymean_bylcz  + value_citymean_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_Tmax.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_Tmax.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_Tmax.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_Tmax.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+LCZ"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_Tmax.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

####################


library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# mean T

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("mean", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymean_bylcz  + value_citymean_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_Tmean.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_Tmean.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_Tmean.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_Tmean.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City clustering",
    "Two-way clustering",
    "Three-way clustering"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_Tmean.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

#########################


library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# min T

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("min", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymin_bylcz=min(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymin_bylcz  + value_citymin_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_Tmin.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_Tmin.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_Tmin.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_Tmin.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City clustering",
    "Two-way clustering",
    "Three-way clustering"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_Tmin.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

##########################


library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# max WBGT

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("max", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymean_bylcz  + value_citymean_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmax.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmax.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_WBGTmax.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmax.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City clustering",
    "Two-way clustering",
    "Three-way clustering"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmax.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

####################


library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# mean WBGT

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("mean", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymean_bylcz  + value_citymean_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmean.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmean.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_WBGTmean.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmean.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City clustering",
    "Two-way clustering",
    "Three-way clustering"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmean.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

#########################


library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# min WBGT

# Robustness 1: pooled regression

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("min", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds))


m0 <- feols(value ~  out_b + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

m2 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city")

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, lcz) %>% dplyr::mutate(value_citymin_bylcz=min(value, na.rm=T)) %>% ungroup()

m3_b <- feols(value ~ out_b : lcz : value_citymin_bylcz  + value_citymin_bylcz:lcz + build_h + build_v + elevation + water |  city^variable + lcz , data=out_ndvi_m_monthly, 
              , cluster="city")

etable(m0, m1, m2, m3_b)

etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmin.tex")
etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmin.tex")
etable(m2, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczmonthspecific_WBGTmin.tex")
etable(m3_b, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmin.tex")

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable")
m1_c <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | city^ variable + lcz, data=out_ndvi_m_monthly, cluster="city+variable+lcz")

etable(
  m1, m1_b, m1_c,
  headers = c(
    "City clustering",
    "Two-way clustering",
    "Three-way clustering"
  )
)

etable(m1, m1_b, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmin.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+LCZ"
       ), replace = T)

##########################
##########################
##########################


# T mean, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("mean", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_Tmean.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_Tmean.rds")) # Export as .tex
  
}

# T max, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("max", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_Tmax.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_Tmax.rds")) # Export as .tex
  
}

# T min, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[!grepl("wbgt", rr)]
rr <- rr[grepl("min", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_Tmin.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_Tmin.rds")) # Export as .tex
  
}

############


# WBGT mean, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("mean", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_WBGTmean.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_WBGTmean.rds")) # Export as .tex
  
}

# WBGT max, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("max", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_",dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_WBGTmax.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_WBGTmax.rds")) # Export as .tex
  
}

# WBGT min, city-specific

rr <- list.files(path="results/regression_data", pattern="rds", full.names=T)
rr <- rr[grepl("wbgt", rr)]
rr <- rr[grepl("min", rr)]

for (i in 1:length(rr)) {
  print(paste0("Reading file ", i, " of ", length(rr)))
  
  out_ndvi_m_monthly <- read_rds(rr[i])
  
  m1 <- feols(value ~  out_b:lcz:variable + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  summary(m1, "cluster")
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczmonthspecific_WBGTmin.rds")) # Export as .tex
  
  m1 <- feols(value ~  out_b:lcz + build_h + build_v + elevation + water  | variable + lcz, data=out_ndvi_m_monthly)
  
  saveRDS(broom::tidy(m1), file = paste0("results/URBCLIM_historical/regressions_/city_monthly_regression_", dplyr::first(out_ndvi_m_monthly$city), "_r1_lczspecific_WBGTmin.rds")) # Export as .tex
  
}
