
library(fixest)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

# max T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[!grepl("WBGT", rr)]
rr <- rr[grepl("max", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_T2Mmax_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_T2Mmax.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
m1[[as.character(lcz_s)]] <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_T2Mmax.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(T2M, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(T2M ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_T2Mmax.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_T2Mmax.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)

####################

# mean T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[!grepl("WBGT", rr)]
rr <- rr[grepl("mean", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_T2Mmean_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_T2Mmean.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m1[[as.character(lcz_s)]] <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_T2Mmean.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(T2M, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(T2M ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_T2Mmean.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_T2Mmean.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)

# min T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[!grepl("WBGT", rr)]
rr <- rr[grepl("min", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_T2M2Mmin_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_T2Mmin.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m1[[as.character(lcz_s)]] <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_T2Mmin.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(T2M, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(T2M ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_T2Mmin.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(T2M ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_T2Mmin.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)

#####


# max T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[grepl("WBGT", rr)]
rr <- rr[grepl("max", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_WBGTmax_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmax.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m1[[as.character(lcz_s)]] <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmax.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(wbgt, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(wbgt ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmax.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmax.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)

####################

# mean T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[grepl("WBGT", rr)]
rr <- rr[grepl("mean", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_WBGTmean_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmean.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m1[[as.character(lcz_s)]] <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmean.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(wbgt, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(wbgt ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmean.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmean.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)

# min T

# Robustness 1: pooled regression

rr <- list.files(path="C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs", pattern="panel_data", full.names=T, recursive = T)
rr <- rr[grepl("WBGT", rr)]
rr <- rr[grepl("min", rr)]
rr <- rr[grepl("rds", rr)]
out_ndvi_m_monthly <- bind_rows(lapply(rr, read_rds), .id="city")
out_ndvi_m_monthly$city <- gsub("_WBGT2Mmin_monthly_panel_data.rds", "", basename(rr))[as.numeric(out_ndvi_m_monthly$city)]

out_ndvi_m_monthly$month <- as.factor(out_ndvi_m_monthly$month)
out_ndvi_m_monthly$year <- as.factor(out_ndvi_m_monthly$year)

m0 <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city")

#etable(m0, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_WBGTmin.tex")

m1 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m1[[as.character(lcz_s)]] <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year, data=out_ndvi_m_monthly_f, cluster="city")
}

etable(m1, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_lczspecific_WBGTmin.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m1); gc()

# Robustness 3: specification with cell-id fixed effects, à la Lunghi

out_ndvi_m_monthly$cellid <- paste0(out_ndvi_m_monthly$x, "_", out_ndvi_m_monthly$y)

out_ndvi_m_monthly <- group_by(out_ndvi_m_monthly, city, month, year, lcz) %>% dplyr::mutate(value_citymean_bylcz=mean(wbgt, na.rm=T)) %>% ungroup()

m3 <- list()

for(lcz_s in sort(unique(out_ndvi_m_monthly$lcz))){
  out_ndvi_m_monthly_f <- out_ndvi_m_monthly %>% filter(lcz==lcz_s)
  m3[[as.character(lcz_s)]] <- feols(wbgt ~ value_citymean_bylcz + out_b : value_citymean_bylcz  + build_h + build_v + elevation + water | city^month + city^year, data=out_ndvi_m_monthly_f, 
                                     , cluster="city")
}

etable(m3, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_celldeviation_WBGTmin.tex", headers = paste0("LCZ", sort(unique(out_ndvi_m_monthly$lcz))))
rm(m3); gc()

#######################

# Robustness 2: different standard errors clustering schemes

m1_b <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month")
m1_c <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year")
m1_d <- feols(wbgt ~  out_b + build_h + build_v + elevation + water  | city^month + city^year + lcz, data=out_ndvi_m_monthly, cluster="city+month+year+lcz")

etable(
  m0, m1_b, m1_c, m1_c,
  headers = c(
    "City",
    "City+month",
    "City+month+year",
    "City+month+year+LCZ"
  )
)

etable(m0, m1_b, m1_c, m1_c, file="results/URBCLIM_historical/regressions_/city_monthly_regression_pooled_r1_SEclustering_WBGTmin.tex",
       headers = c(
         "City",
         "City+month",
         "City+month+year",
         "City+month+year+LCZ"), replace = T)
