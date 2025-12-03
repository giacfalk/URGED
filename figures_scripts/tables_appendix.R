
setwd(stub)
setwd("..")

library(tidyverse)

lcz_shares <- read.csv("output_data/outer.csv") %>% na.omit(.)
scens <- read.csv("output_data/outer_3.csv")

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)
lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

scens$X <- NULL

###

merger <- merge(lcz_shares, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

merger$value <- as.numeric(merger$value)

merger_s <- group_by(merger, UC_NM_MN, year, scen_SGS) %>% dplyr::mutate(value=value*(1/sum(value, na.rm=T)))

merger_s <- merger_s %>% group_by(UC_NM_MN, year, scen_SGS) %>% dplyr::summarise(value=sum(value*SGS, na.rm=T))

merger_s <- filter(merger_s, scen_SGS!="ugs_ref")

merger_s$scen_SGS = ifelse(merger_s$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(merger_s$scen_SGS=="ugs_scen_mod", "Moderate ambition", ifelse(merger_s$scen_SGS=="ugs_scen_impacted", "Decreased provision", "Historical (2016-2023)")))

merger_s$year <- ifelse(merger_s$year<2025, 2020, merger_s$year)

merger_s <- filter(merger_s, !(year==2025 & scen_SGS=="Historical (2016-2023)"))

merger_s <- merger_s %>% group_by(UC_NM_MN, year, scen_SGS) %>% dplyr::summarise(value=mean(value, na.rm=T))

merger_s$year <- as.factor(merger_s$year)

merger_s$scen_SGS <- factor(merger_s$scen_SGS, levels=c("Historical (2016-2023)", "Decreased provision", "Moderate ambition", "High ambition"))

#######

library(modelsummary)

datasummary(Factor(UC_NM_MN)*scen_SGS ~  year*value*Mean, data=merger_s, title = "SGS evolution scenarios, by city and year.")

datasummary(Factor(UC_NM_MN)*scen_SGS ~  year*value*Mean, data=merger_s, title = "SGS evolution scenarios, by city and year.", output = "paper/sgs_scenarios.tex", longtable=T)

#######
#######
#######

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmax_future, wbgt_max) # Deltas for future years
markups$delta <- markups$wbgtmax_future - markups$wbgt_max
markups$wbgt_max <- NULL
markups$wbgtmax_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = mean(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on maximum WBGT temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on maximum WBGT temperature in 2050 across cities and scenarios", output = "paper/climatechange_wbgt_max.tex")

###

#######
#######
#######

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_min.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmin_future, wbgt_min) # Deltas for future years
markups$delta <- markups$wbgtmin_future - markups$wbgt_min
markups$wbgt_min <- NULL
markups$wbgtmin_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = mean(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on minimum WBGT temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on minimum WBGT temperature in 2050 across cities and scenarios", output = "paper/climatechange_wbgt_min.tex")

####

#######
#######
#######

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_mean.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmean_future, wbgt_mean) # Deltas for future years
markups$delta <- markups$wbgtmean_future - markups$wbgt_mean
markups$wbgt_mean <- NULL
markups$wbgtmean_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = mean(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on mean WBGT temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on mean WBGT temperature in 2050 across cities and scenarios", output = "paper/climatechange_wbgt_mean.tex")

###

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_tas_max.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=max(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmax" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, max, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = max(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * max, data=r_s_cc, title = "Climate change impact on maximum temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * max, data=r_s_cc, title = "Climate change impact on maximum temperature in 2050 across cities and scenarios", output = "paper/climatechange_tas_max.tex")

###

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_tas_min.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=min(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmin" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, min, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = min(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * min, data=r_s_cc, title = "Climate change impact on minimum temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * min, data=r_s_cc, title = "Climate change impact on minimum temperature in 2050 across cities and scenarios", output = "paper/climatechange_tas_min.tex")

###

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_tas_mean.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Amman", "Phoenix", "Lima", "Houston")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tas" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

###

r_s_cc <- filter(r_s, !(clim_scen %in% c("High ambition", "Moderate ambition", "Decreased provision")))
r_s_cc <- r_s_cc %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value = mean(value, na.rm=T))

r_s_cc$clim_scen <- factor(r_s_cc$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_cc <- filter(r_s_cc, UC_NM_MN!="Bologna")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on mean temperature in 2050 across cities and scenarios")

datasummary(Factor(UC_NM_MN) ~  clim_scen*value * mean, data=r_s_cc, title = "Climate change impact on mean temperature in 2050 across cities and scenarios", output = "paper/climatechange_tas_mean.tex")

###

setwd(stub)
