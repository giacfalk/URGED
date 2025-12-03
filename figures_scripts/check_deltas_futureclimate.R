
setwd(stub)
setwd("..")

library(tidyverse)
library(data.table)

markups <- readRDS("results/wbgt/wbgt_future.rds")


summary(markups$wbgtmax_future - markups$wbgt_max)
summary(markups$wbgtmin_future - markups$wbgt_min)
summary(markups$wbgtmean_future - markups$wbgt_mean)

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmax" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

summary(markups$delta)

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmin" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

summary(markups$delta)

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tas" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

summary(markups$delta)
