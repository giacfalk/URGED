
library(rworldmap)
library(countrycode)
require(maptools)
library(ggthemes)
library(ggrepel)
library(sf)
library(tidyverse)
library(raster)
library(dplyr)
library(data.table)

setwd(stub)
setwd("..")


r = read.csv("implementation/results/scenarios/absolute_heat_decrease_tas_min.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Sydney", "Dubai", "Lima", "Houston", "Bogota", "Nairobi", "Dhaka")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmin" & year==2050)

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

r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="Decreased provision")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = 1 - ((value[clim_scen=="CurPol"] + value[clim_scen=="High ambition"]) / value[clim_scen=="CurPol"]), potential_GS = 1 - ((value[clim_scen=="GS"] + value[clim_scen=="High ambition"]) / value[clim_scen=="GS"]), potential_SP = 1 - ((value[clim_scen=="SP"] + value[clim_scen=="High ambition"]) / value[clim_scen=="SP"]), potential_ssp585 = 1 - ((value[clim_scen=="ssp585"] + value[clim_scen=="High ambition"]) / value[clim_scen=="ssp585"]) )

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value * 100

r_s_pot$value <- ifelse(r_s_pot$value < 0, 0, r_s_pot$value)

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(value = mean(value, na.rm=T))

###

library(rworldmap)
wrld_simpl_sf <- st_as_sf(rworldmap::countriesLow)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)


sf_c <- read_sf("implementation/boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database

##

grr_m <- merge(r_s_pot, sf_c, "UC_NM_MN")

##

grr_m <- grr_m %>% dplyr::group_by(UC_NM_MN) %>% dplyr::mutate(out_b_mean = mean(value, na.rm=T))

grr_m <- st_as_sf(grr_m)

grr_m$out_b_mean_d <- cut(grr_m$out_b_mean,
                                breaks=c(-Inf, 0, 10, 25, 50, Inf),
                                labels=c('0', '0-10', '10-25', '25-50', '>50'))


ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="lightgrey", lwd=0.25, alpha=0.65)+
  stat_sf_coordinates(data=st_centroid(grr_m), colour="black", size=2.8)+
  stat_sf_coordinates(data=st_centroid(grr_m), aes(colour=out_b_mean_d), size=2)+
  scale_colour_viridis_d(name="%", direction = 1)+
  # scale_colour_steps2(name="%", midpoint = 0, high = "forestgreen", mid="grey", low="darkred")+
  theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1, "cm"))+
  xlab("")+
  ylab("")+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  coord_sf(xlim=c(-11396910, 13580970 ))

ggsave("paper/map_counterbalancing_tas_min.pdf", scale=2, height = 4, width = 6)

##

clima <- read.csv("implementation/cities/cities_urged+GHS.csv") %>% dplyr::select(city, Cls_short)

grr_m <- merge(grr_m, clima, by.x="UC_NM_MN", by.y="city")

grr_m$Cls_short[grr_m$UC_NM_MN=="Basel"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Geneva"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Ghent"] <-  "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Lille"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Singapore"] <- 
  grr_m$Cls_short[grr_m$UC_NM_MN=="Strasbourg"] <- "A" 

grr_m$Cls_short <- factor(grr_m$Cls_short, levels=c("A", "B", "C", "D"), labels=c("Tropical", "Dry", "Temperate", "Continental"))

grr_m$geom <- NULL
grr_m <- grr_m %>% group_by(UC_NM_MN, Cls_short) %>% dplyr::summarize(out_b_mean = mean(value, na.rm=T))

ggplot(grr_m)+
  theme_classic()+
  gg.layers::geom_boxplot2(aes(x=Cls_short, y=out_b_mean), width.errorbar = .25, fill="lightgrey", lwd=0.1)+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks=seq(0, 60, 10), limits = c(0, 60))

ggsave("paper/map_counterbalancing_tas_min_boxplot.pdf", scale=0.5, height = 4, width = 8)

###############

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Sydney", "Dubai", "Lima", "Houston", "Bogota", "Nairobi", "Dhaka")

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

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision" &  clim_scen!="Moderate ambition"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="Decreased provision")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = 1 - ((value[clim_scen=="CurPol"] + value[clim_scen=="High ambition"]) / value[clim_scen=="CurPol"]), potential_GS = 1 - ((value[clim_scen=="GS"] + value[clim_scen=="High ambition"]) / value[clim_scen=="GS"]), potential_SP = 1 - ((value[clim_scen=="SP"] + value[clim_scen=="High ambition"]) / value[clim_scen=="SP"]), potential_ssp585 = 1 - ((value[clim_scen=="ssp585"] + value[clim_scen=="High ambition"]) / value[clim_scen=="ssp585"]) )

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value * 100

r_s_pot$value <- ifelse(r_s_pot$value < 0, 0, r_s_pot$value)

##

tapply(r_s_pot$value, r_s_pot$variable, summary)

##

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(value = mean(value, na.rm=T))

library(rworldmap)
wrld_simpl_sf <- st_as_sf(rworldmap::countriesLow)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)


sf_c <- read_sf("implementation/boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database

##

grr_m <- merge(r_s_pot, sf_c, "UC_NM_MN")

##

grr_m <- grr_m %>% group_by(UC_NM_MN) %>% dplyr::mutate(out_b_mean = mean(value, na.rm=T))

grr_m <- st_as_sf(grr_m)

grr_m$out_b_mean_d <- cut(grr_m$out_b_mean,
                          breaks=c(-Inf, 0, 10, 25, 50, Inf),
                          labels=c('0', '0-10', '10-25', '25-50', '>50'))


ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="lightgrey", lwd=0.25, alpha=0.65)+
  stat_sf_coordinates(data=st_centroid(grr_m), colour="black", size=2.8)+
  stat_sf_coordinates(data=st_centroid(grr_m), aes(colour=out_b_mean_d), size=2)+
  scale_colour_viridis_d(name="%", direction = 1)+
  # scale_colour_steps2(name="%", midpoint = 0, high = "forestgreen", mid="grey", low="darkred")+
  theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1, "cm"))+
  xlab("")+
  ylab("")+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  coord_sf(xlim=c(-11396910, 13580970 ))

ggsave("paper/map_counterbalancing_wbgt_max.pdf", scale=2, height = 4, width = 6)
ggsave("paper/map_counterbalancing_wbgt_max.png", scale=2, height = 4, width = 6)

##

ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="lightgrey", lwd=0.25, alpha=0.65)+
  stat_sf_coordinates(data=st_centroid(grr_m), colour="black", size=2.8)+
  stat_sf_coordinates(data=st_centroid(grr_m), aes(colour=out_b_mean_d), size=2)+
  scale_colour_viridis_d(name="%", direction = 1)+
  # scale_colour_steps2(name="%", midpoint = 0, high = "forestgreen", mid="grey", low="darkred")+
  theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1, "cm"))+
  xlab("")+
  ylab("")+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  coord_sf(
    xlim = c(-2000000, 4000000),
    ylim = c(3500000, 7500000),
    expand = FALSE
  )

ggsave("paper/map_counterbalancing_wbgt_max_eu.pdf", scale=1.2, height = 4, width = 6)
ggsave("paper/map_counterbalancing_wbgt_max_eu.png", scale=1.2, height = 4, width = 6)

##

clima <- read.csv("implementation/cities/cities_urged+GHS.csv") %>% dplyr::select(city, Cls_short)

grr_m <- merge(grr_m, clima, by.x="UC_NM_MN", by.y="city")

grr_m$Cls_short[grr_m$UC_NM_MN=="Basel"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Geneva"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Ghent"] <-  "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Lille"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Singapore"] <- 
  grr_m$Cls_short[grr_m$UC_NM_MN=="Strasbourg"] <- "A" 

grr_m$Cls_short <- factor(grr_m$Cls_short, levels=c("A", "B", "C", "D"), labels=c("Tropical", "Dry", "Temperate", "Continental"))

grr_m$geom <- NULL
grr_m <- grr_m %>% group_by(UC_NM_MN, Cls_short) %>% dplyr::summarize(out_b_mean = mean(value, na.rm=T))

ggplot(grr_m)+
  theme_classic()+
  gg.layers::geom_boxplot2(aes(x=Cls_short, y=out_b_mean), width.errorbar = .25, fill="lightgrey", lwd=0.1)+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks=seq(0, 60, 10), limits = c(0, 60))

ggsave("paper/map_counterbalancing_wbgt_max_boxplot.pdf", scale=.5, height = 4, width = 8)
ggsave("paper/map_counterbalancing_wbgt_max_boxplot.png", scale=.5, height = 4, width = 8)

########

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_mean.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Sydney", "Dubai", "Lima", "Houston", "Bogota", "Nairobi", "Dhaka")

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

r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="Decreased provision")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = 1 - ((value[clim_scen=="CurPol"] + value[clim_scen=="High ambition"]) / value[clim_scen=="CurPol"]), potential_GS = 1 - ((value[clim_scen=="GS"] + value[clim_scen=="High ambition"]) / value[clim_scen=="GS"]), potential_SP = 1 - ((value[clim_scen=="SP"] + value[clim_scen=="High ambition"]) / value[clim_scen=="SP"]), potential_ssp585 = 1 - ((value[clim_scen=="ssp585"] + value[clim_scen=="High ambition"]) / value[clim_scen=="ssp585"]) )

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value * 100

r_s_pot$value <- ifelse(r_s_pot$value < 0, 0, r_s_pot$value)

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(value = mean(value, na.rm=T))

library(rworldmap)
wrld_simpl_sf <- st_as_sf(rworldmap::countriesLow)
wrld_simpl_sf <- st_transform(wrld_simpl_sf, "ESRI:54009")
wrld_simpl_sf <- filter(wrld_simpl_sf, NAME!="Antarctica")

load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_graticules.prj <- spTransform(NE_graticules, CRSobj = PROJ)


sf_c <- read_sf("implementation/boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") # Cities database

##

grr_m <- merge(r_s_pot, sf_c, "UC_NM_MN")

##

grr_m <- grr_m %>% group_by(UC_NM_MN) %>% dplyr::mutate(out_b_mean = mean(value, na.rm=T))

grr_m <- st_as_sf(grr_m)

grr_m$out_b_mean_d <- cut(grr_m$out_b_mean,
                          breaks=c(-Inf, 0, 10, 25, 50, Inf),
                          labels=c('0', '0-10', '10-25', '25-50', '>50'))


ggplot()+
  theme_void()+
  geom_sf(data=wrld_simpl_sf, fill="#fafafa", colour="lightgrey", lwd=0.25, alpha=0.65)+
  stat_sf_coordinates(data=st_centroid(grr_m), colour="black", size=2.8)+
  stat_sf_coordinates(data=st_centroid(grr_m), aes(colour=out_b_mean_d), size=2)+
  scale_colour_viridis_d(name="%", direction = 1)+
  # scale_colour_steps2(name="%", midpoint = 0, high = "forestgreen", mid="grey", low="darkred")+
  theme(legend.position = "bottom", legend.direction = "horizontal",  plot.margin = unit(c(t=0, r=2, b=0, l=0), unit="cm"), legend.key.width = unit(1, "cm"))+
  xlab("")+
  ylab("")+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  coord_sf(xlim=c(-11396910, 13580970 ))

ggsave("paper/map_counterbalancing_wbgt_mean.pdf", scale=2, height = 4, width = 6)

##

clima <- read.csv("implementation/cities/cities_urged+GHS.csv") %>% dplyr::select(city, Cls_short)

grr_m <- merge(grr_m, clima, by.x="UC_NM_MN", by.y="city")

grr_m$Cls_short[grr_m$UC_NM_MN=="Basel"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Geneva"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Ghent"] <-  "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Lille"] <- "C"
grr_m$Cls_short[grr_m$UC_NM_MN=="Singapore"] <- 
  grr_m$Cls_short[grr_m$UC_NM_MN=="Strasbourg"] <- "A" 

grr_m$Cls_short <- factor(grr_m$Cls_short, levels=c("A", "B", "C", "D"), labels=c("Tropical", "Dry", "Temperate", "Continental"))

grr_m$geom <- NULL
grr_m <- grr_m %>% group_by(UC_NM_MN, Cls_short) %>% dplyr::summarize(out_b_mean = mean(value, na.rm=T))

ggplot(grr_m)+
  theme_classic()+
  gg.layers::geom_boxplot2(aes(x=Cls_short, y=out_b_mean), width.errorbar = .25, fill="lightgrey", lwd=0.1)+
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks=seq(0, 60, 10), limits = c(0, 60))

ggsave("paper/map_counterbalancing_wbgt_mean_boxplot.pdf", scale=.5, height = 4, width = 8)
