

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/")

library(tidyverse)
library(modelsummary)
library(data.table)

###########################
###########################
###########################

var= "wbgt"
variant = "max"

lcz_shares <- read.csv("output_data/outer.csv") %>% na.omit(.)
coefs <- read.csv(paste0("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_", ifelse(var=="wbgt", "WBGT", "T2M"),
                         ifelse(variant == "mean", "mean",
                                ifelse(variant=="max", "max", "min")), "_raw_gvi.csv"))

scens <- read.csv("output_data/outer_3.csv")

# List of samplecities now loaded from fcts_labelers_colors.R
#

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)
lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

###

merger <- dplyr::arrange(merger, UC_NM_MN, variable, year, month, scen_SGS)

# lines for total cooling capacity estimate 
# merger_2020 <- filter(merger, year<=2020)
# merger_2020 <- merger_2020 %>% group_by(UC_NM_MN) %>% dplyr::mutate(SGS_base=mean(SGS, na.rm=T)) %>% filter(year==2020)
# merger <- merger_2020

merger_2020 <- dplyr::filter(merger, year<=2025)
merger_2020 <- merger_2020 %>% group_by(UC_NM_MN, variable) %>% dplyr::summarise(SGS_base=mean(SGS, na.rm=T))
merger <- filter(merger, year==2050)
merger <- merge(merger, merger_2020, c("UC_NM_MN", "variable"))
merger$SGS <- merger$SGS - merger$SGS_base


###########
###########

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Sydney", "Dubai", "Lima", "Mexico City", "Bogota", "Nairobi", "Dhaka")

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

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))


###

pops <- read.csv ("cities/cities_pop_bycity_bylcz_2020_2050.csv")

pops <- filter(pops, lcz %in% 1:9)

pops$lcz <- factor(pops$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger_s <- merge(merger, pops, by.x=c("UC_NM_MN", "variable"), by.y=c("UC_NM_MN", "lcz"))

merger_s <- merge(merger_s, r_s, by.x=c("UC_NM_MN"), by.y=c("UC_NM_MN"))

###########################
###########################

ggplot(merger_s_accra_high)+
  geom_hline(yintercept = 0)+
  theme_classic()+
  geom_col(aes(x=scen_SGS, y=product_fut/1e6, fill=scen_SGS))+
  ylab(expression(Delta * PWBGT))+
  xlab("LCZ")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  facet_wrap(vars(UC_NM_MN))+
  ggtitle("Change in population heat exposure due to SGS change in 2050 (current policies scenario), inclusive of population growth")

