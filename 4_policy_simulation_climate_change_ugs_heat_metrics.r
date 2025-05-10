
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub <- paste0(getwd(), "/")
library(tidyverse)
# Source helper files and functions ---------------------------------------
source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found
### 

df <- expand.grid(var=c("tas", "wbgt"), variant=c("mean", "min", "max"))

#######################################
#######################################

functer <- function(var, variant){

print(paste("Processing", var, variant))
  
lcz_shares <- read.csv("output_data/outer.csv") %>% na.omit(.)
coefs <- read.csv(
  paste0("output_data/outer_2", ifelse(var=="wbgt", "_wbgt", ""),
         ifelse(variant == "mean", "_mean",
                ifelse(variant=="max", "_max", "_min")),
         ".csv"))


coefs$UC_NM_MN <- gsub("_wbgt", "", coefs$UC_NM_MN)
coefs$UC_NM_MN <- gsub("_max", "", coefs$UC_NM_MN)
coefs$UC_NM_MN <- gsub("_mean", "", coefs$UC_NM_MN)
coefs$UC_NM_MN <- gsub("_min", "", coefs$UC_NM_MN)

scens <- read.csv("output_data/outer_3.csv")

# List of samplecities now loaded from fcts_labelers_colors.R
#

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

###

nearest_word_match <- function(vec1, vec2, method = "lv") {
  library(stringdist)
  
  dist_matrix <- stringdistmatrix(vec1, vec2, method = method)
  nearest_indices <- apply(dist_matrix, 1, which.min)
  
  data.frame(
    original = vec1,
    closest_match = vec2[nearest_indices],
    distance = dist_matrix[cbind(1:length(vec1), nearest_indices)],
    stringsAsFactors = FALSE
  )
}

nnn <- nearest_word_match(unique(coefs$UC_NM_MN), unique(scens$UC_NM_MN))
nnn$closest_match[106] <- "Rotterdam"
nnn$distance <- NULL
colnames(nnn) <- c("city", "city_provide")

coefs <- merge(coefs, nnn, by.x="UC_NM_MN", by.y="city")
coefs$UC_NM_MN <- coefs$city_provide
coefs$city_provide <- NULL

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)
lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y=c("UC_NM_MN", "lcz"))

###

merger <- dplyr::arrange(merger, UC_NM_MN, variable, year, month, scen_SGS)

# lines for total cooling capacity estimate 
# merger_2020 <- filter(merger, year<=2020)
# merger_2020 <- merger_2020 %>% group_by(UC_NM_MN) %>% dplyr::mutate(SGS_base=mean(SGS, na.rm=T)) %>% filter(year==2020)
# merger <- merger_2020

merger_2020 <- dplyr::filter(merger, year<2025)
merger_2020 <- merger_2020 %>% group_by(UC_NM_MN, variable) %>% dplyr::summarise(SGS_base=mean(SGS, na.rm=T))
merger <- filter(merger, year==2050)
merger <- merge(merger, merger_2020, c("UC_NM_MN", "variable"))
merger$SGS <- merger$SGS - merger$SGS_base

# ggplot(merger %>% filter(UC_NM_MN %in% list_samplecities))+
#   geom_line(aes(x=variable, y=SGS), lwd=1.5)+
#   facet_wrap(vars(UC_NM_MN))+
#   geom_hline(yintercept = 0)

# ggsave("results/scenarios/scens_point_summary.png", height = 8, width = 15)

##

merger_bk <- merger

##

merger$value <- as.numeric(merger$value)

merger_s <- group_by(merger, UC_NM_MN, year, month, scen_SGS) %>% dplyr::mutate(value=value*(1/sum(value, na.rm=T)))

merger_s$product <- merger_s$value * merger_s$coef/100

merger_s <- group_by(merger_s, UC_NM_MN, year, month, scen_SGS) %>% dplyr::summarise(product=sum(product, na.rm=T))

ggplot(merger_s %>% filter(UC_NM_MN %in% list_samplecities))+
  geom_hline(yintercept = 0)+
  geom_line(aes(x=month, y=product*100, colour=scen_SGS))+
  scale_x_continuous(labels=c(1:12), breaks=c(1:12))+
  ylab(paste0("% change in monthly", ifelse(variant=="mean", "average", ifelse(variant=="max", "maximum", "minimum")), ifelse(var=="tas", " air temperature", " WBGT"), " (°C) in response to GVI increase by 1 unit"))+
  facet_wrap(vars(UC_NM_MN), scales = "free")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(paste0("results/scenarios/marginal_heat_decrease_city_", var, "_", variant, ".png"), height = 8, width = 15)

write.csv(merger_s %>% dplyr::mutate(product=product*100), paste0("results/scenarios/marginal_heat_decrease_", var, "_", variant, ".csv"))

##

merger_s <- group_by(merger, UC_NM_MN, year, month, scen_SGS) %>% dplyr::mutate(value=value*(1/sum(value, na.rm=T)))
 
merger_s$product <-merger_s$coef/100 * merger_s$SGS_base

merger_s <- dplyr::group_by(merger_s, UC_NM_MN, year, month, scen_SGS) %>% dplyr::summarise(product=sum(product*value, na.rm=T))

ggplot(merger_s %>% filter(UC_NM_MN %in% list_samplecities))+
  geom_hline(yintercept = 0)+
  geom_line(aes(x=month, y=(product*100), colour=scen_SGS))+
  scale_x_continuous(labels=c(1:12), breaks=c(1:12))+
  ylab(paste0("% change in monthly", ifelse(variant=="mean", "average", ifelse(variant=="max", "maximum", "minimum")), ifelse(var=="tas", " air temperature", " WBGT"), " (°C) due to current SGS level"))+
  facet_wrap(vars(UC_NM_MN), scales = "free")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(paste0("results/scenarios/percentage_heat_decrease_city_current_", var, "_", variant, ".png"), height = 8, width = 15) 

write.csv(merger_s %>% dplyr::mutate(product=product*100), paste0("results/scenarios/percentage_heat_decrease_current_", var, "_", variant, ".csv"))

###

merger_s <- group_by(merger, UC_NM_MN, year, month, scen_SGS) %>% dplyr::mutate(value=value*(1/sum(value, na.rm=T)))

merger_s$product <-merger_s$coef/100 * merger_s$SGS

merger_s <- dplyr::group_by(merger_s, UC_NM_MN, year, month, scen_SGS) %>% dplyr::summarise(product=sum(product*value, na.rm=T))

ggplot(merger_s %>% filter(UC_NM_MN %in% list_samplecities))+
  geom_hline(yintercept = 0)+
  geom_line(aes(x=month, y=(product*100), colour=scen_SGS))+
  scale_x_continuous(labels=c(1:12), breaks=c(1:12))+
  ylab(paste0("% change in monthly", ifelse(variant=="mean", "average", ifelse(variant=="max", "maximum", "minimum")), ifelse(var=="tas", " air temperature", " WBGT"), " (°C) due to future SGS evolution"))+
  facet_wrap(vars(UC_NM_MN), scales = "free")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(paste0("results/scenarios/percentage_heat_decrease_city_", var, "_", variant, ".png"), height = 8, width = 15) 

write.csv(merger_s %>% dplyr::mutate(product=product*100), paste0("results/scenarios/percentage_heat_decrease_", var, "_", variant, ".csv"))


####

# apply climate change markups

if(var=="tas"){

# average monthly temperature by lcz

outer = list.files(path="results", full.names = T, pattern="t_stats", recursive = T)
outer = outer[!grepl("Newcastle", outer)]
outer = bind_rows(lapply(outer, read.csv))
outer <- dplyr::group_by(outer, city, variable, lcz) %>% dplyr::summarise(value=mean(value, na.rm=T))

outer$lcz <- factor(outer$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

colnames(outer)[4] <- "tas"
  
markups <- readRDS("results/scenarios/climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var==ifelse(variant=="mean", "tas", ifelse(variant=="maximum", "tasmax", "tasmin")), pctl %in% c("pct45", "pct55"))

library(data.table)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

} else{
  
  markups_wbgt <- read_rds("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, month, clim_scen, wbgtmean_future, wbgtmin_future, wbgtmax_future, lcz)
  
  if(variant=="mean"){
    markups_wbgt$wbgtmax_future <- NULL
    markups_wbgt$wbgtmin_future <- NULL
  } else if(variant=="max"){
    markups_wbgt$wbgtmean_future <- NULL
    markups_wbgt$wbgtmin_future <- NULL
  } else{
    markups_wbgt$wbgtmean_future <- NULL
    markups_wbgt$wbgtmax_future <- NULL
  }
}

#####

outerss = list.files(path="results", full.names = T, pattern=paste0(ifelse(var=="tas", "t", "wbgt"), "_stats_", variant), recursive = T)

if(var=="tas"){
  outerss = outerss[!grepl("wbgt", outerss)]
  outerss = outerss[!grepl("Newcastle", outerss)]
  
} 

outerss = bind_rows(lapply(outerss, read.csv))

if(var=="tas"){
outerss <- merge(outerss, markups, by=c("city", "variable"))
outerss$X <- NULL
} else{
  
  markups_wbgt$variable <- match(markups_wbgt$month, month.abb)
  markups_wbgt$month <- NULL
  
  outerss <- merge(outerss, markups_wbgt, by=c("city", "variable", "lcz"))
  outerss$X <- NULL
  
}

if(var=="tas"){
outerss$value_fut <- outerss$value + outerss$delta

colnames(outerss)[4] <- "tas"
colnames(outerss)[9] <- "tas_fut"

} else{
  
  colnames(outerss)[4] <- "tas"
  colnames(outerss)[7] <- "tas_fut"
  
}

###

merger <- merger_bk

# merger <- merger[!duplicated(merger),]

merger$value <- as.numeric(merger$value)

outerss$lcz <- factor(outerss$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger_s <- merge(merger, outerss, by.x=c("UC_NM_MN", "month", "variable", "year"), by.y=c("city", "variable", "lcz", "year"))

merger_s <- group_by(merger_s, UC_NM_MN, year, month, scen_SGS, clim_scen) %>% dplyr::mutate(value=value*(1/sum(value, na.rm=T)))

merger_s$product <- ((merger_s$coef * merger_s$SGS)) * merger_s$tas
merger_s$product_fut <- ((merger_s$coef * merger_s$SGS)) * merger_s$tas_fut
  
merger_s <- dplyr::group_by(merger_s, UC_NM_MN, year, month, scen_SGS, clim_scen) %>% dplyr::summarise(product=sum(product*value, na.rm=T), product_fut=sum(product_fut*value, na.rm=T))

library(scales)

# merger_s <- filter(merger_s, scen_SGS=="GVI")

#UC_NM_MN %in% c("London", "Lima", "Mogadishu", "Dhaka") & 

ggplot(merger_s %>% filter(UC_NM_MN %in% list_samplecities))+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_line(aes(x=as.factor(month), y=product_fut/100, colour=clim_scen, group=interaction(clim_scen, scen_SGS), linetype=scen_SGS))+
  ylab(paste0("Change in monthly", ifelse(variant=="mean", "average", ifelse(variant=="max", "maximum", "minimum")), ifelse(var=="tas", " air temperature", " WBGT"), " (°C) in 2050"))+
  ggtitle("")+
  facet_wrap(vars(UC_NM_MN), scales = "free")+
  xlab("Month")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  scale_colour_discrete(name="Climate scenario")+
  scale_x_discrete(labels=c(1,3,6,9,12), breaks=c(1,3,6,9,12))

ggsave(paste0("results/scenarios/absolute_heat_decrease_city_", var, "_", variant, ".pdf"), height = 8, width = 10, scale=1.35)

write.csv(merger_s %>% dplyr::mutate(product_fut=product_fut/100),paste0("results/scenarios/absolute_heat_decrease_", var, "_", variant, ".csv"))


}

mapply(functer, df$var, df$variant)

####

setwd(paste0(stub, "/URGED"))

