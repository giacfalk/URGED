# This file writes a global gridded output template for climate zone-specific, local-climate zone specific CEs.
# Set required packages -------------------------------------------------
rm(list=ls(all=TRUE)) # Removes all previously created variables 
gc()
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory

###

outer = list.files(path="results", full.names = T, pattern="t_data", recursive = T)
outer <- outer[!grepl("Newcastle", outer)]
outer = bind_rows(lapply(outer, read.csv))

outer$lcz <- factor(outer$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

colnames(outer)[5] <- "tas"

###

source("URGED/support/fcts_labelers_colors.R")

ggplot(outer)+ #2025
  theme_classic()+
  geom_boxplot(aes(x=as.factor(variable), y=tas, fill=lcz), lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz)+
  ylab("°C")+
  xlab("Month")+
  facet_wrap(city ~ lcz, scales = "free_y")+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

##


ggplot(outer)+ #2025
  theme_classic()+
  geom_boxplot(aes(x=as.factor(variable), y=tas), outlier.color = "transparent")+
  ylab("°C")+
  xlab("Month")+
  facet_wrap(vars(city), scales = "free_y")+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("urbclim_distr_bycity_tas_monthly.png", last_plot(), height = 7, width = 10, scale=2)

#

outer %>% filter(city=="Barcelona" & variable==6) %>% pull(tas) %>% summary(.)

####

setwd(paste0(stub0, "/URGED"))

