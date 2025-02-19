###
# Process the climate change scenarios versions
###

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(purrr)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move WD one up
stub0 <- paste0(getwd(), "/") # Define a variable with the current WD
###

stub2 <- "climate/provide_urban_climate_data/climatechange/future_deltas"
setwd(stub2)

###

library(readxl)
library(pbapply)
library(parallel)

# Detect number of available cores
num_cores <- detectCores() - 1  # Leave one core free

# Create a cluster
cl <- makeCluster(num_cores)

clusterEvalQ(cl, {
  library(tidyverse); library(readxl); library(pbapply)  # Example package
  NULL  # Prevents printing output
})

paths <- list.files(pattern = "xlsx", recursive = T)

r <- pblapply(paths, function(path){ path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_xlsx, path = path, .name_repair = "unique_quiet")}, cl = cl)

stopCluster(cl)

###

names(r) <- gsub("_ensmean.xlsx", "",  paths)

###

setwd(stub0)

r <- lapply(1:length(r), function(X){bind_rows(r[[X]], .id="var")})

names(r) <-  gsub("_ensmean.xlsx", "",  paths)

r <- bind_rows(r, .id="v")

r$v <- sub("^[^/]*/", "", r$v)

r$scenario <- sub("/.*", "", r$v)

r$v <- sub("^[^/]*/", "", r$v)

r$city <- str_sub(sub("[0-9].*", "", r$v), 1, -2) 

r$year <- parse_number(r$v)

r$v <- NULL

colnames(r)[2] <- "pctl"

###

library(data.table)
r <- data.table(r)

result <- r[, lapply(.SD, mean, na.rm = TRUE), by = .(var, pctl, city, year, clim_scen)] 

###

saveRDS(result, "results/scenarios/climate_change_provide_markups.rds")
