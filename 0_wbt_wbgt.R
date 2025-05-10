# In this script, WBGT is approximated as a linear function of existing WBGT in the historical climate and the deltas for T, RH (hurs) and wind speed (sfcWind) from future climate scenarios
# Testing WBT and WBGT computations
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
# library(purrr)
library(tidyverse)
#devtools::install_github("anacv/HeatStress")
library(HeatStress) # For obtaining WBT
library(bigleaf) # For obtaining met. variables, including pressure by elevation and dew point
library(psychrolib) # Library for computing quantities from psychrometric charts
SetUnitSystem("SI") # For using SI units in psychrolib
library(sf) # For obtaining elevation from GHS
# Source helper files and functions ---------------------------------------
source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found

file_deltas <- "results/scenarios/climate_change_provide_markups.rds"
path_out <- "results/wbgt/"

# Function to approximate RH from Q
sh2rh <- function(Q2, T2, ps){
  # calculate relative humidity from specific humidity (Q)
  # input temperature [K], specific humidity [kg/kg], air pressure [hPa]
  # output relative humidity [%]
  rh = 26.3 * ps * Q2 / exp(17.67 * (T2 - 273.15) / (T2 - 29.65))
}

# Get historical mean WBGT, T and RH per month
# Check whether output data frame exist
# if(file.exists(file_df)) {
#   df <- read_rds(file_df)
#   print("Loaded existing file.")
#   } else {
    print("Running loops")
    ## These files are .csv files found in "results/URBCLIM_historical/wbgt_" for each city
    ## Write a loop that loads these files and binds them together
    #####################
    # Get WBGT
    print("Loading historical mean WBGT data")
    dftemp <- tibble() # Write an empty tibble
    files <- list.files("results/URBCLIM_historical/wbgt_", pattern = "wbgt_stats_mean", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp,
                                   read_csv(i, progress = F, show_col_types = FALSE)) %>%
        dplyr::mutate(var = "wbgt_mean")
    }
    historical_wbgtmean <- dftemp
    
    print("Loading historical minimum WBGT data")
    dftemp <- tibble() # Write an empty tibble
    files <- list.files("results/URBCLIM_historical/wbgt_", pattern = "wbgt_stats_min", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp,
                          read_csv(i, progress = F, show_col_types = FALSE)) %>%
        dplyr::mutate(var = "wbgt_min")
    }
    historical_wbgtmin <- dftemp
    
    print("Loading historical maximum WBGT data")
    dftemp <- tibble() # Write an empty tibble
    files <- list.files("results/URBCLIM_historical/wbgt_", pattern = "wbgt_stats_max", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp,
                          read_csv(i, progress = F, show_col_types = FALSE)) %>%
        dplyr::mutate(var = "wbgt_max")
    }
    historical_wbgtmax <- dftemp
    
    #####################
    # Get air temperature
    print("Loading historical mean air temperature data")
    dftemp <- tibble() # Write an empty tibble
    files <- list.files("results/URBCLIM_historical/t_", pattern = "_stats_mean", full.names = T)
    for (i in files) {
      print(i)
      dftemp <- bind_rows(dftemp,
                                read_csv(i, progress = F, show_col_types = FALSE)) %>%
        dplyr::mutate(var = "t_mean")
    }

    historical_tmean <- dftemp
    
    print("Loading historical minimum air temperature data")
    dftemp <- tibble() # Write an empty tibble
    # Get all files in that directory
    files <- list.files("results/URBCLIM_historical/t_", pattern = "_stats_min", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "t_min")
    }
    historical_tmin <- dftemp
    
    print("Loading historical maximum air temperature data")
    dftemp <- tibble() # Write an empty tibble
    # Get all files in that directory
    files <- list.files("results/URBCLIM_historical/t_", pattern = "_stats_max", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "t_max")
    }
    historical_tmax <- dftemp
    
    # Get specific humidity
    dftemp <- tibble() # Write an empty tibble
    files <- list.files("results/URBCLIM_historical/qv_", pattern = "_stats_mean", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "qv_mean")
    }
    historical_qvmean <- dftemp
    
    #####################
    # Get surface temperature
    dftemp <- tibble() # Write an empty tibble
    # Get all files in that directory
    files <- list.files("results/URBCLIM_historical/lst_", pattern = "_stats_mean", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "ts_mean")
    }
    historical_tsmean <- dftemp
    
    dftemp <- tibble() # Write an empty tibble
    # Get all files in that directory
    files <- list.files("results/URBCLIM_historical/lst_", pattern = "_stats_min", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "ts_min")
    }
    historical_tsmin <- dftemp
    
    dftemp <- tibble() # Write an empty tibble
    # Get all files in that directory
    files <- list.files("results/URBCLIM_historical/lst_", pattern = "_stats_max", full.names = T)
    for (i in files) {
      dftemp <- bind_rows(dftemp, read_csv(i, progress = F)) %>%
        dplyr::mutate(var = "ts_max")
    }
    historical_tsmax <- dftemp
    
    # Merge all data frames
    historical <- bind_rows(historical_wbgtmean, historical_wbgtmin, historical_wbgtmax, historical_tmean, historical_tmin, historical_tmax, historical_tsmean, historical_tsmin, historical_tsmax, historical_qvmean)
    historical_wide <- historical %>%
      dplyr::mutate(month_num = variable) %>% # Change month labeling
      select(-variable, -`...1`) %>% # Make `historical` wide
      pivot_wider(names_from = var, values_from = value)
    
    # Add wbt according to rough estimate
    ## First we need to add the elevation of cities. We can get this directly from GHS.
    ## The variable is EL_AV_ALS, and expressed in metres
    ghs <- read_sf(paste0("boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg"))
    ghs <- sf::st_drop_geometry(ghs)
    cities_provide <- read_csv("climate/provide_urban_climate_data/cities_provide.csv")
    elevation <- read_csv("cities/cities_urged+GHS.csv")
    elevation <- elevation %>%
      select(ID_HDC_G0, city, EL_AV_ALS.x) %>% 
      distinct()
    
    cities_historical <- historical_wide %>% select(city) %>% distinct()
    # write_csv(cities_historical, "cities/cities_historical.csv")
    
    historical_wide <- merge(historical_wide, elevation, by = "city", all.x = TRUE)
    
    ## Then we calculate pressure assuming a standard atmosphere
    
    historical_wide <- historical_wide %>%
      dplyr::mutate(patm = bigleaf::pressure.from.elevation(EL_AV_ALS.x, 273.15) * 10) %>%
      dplyr::mutate(rh = sh2rh(Q2 = qv_mean, T2 = t_mean + 273.15, ps = patm)) %>%
      dplyr::mutate(wbt_stull = HeatStress::wbt.Stull(tas = t_mean, hurs = rh))  # Wet bulb temperature using Stull (2011), valid for p = 1013.25 hpa.
    # Output historical_wide
    write_rds(historical_wide, paste0(path_out, "historical_wide.rds"), compress = "none")
    write_csv(historical_wide, paste0(path_out, "historical_wide.csv"))
    
    # Get Deltas ("markups") for the future years -----------
    deltas <- readRDS(file_deltas)
    # variables <- markups %>% select(var) %>% distinct()
    # Markups needs to be transformed into long format
    deltas <- deltas %>%
      filter(var %in% c("tas", "tasmax", "tasmin",
                        "ts",
                        "hurs")) %>% # Only keep the relevant variables
      dplyr::mutate(var_delta = var) %>%
      select(-var) %>%
      filter(pctl %in% c("pct55", "pct45")) %>% # Only keep pct45 and pct55
      pivot_longer(cols = -c("var_delta", "pctl", "city", "year", "clim_scen"),
                   names_to = "month",
                   values_to = "value_delta") %>%
      dplyr::mutate(month_num = match(month, month.abb))
    
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
    
    nnn <- nearest_word_match(unique(deltas$city), unique(historical_wide$city))

    nnn$closest_match[94] <- "Newcastle"
    nnn$closest_match[3] <- "Alicante"
    nnn$closest_match[53] <- "Islamabad"
    nnn$closest_match[133] <- "Tschwane"
    nnn$closest_match[75] <- "Malaga"
    nnn$closest_match[92] <- "Nassau"
    nnn$closest_match[111] <- "Rotterdam [The Hague]"
    
    nnn$distance <- NULL
    colnames(nnn) <- c("city", "city_provide")
    
    deltas <- merge(deltas, nnn, by.x="city", by.y="city")

    ###
    
    df <- merge(historical_wide, deltas, by.x = c("city", "month_num"), by.y = c("city_provide", "month_num"))
    
    df <- na.omit(df)
    
    # # Save to file:
    # write_rds(df, file_df, compress = "none")
  # }

# Now, make this data.frame wider again, so that we can make calculations with the variables. This means bringing the values for var tas, hurs, ts into their own columns
dfwide <- df %>%
  pivot_wider(names_from = var_delta, values_from = value_delta) %>%
  dplyr::mutate(delta_tas = tas,
         delta_tasmin = tasmin,
         delta_tasmax = tasmax,
         delta_ts = ts,
         delta_hurs = hurs) %>%
  select(-tas, -tasmin, -tasmax, -ts, -hurs)

dfwide$rh <- ifelse(dfwide$rh>100, 100, dfwide$rh)

wbgt_future <- dfwide %>%
  drop_na(t_mean, rh, patm) %>%
  dplyr::mutate(wbtmean_future_stull = HeatStress::wbt.Stull(tas = t_mean + delta_tas, hurs = rh + delta_hurs)) %>%
  dplyr::mutate(wbtmin_future_stull = HeatStress::wbt.Stull(tas = t_min + delta_tasmin, hurs = rh + delta_hurs)) %>%
  dplyr::mutate(wbtmax_future_stull = HeatStress::wbt.Stull(tas = t_max + delta_tasmax, hurs = rh + delta_hurs)) %>%
  dplyr::mutate(wbtmean_future_psychrolib = psychrolib::GetTWetBulbFromRelHum(t_mean, rh*0.01, patm*1e2),
         wbtmean_future_psychrolib_SLP = psychrolib::GetTWetBulbFromRelHum(t_mean, rh*0.01, 1013.25*1e2))

wbgt_future <- wbgt_future %>%
  dplyr::mutate(wbgtmean_future = 0.7 * wbtmean_future_stull + 0.2 * (ts_mean + delta_ts) + 0.1 * (t_mean + delta_tas),
         wbgtmin_future = 0.7 * wbtmin_future_stull + 0.2 * (ts_min + delta_ts) + 0.1 * (t_min + delta_tasmin),
         wbgtmax_future = 0.7 * wbtmax_future_stull + 0.2 * (ts_max + delta_ts) + 0.1 * (t_max + delta_tasmax))

write_rds(wbgt_future, paste0(path_out, "wbgt_future.rds"), compress = "none")
write_csv(wbgt_future, paste0(path_out, "wbgt_future.csv"))

test <- wbgt_future %>%
  filter(city == "Accra", month_num == 6) %>%
  dplyr::arrange(month_num, year)
