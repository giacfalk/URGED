# Debug WBGT
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
library(tidyverse)
library(HeatStress) # For obtaining WBT
library(bigleaf) # For obtaining met. variables, including pressure by elevation and dew point
library(psychrolib) # Library for computing quantities from psychrometric charts

file_deltas <- "results/scenarios/climate_change_provide_markups.rds"
pathout <- "results/debug/deltas/"

deltas <- readRDS(file_deltas)

test_tas1 <- deltas %>%
  filter(var == "tas1")
test_hurs1 <- deltas %>%
  filter(var == "hurs1")


deltaslong <- deltas %>%
  pivot_longer(-c(var, pctl, city, year, clim_scen),
               names_to = "month",
               values_to = "value")
deltaslong <- deltaslong %>%
  mutate(month = factor(month, levels = unique(deltaslong$month)))
# months <- unique(deltaslong$month)
citylist <- unique(deltas$city)
varlist <- unique(deltas$var)
# Group the variables sets for plotting together
sets <- list(c("tas", "tasmin", "tasmax"), c("hurs"), c("sfcWind"), c("ts"))
# Find remaining variables (not in set1 or set2)
used_vars <- unlist(sets)
remaining <- setdiff(varlist, used_vars)
# Add remaining to the list of sets (if any)
if (length(remaining) > 0) {
  sets <- append(sets, list(remaining))
}
for(j in seq_along(sets)){
  current_set <- sets[[j]]
  cat("Processing set", j, ":", paste(current_set, collapse = ", "), "\n")
  for(i in citylist){
    print(sets[[j]])
    print(i)
    dfplot <- deltaslong %>%
      filter(city == i, pctl == "pct55", var %in% sets[[j]])
    ggplot(dfplot, aes(x = year, y = value, color = var, group = var)) +
      geom_line() +
      facet_grid(clim_scen~month) +
      labs(title = paste0(i, " ", sets[[j]], " 55th percentile"),
           x = "Month", y = "") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position = "bottom",
            legend.title = element_blank())
    outname <- paste0(pathout, i, "_", sets[[j]][1], "_55pctl.png")
    ggsave(outname,
           width = 20, height = 16, units = "cm",
           dpi = 300, bg = "white",)
  }
}


# Test what is in the "historical_wide" data.frame that is produced by the script 0_wbt_wbgt.R
historical_wide <- read_rds("results/wbgt/historical_wide.rds")
dfplot <- historical_wide %>%
  filter(city %in% c("Accra", "Phoenix"))
ggplot(dfplot, aes(x = month_num, y = rh)) +
  theme_minimal(base_size = 11) +
  geom_line() + geom_point() +
  facet_grid(lcz~city)
ggsave("results/debug/historical_rh.png",
       width = 20, height = 16, units = "cm",
       dpi = 300, bg = "white")

# Repeat the same for variable qv_mean
ggplot(dfplot, aes(x = month_num, y = qv_mean)) +
  theme_minimal(base_size = 11) +
  geom_line() + geom_point() +
  facet_grid(lcz~city) +
  labs(title = "Historical URBCLIM data, monthly average by LCZ")
ggsave("results/debug/historical_qvmean.png",
       width = 20, height = 16, units = "cm",
       dpi = 300, bg = "white")

# For Accra and Phoenix, plot T, WBGT, WBT-Stull
## WBT-Stull
ggplot(dfplot,
       aes(x = t_mean, y = rh, color = wbt_stull)) +
  theme_minimal(base_size = 11) +
  geom_point() +
  facet_grid(~city) +
  # Make a colorscale appropriate for heat stress
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange", "red"),
                        breaks = c(5, 10, 15, 20, 25, 30),
                        limits = c(5, 30),
                        name = "WBT") +
  theme(legend.position = "right") +
  labs(title = "WBT calculated using Stull (2011)")
ggsave("results/debug/Accra-Phoenix/historical_t+rh+wbt.png",
       width = 20, height = 16, units = "cm",
       dpi = 300, bg = "white")

## WBGT
ggplot(dfplot,
       aes(x = t_mean, y = rh, color = wbgt_mean)) +
  theme_minimal(base_size = 11) +
  geom_point() +
  facet_grid(~city) +
  # Make a colorscale appropriate for heat stress
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange", "red"),
                        breaks = c(5, 10, 15, 20, 25, 30),
                        limits = c(5, 30),
                        name = "WBGT") +
  theme(legend.position = "right") +
  labs(title = "WBT calculated using Stull (2011)")
ggsave("results/debug/Accra-Phoenix/historical_t+rh+wbgt.png",
       width = 20, height = 16, units = "cm",
       dpi = 300, bg = "white")

# Now do the same for the future values
wbgt_future <- read_rds("results/wbgt/wbgt_future.rds")
dfplotfuture <- wbgt_future %>%
  filter(city %in% c("Accra", "Phoenix")) %>%
  filter(pctl == "pct55") %>%
  filter(year == 2050)

## WBGT mean, color T2
ggplot(data = dfplotfuture, aes(color = t_mean)) +
  theme_minimal(base_size = 11) +
  # geom_point() +
  geom_point(aes(x = wbgt_mean, y = wbgt_max)) +
  geom_point(aes(x = wbgtmean_future, y = wbgtmax_future)) +
  geom_segment(aes(x = wbgt_mean, y = wbgt_max, xend = wbgtmean_future, yend = wbgtmax_future),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey40") +
  facet_grid(clim_scen~lcz~city, scales = "free") +
  # Make a colorscale appropriate for heat stress
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange", "red"),
                        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
                        limits = c(0, 40),
                        name = "T2m") +
  theme(legend.position = "right") +
  labs(title = "WBGT mean and max")
ggsave("results/debug/Accra-Phoenix/wbgt_future_colorT2.png",
       width = 50, height = 50, units = "cm",
       dpi = 300, bg = "white")

## WBGT mean, color RH
ggplot(data = dfplotfuture, aes(color = rh)) +
  theme_minimal(base_size = 11) +
  # geom_point() +
  geom_point(aes(x = wbgt_mean, y = wbgt_max)) +
  geom_point(aes(x = wbgtmean_future, y = wbgtmax_future)) +
  geom_segment(aes(x = wbgt_mean, y = wbgt_max, xend = wbgtmean_future, yend = wbgtmax_future),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey40") +
  facet_grid(clim_scen~lcz~city, scales = "free") +
  # Make a colorscale appropriate for heat stress
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange", "red"),
                        breaks = c(0,  10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme(legend.position = "right") +
  labs(title = "WBGT mean and max")
ggsave("results/debug/Accra-Phoenix/wbgt_future_colorhurs.png",
       width = 50, height = 50, units = "cm",
       dpi = 300, bg = "white")


# Calculate WBGT "manually" for the example of
## - Phoenix
## - ssp585
## - lcz == 9
## - pct == pctl55
## - year == 2050, month == 9

phoenix_manually <- wbgt_future %>%
  filter(city == "Phoenix", clim_scen == "ssp585", lcz == 9, pctl == "pct55", year == 2050, month_num == 9)

phoenix_manually <- phoenix_manually %>%
  mutate(t2new = t_mean + delta_tas,
         hursnew = rh + delta_hurs,
         tsnew = ts_mean + delta_ts) %>%
  mutate(wbtmeanfuture_test = wbt.Stull(t2new, hursnew),
         wbgtmeanfuture_test = 0.7 * wbtmeanfuture_test + 0.2 * tsnew + 0.1 * t2new) %>%
  mutate(t2maxnew = t_max + delta_tasmax,
         tsmaxnew = ts_max + delta_ts) %>%
  mutate(wbtmaxfuture_test = wbt.Stull(t2maxnew, hursnew),
         wbgtmaxfuture_test = 0.7 * wbtmaxfuture_test + 0.2 * tsmaxnew + 0.1 * t2maxnew) %>%
  mutate(t2minnew = t_min + delta_tasmin,
         tsminnew = ts_min + delta_ts) %>%
  mutate(wbtminfuture_test = wbt.Stull(t2minnew, hursnew),
         wbgtminfuture_test = 0.7 * wbtminfuture_test + 0.2 * tsminnew + 0.1 * t2minnew)

### wbtmeanfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)
### wbgtmeanfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)
### wbtmaxfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)
### wbtgmaxfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)
### wbtminfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)
### wbgtminfuture_test corresponds to the numbers from the code (0_wbt_wbgt.R)