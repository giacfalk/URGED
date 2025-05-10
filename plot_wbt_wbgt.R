# Plot the outputs from 0_wbt_wbgt.R for analysis
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::group_by)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
library(tidyverse)

path_wbgt <- "results/wbgt/" # Path to the output file from the other script
wbgt_future <- readRDS(paste0(path_wbgt, "wbgt_future.rds")) # Load data set

df <- wbgt_future %>%
  mutate(delta_wbgt = wbgtmean_future - wbgt_mean)

list_cities <- df %>% select(city) %>% distinct()
dfplot <- df %>%
  filter(pctl == "pct55") %>%
  mutate(lcz = as.factor(lcz))

# 1) Plot deltas of WBGT for cities
for (i in list_cities$city) {
  ggplot(dfplot %>% filter(city == i),
         aes(x = month, y = delta_wbgt, color = lcz)) +
           geom_point(size = 0.1) +
    theme_minimal(base_size = 9) +
    facet_grid(clim_scen~year) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    ylim(-1.5, 1.5)
  outname <- paste0(path_wbgt, "plot_", as.character(i), "year_deltawbgt.png")
  ggsave(outname, width = 16.5, height = 9.5, units = "cm", dpi = 300, bg = "white")
}



# Make a test plot for wbt_stull and wbt_psychrolib
ggplot(wbgt_future,
       aes(x = wbtmean_future_psychrolib, y = wbtmean_future_stull, color = patm)) +
  theme_minimal(base_size = 9) +
  geom_abline(intercept = 0, slope = 1, color = "gray20") +
  geom_point(size = 0.01) +
  theme(legend.position = "bottom") +
  scale_color_continuous(type = "viridis") +
  labs(x = "WBT from ASHRAE / psychrolib", y = "WBT from Stull (2011) equation for 1013.25 hPa")
ggsave(paste0(path_out,"wbt_psychrolib-Stull.png"), width = 8.5, height = 9.5, units = "cm", dpi = 300, bg = "white")


ggplot(wbgt_future,
       aes(x = wbtmean_future_psychrolib_SLP, y = wbtmean_future_psychrolib, color = patm)) +
  theme_minimal(base_size = 9) +
  geom_abline(intercept = 0, slope = 1, color = "gray20") +
  geom_point(size = 0.01) +
  theme(legend.position = "bottom") +
  scale_color_continuous(type = "viridis") +
  labs(x = "WBT from ASHRAE / psychrolib for SLP", y = "WBT from ASHRAE / psychrolib with altitude")
ggsave(paste0(path_out, "wbt_psychrolib_sealevel-psychrolib.png"), width = 8.5, height = 9.5, units = "cm", dpi = 300, bg = "white")