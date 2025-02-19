# Visualize the relationship between GVI and other variables (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
# - City-Level Analysis
# - Visualize the results
# - map plots
# Libraries etc ----------------------------
rm(list=ls(all=TRUE)) # Removes all previously created variables 
library(tidyverse)
library(fixest)
library(sf)
library(pdftools)
# Working directory -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory

# Directories and settings ----------------------------
path_data <- "data_provide_cdh_gvi_143cities_withcovariates.rds"
path_results <- "results/figure_scatter/"

# Data Preparation ----------------------------
df <- read_rds(path_data) # The main data frame created in 1b_ugs_heat_metrics.R
df <- dplyr::filter(df, t > 0) # We remove t <= 0 as we use log(t) later on. It appears though that all t > 0
list_lczcity <- c("Compact high-rise", "Compact midrise", "Compact lowrise",
                  "Open high-rise", "Open midrise", "Open lowrise",
                  "Large lowrise", "Sparsely built", "Heavy ind") # List of urban morphology in cities. Lightweight lowrise ommitted as not very important

# Get the number of KG Classes:
Cls <-  df
Cls <- Cls %>%
  sf::st_drop_geometry(Cls) %>% # Drop geometry
  dplyr::select(Cls) %>% # Select the column with the Köppen-Geiger classes
  distinct() # Keep only unique values

# Make a dfplot variable
dfplot <- df %>%
  sf::st_drop_geometry(df) %>%
  mutate(lcz = factor(lcz, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, NA),
                      labels = c("Compact high-rise", "Compact midrise", "Compact lowrise",
                                 "Open high-rise", "Open midrise", "Open lowrise",
                                 "Lightweight lowrise", "Large lowrise", "Sparsely built",
                                 "Heavy ind", "Dense trees", "Scattered trees",
                                 "Bush, scrub", "Low plants", "Bare rock", "Bare soil",
                                 "Water"),
                      ordered = T)) %>%
  mutate(Cls = factor(Cls,
                      levels = c("A", "B", "C", "D"),
                      labels = c("Trop.", "Dry", "Temp.", "Cont."),
                      ordered = T))

# t: my_cooling_degree_hours_curpol
# t_max: my_urbclim_T2M_daily_mean_max_curpol
# t_min: my_urbclim_T2M_daily_mean_min_curpol

# Analysis 1: Cooling Degree Hours and daily min/max temperature ----------------------------
# T2max Grouped by KGC
ggplot(data = dfplot, aes(x = log(t), y = t_max, group = Cls)) +
  theme_minimal(base_size = 11) +
  geom_point(color = "grey20", size = 0.1) +
  geom_smooth(method = "lm", color = "grey50", linewidth = 0.2) +
  facet_wrap(~Cls, scales = "fixed") +
  labs(x = "Cooling Degree Hours (log)", y = "Mean of max air temperature (T2max)") +
  ylim(0, 35) +
  # xlim(0, 16) + 
  scale_color_discrete() +
  theme(legend.position = "none")
outname <- paste0(path_results, "CDH-T2max_facetsKGC.png")
ggsave(outname, width = 16, height = 14, units = "cm", bg = "white")

# T2max Grouped by LCZ
ggplot(data = dfplot, aes(x = log(t), y = t_max, color = Cls, group = Cls)) +
  theme_minimal(base_size = 11) +
  geom_point() +
  geom_smooth(method = "lm", color = "grey50", linewidth = 0.1) +
  facet_wrap(~lcz, scales = "fixed") +
  labs(x = "Cooling Degree Hours (log)", y = "Mean of max air temperature (T2max)") +
  scale_color_discrete() +
  ylim(0, 35)
outname <- paste0(path_results, "CDH-T2max_facetsLCZ.png")
ggsave(outname, width = 16, height = 16, units = "cm", bg = "white")

# T2min Grouped by KGC
ggplot(data = dfplot, aes(x = log(t), y = t_min, group = Cls)) +
  theme_minimal(base_size = 11) +
  geom_point(color = "grey20", size = 0.1) +
  geom_smooth(method = "lm", color = "grey50", linewidth = 0.2) +
  facet_wrap(~Cls, scales = "fixed") +
  labs(x = "Cooling Degree Hours (log)", y = "Mean of min air temperature (T2max)") +
  ylim(0, 35) +
  scale_color_discrete() +
  theme(legend.position = "none")
outname <- paste0(path_results, "CDH-T2min_facetsKGC.png")
ggsave(outname, width = 16, height = 14, units = "cm", bg = "white")
# T2min Grouped by LCZ
ggplot(data = dfplot, aes(x = log(t), y = t_min, color = Cls, group = Cls)) +
  theme_minimal(base_size = 11) +
  geom_point(color = "grey20", size = 0.1) +
  geom_smooth(method = "lm", color = "grey50", linewidth = 0.2) +
  facet_wrap(~lcz, scales = "fixed") +
  labs(x = "Cooling Degree Hours (log)", y = "Mean of min air temperature (T2max)") +
  scale_color_discrete() +
  ylim(0, 35)
outname <- paste0(path_results, "CDH-T2min_facetsLCZ.png")
ggsave(outname, width = 16, height = 16, units = "cm", bg = "white")

# Analysis 2: GVI and CDH
## Faceted by LCZ ----------------------------
### Compute a slope

ggplot(data = dfplot, aes(x = out_b_mean, y = log(t), color = Cls, group = Cls)) +
  theme_minimal(base_size = 9) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm", linewidth = 0.2, color = "grey20") +
  labs(x = "Green View Indez", y = "Cooling Degree Hours (log)") +
  scale_color_discrete() +
  # ylim(0,20) +
  facet_wrap(~lcz, scales = "fixed") +
  theme(legend.position = "bottom")
outname <- paste0(path_results, "GVI-CDH_facets-LCZ.png")
ggsave(outname, width = 16, height = 16, units = "cm", bg = "white")



## Faceted by KGC ----------------------------
dfplot2 <- dfplot %>%
  group_by(Cls) %>%
  mutate(a = coef(lm(log(t) ~ out_b_mean))[1],
         b = coef(lm(log(t) ~ out_b_mean))[2])
ggplot(data = dfplot2, aes(x = out_b_mean, y = log(t), color = Cls, group = Cls)) +
  theme_minimal(base_size = 9) +
  geom_point(size = 0.1) + geom_line(aes(y =a + b * out_b_mean)) + # If this line doesn't disappear behind the black line, there's a problem
  # Print the equation above the line
  geom_text(data = dfplot2%>%dplyr::select(a, b, Cls)%>%distinct(), aes(x = 0, y = 0, label = paste("y = ", round(a, 2), " + ", round(b, 2), "x")), hjust = 0, vjust = 0, size = 3.0, color = "grey20") +
  geom_smooth(method = "lm", linewidth = 0.2, color = "grey20") +
  labs(x = "Green View Indez", y = "Cooling Degree Hours (log)") +
  scale_color_discrete() +
  facet_wrap(~Cls, scales = "fixed") +
  theme(legend.position = "bottom")
outname <- paste0(path_results, "GVI-CDH_facets-KGC.png")
ggsave(outname, width = 16, height = 16, units = "cm", bg = "white")

# Analysis 3: Physical properties of urban morphology: ----------------------------
# How do building height, volumn, population density and LCZ relate to each other?
ggplot(data = dfplot %>% dplyr::filter(lcz %in% list_lczcity), aes(x = build_h, y = pop_dens, color = Cls, group = Cls)) +
  theme_minimal(base_size = 9) +
  geom_point(size = 0.1) +
  geom_smooth(method = "lm", linewidth = 0.2) +
  labs(x = "Building Height", y = "Population Density") +
  scale_color_discrete() +
  facet_wrap(~lcz, scales = "fixed") +
  theme(legend.position = "bottom")
outname <- paste0(path_results, "buildh-popdens_facets-LCZ.png")
ggsave(outname, width = 16, height = 16, units = "cm", bg = "white")
