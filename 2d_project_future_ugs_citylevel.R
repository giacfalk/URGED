# Develop scenarios for urban green and associated heat mitigation potential
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
library(tidyverse)
library(sf)

# Source helper files and functions ---------------------------------------
source("URGED/fcts_labelers_colors.R")
source("URGED/fcts_helpers_debug.R")
source("URGED/fct_scenarios.R") # Here the "boosting" function can be found

# Variables
slopfac1 = 0.50 # Slope factor for the envelope of x% more growth than historically
fac2050a = 0.1 # Make GVI 25% larger than current value by 2050
fac2050b = 0.25 # Make GVI 50% larger than current value by 2050

list_samplecities <- c("Abidjan", "Auckland", "Berlin", "Cape Town", "Guadalajara", "Hanoi", "Hamburg", "Rome", "Miami", "Singapore", "Vienna", "Yaounde")

# Directories and settings ----------------------------
## Input
path_data <- "data_provide_cdh_gvi_143cities_withcovariates.rds"
path_cities_provide <- paste0("climate/provide_urban_climate_data/cities_provide.csv")
path_ugs <- "ugs/after_points_030624.RData" # The input used in 1b_ugs_heat_metrics.R
path_ugs_citynames <- "ugs/after_points_030624_citynames.rds" # Citynames added in 1b_..
path_highestobs <- "ugs/dfhighestobs.rds"
## Output
path_results <- "results/scenarios/"
file_out_df <- paste0(path_results, "dfscenarios.rds")

# Code ########################################################################################

# Pre-process data and add city names -------------------------------------
# load(path_ugs) # year-to-year data of UGS. Var: **out_ndvi_m**
ugs <- read_rds(path_ugs_citynames)
# testcitynames <- ugs %>%
#   sf::st_drop_geometry() %>%
#   dplyr::select(city, country) %>%
#   distinct()
ugs <- ugs %>% dplyr::filter(city != "N/A") # Drop one row with a bad city name

# First assess trends of GVI per city ---------------------------------------
## Is there a clear trend visible?
# provide <- read_rds(path_data) # The main data frame created in 1b_ugs_heat_metrics.R
# cities <- provide %>% sf::st_drop_geometry() %>% dplyr::select(city) %>% distinct()

## Summarize by city and year ---------------------------------------
df_hist <- ugs %>%
  mutate(pid = paste(x, y, sep = "-")) %>% # Make a point-id for each c coordinate
  group_by(city, country, year) %>%
  summarise(npid = n(),
            out_b_meany = mean(out_b, na.rm=T),
            out_b_miny = min(out_b, na.rm=T),
            out_b_maxy = max(out_b, na.rm=T))


# Regression trendline for each city ----------------------------
## Make long with respect to the stats value
df_hist <- df_hist %>%
  pivot_longer(cols = c(out_b_meany, out_b_miny, out_b_maxy),
               names_to = "stats", values_to = "GVI") %>%
  mutate(stats = case_when(stats == "out_b_meany" ~ "mean",
                           stats == "out_b_miny" ~ "min",
                           stats == "out_b_maxy" ~ "max"))
## Regression
df_hist <- df_hist %>%
  mutate(year = as.numeric(year)) %>% # Make year numeric so that it can be used as a var for regression
  group_by(city, country, stats) %>%
  mutate(
    model = list(lm(GVI ~ year)),
    slope = coef(model[[1]])["year"],     # Extracts slope of `year`
    intercept = coef(model[[1]])["(Intercept)"]
  ) %>%
  ungroup() %>%
  mutate(type = "Data")

## Plot to see all the regressions / trends ---------------------------------------
dflabel <- df_hist %>%
  dplyr::filter(year == 2016 | year == 2023)
p <- ggplot(df_hist, aes(x = year, y = GVI, color = stats)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = dflabel, aes(label = stats), hjust = 0, vjust = 0) +
  facet_wrap(~city) +
  theme_minimal() +
  theme(legend.position = "none")
outname <- paste0(path_results, "ugstrends_all_cities_regression.png")
ggsave(plot = p, filename = outname, width = 30, height = 60, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Now make a regression for the scenarios based on 2016 - 2023 for mean(GVI) ---------------------------------------
future_years <- seq(2025, 2050, by = 5)
df_fut <- df_hist %>%
  dplyr::filter(stats == "mean",
                year == 2016) %>%
  dplyr::select(-npid, -model, -year, -GVI) %>%
  expand_grid(year = future_years, .) %>% # Expand to future years
  mutate(GVI = intercept + slope * year,
         GVI_upr = intercept + slope * 2023 + slope * (1 + slopfac1) * (year - 2023),
         GVI_lwr = intercept + slope * 2023 + slope * (1 - slopfac1) * (year - 2023),
         type = "Regression")

# # Merge with original dataframe
df <- df_hist %>%
  dplyr::select(-npid, -model) %>%
  bind_rows(df_fut) %>%
  dplyr::filter(stats == "mean")

# Plot for all cities ---------------------------------------
ggplot(df, aes(x = year, y = GVI)) +
  geom_point(data = df %>% dplyr::filter(type == "Data"), color = "black") +
  geom_line(data = df %>% dplyr::filter(type == "Regression"), color = "grey30") +
  facet_wrap(~city) +
  theme_minimal() +
  theme(legend.position = "none")
outname <- paste0(path_results, "ugstrends_all_cities_future.png")
ggsave(filename = outname, width = 30, height = 90, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Plot for a number of selected cities ---------------------------------------
dfplot <- df %>%
  dplyr::filter(city %in% list_samplecities)
ggplot(dfplot, aes(x = year, y = GVI, color = type)) +
  geom_point(data = dfplot %>% dplyr::filter(type == "Data")) +
  geom_ribbon(data = dfplot %>% dplyr::filter(type == "Regression"),
              aes(ymin = GVI_lwr, ymax = GVI_upr), alpha = 0.2, linewidth = 0) +
  geom_line(data = dfplot %>% dplyr::filter(type == "Regression")) +
  facet_wrap(~city) +
  theme_minimal(base_size = 11) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 60)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_color_manual(values = c("grey10", "grey30")) +
  theme(legend.position = "bottom")
outname <- paste0(path_results, "ugstrends_selectcities_future_slopfac", slopfac1, ".png")
ggsave(filename = outname, width = 16, height = 16, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Create a "growth envelope" of +/- 25% of observed growth rate. Or SD? ----------------------------
## First calculate the most recent ("latest") values for GVI
dflatest <- df_hist %>%
  dplyr::filter(stats == "mean") %>%
  group_by(city, country) %>%
  mutate(GVImax = max(GVI, na.rm = T),
         GVImin = min(GVI, na.rm = T),
         GVIm = mean(GVI, na.rm = T)) %>%
  dplyr::filter(year == max(year)) %>%
  mutate(GVIlatest = GVI,
         yearlatest = year) %>%
  dplyr::select(city, yearlatest, GVIlatest, GVImin, GVImax, GVIm) %>%
  distinct()%>%
  ungroup()
## No produce the envelope
dfenvelope <- merge(df, dflatest, by = c("city", "country"), all.x = T) %>%
  arrange(city, year)
df_scenarios1 <- dfenvelope %>%
  dplyr::filter(year > 2023) %>% # Only future years
  mutate(type = "Envelope") %>%
  # GVI@2025 +/- 33% and +/- 66% above current mean values
  mutate(GVI_proja_upr = GVIm * (1 + fac2050a * (year - yearlatest)/(2050 - yearlatest)),
         GVI_proja_lwr = GVIm * (1 - fac2050a * (year - yearlatest)/(2050 - yearlatest)), 
         GVI_projb_upr = GVIm * (1 + fac2050b * (year - yearlatest)/(2050 - yearlatest)),
         GVI_projb_lwr = GVIm * (1 - fac2050b * (year - yearlatest)/(2050 - yearlatest))
         )
## Add "fancy" slope (the amplified and damped version of the slope)
df_scenarios2 <- dfenvelope %>%
  dplyr::filter(year > 2023) %>%
  mutate(type = "Regression*") %>%
  mutate(
    slopefancy = fct_slope_a(slope), # The function fct_slope_a() can be found in fct_scenarios.R
    # slopefancy = .5 - 1 / (1 + exp(slope*10)),
    GVI = intercept + slope * yearlatest + slopefancy * (year - yearlatest)
  )
    
df_scenarios3 <- df_scenarios2 %>%
  mutate(type = "Regression*-Env.",
         GVI_fancya_upr = GVIm * (1 + (slopefancy + fac2050a) * (year - yearlatest)/(2050 - yearlatest)),
         GVI_fancya_lwr = GVIm * (1 - (-slopefancy + fac2050a) * (year - yearlatest)/(2050 - yearlatest)),
         GVI_fancyb_upr = GVIm * (1 + (slopefancy + fac2050b) * (year - yearlatest)/(2050 - yearlatest)),
         GVI_fancyb_lwr = GVIm * (1 - (-slopefancy + fac2050b) * (year - yearlatest)/(2050 - yearlatest))
  )


# Merge everything together
dfscenarios <- bind_rows(df_scenarios1, df_scenarios2, df_scenarios3)
## Add the restraining by Clsmain, LCZ
dfrestrained <- merge(dfscenarios, dfhighe)



# Make dfplot
dfplot <- bind_rows(df, dfscenarios)
# OUTPUT: Write to file
write_rds(dfplot, file_out_df)
dfplotselect <- dfplot %>%
  dplyr::filter(city %in% list_samplecities)

# Plot envelope for scenarios factors a, b ----------------------------
ggplot(dfplotselect, aes(x = year, y = GVI, color = type)) +
  geom_point(data = dfplotselect %>% dplyr::filter(type == "Data"), color = "grey10") +
  geom_ribbon(data = dfplotselect %>% dplyr::filter(year >= yearlatest), aes(ymin = GVI_projb_lwr, ymax = GVI_projb_upr), alpha = 0.8, fill = "grey70", linewidth = 0) +
  geom_ribbon(data = dfplotselect %>% dplyr::filter(year >= yearlatest), aes(ymin = GVI_proja_lwr, ymax = GVI_proja_upr), alpha = 0.8, fill = "grey30", linewidth = 0) +
  facet_wrap(~city) +
  # labs(title = paste0("GVI growth scenarios, " as.character(100*fac2050a), "s")) +
  theme_minimal(base_size = 13) +
  theme(axis.title.x = element_blank()) +
  # scale_y_continuous(limits = c(0, 40)) +
  theme(legend.position = "none")
outname <- paste0(path_results, "ugstrends_selectcities_faca", fac2050a, "_facb", fac2050b, "_mean.png")
ggsave(filename = outname, width = 16, height = 16, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Plot the version with cone and fancy slope for selected cities  ----------------------------
## Define colorscale:
list_colors_regression <- c("grey20", "grey20", "steelblue", "steelblue")
list_fill_regression <- c("steelblue")

ggplot(dfplotselect, aes(x = year, y = GVI, color = type)) +
  geom_ribbon(data = dfplotselect %>% dplyr::filter(type == "Regression*-Env."), aes(ymin = GVI_fancya_lwr, ymax = GVI_fancya_upr), alpha = 0.7, fill = "steelblue", linewidth = 0) +
  geom_ribbon(data = dfplotselect %>% dplyr::filter(type == "Regression*-Env."), aes(ymin = GVI_fancyb_lwr, ymax = GVI_fancyb_upr), alpha = 0.3, fill = "steelblue", linewidth = 0) +
  geom_point(data = dfplotselect %>% dplyr::filter(type == "Data")) +
  geom_line(data = dfplotselect %>% dplyr::filter(type %in% c("Regression", "Regression*"))) +
  facet_wrap(~city) +
  # labs(title = paste0("GVI growth scenarios, " as.character(100*fac2050a), "s")) +
  theme_minimal(base_size = 13) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 0)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_continuous(breaks = c(2020, 2035, 2050)) +
  scale_color_manual(values = list_colors_regression) +
  scale_fill_manual(values = list_fill_regression) +
  theme(legend.position = "bottom", legend.title = element_blank())
outname <- paste0(path_results, "ugstrends_selectcities_fancyslope", fac2050a, "_facb", fac2050b, "_mean.png")
ggsave(filename = outname, width = 16, height = 16, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

## Plot the version with cone and fancy slope for ALL CITIES  ----------------------------
ggplot(dfplot, aes(x = year, y = GVI, color = type)) +
  geom_ribbon(data = dfplot %>% dplyr::filter(type == "Regression*-Env."), aes(ymin = GVI_fancya_lwr, ymax = GVI_fancya_upr), alpha = 0.7, fill = "steelblue", linewidth = 0) +
  geom_ribbon(data = dfplot %>% dplyr::filter(type == "Regression*-Env."), aes(ymin = GVI_fancyb_lwr, ymax = GVI_fancyb_upr), alpha = 0.3, fill = "steelblue", linewidth = 0) +
  geom_point(data = dfplot %>% dplyr::filter(type == "Data")) +
  geom_line(data = dfplot %>% dplyr::filter(type %in% c("Regression", "Regression*"))) +
  facet_wrap(~city) +
  # labs(title = paste0("GVI growth scenarios, " as.character(100*fac2050a), "s")) +
  theme_minimal(base_size = 13) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 0)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_continuous(breaks = c(2020, 2035, 2050)) +
  scale_color_manual(values = list_colors_regression) +
  scale_fill_manual(values = list_fill_regression) +
  theme(legend.position = "bottom", legend.title = element_blank())
outname <- paste0(path_results, "ugstrends_selectcities_fancyslope", fac2050a, "_facb", fac2050b, "_mean_ALL.png")
ggsave(filename = outname, width = 30, height = 90, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)