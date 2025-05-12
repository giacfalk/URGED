# Develop scenarios for urban green and associated heat mitigation potential.
# Here, use the point estimates produced and cluster them by city, country, lcz, kgz.
rm(list=ls(all=TRUE)) # Removes all previously created variables 
# Working directory [RStudio] -------------------------------------------------------
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set work directory to current file location
setwd('..') # Move one up
stub0 <- paste0(getwd(), "/") # Base working directory
# Libraries etc ----------------------------
library(conflicted)
conflicts_prefer(dplyr::filter) # Use the filter command from dplyr, even if stats is loaded
library(tidyverse)
# library(sf)

# Source helper files and functions ---------------------------------------
source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found
# Directories and settings ----------------------------
## Input
path_ugs_complete <- "ugs/after_points_100425_completedatabase.rds" # Citynames added in 1b_..
path_highestobs <- "ugs/dfhighestobs.rds" # Highest observations and 10% / 90% percentiles as made in '2e_ugs_frontrunner_cities.R'
## Output
path_results <- "results/scenarios/"
file_out_df <- paste0(path_results, "dfscenarios_pointlevel.rds")


# Code #
# Pre-process data and add city names -------------------------------------
ugs <- read_rds(path_ugs_complete)
ugs <- ugs %>% 
  filter(city != "N/A") %>% # Drop one row with a bad city name
  filter(lcz_filter_v3 <= 10) %>% # Only keep urban form classes that are urban
  select(-id) # Delete the `id` column that wasn't present in the 030624 dataset.

# First determine highest and lowest observations ---------------------------
dffrontrunners <- ugs %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%   # Filter the land cover classes that are not urban (i.e. lcz_filter_v3 <= 10)
  dplyr::filter(lcz_filter_v3 != 7) %>% # Remove the lightweight low-rise class, which is an outlier (only a few informal settlement data points in Lagos)
  group_by(lcz_filter_v3, Cls_short) %>%
  arrange(desc(out_b), .by_group = TRUE) %>% # Sort within each group by value in descending order
  mutate( # Create the 10th and 90th percentile entries
    boundlwr = stats::quantile(out_b, prob=c(.1,.5,.9))[[1]],
    boundlwr_type = "10pct",
    boundupr = stats::quantile(out_b, prob=c(.1,.5,.9))[[3]],
    boundupr_type = "90pct",) %>%
  ungroup() %>% # Ungroup to return a standard data frame
  dplyr::select(-ID, -out_b, -x, -y, -year) %>%
  select(-country, -CTR_MN_ISO,-GRGN_L1,-GRGN_L2,-UC_NM_LST, EL_AV_ALS) %>%
  distinct()

dfhighestobs <- dffrontrunners %>%
  select(Cls_short, lcz_filter_v3, starts_with("bound")) %>%
  distinct()

# Save to file
saveRDS(dfhighestobs, file = paste0("ugs/dfhighestobs.rds"))

# First get share of LCZ and KGZ per city
## Create spatial averages, but still organized per year.
dfspat <- ugs %>%
  group_by(city, country, lcz_filter_v3, Cls_short, year) %>%
  mutate(npid_s = n(),
            out_b_mean_s = mean(out_b, na.rm=T),
            out_b_median_s = median(out_b, na.rm=T),
            out_b_min_s = min(out_b, na.rm=T),
            out_b_max_s = max(out_b, na.rm=T)) %>%
  dplyr::select(-out_b, -ID, -x, -y) %>%
  distinct() %>%
  ungroup() %>%
  mutate(yearlatest = max(year))

# Add the share of each lcz, making use of the npid... column
dfspat <- dfspat %>%
  group_by(city, country, year) %>%
  mutate(sumid_s = sum(npid_s)) %>%
  group_by(city, country, year, lcz_filter_v3) %>%
  mutate(lczshare_s = npid_s / sumid_s) %>%
  ungroup()

# Create spatial *and* temporal averages, i.e. one number per city, country, LCZ, Cls_short. One value for all years 2016-2023.
dfspattemp <- ugs %>%
  group_by(city, country, lcz_filter_v3, Cls_short) %>%
  # First get regression coefficients by LCZ
  mutate(
    model = list(lm(out_b ~ year)),
    slope = coef(model[[1]])["year"],     # Extracts slope of `year`
    intercept = coef(model[[1]])["(Intercept)"]
  ) %>%
  dplyr::select(-model) %>% # Get rid of the model column, as this makes the code crash
  mutate(npid_st = n() / (max(year) - min(year) +  1),
         out_b_mean_st = mean(out_b, na.rm=T),
         out_b_median_st = median(out_b, na.rm=T),
         out_b_min_st = min(out_b, na.rm=T),
         out_b_max_st = max(out_b, na.rm=T),
         out_b_quart_upr = quantile(out_b, prob=c(.25,.5,.75))[[3]], # Define 75th percentile
         out_b_quart_lwr = quantile(out_b, prob=c(.25,.5,.75))[[1]] # Define 25th percentile
         ) %>%
  ungroup() %>%
  dplyr::select(-out_b, -ID, -x, -y, -year, -EL_AV_ALS) %>%
  distinct()

# test <- c(13,34,53,23,73,25,45) # For debugging quartiles
# quantile(test, prob=c(.25,.5,.75))[[3]] # For debugging quartiles

dfspattemp <- dfspattemp %>%
  group_by(city, country) %>%
  mutate(sumid_st = sum(npid_st)) %>%
  group_by(city, country, lcz_filter_v3) %>%
  mutate(lczshare_st = npid_st / sumid_st) %>%
  ungroup()

# _s stands for spatial average, _st stands for spatial and temporal average
# For large datasets, we need to split the data.frame into two smaller chunks to make the merging work
citylist <- dfspat$city %>%
  unique() %>%
  sort()

dftemp <- merge(dfspat, dfspattemp,
                 by = c("city", "country", "lcz_filter_v3", "Cls_short", "Cls", "ID_HDC_G0", "CTR_MN_ISO", "GRGN_L1", "GRGN_L2", "UC_NM_LST"), all = T)

# Merge with the "highest and lowest observed" dataset created earlier.
df <- merge(dftemp, dfhighestobs, by = c("Cls_short", "lcz_filter_v3"), all.x = T)

# The data.frame df now contains the GVI values on spatial average, as well as spatio-temporal average. Classified by LCZ and Cls_short.
# Now get make scenarios with 25% and 50% more grwoth by 2050, in analogy to the code in 2e
future_years <- seq(2025, 2050, by = 5)

## Based on the spat_temp averages
dffuture <- df %>%
  dplyr::select(-year, -npid_s, -sumid_s, -lczshare_s, -out_b_mean_s, -out_b_median_s, -out_b_min_s, -out_b_max_s) %>%
  distinct() %>%
  expand_grid(year = future_years, .) %>%
  mutate(ugs_ref = out_b_median_st) %>%
  # Here, compute the scenarios of a) reference with climate impacts, b) moderate ambition, c) high ambition
  mutate(ugs_scen_impacted =
           ugs_ref + (out_b_quart_lwr - ugs_ref) * (year - 2020)/(2050 - 2020),
         ugs_scen_mod =
           ugs_ref + (out_b_quart_upr - ugs_ref) * (year - 2020)/(2050 - 2020),
         ugs_scen_hgh =
           ugs_ref + (boundupr - ugs_ref) * (year - 2020)/(2050 - 2020)
         )

# Constrain the projected values by the observed lower and upper values
## (Merged earlier above in the code)
dffuture <- dffuture %>%
  mutate(hitboundsa = ifelse(ugs_scen_mod > boundupr, "Yes", "No"),
         hitboundsb = ifelse(ugs_scen_hgh > boundupr, "Yes", "No")) %>%
  mutate(ugs_scen_mod =
           ifelse(hitboundsa == "Yes", boundupr, ugs_scen_mod),
         ugs_scen_hgh =
           ifelse(hitboundsb == "Yes", boundupr, ugs_scen_hgh),
         )

# Merge the future data with the historic data in df
dfscen <- merge(dffuture, df, all = T)

write_rds(dfscen, "results/scenarios/dfscen_pointlevel.rds")


################
# Some plotting
# Plot the evolution of the scenarios for four sample cities
dfplot <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10, city %in% list_samplecities) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3))
dfplot <- dfplot %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  # Rename "GVI_proj_a" to "upper scenario"
  mutate(scen = factor(scen,
                       levels = c("ugs_scen_impacted", "ugs_scen_mod", "ugs_scen_hgh"),
                       labels = c("Climate impacts", "Moderate ambition", "High ambition")))

# Create a data.frame to show the observed variation in the historical data. It is easier to display this from a new data.frame, and not using dfplot2.
dfplot_histav <- dfplot %>%
  dplyr::filter(year == 2020) %>%
  select(city, year, scen, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.1)

ggplot(data = dfplot,
       aes(x = year,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           linetype = scen)) +
  geom_line(aes(y = GVI), alpha = 0.5, linewidth = 0.65) +
  # # Optional: Use geom_segment to show arrows
  # geom_segment(data = arrow_data_a,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #   arrow = arrow(length = unit(0.2, "cm")),
  #   show.legend = FALSE) +
  # geom_segment(data = arrow_data_b,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              show.legend = FALSE) +
  geom_point(data = dfplot %>% filter(year %in% c(2025, 2050)),
             aes(x = year, y = GVI), shape = 1) +
  geom_point(data = dfplot_histav,
             aes(x = year_jittered, y = out_b_mean_st), shape = 5) +
  geom_errorbar(data = dfplot_histav,
                aes(x = year_jittered,
                    ymin = out_b_quart_lwr,
                    ymax = out_b_quart_upr), width = 0.5) +
  # geom_point(aes(y = GVI[length(GVI)-1])) +
  theme_minimal(base_size = 9) +
  facet_wrap(~city) +
  theme_minimal() +
  ylab("Street Green Space (GVI)") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c("dotted", "solid", "twodash")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )
outname <- paste0(path_results, "bylcz_simplenevelope_samplecities.png")
ggsave(filename = outname, width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)
ggsave(filename = "~/Library/CloudStorage/Dropbox/Apps/Overleaf/URGED_papers/figures/GVI-scenarios/bylcz_simplenevelope_samplecities.png", width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300, limitsize = FALSE) # Also safe for use in Overleaf

###############
# Show the same for ALL CITIES
dfplot_all <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3))
dfplot_all <- dfplot_all %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  # Rename "GVI_proj_a" to "upper scenario"
  mutate(scen = factor(scen,
                       levels = c("ugs_scen_impacted", "ugs_scen_mod", "ugs_scen_hgh"),
                       labels = c("Climate impacts", "Moderate ambition", "High ambition")))

# Create a data.frame to show the observed variation in the historical data. It is easier to display this from a new data.frame, and not using dfplot2.
dfplot_histav_all <- dfplot_all %>%
  dplyr::filter(year == 2020) %>%
  select(city, year, scen, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.1)

# Show the development of GVI in each LCZ for the sample cities.
ggplot(data = dfplot_all,
       aes(x = year,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           linetype = scen)) +
  geom_line(aes(y = GVI), alpha = 0.5, linewidth = 0.65) +
  # # Optional: Use geom_segment to show arrows
  # geom_segment(data = arrow_data_a,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #   arrow = arrow(length = unit(0.2, "cm")),
  #   show.legend = FALSE) +
  # geom_segment(data = arrow_data_b,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              show.legend = FALSE) +
  geom_point(data = dfplot_all %>% filter(year %in% c(2025, 2050)),
             aes(x = year, y = GVI), shape = 1) +
  geom_point(data = dfplot_histav_all,
             aes(x = year_jittered, y = out_b_mean_st), shape = 5) +
  geom_errorbar(data = dfplot_histav_all,
                aes(x = year_jittered,
                    ymin = out_b_quart_lwr,
                    ymax = out_b_quart_upr), width = 0.5) +
  # geom_point(aes(y = GVI[length(GVI)-1])) +
  theme_minimal(base_size = 9) +
  facet_wrap(~city) +
  theme_minimal() +
  ylab("Street Green Space (GVI)") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c("dotted", "solid", "twodash")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )
outname <- paste0(path_results, "bylcz_simplenevelope_allcities.png")
ggsave(filename = outname, width = 60, height = 90, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

################
# Show number of points byLCZ for each city

test <- ugs %>%
  filter(city == "Los Angeles", lcz_filter_v3 == 6)
test2 <- dfspattemp %>%
  filter(city == "Los Angeles", lcz_filter_v3 == 6)
test3 <- dfplot_all %>%
  filter(city == "Los Angeles", lcz_filter_v3 == "Open lowrise")

ggplot(data = dfplot_all %>% filter(year == 2016), # We need to filter for year, because we otherwise double-count over all observation years..
       aes(x = city,
           y = npid_st,
           # color = lcz_filter_v3,
           fill = lcz_filter_v3,
           group = lcz_filter_v3)) +
  theme_minimal(base_size = 9) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  # scale_y_continuous(labels = scales::comma) +
  labs(y = "Number of observation points per LCZ, averaged over space and time") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 6),
        legend.key.size = unit(0.2, "cm"))
outname <-paste0(path_results, "npoints_by_lcz_allcities.png")
ggsave(filename = outname, width = 30, height = 20, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

###########
# Descriptive statistics of dfscen
## Scatterplot of the projected values
dfplot <- dfspattemp %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  mutate(Cls_short = fct_labeler_Clsmain2(Cls_short))

# Show boxpplot for distribution of urban green by LCZ and Clsmain
ggplot(data = dfplot, aes(x = lcz_filter_v3, y = out_b_mean_st, color = Cls_short)) +
  geom_boxplot() +
  theme_minimal(base_size = 9) +
  # facet_wrap(~Cls_short) +
  scale_color_manual(values = colors_Clsmain) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm")) +
  ylab("Mean observed GVI")
outname <- paste0(path_results, "boxplot_out_b_mean_st.png")
ggsave(filename = outname, width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300)
####

setwd(paste0(stub0, "/URGED"))
