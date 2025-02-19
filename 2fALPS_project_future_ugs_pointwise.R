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
source("URGED/fcts_labelers_colors.R")
source("URGED/fcts_helpers_debug.R")
source("URGED/fct_scenarios.R") # Here the "filtering" function can be found

# Variables
slopfac1 = 0.50 # Slope factor for the envelope of x% more growth than historically
fac2050a = 0.25 # Make GVI 25% larger than current value by 2050
fac2050b = 0.5 # Make GVI 50% larger than current value by 2050

list_samplecities <- c("Abidjan", "Berlin", "Miami", "Singapore")

# Directories and settings ----------------------------
## Input
path_ugs_complete <- "ugs/after_points_030624_completedatabase.rds" # Citynames added in 1b_..
path_highestobs <- "ugs/dfhighestobs.rds" # Highest observations and 10% / 90% percentiles as made in '2e_frontrunner...'
## Output
path_results <- "results/scenarios_ALPS/"
file_out_df_rds <- paste0(path_results, "dfscen_pointlevel.rds")
file_out_df_csv <- paste0(path_results, "dfscen_pointlevel.csv")

# Code ########################################################################################

# Pre-process data and add city names -------------------------------------
# load(path_ugs) # year-to-year data of UGS. Var: **out_ndvi_m**
ugs <- read_rds(path_ugs_complete)
ugs <- ugs %>% 
  filter(city != "N/A") %>% # Drop one row with a bad city name
  filter(lcz_filter_v3 <= 10) # Only keep urban form classes that are urban

# First get share of LCZ and KGZ per city
## Spatial averages
dfspat <- ugs %>%
  group_by(city, country, lcz_filter_v3, Cls_short, year) %>%
  mutate(npid_s = n(),
            out_b_mean_s = mean(out_b, na.rm=T),
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

# Spatial *and* temporal averages
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
         out_b_min_st = min(out_b, na.rm=T),
         out_b_max_st = max(out_b, na.rm=T),
         out_b_quart_lwr = quantile(out_b, prob=c(.25,.5,.75))[[1]],
         out_b_quart_upr = quantile(out_b, prob=c(.25,.5,.75))[[3]]
         ) %>%
  ungroup() %>%
  dplyr::select(-out_b, -ID, -x, -y, -year) %>%
  distinct()

test <- c(13,34,53,23,73,25,45)
quantile(test, prob=c(.25,.5,.75))[[3]]

dfspattemp <- dfspattemp %>%
  group_by(city, country) %>%
  mutate(sumid_st = sum(npid_st)) %>%
  group_by(city, country, lcz_filter_v3) %>%
  mutate(lczshare_st = npid_st / sumid_st) %>%
  ungroup()

# _s stands for spatial average, _st stands for spatial and temporal average

dftemp <- merge(dfspat, dfspattemp,
                by = c("city", "country", "lcz_filter_v3", "Cls_short", "Cls",
                       "ID_HDC_G0", "CTR_MN_ISO", "GRGN_L1", "GRGN_L2", "UC_NM_LST"), all = T)
# Merge with the "highest observed" dataset
dfhighestobs <- read_rds(path_highestobs)
df <- merge(dftemp, dfhighestobs, by = c("Cls_short", "lcz_filter_v3"), all.x = T)

# This data.frame df now contains the GVI values on spatial average, as well as spatio-temporal average. Classified by LCZ and Cls_short.
# Now get make scenarios with 25% and 50% more grwoth by 2050, in analogy to the code in 2e
future_years <- seq(2025, 2050, by = 5)

## Based on the spat_temp averages
dffuture <- df %>%
  dplyr::select(-year, -npid_s, -sumid_s, -lczshare_s, -out_b_mean_s, -out_b_min_s, -out_b_max_s) %>%
  distinct() %>%
  expand_grid(year = future_years, .) %>%
  # mutate(GVI_proja_upr = out_b_mean_st * (1 + fac2050a * (year - yearlatest)/(2050 - yearlatest)),
  #        GVI_proja_lwr = out_b_mean_st * (1 - fac2050a * (year - yearlatest)/(2050 - yearlatest)), 
  #        GVI_projb_upr = out_b_mean_st * (1 + fac2050b * (year - yearlatest)/(2050 - yearlatest)),
  #        GVI_projb_lwr = out_b_mean_st * (1 - fac2050b * (year - yearlatest)/(2050 - yearlatest))
  mutate(GVI_proja_upr = out_b_quart_upr * (1 + fac2050a * (year - yearlatest)/(2050 - yearlatest)),
         GVI_proja_lwr = out_b_quart_lwr * (1 - fac2050a * (year - yearlatest)/(2050 - yearlatest)), 
         GVI_projb_upr = out_b_quart_upr * (1 + fac2050b * (year - yearlatest)/(2050 - yearlatest)),
         GVI_projb_lwr = out_b_quart_lwr * (1 - fac2050b * (year - yearlatest)/(2050 - yearlatest))
  ) # Here we "relabel" `out_b_mean` to `GVI`

# Constrain the projected values by the observed lower and upper values
## This is up for experimentation, we use quintiles
dffuture <- dffuture %>%
  mutate(hitboundsa = ifelse(GVI_proja_upr > boundupr, "Yes", "No"),
         hitboundsb = ifelse(GVI_projb_upr > boundupr, "Yes", "No")) %>%
  mutate(GVI_proja_upr =
           ifelse(hitboundsa == "Yes", boundupr, GVI_proja_upr),
         GVI_projb_upr =
           ifelse(hitboundsb == "Yes", boundupr, GVI_projb_upr),
         GVI_proja_lwr =
           ifelse(GVI_proja_lwr < boundlwr, boundlwr, GVI_proja_lwr),
         GVI_projb_lwr =
           ifelse(GVI_projb_lwr < boundlwr, boundlwr, GVI_projb_lwr))

# Merge the future data with the historic data in df
dfscen <- merge(dffuture, df, all = T)
# Add a general out_b_mean for historical and future years for the ALPS project
dfscen <- dfscen %>%
  mutate(GVI_ALPS = ifelse(!is.na(out_b_mean_s), out_b_mean_s, (GVI_proja_upr + GVI_proja_lwr)/2))

write_rds(dfscen, file_out_df_rds)
write_csv(dfscen, file_out_df_csv)

# Show for selected cities
dfplot <- dfscen %>%
  dplyr::filter(city %in% list_samplecities) %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  group_by(city, lcz_filter_v3)

# New plot: Instead of the plots show the 2016-2023 average with "error" bars which denote observed min./max.
dfplot_histav <- dfplot %>%
  dplyr::filter(year == 2020) %>%
  select(city, year, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.2)

dfplot2 <- dfplot %>% # Make this long w.r.t. the year
  pivot_longer(cols = c(GVI_proja_upr, GVI_projb_lwr), names_to = "scen", values_to = "GVI") %>%
  # Rename "GVI_proj_a" to "upper scenario"
  mutate(scen = factor(scen,
                          levels = c("GVI_proja_upr", "GVI_projb_lwr"),
                          labels = c("upper scenario", "lower scenario")))
  # mutate(scen = ifelse(scen == "GVI_proja_upr", "optimistic", scen),
  #        scen = ifelse(scen == "GVI_projb_lwr", "pessimistic", scen))

dfplot_histav2 <- dfplot2 %>%
  dplyr::filter(year == 2020) %>%
  select(city, year, scen, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.2)

# arrow_data_a <- dfplot2 %>%
#   filter(year %in% c(2025, 2050)) %>%
#   filter(scen == "upper scenario") %>%
#   select(city, year, GVI, lcz_filter_v3, scen) %>%
#   pivot_wider(names_from = year, values_from = GVI) %>% # Reshape to wide format
#   rename(y = `2025`, yend = `2050`) # Rename columns for easier use
# 
# arrow_data_b <- dfplot2 %>%
#   filter(year %in% c(2025, 2050)) %>%
#   filter(scen == "lower scenario") %>%
#   select(city, year, GVI, lcz_filter_v3, scen) %>%
#   pivot_wider(names_from = year, values_from = GVI) %>% # Reshape to wide format
#   rename(y = `2025`, yend = `2050`) # Rename columns for easier use


############################
# This is the plot where I attempt a legend using dfplot2
ggplot(data = dfplot2,
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
  geom_point(data = dfplot2 %>% filter(year %in% c(2025, 2050)),
             aes(x = year, y = GVI), shape = 1) +
  geom_point(data = dfplot_histav2,
             aes(x = year_jittered, y = out_b_mean_st), shape = 5) +
  geom_errorbar(data = dfplot_histav2,
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
  scale_x_continuous(breaks = c(2025, 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c(1, 3)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )
outname <- paste0(path_results, "bylcz_simplenevelope_samplecities_new_legend.png")
ggsave(filename = outname, width = 16.5, height = 13, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Show the same for ALL CITIES
# Show for selected cities
dfplot <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3))

# Show the development of GVI in each LCZ for the sample cities.
ggplot(data = dfplot,
       aes(x = year,
           # color = interaction(lcz_filter_v3, Cls_short),
           fill = lcz_filter_v3,
           group = lcz_filter_v3)) +
  geom_ribbon(aes(ymin = GVI_proja_lwr, ymax = GVI_proja_upr), alpha = 0.6, linewidth = 0.05, color = "grey40") +
  theme_minimal(base_size = 9) +
  # geom_line(aes(y = out_b_maxmean10), alpha = 0.6) +
  facet_wrap(~city) +
  theme_minimal() +
  scale_fill_manual(values = colors_lcz) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm"))
outname <- paste0(path_results, "bylcz_simplenevelope_allcities.png")
ggsave(filename = outname, width = 90, height = 60, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Show number of points byLCZ for each city
ggplot(data = dfplot,
       aes(x = city,
           y = npid_st,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           group = lcz_filter_v3)) +
  theme_minimal(base_size = 9) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 6),
        legend.key.size = unit(0.2, "cm"))

outname <- paste0(path_results, "npoints_by_lcz_allcities.png")
ggsave(filename = outname, width = 30, height = 20, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

 # Plot the theoretical limits for climate and lcz
dfplot <- dfhighestobs %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  mutate(Cls_short = fct_labeler_Clsmain2(Cls_short))

# Version a)
ggplot(data = dfplot,
       aes(x = lcz_filter_v3,
           y = boundupr,
           group = Cls_short,
           fill = Cls_short)) +
  theme_minimal(base_size = 9) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "grey40", linewidth = 0.05) +
  scale_color_manual(values = colors_Clsmain) +
  scale_fill_manual(values = colors_Clsmain) +
  ylab("Highest observed GVI (avg. of top 100)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 66, hjust = 1),
        legend.key.size = unit(0.2, "cm"))
outname <- paste0(path_results, "out_b_max_100_dodged.png")
ggsave(filename = outname, width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

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

# Show 


# Determine historical growth rate