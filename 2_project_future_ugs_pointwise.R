# Develop scenarios for urban green and associated heat mitigation potential.
# Here, use the point estimates produced and cluster them by city, country, lcz, kgz.
# For required packages and scripts see 00_sourcer.R
# # Directories and settings ----------------------------
## Input
path_ugs_complete <- "../ugs/after_points_100425_completedatabase.rds" # Citynames added in 1b_..
path_highestobs <- "../ugs/dfhighestobs.rds" # Highest observations and 10% / 90% percentiles as made in '2e_ugs_frontrunner_cities.R'
## Output
path_results <- "../results/scenarios/"

# Code #
# Pre-process data and add city names --------------
ugs <- read_rds(path_ugs_complete)
ugs <- ugs %>% 
  filter(city != "N/A") %>% # Drop one row with a bad city name
  filter(lcz_filter_v3 <= 10) %>% # Only keep urban form classes that are urban
  select(-id) %>% # Delete the `id` column that wasn't present in the 030624 dataset.
  dplyr::filter(lcz_filter_v3 <= 10) %>%   # Filter the land cover classes that are not urban (i.e. lcz_filter_v3 <= 10)
  dplyr::filter(lcz_filter_v3 != 7) %>% # Remove the lightweight low-rise class, which is an outlier (only a few informal settlement data points in Lagos)
  dplyr::filter(lcz_filter_v3 != 10) # Remove the heavy industry class

# Remove points that are in adjacent countries
ugs <- ugs %>%
  filter(
    !(city == "Singapore" & country == "MY"),
    !(city == "Asuncion" & country == "AR"),
    !(city == "Basel" & country %in% c("DE", "FR")),
    !(city == "Buffalo" & country == "CA"),
    !(city == "Ciudad Juárez" & country == "US"),
    !(city == "Comilla" & country == "IN"),
    !(city == "Detroit" & country == "CA"),
    !(city == "Geneva" & country == "FR"),
    !(city == "Ghent" & country == "NL"),
    !(city == "Guangzhou" & country == "HK"),
    !(city == "Kinshasa" & country == "CG"),
    !(city == "Lille" & country == "BE"),
    !(city == "Lomé" & country == "GH"),
    !(city == "N'Djamena" & country == "CM"),
    !(city == "N'Djamena" & country == "MY"),
    !(city == "Strasbourg" & country == "DE"),
    !(city == "Tijuana" & country == "US")
  )

# First determine highest and lowest observations ---------------------------
dffrontrunners <- ugs %>%
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
  distinct() %>%
  # Remove NA in Cls_short
  drop_na(Cls_short)

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


testspat1 <- dfspat %>%
  filter(city == "Singapore")

testsum <- dfspat %>% select(city, country) %>% distinct()
testsum <- testsum %>% filter(duplicated(city) | duplicated(city, fromLast = TRUE)) # Only Valencia (ES, VE) remains

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

testspattemp1 <- dfspattemp %>% 
  filter(city == "Singapore")

# _s stands for spatial average, _st stands for spatial and temporal average
# Key data.frames `dfspat` (spatially averaged GVI values [over all points], seperated by city and LCZ) and `dfspattemp` (spatially and temporally [2016-2023] averaged GVI values, seperated by city and LCZ).
# For large datasets, we need to split the data.frame into two smaller chunks to make the merging work on a 2018 Macbook Pro.

dftemp <- merge(dfspat, dfspattemp,
                 by = c("city", "country", "lcz_filter_v3", "Cls_short", "Cls", "ID_HDC_G0", "CTR_MN_ISO", "GRGN_L1", "GRGN_L2", "UC_NM_LST"), all = T)

testdftemp <- dftemp %>%
  filter(city == "Singapore")

# Merge with the "highest and lowest observed" dataset created earlier.
dfugshist <- merge(dftemp, dfhighestobs, by = c("Cls_short", "lcz_filter_v3"), all.x = T)

testdfugshist <- dfugshist %>%
  filter(city == "Singapore")

rm(dftemp)

# The data.frame df now contains the GVI values on spatial average, as well as spatio-temporal average. Classified by LCZ and Cls_short.
# Now get make scenarios with 25% and 50% more grwoth by 2050, in analogy to the code in 2e
future_years <- seq(2025, 2050, by = 5)

## Based on the spat_temp averages
dffuture <- dfugshist %>%
  dplyr::select(-year, -npid_s, -sumid_s, -lczshare_s, -out_b_mean_s, -out_b_median_s, -out_b_min_s, -out_b_max_s) %>%
  distinct() %>%
  expand_grid(year = future_years, .) %>%
  mutate(ugs_ref = out_b_median_st) %>%
  # Here, compute the scenarios of a) reference with Decreased provision, b) moderate ambition, c) high ambition
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
dfscen <- dffuture

testdffuture <- dffuture %>%
  filter(city == "Singapore")

# Merge the future data with the historic data in df
dfscen_pointlevel <- merge(dfscen, dfugshist %>% dplyr::select(-base::intersect(colnames(df), colnames(dffuture))[-c(3:4)]), by=c("city", "country"))
dfscen_pointlevel <- na.omit(dfscen_pointlevel)

testdfscen_pointlevel <- dfscen_pointlevel %>%
  filter(city == "Singapore")

#####
# Save results
write_rds(ugs, file = paste0("../ugs/ugs_cleaned_100425.rds"))
write_rds(dfhighestobs, file = paste0("../ugs/dfhighestobs.rds"))
write_rds(dfspat, "../results/scenarios/dfspat.rds")
write_rds(dfspattemp, "../results/scenarios/dfspattemp.rds")
write_rds(dfscen, "../results/scenarios/dfscen.rds")
write_rds(dfscen_pointlevel, "../results/scenarios/dfscen_pointlevel.rds")