
library(tidyverse)
library(sf)

###

data <- read_rds("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/outputs/elasticities/rome_WBGTmax_monthly_panel_data.rds")

city_boundary <- st_read("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/boundaries/Com01012023_g_WGS84.shp") %>% filter(COMUNE=="Roma") %>% st_transform(crs = 4326) 

city_boundary <- st_read("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/boundaries/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg") %>% filter(UC_NM_MN=="Rome") %>% st_transform(crs = 4326) 

###


setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation")

source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found

###

data_wbgs <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(wbgt=mean(wbgt, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) 

data_lcz <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(lcz=median(as.numeric(as.character(lcz)), na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_filter(city_boundary) %>% mutate(lcz = as.factor(lcz))

p1 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_lcz, aes(color =lcz), size=0.5) +
  scale_color_manual(values = colors_lcz_built[as.numeric(levels(data_lcz$lcz))], name="") +
  theme_void() +
  labs(title = "LCZ")

p2 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_wbgs, aes(color = wbgt), size=0.5) +
  scale_color_viridis_c(name="°C") +
  theme_void() +
  labs(title = "Avg. WBGTmax in July")


data_gvi <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(out_b=mean(out_b, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_filter(city_boundary)

p3 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_gvi, aes(color = out_b), size=0.5) +
  scale_color_distiller(palette="Greens", name="GVI", direction = 1) +
  theme_void() +
  labs(title = "Green View Index")

data_build_h <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(build_h=mean(build_h, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_filter(city_boundary)

p4 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_build_h, aes(color = build_h), size=0.5) +
  scale_color_distiller(palette="Blues", name="m", direction = 1) +
  theme_void() +
  labs(title = "Avg. building height")


data_elevation <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(elevation=mean(elevation, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_filter(city_boundary)

p5 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_elevation, aes(color = elevation), size=0.5) +
  scale_color_distiller(palette="Reds", name="m", direction = 1) +
  theme_void() +
  labs(title = "Elevation")

data_pop_dens <- data %>% group_by(x, y) %>% dplyr::filter(month == 7) %>% dplyr::summarise(pop_dens=mean(pop_dens, na.rm = TRUE)) %>% ungroup() %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_filter(city_boundary)

p6 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = "grey") +
  geom_sf(data = data_pop_dens, aes(color = log(pop_dens+1)), size=0.5) +
  scale_color_viridis_c(name="pop./skm", option = "A") +
  theme_void() +
  labs(title = "log. pop. density")

################
################

library(patchwork)

((p1 + p2 + p3) / (p4 + p5 + p6))

ggsave("paper/maps_rome_example.png", height=3, width=7, scale=1.5)
ggsave("paper/maps_rome_example.pdf", height=3, width=7, scale=1.2)
