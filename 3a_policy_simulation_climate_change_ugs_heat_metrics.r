
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'
res_dir <- paste0(stub, 'results/', sep ='')

out_ndvi_m <- read_rds(paste0(stub, "climate/provide_urban_climate_data/data_provide_cdh_gvi_143cities_withcovariates.rds"))
out_ndvi_m <- filter(out_ndvi_m, t>0)

fitted_models <- read_rds(paste0(res_dir, "ugs_cdh_cityspecific_model.rds"))

###

polinc<- 0.2 # policy % increase

out_ndvi_m$t_policy <- out_ndvi_m$t + (((fitted_models$estimate[grep("Berlin", fitted_models$city)])) * out_ndvi_m$out_b_mean * polinc)*out_ndvi_m$t

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  geom_point(aes(x=x, y=y, colour=((t_policy/t)-1)*100))+
  scale_colour_distiller(palette="Blues", name="variation (%)")

ggplot(out_ndvi_m %>% filter(city=="Berlin"))+
  geom_point(aes(x=x, y=y, colour=((t_policy/t)-1)*t))+
  scale_colour_distiller(palette="Blues", name="variation (CDH/yr.)")

###

polinc_abs <- 25 # policy target gvi

diff <- polinc_abs - out_ndvi_m$out_b_mean
diff[diff<0] <- 0

out_ndvi_m$t_policy <- out_ndvi_m$t + (diff*(fitted_models$estimate[grep("Athens",fitted_models$city)])*out_ndvi_m$t)
out_ndvi_m$t_policy_2050 <- out_ndvi_m$t_2050 + (diff*(fitted_models$estimate[grep("Athens",fitted_models$city)])*out_ndvi_m$t_2050)

athens_plot_a <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=((t_policy/t)-1)*100))+
  scale_colour_distiller(palette="Blues", name="policy impact variation (%)")

athens_plot_b <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=((t_policy/t)-1)*t))+
  scale_colour_distiller(palette="Blues", name="policy impact (CDH/yr.)")

athens_plot_c <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=((t_policy_2050/t_2050)-1)*t_2050))+
  scale_colour_distiller(palette="Blues", name="policy impact (CDH/yr.),\n2050 climate change")

athens_plot_d <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=((t_2050/t)-1)*t))+
  scale_colour_distiller(palette="Blues", name="change (CDH/yr.),\n2050 climate change, \nno policy")

athens_plot_e <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=x, y=y, colour=((t_policy_2050/t)-1)*t))+
  scale_colour_distiller(palette="Blues", name="change (CDH/yr.),\n2050 climate change, \n policy")

athens_plot_f <- ggplot(out_ndvi_m %>% filter(city=="Athens"))+
  theme_classic()+
  geom_point(aes(x=(((t_2050/t)-1)*t), y=((t_policy_2050/t)-1)*t, colour=out_b_mean))+
  geom_abline()+
  scale_x_continuous(limits = c(400, 1600))+
  scale_y_continuous(limits = c(400, 1600))+
  scale_colour_distiller(palette = "YlGn", direction = 1)+
  xlab("No policy")+
  ylab("Policy")

###

out_b_mean_chd <- out_ndvi_m %>% group_by(city) %>% dplyr::summarise(t=mean(t, na.rm=T))
out_b_mean_chd <- merge(out_b_mean_chd, fitted_models, "city")

library(ggrepel)

ggplot(out_b_mean_chd %>% filter(p.value<0.01))+
  geom_point(aes(x=t, y=estimate*100))+
  geom_smooth(aes(x=t, y=estimate*100), method = "lm", formula = y ~ x + I(x^2), size = 1)+
  scale_x_log10()+
  geom_label_repel(aes(x=t, y=estimate*100, label=city))


ggplot(out_b_mean_chd %>% filter(p.value<0.01 & t>1000))+
  geom_point(aes(x=t, y=estimate*100))+
  geom_smooth(aes(x=t, y=estimate*100), method = "lm", formula = y ~ x + I(x^2), size = 1)+
  scale_x_log10()+
  geom_label_repel(aes(x=t, y=estimate*100, label=city))
