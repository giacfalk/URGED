
##############################################################################

# This Rscript: 

#   1) estimate the elasticity of electricity consumption to hot temperatures

##############################################################################

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
require(data.table)
require(tidyverse)
require(fixest)
require(haven)
require(fabricatr)
require(texreg)
require(xtable)
require(stargazer)
require(effects)
require(marginaleffects)
library(sf)

stub <- 'C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/'

setwd(stub)

# Set directories
res_dir <- paste0(stub, 'results/', sep ='')

########

df <- read_rds("energy/processed_data.rds")
df <- df[df$cons_month>50,]

####

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

###

# set hot months categorical variable

df$hotmonths <- as.factor(ifelse(as.numeric(df$month)>5 & as.numeric(df$month)<9, 1, 0))

df$quartile_reddito <- cut(df$reddito_medio, quantile(df$reddito_medio, by=seq(0, 1, 0.2), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"))

################

reg_def2 <- feols(log(cons_month) ~ tmax:hotmonths + tmax:log(gvi_mean):hotmonths |  pod + comune + year + month + region^year, data=df)
summary(reg_def2, cluster = ~comune+pod)

reg_def2_lev <- feols(log(cons_month) ~ tmax:hotmonths + tmax:gvi_mean:hotmonths |  pod + comune + year + month + region^year, data=df, combine.quick = F)
summary(reg_def2_lev, cluster = ~comune+pod)

etable(reg_def2_lev, cluster = ~comune+pod, export = "results/regtab2.png")

coef(reg_def2)[4]*100

write_rds(reg_def2, paste0(res_dir, "ugs_ely_elasticity_model.rds"))

#

targets <- target <- c(20)

sum_tab <- data.frame("avg. % reduction", "TWh/yr", "% yearly resid. electr. cons.", "MT CO2")
colnames(sum_tab) <- c("avg. % reduction", "TWh/yr", "% yearly resid. electr. cons.", "MT CO2")

for(target in targets){
  
  df_policy <- df
  df_policy$gvi_mean_orig <- df_policy$gvi_mean
  
  # target based policy
  df_policy$gvi_mean <- ifelse(df_policy$gvi_mean_orig<target, target, df_policy$gvi_mean_orig)
  
  # percentage based policy
  # df_policy$gvi_mean <- df_policy$gvi_mean_orig*1.1
  
  ###
  
  pr1 <- exp(predict(reg_def2_lev, newdata = df))
  pr2 <- exp(predict(reg_def2_lev, newdata = df_policy))
  
  pr_diff <- 1- (pr2 / pr1)
  
  df_policy$delta <- pr_diff
  
  library(geodata)
  library(sf)
  
  province <- st_as_sf(gadm(country = "ITA", level = 3, path = getwd()))
  province$comune <- province$NAME_3
  
  df_policy <- df_policy %>% group_by(comune, pod) %>% dplyr::summarise(delta=mean(delta, na.rm=T), gvi_mean=mean(gvi_mean, na.rm=T), gvi_mean_orig=mean(gvi_mean_orig, na.rm=T), cons_month=mean(cons_month, na.rm=T), pop=mean(pop, na.rm=T)) %>% ungroup() %>% group_by(comune) %>%  dplyr::summarise(delta=mean(delta, na.rm=T), gvi_mean=mean(gvi_mean, na.rm=T), gvi_mean_orig=mean(gvi_mean_orig, na.rm=T), cons_month=mean(cons_month, na.rm=T), pop=mean(pop, na.rm=T))
  
  library(fuzzyjoin)
  
  df_policyy <- stringdist_left_join(df_policy,province, by="comune", distance_col = "distance")
  
  df_policyy <- df_policyy %>% group_by(comune.x) %>% filter(distance==min(distance))
  
  df_policy_sf <- st_as_sf(df_policyy)
  
  a <- ggplot()+
    theme_void()+
    geom_sf(data=df_policy_sf, aes(fill=delta*100), colour="transparent")+
    geom_sf(data=province, fill="transparent", colour="black", lwd=0.01)+
    scale_fill_distiller(name="", palette = "YlOrRd", direction = 1)
  
  library(patchwork)
  
  a <- a + plot_annotation(title="Expected % avg. monthly consumption reduction under UGS >=20 policy")
  
  ggsave(paste0("results/policy_", target, ".png"), a, width=4.5, heigh=3, scale=2, bg = "white")
  

  nhouseholds <- 26e6
  
  i1 <- weighted.mean(df_policyy$delta*100, df_policyy$cons_month, na.rm=T)
  
  i2 <- weighted.mean(df_policyy$delta * df_policyy$cons_month * 12, df_policyy$pop, na.rm = T) * nhouseholds / 1e9 # twh
  
  i3 <- (weighted.mean(df_policyy$delta * df_policyy$cons_month * 12, df_policyy$pop, na.rm = T) * nhouseholds  * 1272) / 1e12 # mtco2
  
  i4 <- (((weighted.mean(df_policyy$delta * df_policyy$cons_month * 12, df_policyy$pop, na.rm = T) * nhouseholds)  / 1e9) / 64.5) * 100  #% ot total ely
  # https://www.terna.it/it/sistema-elettrico/statistiche/evoluzione-mercato-elettrico/consumi-totale
  
  c(i1, i2, i4, i3)
  
  sum_tab[match(target, targets),] <- c(i1, i2, i4, i3)
  
}

sum_tab <- sum_tab %>% mutate_all(as.numeric) %>% mutate_if(is.numeric, round, 2)
rownames(sum_tab) <- targets

stargazer::stargazer(sum_tab, digits=2, digits.extra=2, summary = F, out = "results/sim_res.tex")
stargazer::stargazer(sum_tab, digits=2, digits.extra=2, summary = F, out = "results/sim_res.html", type = "html")


###
###
###

cdh <- read_rds("climate/cdh_2020_2022_comuni.rds")

df <- merge(df, cdh, by=c("comune", "year", "month"))

##

# results from 1st stage

df_1st <- read_rds(paste0(res_dir, "ugs_cdh_cityspecific_model_ITA.rds"))

df <- merge(df, df_1st, by.x="comune", by.y="COMUNE")

###

reg_def2_lev_cdh <- feols(log(cons_month) ~ cdh + cdh^2 |  pod + comune + year + month + region^year, data=df, combine.quick = F)
summary(reg_def2_lev_cdh, cluster = ~comune+pod)

plot_predictions(reg_def2_lev_cdh, condition = "cdh", transform = "exp")

etable(reg_def2_lev_cdh, cluster = ~comune+pod, export = "results/regtab3.png")

###

# policy

df <- filter(df, cdh>0)

df$gvi_mean_policy <- df$gvi_mean * 1.25

df$cdh_policy <- df$cdh * (1+((exp(df$estimate)-1)*(df$gvi_mean_policy - df$gvi_mean)))

df$predicted_ely <- exp(predict(reg_def2_lev_cdh, newdata=df))

df$cdh <- df$cdh_policy

df$predicted_ely_policy <- exp(predict(reg_def2_lev_cdh, newdata=df))

df$reduction <- (df$predicted_ely_policy / df$predicted_ely) - 1

summary((df$predicted_ely_policy / df$predicted_ely) - 1)

df %>% dplyr::group_by(comune) %>% dplyr::summarise(reduction=mean(reduction, na.rm=T)*100)
