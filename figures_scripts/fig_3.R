# In case you need to install gg.layers:
# install.packages("remotes")
# remotes::install_github("rpkgs/gg.layers")

library(sf)
library(tidyverse)

setwd("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation")

source("URGED/support/fcts_labelers_colors.R") # Here also the samplecities are defined
source("URGED/support/fcts_helpers_debug.R")
source("URGED/support/fct_scenarios.R") # Here the "filtering" function can be found

# wbgt max

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmax_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmax_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * max],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_max.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * max],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_max_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))

###

# wbgt mean

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmean_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmean_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * mean],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_mean.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * mean],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_mean_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))

# wbgt min

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmin_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_WBGTmin_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * min],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_min.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[wg * "," * " " * min],
                       partialdiff * GVI)))+  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_wbgt_min_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))


# T2M max

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmax_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmax_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * max],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_max.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * max],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_max_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))

###

# T2M mean

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmean_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmean_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * mean],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_mean.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * mean],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_mean_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))

# T2M min

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmin_raw_gvi.csv")
coefs_sig <- read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/URGED/armande/summary_city_lcz_month_T2Mmin_raw_gvi_significant.csv")
coefs_sig <- coefs_sig %>% dplyr::select(1,2,3,5)
colnames(coefs_sig) <- c("city", "lcz", "month", "coef_sig")
coefs <- merge(coefs, coefs_sig, by=c("city", "lcz", "month"), all.x=T)
scens <- read.csv("output_data/outer_3.csv")

library(stringdist)

closest <- sapply(unique(coefs$city), function(x) {
  unique(lcz_shares$UC_NM_MN)[which.min(stringdist(x, unique(lcz_shares$UC_NM_MN), method = "jw"))]
})

coefs$city <- closest[match(coefs$city, names(closest))]

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$lcz <- factor(coefs$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))


merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("city", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  dplyr::mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                                 labels = c("Tropical", "Dry", "Temperate", "Continental"),
                                 ordered = T))


###

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * min],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_min.pdf", height=12, width=10, scale=0.85)

ggplot(merger %>% filter(year==2025))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef_sig, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression(frac(partialdiff * T[a * "," * " " * min],
                       partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))+
  coord_cartesian(ylim = c(-0.075, 0.025))

ggsave("paper/fig1_T2M_min_sign.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary(merger %>% filter(year==2025) %>% pull(coef)) 
summary(merger %>% filter(year==2025) %>% pull(coef_sig)) 
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(variable), summary)
tapply(merger %>% filter(year==2025) %>% pull(coef), merger %>% filter(year==2025) %>% pull(kg_cl_1), summary)
t.test(merger %>% filter(year==2025) %>% pull(coef))
