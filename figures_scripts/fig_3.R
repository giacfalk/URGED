# In case you need to install gg.layers:
# install.packages("remotes")
# remotes::install_github("rpkgs/gg.layers")

setwd(stub)

library(sf)

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_max.csv")   %>% mutate(UC_NM_MN = gsub("_max", "", UC_NM_MN))
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))
# Use the labeling function
# lcz_shares <- lcz_shares %>%
#   mutate(variable = fct_labeler_lcz2(variable))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))
# scens <- scens %>%
  # mutate(lcz = fct_labeler_lcz2(lcz))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                labels = c("Tropical", "Dry", "Temperate", "Continental"),
                ordered = T))

###
# 
# source("URGED/support/fcts_labelers_colors.R") # Loaded in 00_sourcer.R

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * T[a[max]], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_max.pdf", height=12, width=10, scale=0.85)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###################
###################
###################
###################

# same but with mean WBGT

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_wbgt_max.csv")  %>% mutate(UC_NM_MN = gsub("_max_wbgt", "", UC_NM_MN))
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                         labels = c("Tropical", "Dry", "Temperate", "Continental"),
                         ordered = T))


###

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * WBGT[max], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_wbgt_max.pdf", height=12, width=10, scale=0.85)

###

# summary stats for paper

summary( merger %>% filter(year==2025 & variable!="Lightweight lowrise") %>% pull(coef))

tapply(merger %>% filter(year==2025 & variable!="Lightweight lowrise") %>% pull(coef), merger %>% filter(year==2025 & variable!="Lightweight lowrise") %>% pull(variable), summary)

tapply(merger %>% filter(year==2025 & variable!="Lightweight lowrise") %>% pull(coef), merger %>% filter(year==2025 & variable!="Lightweight lowrise") %>% pull(kg_cl_1), summary)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###################

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_min.csv")   %>% mutate(UC_NM_MN = gsub("_min", "", UC_NM_MN))
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                         labels = c("Tropical", "Dry", "Temperate", "Continental"),
                         ordered = T))

###

source("URGED/support/fcts_labelers_colors.R")

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * T[a[min]], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_min.pdf", height=12, width=10, scale=0.85)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###################
###################
###################
###################

# same but with mean WBGT

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_wbgt_min.csv")  %>% mutate(UC_NM_MN = gsub("_min_wbgt", "", UC_NM_MN))
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                         labels = c("Tropical", "Dry", "Temperate", "Continental"),
                         ordered = T))

###

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * WBGT[min], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_wbgt_min.pdf", height=12, width=10, scale=0.85)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###################

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_mean.csv")
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                         labels = c("Tropical", "Dry", "Temperate", "Continental"),
                         ordered = T))

###

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * T[a[mean]], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_mean.pdf", height=12, width=10, scale=0.85)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###################
###################
###################
###################

# same but with mean WBGT

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_wbgt_mean.csv")  %>% mutate(UC_NM_MN = gsub("_wbgt", "", UC_NM_MN))
scens <- read.csv("output_data/outer_3.csv")

###

lcz_shares <- dplyr::select(lcz_shares, -(c(1,3,4)))
lcz_shares <- reshape2::melt(lcz_shares, 1)
lcz_shares$variable <- gsub("lcz_frac_", "", lcz_shares$variable)

lcz_shares$variable <- factor(lcz_shares$variable, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

coefs$X <- NULL
scens$X <- NULL

###

scens$lcz <- factor(scens$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

merger <- merge(lcz_shares, coefs, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))
merger  <- merge(merger, scens, by.x=c("UC_NM_MN", "variable"), by.y = c("UC_NM_MN", "lcz"))

kg <- read_sf("results/cities_database_climatezones.gpkg")
kg <- kg %>% dplyr::select(UC_NM_MN, kg_cl_1) %>% st_set_geometry(NULL)

merger <- merge(merger, kg, "UC_NM_MN")
merger <- merger %>%
  mutate(kg_cl_1 = factor(kg_cl_1, levels = c("A", "B", "C", "D"),
                         labels = c("Tropical", "Dry", "Temperate", "Continental"),
                         ordered = T))

###

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(month), y=coef, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz[-7])+
  scale_y_continuous(limits=c(-1, 0.25), breaks = seq(-1, 0.25, 0.25))+
  ylab(expression(frac(partialdiff * WBGT[mean], partialdiff * GVI)))+
  xlab("Month")+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 8))

ggsave("paper/fig1_wbgt_mean.pdf", height=12, width=10, scale=0.85)

###

summary(merger %>% filter(year==2025) %>% pull(coef)) 

t.test(merger %>% filter(year==2025) %>% pull(coef))

###

setwd(stub)
