
setwd(stub)

ww <- read_csv(list.files(path=paste0("results/URBCLIM_historical/wbgt_"), pattern="wbgt_stats_max", full.names = T)) 

ww$lcz <- factor(ww$lcz, levels=1:9, labels = c("Compact highrise", "Compact midrise", "Compact lowrise", "Open highrise", "Open midrise", "Open lowrise", "Lightweight lowrise", "Large lowrise", "Sparsely built"))

# same but with mean WBGT

lcz_shares <- read.csv("output_data/outer.csv")
coefs <- read.csv("output_data/outer_2_wbgt_max.csv")  %>%
  mutate(UC_NM_MN = gsub("_max_wbgt", "", UC_NM_MN))
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


merger <- merge(merger, ww, by.x=c("UC_NM_MN", "variable", "month"), by.y=c("city", "lcz", "variable"))

merger$tas_bin <- cut(merger$value.y, breaks = c(-10, 15, 20, 25, 30, 35, 40, 50), right=F,
                      labels = c("<15°C", "15-20°C", "20-25°C", "25-30°C", "30-35°C", "35-40°C", ">=40°C"))

###

merger$current_cooling <- merger$coef * merger$SGS


###

summary(merger$current_cooling)

###

ggplot(merger %>% filter(year==2025 & variable!="Lightweight lowrise"))+ #2025
  theme_classic()+
  geom_hline(yintercept = 0)+
  gg.layers::geom_boxplot2(aes(x=as.factor(tas_bin), y= current_cooling, fill=variable), width.errorbar = 0.1, show.legend = F, lwd=0.00001)+
  scale_fill_manual(name="LCZ", values = colors_lcz_no7no10)+
  ylab(expression("Effect of current SGS levels on " * T["wg, max"]))+
  xlab(expression("Monthly average " * T["wg, max"]))+
  facet_grid(variable ~ kg_cl_1)+
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 60, vjust = 0.5))+
  scale_y_continuous(limits=c(-2.75, 0.25), breaks = seq(-2.75, 0.25, 0.5))

ggsave("paper/fig1_max_current_bins.pdf", height=12, width=10, scale=0.85)


