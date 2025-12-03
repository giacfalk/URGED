
library(tidyverse)
library(modelsummary)
library(data.table)

setwd(stub)
setwd("..")

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmax_future, wbgt_max) # Deltas for future years
markups$delta <- markups$wbgtmax_future - markups$wbgt_max
markups$wbgt_max <- NULL
markups$wbgtmax_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level maximum WBGT around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree * WBGT))

ggsave("paper/scen_result_wbgt_max_boxplot.png", height = 6, width = 6, scale=1.4)

###########################

View(r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),])

###########################

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_mean.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmean_future, wbgt_mean) # Deltas for future years
markups$delta <- markups$wbgtmean_future - markups$wbgt_mean
markups$wbgt_mean <- NULL
markups$wbgtmean_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level mean WBGT around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree * WBGT))

ggsave("paper/scen_result_wbgt_mean_boxplot.png", height = 6, width = 6, scale=1.4)


###########################

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_min.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmin_future, wbgt_min) # Deltas for future years
markups$delta <- markups$wbgtmin_future - markups$wbgt_min
markups$wbgt_min <- NULL
markups$wbgtmin_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level minimum WBGT around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree * WBGT))

ggsave("paper/scen_result_wbgt_min_boxplot.png", height = 6, width = 6, scale=1.4)

###

r = read.csv("results/scenarios/absolute_heat_decrease_tas_max.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/Climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmax" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level maximum 2-m air temperature around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree *  C^degree))

ggsave("paper/scen_result_tas_max_boxplot.png", height = 6, width = 6, scale=1.4)

###########################

r = read.csv("results/scenarios/absolute_heat_decrease_tas_mean.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/Climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tas" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level mean 2-m air temperature around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree *  C^degree))

ggsave("paper/scen_result_tas_mean_boxplot.png", height = 6, width = 6, scale=1.4)

###########################

r = read.csv("results/scenarios/absolute_heat_decrease_tas_min.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/scenarios/Climate_change_provide_markups.rds") # Deltas for future years

markups <- markups %>% filter(var=="tasmin" & year==2050)

markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, var)] 

markups <- reshape2::melt(markups, c(1:4))
markups$variable <- match(markups$variable, month.abb)
colnames(markups)[6] <- "delta"

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s <- r_s %>% filter(UC_NM_MN %in% list_samplecities)

r_s <- filter(r_s, !(variable=="delta" &scen_SGS!="Decreased provision"))

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

ggplot()+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x=clim_scen, y=value, fill=clim_scen))+
  geom_bar(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x=clim_scen, y=value, fill=clim_scen), stat = "summary", fun = mean, colour="black")+
  facet_wrap(vars(UC_NM_MN))+
  geom_segment(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "ssp585") + 0.45, xend = which(levels(clim_scen) == "ssp585") + 0.45, y = -2, yend = 5), linetype = "dashed", colour="grey")+
  geom_text(data=r_s[r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585"),], aes(x = which(levels(clim_scen) == "GS") + 0.25, y = 5, label = "Climate change impact"), size=3.5, colour="black")+
  geom_text(data=r_s[!(r_s$clim_scen %in% c("SP", "GS", "CurPol", "ssp585")),], aes(x = which(levels(clim_scen) == "Moderate ambition"), y = 5, label = "SGS scenario impact"), size=3.5, colour="black")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_blank(), legend.title.align = 0.5)+
  scale_fill_manual(name="Climate change impacts and street green space evolution scenarios", values=c("lightyellow", "yellow2", "gold", "darkred", "#ffa6a6", "lightgreen",  "forestgreen"))+
  ggtitle("Climate change and SGS scenarios: impact on city-level minimum 2-m air temperature around 2050")+
  xlab("")+
  guides(fill = guide_legend(nrow = 1, title.position = "top"))+
  ylab(expression(Delta * C^degree *  C^degree))

ggsave("paper/scen_result_tas_min_boxplot.png", height = 6, width = 6, scale=1.4)

#########
#########

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmax_future, wbgt_max, pctl) # Deltas for future years
markups$delta <- markups$wbgtmax_future - markups$wbgt_max
markups$wbgt_max <- NULL
markups$wbgtmax_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="Decreased provision")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = value[clim_scen=="CurPol"] + value[clim_scen=="High ambition"], potential_GS = value[clim_scen=="GS"] + value[clim_scen=="High ambition"], potential_SP = value[clim_scen=="SP"]+ value[clim_scen=="High ambition"], potential_ssp585 = value[clim_scen=="ssp585"] + value[clim_scen=="High ambition"])

summary(r_s_pot$potential_ssp585)
summary(r_s_pot$potential_CurPol)
summary(r_s_pot$potential_GS)
summary(r_s_pot$potential_SP)

###

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value

r_s_pot_part_1 <- r_s_pot

###
###

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmax_future, wbgt_max, pctl) # Deltas for future years
markups$delta <- markups$wbgtmax_future - markups$wbgt_max
markups$wbgt_max <- NULL
markups$wbgtmax_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

r_s_pot <- filter(r_s, clim_scen!="Decreased provision" & clim_scen!="Decreased provision")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = value[clim_scen=="CurPol"] + value[clim_scen=="Moderate ambition"], potential_GS = value[clim_scen=="GS"] + value[clim_scen=="Moderate ambition"], potential_SP = value[clim_scen=="SP"] + value[clim_scen=="Moderate ambition"], potential_ssp585 = value[clim_scen=="ssp585"] + value[clim_scen=="Moderate ambition"])

summary(r_s_pot$potential_ssp585)
summary(r_s_pot$potential_CurPol)
summary(r_s_pot$potential_GS)
summary(r_s_pot$potential_SP)

###

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value

r_s_pot_part_2 <- r_s_pot


###
###

r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Nairobi", "Houston", "Tokyo")

r <- filter(r, scen_SGS!="ugs_ref")

r$scen_SGS = ifelse(r$scen_SGS=="ugs_scen_hgh", "High ambition", ifelse(r$scen_SGS=="ugs_scen_mod", "Moderate ambition", "Decreased provision"))

r$scen_SGS  <- factor(r$scen_SGS, levels = c("Decreased provision", "Moderate ambition", "High ambition"))

r <- dplyr::group_by(r, UC_NM_MN, month, scen_SGS) %>% dplyr::summarise(product_fut=mean(product_fut, na.rm=T))

##

markups <- readRDS("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, clim_scen, month, wbgtmax_future, wbgt_max, pctl) # Deltas for future years
markups$delta <- markups$wbgtmax_future - markups$wbgt_max
markups$wbgt_max <- NULL
markups$wbgtmax_future <- NULL
markups <- markups %>% filter(year==2050)
markups <- data.table(markups)
markups$pctl <- NULL
markups <- markups[, lapply(.SD, mean, na.rm = TRUE), by = .(city, year, clim_scen, month)] 

colnames(markups)[4] <- "variable"
colnames(markups)[5] <- "delta"

markups$variable <- match(markups$variable, month.abb)

r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))

r$month <- NULL
r$year <- NULL
r$var <- NULL

r_s <- reshape2::melt(r, c(1:2, 4))

##

r_s$clim_scen <- factor(r_s$clim_scen, levels=c("SP", "GS", "CurPol", "ssp585"))

r_s_1 <- filter(r_s, variable=="delta")
r_s_2 <- filter(r_s, variable!="delta")

r_s_1$variable <- NULL
r_s_2$variable <- NULL

r_s_1$scen_SGS <- NULL
r_s_2$clim_scen <- r_s_2$scen_SGS
r_s_2$scen_SGS <- NULL

r_s <- bind_rows(r_s_1, r_s_2)

r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="High ambition")
r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% summarise(value=mean(value, na.rm=T))

r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = value[clim_scen=="CurPol"] + value[clim_scen=="Decreased provision"], potential_GS = value[clim_scen=="GS"] + value[clim_scen=="Decreased provision"], potential_SP = value[clim_scen=="SP"]+ value[clim_scen=="Decreased provision"], potential_ssp585 = value[clim_scen=="ssp585"] + value[clim_scen=="Decreased provision"])

summary(r_s_pot$potential_ssp585)
summary(r_s_pot$potential_CurPol)
summary(r_s_pot$potential_GS)
summary(r_s_pot$potential_SP)

###

r_s_pot <- reshape2::melt(r_s_pot, 1)
r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)

r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
r_s_pot$value <- r_s_pot$value

r_s_pot_part_3 <- r_s_pot

##########################################
##########################################
##########################################

r_s_pot_part_1$sgs_scenario <- "High ambition"
r_s_pot_part_2$sgs_scenario <- "Moderate ambition"
r_s_pot_part_3$sgs_scenario <- "Decreased provision"

rrr <- bind_rows(r_s_pot_part_1, r_s_pot_part_2, r_s_pot_part_3)

rrr$sgs_scenario <- factor(rrr$sgs_scenario, levels=c("Decreased provision", "Moderate ambition", "High ambition"))

library(modelsummary)

datasummary(Factor(UC_NM_MN) ~  variable*sgs_scenario*value * mean, data=rrr, title = "Climate change impacts in city-level maximum WBGT around 2050 by SGS evolution scenarios and climate change scenarios.")

datasummary(Factor(UC_NM_MN) ~  variable*sgs_scenario*value * mean, data=rrr, title = "Climate change impacts in city-level maximum WBGT around 2050 by SGS evolution scenarios and climate change scenarios.", output = "paper/counterbalancing.tex")

############

stargazer::stargazer(as.data.frame(unique(rrr$UC_NM_MN)), summary = F, rownames = F, output = "paper/cities_list.tex")

############

setwd(paste0(stub, "/URGED"))
