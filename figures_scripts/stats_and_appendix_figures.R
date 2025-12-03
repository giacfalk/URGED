
setwd(stub)
setwd("..")

library(tidyverse)
library(modelsummary)
library(data.table)


markups_wbgt <- read_rds("results/wbgt/wbgt_future.rds") %>% dplyr::select(city, year, month, clim_scen, wbgtmax_future, wbgt_max, lcz)

markups_wbgt$delta <- markups_wbgt$wbgtmax_future - markups_wbgt$wbgt_max

markups_wbgt <- filter(markups_wbgt, year==2050)

tapply(markups_wbgt$delta, markups_wbgt$clim_scen, summary)
 
View(markups_wbgt[which(markups_wbgt$delta<0),])

###########################
###########################
###########################

r = read.csv("implementation/results/scenarios/absolute_heat_decrease_wbgt_max.csv")

list_samplecities = c("Berlin", "Singapore", "Tokyo", "Accra", "Cairo", "Sydney", "Dubai", "Lima", "Houston", "Bogota", "Nairobi", "Dhaka")

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

###########################

r_s[(r_s$clim_scen %in% c("High ambition")),] %>% dplyr::group_by(UC_NM_MN) %>% dplyr::summarise(value=mean(value, na.rm=T))
View(r_s[(r_s$clim_scen %in% c("High ambition")),] %>% dplyr::group_by(UC_NM_MN) %>% dplyr::summarise(value=mean(value, na.rm=T)))


####

View(r_s %>% dplyr::group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T)))

rrrr <- r_s %>% dplyr::group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T))

hist(rrrr$value[rrrr$clim_scen=="High ambition"], xlab="WBGT change", breaks=30)

hist(rrrr$value[rrrr$clim_scen=="CurPol"], xlab="WBGT change", breaks=30)

sd(abs(rrrr$value[rrrr$clim_scen=="High ambition"]))
sd(abs(rrrr$value[rrrr$clim_scen=="CurPol"]))

####

# boxplot and points in correspondence of the top and lowest cities

library(ggrepel)
library(patchwork)

listino <- c("Nairobi", "Dhaka", "Bogota", "Tokyo", "Lagos")
listino2 <- c("Mogadishu", "Budapest", "Vienna", "Turin", "Dakar")

p_a <- ggplot()+
  theme_classic()+
  geom_boxplot(data=rrrr[rrrr$clim_scen=="CurPol",], aes(x=clim_scen, y=value))+
  geom_point(data=rrrr[rrrr$clim_scen=="CurPol" & rrrr$UC_NM_MN %in% c(listino),], aes(x=clim_scen, y=value), colour="#66CDAA")+
  geom_text_repel(data=rrrr[rrrr$clim_scen=="CurPol" & rrrr$UC_NM_MN %in% c(listino),], aes(x=clim_scen, y=value, label=UC_NM_MN), nudge_x = .25, colour="#66CDAA")+
  geom_point(data=rrrr[rrrr$clim_scen=="CurPol" & rrrr$UC_NM_MN %in% c(listino2),], aes(x=clim_scen, y=value), colour="red")+
  geom_text_repel(data=rrrr[rrrr$clim_scen=="CurPol" & rrrr$UC_NM_MN %in% c(listino2),], aes(x=clim_scen, y=value, label=UC_NM_MN), nudge_x = -.25, force=2, colour="red")+
  xlab("")+
  ylab("Maximum WBGT change")


p_b <- ggplot()+
  theme_classic()+
  geom_boxplot(data=rrrr[rrrr$clim_scen=="High ambition",], aes(x=clim_scen, y=value))+
  geom_point(data=rrrr[rrrr$clim_scen=="High ambition" & rrrr$UC_NM_MN %in% c(listino),], aes(x=clim_scen, y=value), colour="#66CDAA")+
  geom_text_repel(data=rrrr[rrrr$clim_scen=="High ambition" & rrrr$UC_NM_MN %in% c(listino),], aes(x=clim_scen, y=value, label=UC_NM_MN), nudge_x = .25, force=2, colour="#66CDAA")+
  geom_point(data=rrrr[rrrr$clim_scen=="High ambition" & rrrr$UC_NM_MN %in% c(listino2),], aes(x=clim_scen, y=value), colour="red")+
  geom_text_repel(data=rrrr[rrrr$clim_scen=="High ambition" & rrrr$UC_NM_MN %in% c(listino2),], aes(x=clim_scen, y=value, label=UC_NM_MN), nudge_x = -.25, force=2, colour="red")+
  xlab("")+
  ylab("Maximum WBGT change")

####

p_a + p_b

ggsave("paper/boxplot_counterbalancing_drivers_top_worst_cities.png", width=4, height=4, dpi=300, scale=2)
