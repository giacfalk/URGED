  r = read.csv("results/scenarios/absolute_heat_decrease_wbgt_max.csv")
  
  
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
  
  library(stringdist)
  
  closest <- sapply(unique(r$UC_NM_MN), function(x) {
    unique(markups$city)[which.min(stringdist(x, unique(markups$city), method = "jw"))]
  })
  
  r$UC_NM_MN <- closest[match(r$UC_NM_MN, names(closest))]
  
  r <- merge(r, markups, by.x=c("UC_NM_MN", "month"), by.y=c("city", "variable"))
  
  r$month <- NULL
  r$year <- NULL
  r$var <- NULL
  
  r_s <- reshape2::melt(r, c(1:2, 4))
  
  ##
  
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
  
  r_s_pot <- filter(r_s, clim_scen!="Moderate ambition" & clim_scen!="Decreased provision")
  r_s_pot <- r_s_pot %>% group_by(UC_NM_MN, clim_scen) %>% dplyr::summarise(value=mean(value, na.rm=T))
  
  r_s_pot <- r_s_pot %>% group_by(UC_NM_MN) %>% dplyr::summarise(potential_CurPol = 1 - ((value[clim_scen=="CurPol"] + value[clim_scen=="High ambition"]) / value[clim_scen=="CurPol"]), potential_GS = 1 - ((value[clim_scen=="GS"] + value[clim_scen=="High ambition"]) / value[clim_scen=="GS"]), potential_SP = 1 - ((value[clim_scen=="SP"] + value[clim_scen=="High ambition"]) / value[clim_scen=="SP"]), potential_ssp585 = 1 - ((value[clim_scen=="ssp585"] + value[clim_scen=="High ambition"]) / value[clim_scen=="ssp585"]) )
  
  r_s_pot <- reshape2::melt(r_s_pot, 1)
  r_s_pot$variable <- gsub("potential_", "", r_s_pot$variable)
  
  r_s_pot$variable <- factor(r_s_pot$variable, levels=c("SP", "GS", "CurPol", "ssp585"))
  r_s_pot$value <- r_s_pot$value * 100
  
  # r_s_pot$value <- ifelse(r_s_pot$value < 0, NA, r_s_pot$value)
  
  summary(r_s_pot$value[r_s_pot$variable=="CurPol"])
  summary(r_s_pot$value[r_s_pot$variable=="GS"])
  summary(r_s_pot$value[r_s_pot$variable=="SP"])
  summary(r_s_pot$value[r_s_pot$variable=="ssp585"])
  
  ###
  
  View(r_s_pot[r_s_pot$variable=="CurPol",])
  