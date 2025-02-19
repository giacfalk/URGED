# Helper and debug files
fct_count_Cls_SR <- function(df) {
  # Debug: Make a list of cities to see how many are in each climate and subregion
  
  Clslist <- df %>% sf::st_drop_geometry()%>% select(Cls, city, `Sub-region Name`) %>% distinct() %>% arrange(Cls)
  write_csv(Clslist, "climate/provide_urban_climate_data/cities/providecities-by-cls_SR.csv")
  Clscounts <- Clslist %>%
    dplyr::select(-city) %>%
    group_by(Cls) %>%
    summarise(n_Cls = n()) %>%
    ungroup()
  write_csv(Clscounts, "climate/provide_urban_climate_data/cities/providecities-by-cls.csv")
  SRcounts <- Clslist %>%
    dplyr::select(-city) %>%
    group_by(`Sub-region Name`) %>%
    summarise(n_SR = n()) %>%
    ungroup()
  write_csv(SRcounts, "climate/provide_urban_climate_data/cities/providecities-by-subregion.csv")
  # Plot the number of cities in each Köppen-Geiger class
  ggplot(data = Clscounts, aes(x = Cls, y = n_Cls)) +
    theme_minimal(base_size = 11) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = seq(1,22), labels = seq(1,22)) +
    labs(x = "Köppen-Geiger class", y = "Number of cities") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
  ggsave(filename = "climate/provide_urban_climate_data/cities/providecities-by-cls.png", width = 12, height = 8, units = "cm", bg = "white")
  
  # Plot the number of cities in each Sub-region
  ggplot(data = SRcounts, aes(x = `Sub-region Name`, y = n_SR)) +
    theme_minimal(base_size = 11) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = seq(1,13), labels = seq(1,13)) +
    labs(x = "", y = "Number of cities") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank())
  ggsave(filename = "climate/provide_urban_climate_data/cities/providecities-by-subregion.png", width = 12, height = 8, units = "cm", bg = "white")
}

