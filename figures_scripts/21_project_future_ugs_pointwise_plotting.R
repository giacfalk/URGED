################
# Create the plots for future SGS / UGS

dfscen <- read_rds("../results/scenarios/dfscen.rds")
ugs <- read_rds("../ugs/ugs_cleaned_100425.rds")


# list_samplecities defined in fcts_labelers_colors.R

################
# Plot the evolution of the scenarios for four sample cities
dfplot <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10, city %in% list_samplecities) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3))
dfplot <- dfplot %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  # Rename "GVI_proj_a" to "upper scenario"
  mutate(scen = factor(scen,
                       levels = c("ugs_scen_impacted", "ugs_scen_mod", "ugs_scen_hgh"),
                       labels = c("Decreased provision", "Moderate ambition", "High ambition")))

# Create a data.frame to show the observed variation in the historical data. It is easier to display this from a new data.frame, and not using dfplot2.
dfplot_histav <- dfplot %>%
  dplyr::filter(year == 2025) %>%
  select(city, year, scen, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.1)

dfplot$labeller <- paste0(dfplot$city, ", KGC = ", dfplot$Cls_short)

ggplot(data = dfplot,
       aes(x = year,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           linetype = scen)) +
  geom_line(aes(y = GVI), alpha = 0.5, linewidth = 0.65) +
  # # Optional: Use geom_segment to show arrows
  # geom_segment(data = arrow_data_a,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #   arrow = arrow(length = unit(0.2, "cm")),
  #   show.legend = FALSE) +
  # geom_segment(data = arrow_data_b,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              show.legend = FALSE) +
  geom_point(data = dfplot %>% filter(year %in% c(2025, 2050)),
             aes(x = year, y = GVI), shape = 1) +
  geom_point(data = dfplot_histav,
             aes(x = year_jittered, y = out_b_mean_st), shape = 5) +
  geom_errorbar(data = dfplot_histav,
                aes(x = year_jittered,
                    ymin = out_b_quart_lwr,
                    ymax = out_b_quart_upr), width = 0.5) +
  # geom_point(aes(y = GVI[length(GVI)-1])) +
  theme_minimal(base_size = 9) +
  facet_wrap(~labeller) +
  theme_minimal() +
  ylab("Street Green Space (GVI)") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050), labels = c("2016-2023", 2025, 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c("dotted", "solid", "twodash")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )
outname <- paste0(path_results, "bylcz_simplenevelope_samplecities.png")
ggsave(filename = outname, width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

###############
# Show the same for ALL CITIES
dfplot_all <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3))
dfplot_all <- dfplot_all %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  # Rename "GVI_proj_a" to "upper scenario"
  mutate(scen = factor(scen,
                       levels = c("ugs_scen_impacted", "ugs_scen_mod", "ugs_scen_hgh"),
                       labels = c("Decreased provision", "Moderate ambition", "High ambition")))

# Create a data.frame to show the observed variation in the historical data. It is easier to display this from a new data.frame, and not using dfplot2.
dfplot_histav_all <- dfplot_all %>%
  dplyr::filter(year == 2020) %>%
  select(city, year, scen, lcz_filter_v3, out_b_mean_st, out_b_min_st, out_b_max_st, starts_with("out_b_quart")) %>%
  distinct() %>%
  group_by(city) %>%
  mutate(year_jittered = 2016 + as.integer(lcz_filter_v3) / 1.1)

# Show the development of GVI in each LCZ for the sample cities.
ggplot(data = dfplot_all,
       aes(x = year,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           linetype = scen)) +
  geom_line(aes(y = GVI), alpha = 0.5, linewidth = 0.65) +
  # # Optional: Use geom_segment to show arrows
  # geom_segment(data = arrow_data_a,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #   arrow = arrow(length = unit(0.2, "cm")),
  #   show.legend = FALSE) +
  # geom_segment(data = arrow_data_b,
  #              aes(x = 2025, xend = 2050, y = y, yend = yend),
  #              arrow = arrow(length = unit(0.2, "cm")),
  #              show.legend = FALSE) +
  geom_point(data = dfplot_all %>% filter(year %in% c(2025, 2050)),
             aes(x = year, y = GVI), shape = 1) +
  geom_point(data = dfplot_histav_all,
             aes(x = year_jittered, y = out_b_mean_st), shape = 5) +
  geom_errorbar(data = dfplot_histav_all,
                aes(x = year_jittered,
                    ymin = out_b_quart_lwr,
                    ymax = out_b_quart_upr), width = 0.5) +
  # geom_point(aes(y = GVI[length(GVI)-1])) +
  theme_minimal(base_size = 9) +
  facet_wrap(~city) +
  theme_minimal() +
  ylab("Street Green Space (GVI)") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c("dotted", "solid", "twodash")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )
outname <- paste0(path_results, "bylcz_simplenevelope_allcities.png")
ggsave(filename = outname, width = 60, height = 90, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

################
# Show number of points byLCZ for each city

test <- ugs %>%
  filter(city == "Los Angeles", lcz_filter_v3 == 6)
test2 <- dfspattemp %>%
  filter(city == "Los Angeles", lcz_filter_v3 == 6)
test3 <- dfplot_all %>%
  filter(city == "Los Angeles", lcz_filter_v3 == "Open lowrise")

ggplot(data = dfplot_all %>% filter(year == 2016), # We need to filter for year, because we otherwise double-count over all observation years..
       aes(x = city,
           y = npid_st,
           # color = lcz_filter_v3,
           fill = lcz_filter_v3,
           group = lcz_filter_v3)) +
  theme_minimal(base_size = 9) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  # scale_y_continuous(labels = scales::comma) +
  labs(y = "Number of observation points per LCZ, averaged over space and time") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 6),
        legend.key.size = unit(0.2, "cm"))
outname <-paste0(path_results, "npoints_by_lcz_allcities.png")
ggsave(filename = outname, width = 30, height = 20, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

###########
# Descriptive statistics of dfscen
## Scatterplot of the projected values
dfplot <- dfspattemp %>%
  dplyr::filter(lcz_filter_v3 <= 10) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  mutate(Cls_short = fct_labeler_Clsmain2(Cls_short))

# Show boxpplot for distribution of urban green by LCZ and Clsmain
ggplot(data = dfplot, aes(x = lcz_filter_v3, y = out_b_mean_st, color = Cls_short)) +
  geom_boxplot() +
  theme_minimal(base_size = 9) +
  # facet_wrap(~Cls_short) +
  scale_color_manual(values = colors_Clsmain) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm")) +
  ylab("Mean observed GVI")
outname <- paste0(path_results, "boxplot_out_b_mean_st.png")
ggsave(filename = outname, width = 16.5, height = 12, units = "cm", bg = "white", dpi = 300)
####

# New plots November 2025
# Plot the "ugs" data: All single points.
dfplot <- ugs %>%
  dplyr::filter(is.na(Cls_short) == FALSE) %>%
  mutate(lcz = lcz_filter_v3,
         Clsmain = Cls_short) %>%
  select(-lcz_filter_v3, -Cls_short)
# Label the LCZ:
dfplot <- fct_labeler_lcz(dfplot)
# Label the climate zones
dfplot <- fct_labeler_Clsmain(dfplot)
  

ggplot(data = dfplot, aes(x = as.factor(Clsmain), y = out_b, color = Clsmain, fill = Clsmain)) +
  geom_violin(quantile.color = "gray30", quantile.linetype = 1) +
  # geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal(base_size = 9) +
  facet_wrap(~lcz) +
  scale_color_manual(values = colors_Clsmain) +
  scale_fill_manual(values = colors_Clsmain) +
  ylim(0, 50) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.size = unit(0.2, "cm")) +
  ylab("Observed GVI at point level")

ggsave(filename = paste0(path_results, "violin_ugsbypoints_by_lcz_Clsmain.png"), width = 16, units = "cm", bg = "white", dpi = 300)

## First plot the distribution of historical GVI by LCZ and Clsmain using violin plots

ggplot(data = dfscen, aes(x = lcz_filter_v3, out_b_mean_st)) +
  geom_violin() +
  facet_grid(~Cls_short)

  


