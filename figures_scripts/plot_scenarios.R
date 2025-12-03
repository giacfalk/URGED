# Plot the subplots for the scenarios. These will be assembled in a different graphics software later.

dfscen <- read_rds("../results/scenarios/dfscen.rds")
ugs <- read_rds("../ugs/ugs_cleaned_100425.rds")

# LCV evolution plot for ONE city, much simplified. Top panel for paper
samplecity <-  "Vienna"
samplelczs <- c("Open lowrise", "Large lowrise", "Compact midrise")
citytest <- dfscen %>%
  dplyr::filter(lcz_filter_v3 <= 10,
                city == samplecity) %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  filter(lcz_filter_v3 %in% samplelczs) %>%
  select(lcz_filter_v3, Cls_short, lczshare_st) %>%
  distinct()

dfplot <- dfscen %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  mutate(Cls_short = fct_labeler_Clsmain2(Cls_short)) %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  filter(city == samplecity) %>%
  # filter(lcz_filter_v3 %in% samplelczs) %>%
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

#######################################
# First, violin plot
ugsplot <- ugs %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  # mutate(lcz_filter_v3 = factor(lcz_filter_v3, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, NA),
  #                               labels = c("Compact hi", "Compact mid", "Compact lo", "Open hi", "Open mid", "Open lo", "Light lo", "Large lo", "Sparse",
  #                                          "Heavy industry", "Dense trees", "Scattered trees",
  #                                          "Bush, scrub", "Low plants", "Bare rock", "Bare soil",
  #                                          "Water"),
  #                               ordered = T)) %>%
  filter(city == samplecity) %>%
  filter(lcz_filter_v3 %in% samplelczs)
  
colors_lcz_vienna <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6", "#6a3d9a")
colors_lcz_vienna_LCZ3 <- c("#1f78b4", "#e31a1c", "#ff7f00")


ggplot(data = ugsplot, aes(y = out_b, color = lcz_filter_v3,
                           fill = lcz_filter_v3)) +
  theme_minimal(base_size = 9) +
  geom_violin(aes(x = lcz_filter_v3), quantile.color = "gray30", quantile.linetype = 1) +
  ylab("Street Green Space (GVI)") +
  # Limit y axis between 0 and 50
  ylim(0,45) +
  scale_fill_manual(values = colors_lcz_vienna) +
  scale_color_manual(values = colors_lcz_vienna) +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # Legend to bottom
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9),
        plot.margin = margin(t = 2.5, r = 0, b = 15, l = 0, unit = "pt"))

outname <- paste0(path_results, "bylcz_violin-allpoints_samplelcz_vienna_3LCZ")
ggsave(filename = paste0(outname, ".png"),
       width = .5 * overleafwidth, height = 10, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)
ggsave(filename = paste0(outname, ".svg"),
       width = .5 * overleafwidth, height = 10, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)

# Second, line plot with scenarios
ggplot(data = dfplot,
       aes(x = year,
           color = lcz_filter_v3,
           fill = lcz_filter_v3,
           linetype = scen)) +
  geom_line(aes(y = GVI), alpha = 0.9, linewidth = 0.5) +
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
             aes(x = year, y = GVI), shape = 21, size = 1.5) +
  theme_minimal(base_size = 9) +
  theme_minimal() +
  ylim(0,45) +
  # No y-axis tick labels
  scale_fill_manual(values = colors_lcz_vienna) +
  scale_color_manual(values = colors_lcz_vienna) +
  scale_x_continuous(breaks = c(2025, 2030, 2035, 2040, 2045, 2050),
                     labels = c("2025", 2030, 2035, 2040, 2045, 2050)) +
  scale_linetype_manual(values = c("dotted", "solid", "twodash")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # legend.key.size = unit(0.2, "cm"),
        plot.margin = margin(t = 2.5, r = 0, b = 15, l = 0, unit = "pt"),
        panel.grid.minor.x = element_blank()) +  # Remove vertical minor grid lines
  guides(
    color = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    fill = guide_legend(order = 1, nrow = 3),          # Ensure color is first
    linetype = guide_legend(order = 2, nrow = 3)        # Ensure linetype is second
  )

outname <- paste0(path_results, "bylcz_simplenevelope-allpoints_samplelcz_vienna")
ggsave(filename = paste0(outname, ".png"), width = 0.5*overleafwidth, height = 10, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)
ggsave(filename = paste0(outname, ".svg"), width = 0.5*overleafwidth, height = 10, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)


# Now plot the whole dataset of scenarios for all cities, faceted by lcz and KGC

dfplot <- dfscen %>%
  mutate(lcz_filter_v3 = fct_labeler_lcz2(lcz_filter_v3)) %>%
  mutate(Cls_short = fct_labeler_Clsmain2(Cls_short)) %>% # For using linetype as legend, we need to further modify dfplot and make it long.
  pivot_longer(cols = starts_with("ugs_scen"), names_to = "scen", values_to = "GVI") %>%
  # filter(city == samplecity) %>%
  # filter(lcz_filter_v3 %in% samplelczs) %>%
  # Rename "GVI_proj_a" to "upper scenario"
  filter(year %in% c(2025, 2050)) %>%
  filter(!is.na(Cls_short))

# outb2025 <- dfplot %>%
#   filter(year == 2025) %>%
#   pivot_wider(names_from = year, values_from = out_b_mean_st) %>%
#   select(-GVI, -scen) %>%
#   mutate(GVI = `2025`, scen = "Baseline") %>%
#   select(-`2025`)

outb2025 <- dfplot %>%
  filter(year == 2025) %>%
  mutate(GVI = out_b_mean_st,
         scen = "Baseline") %>%
  select(-year)

dfplot <- dfplot %>%
  filter(year == 2050) %>%
  select(-year)

dfplot2 <- rbind(dfplot, outb2025)
dfplot2 <- dfplot2 %>%
  mutate(scen = factor(scen,
                       levels = c("Baseline", "ugs_scen_impacted", "ugs_scen_mod", "ugs_scen_hgh"),
                       labels = c("Baseline", "Decreased Provision", "Moderate Ambition", "High Ambition")))
  



ggplot(data = dfplot2, aes(x = scen, y = GVI, color = lcz_filter_v3)) +
  theme_minimal(base_size = 9) +
  geom_boxplot() +
  facet_grid(lcz_filter_v3~Cls_short) +
  # scale_fill_manual(values = colors_lcz) +
  scale_color_manual(values = colors_lcz) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))
  

ggsave(filename = paste0(path_results, "bylcz_simplenevelope-boxplots_allcities_alllcz_allkgc.png"),
       width = overleafwidth, height = 20, units = "cm", bg = "white", dpi = 300, limitsize = FALSE)
