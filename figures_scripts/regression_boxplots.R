
setwd(stub)

# Load necessary package
library(broom)
library(dplyr)
library(fixest)
library(tidyverse)
library(gg.layers)

# Create some example regression models
models_names <- list.files(path="results/URBCLIM_historical/regressions_", pattern="", full.names = T, recursive=T)
models_names <- models_names[grepl("rds", models_names)]
models <- lapply(models_names, readRDS)

names(models) <- basename(models_names)

# Combine into a single data frame
combined_df <- bind_rows(models, .id="model")
combined_df$model <- gsub("city_monthly_regression_", "", combined_df$model)
combined_df$model <- gsub(".rds", "", combined_df$model)

combined_df$term[combined_df$term=="build_h"] <- "Buldings height"
combined_df$term[combined_df$term=="build_v"] <- "Buldings volume"
combined_df$term[combined_df$term=="elevation"] <- "Elevation"
combined_df$term[combined_df$term=="water"] <- "Water presence"
combined_df$term[combined_df$term=="out_b:lcz3"] <- "Green space, LCZ3"
combined_df$term[combined_df$term=="out_b:lcz5"] <- "Green space, LCZ5"
combined_df$term[combined_df$term=="out_b:lcz6"] <- "Green space, LCZ6"
combined_df$term[combined_df$term=="out_b:lcz8"] <- "Green space, LCZ8"
combined_df$term[combined_df$term=="out_b:lcz9"] <- "Green space, LCZ9"
combined_df$term[combined_df$term=="out_b:lcz2"] <- "Green space, LCZ2"
combined_df$term[combined_df$term=="out_b:lcz4"] <- "Green space, LCZ3"
combined_df$term[combined_df$term=="out_b:lcz1"] <- "Green space, LCZ1"
combined_df$term[combined_df$term=="out_b:lcz7"] <- "Green space, LCZ7"

combined_df$term <- factor(combined_df$term, levels=c("Buldings height", "Buldings volume", "Elevation", "Water presence", "Green space, LCZ1", "Green space, LCZ2", "Green space, LCZ3", "Green space, LCZ4", "Green space, LCZ5", "Green space, LCZ6", "Green space, LCZ7", "Green space, LCZ8", "Green space, LCZ9"))

combined_df$var <- ifelse(grepl("wbgt", combined_df$model), "WBGT", "T")
combined_df$variant <- ifelse(grepl("max", combined_df$model), "Max", ifelse(grepl("min", combined_df$model), "Min", "Mean"))

###

combined_df$city <- sub("_.*", "", combined_df$model)

###

ggplot(combined_df %>% filter(var=="WBGT" & variant=="Max"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, maximum WBGT")

ggsave("paper/boxplot_max_wbgt.pdf", width=6, height=4, scale=1.5)

###

ggplot(combined_df %>% filter(var=="WBGT" & variant=="Min"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, minimum WBGT")

ggsave("paper/boxplot_min_wbgt.pdf", width=6, height=4, scale=1.5)


###

ggplot(combined_df %>% filter(var=="WBGT" & variant=="Mean"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, mean WBGT")

ggsave("paper/boxplot_mean_wbgt.pdf", width=6, height=4, scale=1.5)

###

ggplot(combined_df %>% filter(var=="T" & variant=="Max"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, maximum temperature")

ggsave("paper/boxplot_max_t.pdf", width=6, height=4, scale=1.5)

###

ggplot(combined_df %>% filter(var=="T" & variant=="Min"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, minimum temperature")

ggsave("paper/boxplot_min_t.pdf", width=6, height=4, scale=1.5)


###

ggplot(combined_df %>% filter(var=="T" & variant=="Mean"))+
  theme_classic()+
  geom_boxplot2(aes(y=estimate, fill=term), width.errorbar = 0.2, show.legend = F)+
  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(vars(term), scales="free_y")+
  ggtitle("Distribution of regression coefficients across cities, mean temperature")

ggsave("paper/boxplot_mean_t.pdf", width=6, height=4, scale=1.5)

