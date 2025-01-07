rm(list =ls())
options(scipen=999)
options(digits=5)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(sjPlot)
library(readxl)
library(here)
library(extrafont)
library(ggbreak)
library(patchwork)
source("paths-mac.R")

# font_import() # run only once
# loadfonts(device="win") # run only once

# Load data ----
here_output_files <- here(path_project, "outputs", "models", "models-no-interaction")
df_full_models <- read.csv(here(here_output_files, "models_consolidated_coefficients.csv"))
head(df_full_models)

# Data Processing 

## Process the coefficients ----
### Drop missing rows in the dataframe
df_full_models <- df_full_models |> drop_na()

### Drop rows that contain the string "harmo" in the term column
df_full_models <- df_full_models |> filter(!str_detect(term, "harmo"))
View(df_full_models)

## Create Labels ----
### For duration
labels_duration <- c("Extreme heat day", "Heatwave: 2 days", "Heatwave: 3 days", "Heatwave: 5 days")
#### repeat the label 6 times
labels_duration <- rep(labels_duration, 6)

### For thresholds
labels_threshold_perc <- c("WBGTmax >= 90th Percentile", "WBGTmax >= 95th Percentile", "WBGTmax >= 97th Percentile")
labels_threshold_perc <- rep(labels_threshold_perc, each = 4)
labels_threshold_abs <- c("WBGTmax >= 30°C", "WBGTmax >= 31°C", "WBGTmax >= 32°C")
labels_threshold_abs <- rep(labels_threshold_abs, each = 4)
#### combine these labels and repeat the label 4 times
labels_threshold_comb <- c(labels_threshold_abs, labels_threshold_perc)

## Add labels to the dataframe ----
df_full_models$duration_label <- labels_duration
df_full_models$threshold_label <- labels_threshold_comb
View(df_full_models)

## Set order of duration variable ----
## Order the levels of the Contrast variable
ord_duration <- c("Extreme heat day", "Heatwave: 2 days", "Heatwave: 3 days", "Heatwave: 5 days")
df_full_models$duration_label <- factor(df_full_models$duration_label, levels=ord_duration)
df_full_models$duration_label <- fct_reorder(df_full_models$duration_label, desc(df_full_models$duration_label))

# Plotting ----
## Subset data ----
df_plot_abs <- df_full_models |> filter(str_detect(threshold_label, "°C"))
df_plot_perc <- df_full_models |> filter(str_detect(threshold_label, "9"))

## Call the function to plot ----
source(here("1-scripts", "6.3-function-to-plot-full-models.R"))

## Plotting ----
plot_abs <- func_plot_full_model(df_plot_abs)
plot_perc <- func_plot_full_model(df_plot_perc)

## Save the plots ----
ggsave(here(path_project, "outputs", "figures", "full_model_abs.svg"), plot_abs, width = 8, height = 10, dpi = 600)
ggsave(here(path_project, "outputs", "figures", "full_model_perc.svg"), plot_perc, width = 8, height = 10, dpi = 600)


# Plot for the defense PPT
## Separate plots for 30, 31, and 32°C
df_abs_30 <- df_full_models |> filter(str_detect(threshold_label, "30"))
df_abs_31 <- df_full_models |> filter(str_detect(threshold_label, "31"))
df_abs_32 <- df_full_models |> filter(str_detect(threshold_label, "32"))

## Call the function to plot ----
plot_abs_30 <- func_plot_full_model(df_abs_30)
plot_abs_31 <- func_plot_full_model(df_abs_31)
plot_abs_32 <- func_plot_full_model(df_abs_32)

## Save the plots ----
ggsave(here(path_project, "outputs", "figures", "full_model_abs_30.png"), 
              plot_abs_30, width = 8, height = 4, dpi = 300, bg = "white")
ggsave(here(path_project, "outputs", "figures", "full_model_abs_31.png"),
              plot_abs_31, width = 8, height = 4, dpi = 300, bg = "white")
ggsave(here(path_project, "outputs", "figures", "full_model_abs_32.png"),
              plot_abs_32, width = 8, height = 4, dpi = 300, bg = "white")
