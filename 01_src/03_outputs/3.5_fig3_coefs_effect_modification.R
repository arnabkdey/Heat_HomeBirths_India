# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the odds ratios and RERI for the effect modification analysis
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, readxl, ggpubr, cowplot)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "03_outputs", "utils", "function_EM_plots.R"))
source(here("01_src", "03_outputs", "utils", "function_process_excel_data.R"))

# load data ----
## odds ratios ----
file_path_or <- here(path_project, "outputs", 
                     "models", "effect_modification", "results_main",
                     "subgroup_odds_ratios.xlsx")

df_or <- read_excel_sheets(file_path_or) |>
  standardize_modifiers() |>
  standardize_levels()

## RERI ----
file_path_reri <- here(path_project, "outputs", 
                       "models", "effect_modification", "results_main",
                       "subgroup_reri.xlsx")

df_reri <- read_excel_sheets(file_path_reri) |>
  standardize_modifiers() |>
  standardize_levels()

# Filter by exposure type
df_or_wb <- df_or |> filter(Exposure == "hw_wb_85_4d")
df_or_db <- df_or |> filter(Exposure == "hw_db_85_4d")
df_reri_wb <- df_reri |> filter(Exposure == "hw_wb_85_4d")
df_reri_db <- df_reri |> filter(Exposure == "hw_db_85_4d")

# Define modifier categories
modifiers <- c(
  "A. Religion", 
  "B. Marginalized caste", 
  "C. Woman's education", 
  "D. Household wealth", 
  "E. Residence", 
  "F. Distance is a big problem", 
  "G. JSY high-focus state", 
  "H. State home birth rate", 
  "I. Long-term temperature"
)

# Create plots ----
## Wet bulb plots
final_plots_wb <- create_modifier_plots(df_or_wb, df_reri_wb, modifiers)

## Combine all wet bulb plots
all_final_plots_wb <- plot_grid(plotlist = final_plots_wb, ncol = 3, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

## Save wet bulb plot
ggsave(here(path_project, "outputs", "figures", "Plot_EM_wbgt.pdf"),
       plot = all_final_plots_wb, 
       width = 7.5, 
       height = 8, 
       dpi = 600)

## Dry bulb plots
final_plots_db <- create_modifier_plots(df_or_db, df_reri_db, modifiers)

## Combine all dry bulb plots
all_final_plots_db <- plot_grid(plotlist = final_plots_db, ncol = 3, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

## Save dry bulb plot
# ggsave(here(path_project, "outputs", "figures", "Plot_EM_db.svg"), 
#        plot = all_final_plots_db, 
#        width = 8, 
#        height = 8, 
#        dpi = 600)

# Save consolidated data ----
write_csv(df_or, here(path_project, "outputs", "models", "effect_modification", "results_main", "subgroup_ORs_consolidated.csv"))
write_csv(df_reri, here(path_project, "outputs", "models", "effect_modification", "results_main", "subgroup_reri_consolidated.csv"))