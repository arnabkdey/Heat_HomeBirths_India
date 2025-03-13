# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script processes the effect modification results and saves them to excel
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, readxl, writexl, here)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "03_outputs", "utils", "function_process_excel_data.R"))

# load data ----
## WBGT ----
### Odds Ratios
file_path_wb_or <- here(path_project, "outputs", 
                     "models", "effect_modification", "results_suppl", "wbgt",
                     "subgroup_odds_ratios.xlsx")

df_wb_or <- read_excel_sheets(file_path_wb_or) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, OR, CI_Lower = OR_LowerCI, CI_Upper = OR_UpperCI) |>
  mutate(OR = round(OR, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2)) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))

### Interaction p-values
file_path_pvals_wb <- here(path_project, "outputs", 
                        "models", "effect_modification", "results_suppl", "wbgt", 
                        "subgroup_interaction_pvalues.xlsx")

df_pvals_wb <- read_excel_sheets(file_path_pvals_wb) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, Interaction_P) |>
  mutate(PValue = round(Interaction_P, 3)) |>
  select(-Interaction_P) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))


### RERI
file_path_wb_reri <- here(path_project, "outputs", 
                       "models", "effect_modification", "results_suppl", "wbgt",
                       "subgroup_reri.xlsx")

df_wb_reri <- read_excel_sheets(file_path_wb_reri) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, RERI, CI_Lower = RERI_LowerCI, CI_Upper = RERI_UpperCI) |>
  mutate(RERI = round(RERI, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2)) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))

## Dry Bulb ----
### Odds Ratios 
file_path_db_or <- here(path_project, "outputs", 
                     "models", "effect_modification", "results_suppl", "db",
                     "subgroup_odds_ratios.xlsx")

df_db_or <- read_excel_sheets(file_path_db_or) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, OR, CI_Lower = OR_LowerCI, CI_Upper = OR_UpperCI) |>
  mutate(OR = round(OR, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2)) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))

### Interaction p-values
file_path_pvals_db <- here(path_project, "outputs", 
                        "models", "effect_modification", "results_suppl", "db", 
                        "subgroup_interaction_pvalues.xlsx")

df_pvals_db <- read_excel_sheets(file_path_pvals_db) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, Interaction_P) |>
  mutate(PValue = round(Interaction_P, 3)) |>
  select(-Interaction_P) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))

### RERI 
file_path_db_reri <- here(path_project, "outputs", 
                       "models", "effect_modification", "results_suppl", "db",
                       "subgroup_reri.xlsx")

df_db_reri <- read_excel_sheets(file_path_db_reri) |>
  standardize_modifiers() |>
  standardize_levels() |>
  standardize_exposures() |>
  select(Exposure, Modifier, ModifierLevel, RERI, CI_Lower = RERI_LowerCI, CI_Upper = RERI_UpperCI) |>
  mutate(RERI = round(RERI, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2)) |>
  mutate(Modifier = str_replace(Modifier, "^[A-Z]\\. ", ""))

# Save dataframes to Excel with multiple sheets ----

## WBGT ----
### Odds Ratios
output_file_wb_or <- here(path_project, "outputs", 
                          "tables", "supplementary", 
                          "EM_wb_or_wide.xlsx")
save_df_to_excel_by_column(df_wb_or, 
  output_file_wb_or, "Modifier", pivot_wider = TRUE)

### Interaction p-values
output_file_pvals_wb <- here(path_project, "outputs", 
                          "tables", "supplementary", 
                          "EM_wb_pvals.xlsx")
                          
save_df_to_excel_by_column(df_pvals_wb,
  output_file_pvals_wb, "Modifier", pivot_wider = FALSE)

### RERI
output_file_wb_reri <- here(path_project, "outputs", 
                            "tables", "supplementary",
                            "EM_wb_reri.xlsx")

save_df_to_excel_by_column(df_wb_reri, 
  output_file_wb_reri, "Modifier", pivot_wider = FALSE)


## Dry Bulb
### Odds Ratios
output_file_db_or <- here(path_project, "outputs", 
                          "tables", "supplementary", 
                          "EM_db_or_wide.xlsx")
save_df_to_excel_by_column(df_db_or, 
  output_file_db_or, "Modifier", pivot_wider = TRUE)

### Interaction p-values
output_file_pvals_db <- here(path_project, "outputs", 
                          "tables", "supplementary", 
                          "EM_db_pvals.xlsx")

save_df_to_excel_by_column(df_pvals_db,
  output_file_pvals_db, "Modifier", pivot_wider = FALSE) 

### RERI
output_file_db_reri <- here(path_project, "outputs", 
                            "tables", "supplementary",
                            "EM_db_reri.xlsx")

save_df_to_excel_by_column(df_db_reri, 
  output_file_db_reri, "Modifier", pivot_wider = FALSE)
