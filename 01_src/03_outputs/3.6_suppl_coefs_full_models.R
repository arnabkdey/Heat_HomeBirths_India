# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script processes the full models and sensitivity analysis results and saves them to excel
# @date: March 2025


# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "03_outputs", "utils", "function_process_excel_data.R"))

# load data ----
## Full models
output_xlsx <- here(path_project, "outputs", "models", "full_models", "coefs_all_models.xlsx")
df_all_models <- read.xlsx(output_xlsx)

## Sensitivity analysis
output_xlsx <- here(path_project, "outputs", "models", "full_models", "coefs_all_models_sensi.xlsx")
df_all_models_sensi <- read.xlsx(output_xlsx)

# process data ----
## Full models
df_all_models2 <- df_all_models |>
  mutate(Exposure = exposure) |>
  select(Exposure = exposure, exposure_type, OR = estimate, CI_Lower = conf_low, CI_Upper = conf_high) |>
  standardize_exposures() |>
  mutate(OR = round(OR, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2))

## Sensitivity analysis
df_all_models_sensi2 <- df_all_models_sensi |>
  mutate(Exposure = exposure) |>
  select(Exposure = exposure, exposure_type, OR = estimate, CI_Lower = conf_low, CI_Upper = conf_high) |>
  standardize_exposures() |>
  mutate(OR = round(OR, 2), CI_Lower = round(CI_Lower, 2), CI_Upper = round(CI_Upper, 2))

# save to excel in different sheets ----
## Full models
output_full <- here(path_project, "outputs", "tables", "supplementary", "coefs_all_models.xlsx")
save_df_to_excel_by_column(df_all_models2, output_full, "exposure_type", pivot_wider = FALSE)

## Sensitivity analysis
output_sensi <- here(path_project, "outputs", "tables", "supplementary", "coefs_all_models_sensi.xlsx")
save_df_to_excel_by_column(df_all_models_sensi2, output_sensi, "exposure_type", pivot_wider = FALSE)
