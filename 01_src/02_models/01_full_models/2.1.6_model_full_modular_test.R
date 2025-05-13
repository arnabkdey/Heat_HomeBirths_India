# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for the paper using modular functions
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, here, tictoc, beepr)
pacman::p_load(MatchIt, cobalt, sandwich, survey, geepack)

# Set paths ----
source(here("paths.R"))

# Source utility functions ----
source(here("01_src", "02_models", "utils", "function_iptw_weights.R"))
source(here("01_src", "02_models", "utils", "function_love_plots.R"))
source(here("01_src", "02_models", "utils", "function_gee_model.R"))
source(here("01_src", "02_models", "utils", "function_full_analysis.R"))

# Read data ----
df_paper_final <- readRDS(here(path_project, "data", "processed_data", 
  "1.3.1_final_data_for_paper.rds"))

# Set constants ----
## Define exposure variables ----
varlist_exposure_all <- c(
  # Wet bulb temperature (absolute)
  # # Wet bulb temperature (percentile)
  "hotday_wb_80", "hw_wb_80_2d", "hw_wb_80_3d", "hw_wb_80_4d", "hw_wb_80_5d")

## Define covariates for direct matching ----
covariates_all <- c("mat_edu_level", "mat_age_grp_at_birth", 
                "mat_parity_fac", "mat_birth_order", 
                "mat_media_exp_any",
                "hh_caste_club",
                "hh_religion_club", "hh_access_issue_distance", 
                "hh_wealth_quintile_ru_og", "rural")

## Convert all covariates to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(covariates_all), as.factor))


# Run full analysis ----
results <- run_full_analysis(
  data = df_paper_final,
  exposure_vars = varlist_exposure_all,
  covariates = covariates_all,
  dv_var = "dv_home_del_num",
  cluster_var = "psu_fac",
  output_dir = here(path_project, "outputs", "models", "full_models", "main_models_test")
) 
