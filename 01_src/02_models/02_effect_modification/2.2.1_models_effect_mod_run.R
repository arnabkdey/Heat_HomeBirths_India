# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the effect modification models for the paper.
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here, tictoc)
pacman::p_load(MatchIt, cobalt, sandwich, survey, ggplot2, geepack, performance, splines)

# Set paths ----
source(here("paths_mac.R"))

# read datasets ----
df_paper_final <- readRDS(here(
  path_project, "data", "processed_data", 
  "1.3.1_final_data_for_paper.rds"))

# set constants ----
## Exposure variables
varlist_exposure_all <- c(
  ## Wet bulb temperature (percentile)
  "hotday_wb_80", "hw_wb_80_2d", "hw_wb_80_3d", "hw_wb_80_4d", "hw_wb_80_5d",
  "hotday_wb_825", "hw_wb_825_2d", "hw_wb_825_3d", "hw_wb_825_4d", "hw_wb_825_5d",
  "hotday_wb_85", "hw_wb_85_2d", "hw_wb_85_3d", "hw_wb_85_4d", "hw_wb_85_5d",
  "hotday_wb_875", "hw_wb_875_2d", "hw_wb_875_3d", "hw_wb_875_4d", "hw_wb_875_5d",
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_4d", "hw_wb_90_5d",
  "hotday_wb_925", "hw_wb_925_2d", "hw_wb_925_3d", "hw_wb_925_4d", "hw_wb_925_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_4d", "hw_wb_95_5d",
  # ## Dry bulb temperature (percentile)
  "hotday_db_80", "hw_db_80_2d", "hw_db_80_3d", "hw_db_80_4d", "hw_db_80_5d",
  "hotday_db_825", "hw_db_825_2d", "hw_db_825_3d", "hw_db_825_4d", "hw_db_825_5d",
  "hotday_db_85", "hw_db_85_2d", "hw_db_85_3d", "hw_db_85_4d", "hw_db_85_5d",
  "hotday_db_875", "hw_db_875_2d", "hw_db_875_3d", "hw_db_875_4d", "hw_db_875_5d",
  "hotday_db_90", "hw_db_90_2d", "hw_db_90_3d", "hw_db_90_4d", "hw_db_90_5d",
  "hotday_db_925", "hw_db_925_2d", "hw_db_925_3d", "hw_db_925_4d", "hw_db_925_5d",
  "hotday_db_95", "hw_db_95_2d", "hw_db_95_3d", "hw_db_95_4d", "hw_db_95_5d"
)

## modifier variables
modifier_vars <- c("rural", "hh_access_issue_distance", "lt_mean_wbgt_tert_psu", 
                   "state_janani_bi", "state_home_birth_bi", "mat_edu_level_bi", 
                   "hh_wealth_poorest2", "hh_religion_bi", "hh_caste_bi")

# Prepare data ----
### convert all modifier variables to factors
df_paper_final <- df_paper_final |>
  mutate(across(all_of(modifier_vars), as.factor))

### Relevel modifier variables
df_paper_final$state_home_birth_bi <- relevel(df_paper_final$state_home_birth_bi, ref = "Medium_or_High_HB")
df_paper_final$hh_wealth_poorest2 <- relevel(df_paper_final$hh_wealth_poorest2, ref = "richer3")
df_paper_final$mat_edu_level_bi <- relevel(df_paper_final$mat_edu_level_bi, ref = "primary or higher")
df_paper_final$hh_access_issue_distance <- relevel(df_paper_final$hh_access_issue_distance, ref = "not-a-big-prob")
df_paper_final$hh_caste_bi <- relevel(df_paper_final$hh_caste_bi, ref = "general")

# Create weights ----
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
 
# Run models in a loop ----
# Create empty list to store model results
model_results_list <- list()

# Loop through each exposure variable
for (exposure_var in varlist_exposure_all) {
  # Create weights for this exposure variable
  
  # Fit propensity score model
  ps_formula <- as.formula(paste(exposure_var, "~", 
                                paste(covariates_all, collapse = " + "), 
                                "+ state_name_fac"))
      
  ps_model_state_fe <- glm(ps_formula, data = df_paper_final, 
    family = binomial(link = "logit"))
  
  # Predict propensity scores      
  df_paper_final$ps_score <- predict(ps_model_state_fe, type = "response")
      
  # Trim propensity scores
  df_paper_final$ps_score_trimmed <- ifelse(df_paper_final$ps_score < 
    quantile(df_paper_final$ps_score, 0.01, na.rm = TRUE), 
      quantile(df_paper_final$ps_score, 0.01, na.rm = TRUE), 
    ifelse(df_paper_final$ps_score > 
      quantile(df_paper_final$ps_score, 0.99, na.rm = TRUE), 
        quantile(df_paper_final$ps_score, 0.99, na.rm = TRUE), df_paper_final$ps_score))
  
  # Calculate stabilized weights
  df_paper_final$stabilized_weight <- ifelse(df_paper_final[[exposure_var]] == 1, 
      mean(df_paper_final[[exposure_var]], na.rm = TRUE)/df_paper_final$ps_score_trimmed, 
      mean(1-df_paper_final[[exposure_var]], na.rm = TRUE)/(1-df_paper_final$ps_score_trimmed))
  
  # Loop through each modifier variable
  for (modifier_var in modifier_vars) {
    # Generate model name
    model_name <- paste0(exposure_var, "_X_", modifier_var)
    
    # Generate interaction model formula
    gee_formula <- as.formula(
      paste("dv_home_del_num ~", exposure_var, "*", modifier_var,
        "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
    
    # Try to fit the GEE model with error handling
    tryCatch({
      # Fit the GEE model
      model <- geeglm(
        gee_formula,
        data = df_paper_final,
        id = psu_fac, 
        weights = stabilized_weight,
        family = binomial(link = "logit"),
        corstr = "ar1"
      )
      
      # Store the model in the list
      model_results_list[[model_name]] <- model
      
      # Print progress
      cat("Completed model:", model_name, "\n")
      
    }, error = function(e) {
      # Handle errors
      cat("Error in model:", model_name, "\n")
      cat("Error message:", e$message, "\n")
    })
  }
}

# Save the model results list
model_results_list |> saveRDS(here(
  path_project, "outputs", "models", "effect-modification",
  "gee_interaction_models_main.rds"))

