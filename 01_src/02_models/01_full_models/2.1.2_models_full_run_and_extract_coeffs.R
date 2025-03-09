# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for the paper and extracts coefficients for the final paper.
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, here, tictoc, beepr)
pacman::p_load(MatchIt, cobalt, sandwich, survey, geepack)

# Set paths ----
source(here("paths.R"))

# read datasets ----
df_paper_final <- readRDS(here(path_project, "data", "processed_data", 
  "1.3.1_final_data_for_paper.rds"))

# source functions to extract coefficients ----
source(here("src", "02_models", "utils", "function_to_extract_full_model_results.R"))

# set constants ----
## Define exposure variables ----
varlist_exposure_all <- c(
  # Wet bulb temperature (absolute)
  # # Wet bulb temperature (percentile)
  "hotday_wb_80", "hw_wb_80_2d", "hw_wb_80_3d", "hw_wb_80_4d", "hw_wb_80_5d",
  "hotday_wb_825", "hw_wb_825_2d", "hw_wb_825_3d", "hw_wb_825_4d", "hw_wb_825_5d",
  "hotday_wb_85", "hw_wb_85_2d", "hw_wb_85_3d", "hw_wb_85_4d", "hw_wb_85_5d",
  "hotday_wb_875", "hw_wb_875_2d", "hw_wb_875_3d", "hw_wb_875_4d", "hw_wb_875_5d",
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_4d", "hw_wb_90_5d",
  "hotday_wb_925", "hw_wb_925_2d", "hw_wb_925_3d", "hw_wb_925_4d", "hw_wb_925_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_4d", "hw_wb_95_5d",
  ## Dry bulb temperature (percentile)
  "hotday_db_80", "hw_db_80_2d", "hw_db_80_3d", "hw_db_80_4d", "hw_db_80_5d",
  "hotday_db_825", "hw_db_825_2d", "hw_db_825_3d", "hw_db_825_4d", "hw_db_825_5d",
  "hotday_db_85", "hw_db_85_2d", "hw_db_85_3d", "hw_db_85_4d", "hw_db_85_5d",
  "hotday_db_875", "hw_db_875_2d", "hw_db_875_3d", "hw_db_875_4d", "hw_db_875_5d",
  "hotday_db_90", "hw_db_90_2d", "hw_db_90_3d", "hw_db_90_4d", "hw_db_90_5d",
  "hotday_db_925", "hw_db_925_2d", "hw_db_925_3d", "hw_db_925_4d", "hw_db_925_5d",
  "hotday_db_95", "hw_db_95_2d", "hw_db_95_3d", "hw_db_95_4d", "hw_db_95_5d"
)

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

# Prepare datasets ----
df_effects <- list(df_paper_final)
names(df_effects) <- c("alldata")

# Setup progress tracking ----
total_dfs <- length(names(df_effects))
total_exposures <- length(varlist_exposure_all)
total_combinations <- total_dfs * total_exposures
processed_combinations <- 0
successful_models <- 0
skipped_models <- 0

cat("\n===============================================================\n")
cat("Starting model fitting for", total_combinations, "total combinations\n")
cat("===============================================================\n")
cat("Datasets:", total_dfs, "\n")
cat("Exposure variables:", total_exposures, "\n\n")

# Start overall timer
tic("Total execution time")

# Model fitting ----
## Initialize an empty list to store all models
all_models <- list()

## Loop over all combinations of df_effect, exposure_var
for (df_idx in 1:length(names(df_effects))) {
  df_name <- names(df_effects)[df_idx]
  
  cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing dataset", df_idx, "of", total_dfs, ":", df_name, "\n")
  
  ### Make a copy of covariates_all to restore at the end of each dataset iteration
  covariates_current <- covariates_all
  
  ### Extract the current dataset
  df_cur <- df_effects[[df_name]]
  cat("  - Working with", format(nrow(df_cur), big.mark=","), "observations\n")
  
  # Start timer for this dataset
  tic(paste("Dataset:", df_name))
  
  # Track skipped combinations for this dataset
  dataset_skipped <- 0
  dataset_success <- 0
  
  for (exp_idx in 1:length(varlist_exposure_all)) {
      exposure_var <- varlist_exposure_all[exp_idx]
      processed_combinations <- processed_combinations + 1
      
      # Calculate overall progress percentage
      progress_pct <- round(processed_combinations / total_combinations * 100, 1)
      
      cat("  [", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
          ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
      
      # Check if exposure_var has at least two unique values
      exposure_values <- table(df_cur[[exposure_var]])
      if (length(exposure_values) < 2) {
        warning(paste("    Skipping combination:", df_name, exposure_var, 
                      "because exposure_var has only one unique value."))
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next  # Skip to the next iteration
      }
      
      # Show distribution of exposure variable
      cat("    - Exposure distribution:", paste(names(exposure_values), "=", exposure_values, collapse=", "), "\n")

      # Create weights ----
      cat("    - Fitting propensity score model...\n")
      
      ## Fit logistic regression model for propensity scores
      ps_formula <- as.formula(paste(exposure_var, "~", 
                                     paste(covariates_current, collapse = " + "), 
                                     "+ state_name_fac"))
      
      ps_model_state_fe <- tryCatch({
        glm(ps_formula, data = df_cur, family = binomial(link = "logit"))
      }, error = function(e) {
        cat("    *** ERROR in propensity score model:", conditionMessage(e), "\n")
        return(NULL)
      })
      
      if (is.null(ps_model_state_fe)) {
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next
      }
      
      df_cur$ps_score <- predict(ps_model_state_fe, type = "response")
      
      ## Trim propensity scores
      ps_quantiles <- quantile(df_cur$ps_score, c(0.01, 0.99), na.rm = TRUE)
      cat("    - Propensity score range:", 
          sprintf("min=%.4f, q01=%.4f, q99=%.4f, max=%.4f", 
                  min(df_cur$ps_score, na.rm=TRUE),
                  ps_quantiles[1],
                  ps_quantiles[2],
                  max(df_cur$ps_score, na.rm=TRUE)), "\n")
      
      df_cur$ps_score_trimmed <- ifelse(
        df_cur$ps_score < ps_quantiles[1], 
        ps_quantiles[1], 
        ifelse(df_cur$ps_score > ps_quantiles[2], 
               ps_quantiles[2], df_cur$ps_score))
      
      ## Calculate stabilized weights
      exposure_mean <- mean(df_cur[[exposure_var]], na.rm = TRUE)
      df_cur$stabilized_weight <- ifelse(df_cur[[exposure_var]] == 1, 
                                        exposure_mean/df_cur$ps_score_trimmed, 
                                        (1-exposure_mean)/(1-df_cur$ps_score_trimmed))
      
      # Check weight distribution
      weight_quantiles <- quantile(df_cur$stabilized_weight, c(0.01, 0.25, 0.5, 0.75, 0.99), na.rm = TRUE)
      cat("    - Weight distribution:", 
          sprintf("min=%.2f, q01=%.2f, median=%.2f, q99=%.2f, max=%.2f", 
                  min(df_cur$stabilized_weight, na.rm=TRUE),
                  weight_quantiles[1],
                  weight_quantiles[3],
                  weight_quantiles[5],
                  max(df_cur$stabilized_weight, na.rm=TRUE)), "\n")
      
      # Modelling ----
      cat("    - Fitting GEE model...\n")
      
      ## GEE model with stabilized weights
      gee_formula <- as.formula(paste("dv_home_del_num ~", exposure_var,
                                     "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
      
      gee_model <- tryCatch({
        geeglm(
          gee_formula,
          data = df_cur,
          id = psu_fac, 
          weights = stabilized_weight,
          family = binomial(link = "logit"),
          corstr = "ar1"
        )
      }, error = function(e) {
        cat("    *** ERROR in GEE model:", conditionMessage(e), "\n")
        return(NULL)
      })
      
      if (is.null(gee_model)) {
        skipped_models <- skipped_models + 1
        dataset_skipped <- dataset_skipped + 1
        next
      }
      
      # Get model summary and show key statistics
      model_summary <- summary(gee_model)
      exposure_coef <- coef(model_summary)[exposure_var, ]
      
      cat("    - Model results for", exposure_var, ":", 
          sprintf("coef=%.3f, SE=%.3f, p=%.4f", 
                  exposure_coef[1], 
                  exposure_coef[2], 
                  exposure_coef[4]), "\n")
      
      # Store the model in the list with a unique name
      model_name <- paste("gee-model", df_name, exposure_var, sep = "-")
      all_models[[model_name]] <- gee_model
      successful_models <- successful_models + 1
      dataset_success <- dataset_success + 1
  }
  
  # End timer for this dataset
  toc()
  
  cat("Dataset", df_name, "completed:", 
      dataset_success, "successful models,", 
      dataset_skipped, "skipped models.\n")
}

# End total timer
toc()

# Get output paths
output_rds <- here(path_project, "outputs", "models", "full_models", "models_all.rds")
output_xlsx <- here(path_project, "outputs", "models", "full_models", "coefs_all_models.xlsx")

# Print final summary
cat("\n===============================================================\n")
cat("Final Summary\n")
cat("===============================================================\n")
cat("Total datasets processed:", total_dfs, "\n")
cat("Total exposure variables:", total_exposures, "\n")
cat("Total combinations:", total_combinations, "\n")
cat("Successful models:", successful_models, "\n")
cat("Skipped combinations:", skipped_models, "\n")
cat("\n")

# Save all models as a single list
cat("Saving all models to:", basename(output_rds), "\n")
saveRDS(all_models, output_rds)

# Extract coefficients ----
cat("\nExtracting coefficients and saving to Excel...\n")
tic("Coefficient extraction")
process_models_to_excel(output_rds, output_xlsx)
toc()

cat("\nComplete! All results saved to:", basename(output_xlsx), "\n")

# Optional: add a beep to signal completion
try(beepr::beep(sound = 'ping'), silent = TRUE)

