# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script runs the full models for the paper and checks covariate balance using Love plots.
# @date: March 2025

# Load Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here, tictoc, beepr)
pacman::p_load(MatchIt, cobalt, sandwich, survey, ggplot2, geepack)

# Set paths ----
source(here("paths_mac.R"))

# read datasets ----
df_paper_final <- readRDS(here(path_project, "data", 
  "processed_data", 
  "1.3.1_final_data_for_paper.rds"))

# set constants ----
## Define exposure variables ----
varlist_exposure_all <- c(
  # Wet bulb temperature (percentile)
  "hotday_wb_85", "hw_wb_85_2d", "hw_wb_85_3d", "hw_wb_85_4d", "hw_wb_85_5d",
  "hotday_wb_875", "hw_wb_875_2d", "hw_wb_875_3d", "hw_wb_875_4d", "hw_wb_875_5d",
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_4d", "hw_wb_90_5d",
  "hotday_wb_925", "hw_wb_925_2d", "hw_wb_925_3d", "hw_wb_925_4d", "hw_wb_925_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_4d", "hw_wb_95_5d",
  ## Dry bulb temperature (percentile)
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

cat("Starting processing of", total_combinations, "total combinations\n")
cat("===========================================================\n")

# Model fitting ----
## Initialize an empty list to store all models
all_models <- list()

# Start overall timer
tic("Total execution time")

## Loop over all combinations of df_effect, exposure_var
for (df_idx in 1:length(names(df_effects))) {
  df_name <- names(df_effects)[df_idx]
  
  cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing dataset", df_idx, "of", total_dfs, ":", df_name, "\n")
  
  ### Make a copy of covariates_all to restore at the end of each dataset iteration
  covariates_current <- covariates_all
  
  ### Extract the current dataset
  df_cur <- df_effects[[df_name]]
  
  # Start timer for this dataset
  tic(paste("Dataset:", df_name))
  
  # Track skipped combinations for this dataset
  skipped_count <- 0
  
  for (exp_idx in 1:length(varlist_exposure_all)) {
      exposure_var <- varlist_exposure_all[exp_idx]
      processed_combinations <- processed_combinations + 1
      
      # Calculate overall progress percentage
      progress_pct <- round(processed_combinations / total_combinations * 100, 1)
      
      cat("  [", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
          ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
      
      # Check if exposure_var has at least two unique values
      if (length(unique(df_cur[[exposure_var]])) < 2) {
        warning(paste("  Skipping combination:", df_name, exposure_var, 
                      "because exposure_var has only one unique value."))
        skipped_count <- skipped_count + 1
        next  # Skip to the next iteration
      }

      # Create weights ----
      cat("    - Fitting propensity score model...\n")
      
      ## Fit logistic regression model for propensity scores
      ps_model_state_fe <- glm(as.formula(paste(exposure_var, "~", 
        paste(covariates_current, collapse = " + "), "+ state_name_fac")),
      data = df_cur,
      family = binomial(link = "logit"))
      
      df_cur$ps_score <- predict(ps_model_state_fe, type = "response")
      
      ## Trim propensity scores
      df_cur$ps_score_trimmed <- ifelse(
        df_cur$ps_score < quantile(df_cur$ps_score, 0.01, na.rm = TRUE), 
          quantile(df_cur$ps_score, 0.01, na.rm = TRUE), 
          ifelse(df_cur$ps_score > quantile(df_cur$ps_score, 0.99, na.rm = TRUE), 
                  quantile(df_cur$ps_score, 0.99, na.rm = TRUE), df_cur$ps_score))
      
      ## Calculate stabilized weights
      df_cur$stabilized_weight <- ifelse(df_cur[[exposure_var]] == 1, 
          mean(df_cur[[exposure_var]], na.rm = TRUE)/df_cur$ps_score_trimmed, 
          mean(1-df_cur[[exposure_var]], na.rm = TRUE)/(1-df_cur$ps_score_trimmed))
      
      # Check balance ----
      cat("    - Creating Love plot...\n")
      
      ## Create the Love plot
      setDT(df_cur)
      love_plot <- love.plot(
        df_cur |> select(all_of(covariates_current)), 
        treat = df_cur[[exposure_var]],
        weights = df_cur$stabilized_weight,
        binary = "std",
        threshold = 0.1,
        abs = FALSE,
        var.order = "alphabetical",
        colours = c("#E41A1C", "#377EB8"),  # Colors for unweighted and weighted
        shapes = c(16, 17),  # Shapes for unweighted and weighted
        size = 3,
        position = "center",
        title = "Covariate Balance Before and After Weighting",
        sample.names = c("Unweighted", "Weighted"),  # Names for the legend
        line = FALSE
      )
      
      ## Customize the plot to move the legend to the bottom
      love_plot <- love_plot +
        theme(
          legend.position = "bottom",  # Move legend to the bottom
          legend.title = element_blank(),  # Remove legend title
          legend.text = element_text(size = 12),  # Adjust legend text size
          legend.key.size = unit(1.5, "lines")  # Adjust legend key size
        )
      
      # Save the plot ----
      output_path <- here(path_project, "outputs", "figures", "love_plots",
             paste0("love-", df_name, "-", exposure_var, ".png"))
      
      cat("    - Saving plot to:", basename(output_path), "\n")
      
      ggsave(
        output_path, 
        plot = love_plot, 
        width = 20, 
        height = 16, 
        units = "in",
        dpi = 300,
        bg = "white"
      )
  }
  
  # End timer for this dataset
  toc()
  
  cat("Dataset", df_name, "completed. Skipped", skipped_count, "of", total_exposures, "exposure variables.\n")
}

# End total timer
toc()

# Print final summary
cat("\n=== Final Summary ===\n")
cat("Total datasets processed:", total_dfs, "\n")
cat("Total exposure variables:", total_exposures, "\n")
cat("Total combinations processed:", processed_combinations, "\n")
cat("Complete!\n")

# Optional: add a beep to signal completion
try(beepr::beep(sound = 'ping'), silent = TRUE)