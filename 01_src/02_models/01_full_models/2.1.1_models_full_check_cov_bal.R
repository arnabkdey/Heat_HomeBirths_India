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
source(here("paths.R"))

# read datasets ----
df_paper_final <- readRDS(here(path_project, "data", 
  "processed_data", 
  "1.3.1_final_data_for_paper.rds"))
nrow(df_paper_final)
# set constants ----
## Define exposure variables ----
varlist_exposure_all <- c(
  # Wet bulb temperature (percentile)
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
      
      ## Define variable labels
      var_labels <- c(
        "hh_access_issue_distance_not-a-big-prob" = "Distance not a big problem for healthcare access",
        "hh_caste_club_OBC" = "Caste: Other Backward Classes",
        "hh_caste_club_Other" = "Caste: Other",
        "hh_caste_club_SC" = "Caste: Scheduled Caste",
        "hh_caste_club_ST" = "Caste: Scheduled Tribe",
        "hh_religion_club_christian" = "Religion: Christian",
        "hh_religion_club_hindu" = "Religion: Hindu",
        "hh_religion_club_muslim" = "Religion: Muslim",
        "hh_religion_club_other" = "Religion: Other",
        "hh_wealth_quintile_ru_og_middle" = "Household wealth: Middle quintile",
        "hh_wealth_quintile_ru_og_poorer" = "Household wealth: Poorer quintile",
        "hh_wealth_quintile_ru_og_poorest" = "Household wealth: Poorest quintile",
        "hh_wealth_quintile_ru_og_richer" = "Household wealth: Richer quintile",
        "hh_wealth_quintile_ru_og_richest" = "Household wealth: Richest quintile",
        "mat_age_grp_at_birth_15-19" = "Mother's age at birth: 15-19 years",
        "mat_age_grp_at_birth_20-24" = "Mother's age at birth: 20-24 years",
        "mat_age_grp_at_birth_25-29" = "Mother's age at birth: 25-29 years",
        "mat_age_grp_at_birth_30-49" = "Mother's age at birth: 30-49 years",
        "mat_birth_order_Four or more" = "Birth order: Fourth or higher",
        "mat_birth_order_One" = "Birth order: First",
        "mat_birth_order_Three" = "Birth order: Third",
        "mat_birth_order_Two" = "Birth order: Second",
        "mat_edu_level_higher" = "Mother's education: Higher",
        "mat_edu_level_no education" = "Mother's education: No education",
        "mat_edu_level_primary" = "Mother's education: Primary",
        "mat_edu_level_secondary" = "Mother's education: Secondary",
        "mat_media_exp_any_yes" = "Mother exposed to media: Yes",
        "mat_parity_fac_1 child" = "Number of children: 1 child",
        "mat_parity_fac_2 children" = "Number of children: 2 children",
        "mat_parity_fac_3 children" = "Number of children: 3 children",
        "mat_parity_fac_4+ children" = "Number of children: 4 or more children",
        "rural_rural" = "Rural residence"
      )

      ## Define exposure labels
      exposure_labels <- c(
        "hotday_db_80" = "DBT >= 80th percentile, 1-day",
        "hotday_wb_80" = "WBGT >= 80th percentile, 1-day",
        "hw_db_80_2d" = "DBT >= 80th percentile, 2-days",
        "hw_wb_80_2d" = "WBGT >= 80th percentile, 2-days",
        "hw_db_80_3d" = "DBT >= 80th percentile, 3-days",
        "hw_wb_80_3d" = "WBGT >= 80th percentile, 3-days",
        "hw_db_80_4d" = "DBT >= 80th percentile, 4-days",
        "hw_wb_80_4d" = "WBGT >= 80th percentile, 4-days",
        "hw_db_80_5d" = "DBT >= 80th percentile, 5-days",
        "hw_wb_80_5d" = "WBGT >= 80th percentile, 5-days",
        "hotday_db_825" = "DBT >= 82.5th percentile, 1-day",
        "hotday_wb_825" = "WBGT >= 82.5th percentile, 1-day",
        "hw_db_825_2d" = "DBT >= 82.5th percentile, 2-days",
        "hw_wb_825_2d" = "WBGT >= 82.5th percentile, 2-days",
        "hw_db_825_3d" = "DBT >= 82.5th percentile, 3-days",
        "hw_wb_825_3d" = "WBGT >= 82.5th percentile, 3-days",
        "hw_db_825_4d" = "DBT >= 82.5th percentile, 4-days",
        "hw_wb_825_4d" = "WBGT >= 82.5th percentile, 4-days",
        "hw_db_825_5d" = "DBT >= 82.5th percentile, 5-days",
        "hw_wb_825_5d" = "WBGT >= 82.5th percentile, 5-days",
        "hotday_db_85" = "DBT >= 85th percentile, 1-day",
        "hotday_wb_85" = "WBGT >= 85th percentile, 1-day",
        "hw_db_85_2d" = "DBT >= 85th percentile, 2-days",
        "hw_wb_85_2d" = "WBGT >= 85th percentile, 2-days",
        "hw_db_85_3d" = "DBT >= 85th percentile, 3-days",
        "hw_wb_85_3d" = "WBGT >= 85th percentile, 3-days",
        "hw_db_85_4d" = "DBT >= 85th percentile, 4-days",
        "hw_wb_85_4d" = "WBGT >= 85th percentile, 4-days",
        "hw_db_85_5d" = "DBT >= 85th percentile, 5-days",
        "hw_wb_85_5d" = "WBGT >= 85th percentile, 5-days",
        "hotday_db_875" = "DBT >= 87.5th percentile, 1-day",
        "hotday_wb_875" = "WBGT >= 87.5th percentile, 1-day",
        "hw_db_875_2d" = "DBT >= 87.5th percentile, 2-days",
        "hw_wb_875_2d" = "WBGT >= 87.5th percentile, 2-days",
        "hw_db_875_3d" = "DBT >= 87.5th percentile, 3-days",
        "hw_wb_875_3d" = "WBGT >= 87.5th percentile, 3-days",
        "hw_db_875_4d" = "DBT >= 87.5th percentile, 4-days",
        "hw_wb_875_4d" = "WBGT >= 87.5th percentile, 4-days",
        "hw_db_875_5d" = "DBT >= 87.5th percentile, 5-days",
        "hw_wb_875_5d" = "WBGT >= 87.5th percentile, 5-days",
        "hotday_db_90" = "DBT >= 90th percentile, 1-day",
        "hotday_wb_90" = "WBGT >= 90th percentile, 1-day",
        "hw_db_90_2d" = "DBT >= 90th percentile, 2-days",
        "hw_wb_90_2d" = "WBGT >= 90th percentile, 2-days",
        "hw_db_90_3d" = "DBT >= 90th percentile, 3-days",
        "hw_wb_90_3d" = "WBGT >= 90th percentile, 3-days",
        "hw_db_90_4d" = "DBT >= 90th percentile, 4-days",
        "hw_wb_90_4d" = "WBGT >= 90th percentile, 4-days",
        "hw_db_90_5d" = "DBT >= 90th percentile, 5-days",
        "hw_wb_90_5d" = "WBGT >= 90th percentile, 5-days",
        "hotday_db_925" = "DBT >= 92.5th percentile, 1-day",
        "hotday_wb_925" = "WBGT >= 92.5th percentile, 1-day",
        "hw_db_925_2d" = "DBT >= 92.5th percentile, 2-days",
        "hw_wb_925_2d" = "WBGT >= 92.5th percentile, 2-days",
        "hw_db_925_3d" = "DBT >= 92.5th percentile, 3-days",
        "hw_wb_925_3d" = "WBGT >= 92.5th percentile, 3-days",
        "hw_db_925_4d" = "DBT >= 92.5th percentile, 4-days",
        "hw_wb_925_4d" = "WBGT >= 92.5th percentile, 4-days",
        "hw_db_925_5d" = "DBT >= 92.5th percentile, 5-days",
        "hw_wb_925_5d" = "WBGT >= 92.5th percentile, 5-days",
        "hotday_db_95" = "DBT >= 95th percentile, 1-day",
        "hotday_wb_95" = "WBGT >= 95th percentile, 1-day",
        "hw_db_95_2d" = "DBT >= 95th percentile, 2-days",
        "hw_wb_95_2d" = "WBGT >= 95th percentile, 2-days",
        "hw_db_95_3d" = "DBT >= 95th percentile, 3-days",
        "hw_wb_95_3d" = "WBGT >= 95th percentile, 3-days",
        "hw_db_95_4d" = "DBT >= 95th percentile, 4-days",
        "hw_wb_95_4d" = "WBGT >= 95th percentile, 4-days",
        "hw_db_95_5d" = "DBT >= 95th percentile, 5-days",
        "hw_wb_95_5d" = "WBGT >= 95th percentile, 5-days"
      )

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
        var.names = var_labels,
        colours = c("#E41A1C", "#377EB8"),  # Colors for unweighted and weighted
        shapes = c(16, 17),  # Shapes for unweighted and weighted
        size = 3,
        position = "center",
        title = paste0("Covariate Balance Before and After Weighting\n", 
          "Exposure Variable: ",
          exposure_labels[exposure_var]),
        sample.names = c("Unweighted", "Weighted"),  # Names for the legend
        line = TRUE
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
  
  cat("Dataset", df_name, "completed. Skipped", skipped_count, "of", 
  total_exposures, "exposure variables.\n")
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
