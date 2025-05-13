# Function to run the full analysis
run_full_analysis <- function(data, 
                            exposure_vars,
                            covariates,
                            dv_var,
                            cluster_var,
                            output_dir) {
  
  # Create output directories if they don't exist
  dir.create(file.path(output_dir, "love_plots"), recursive = TRUE, showWarnings = FALSE)
  
  # Initialize lists to store results
  all_models <- list()
  all_coefficients <- list()
  
  # Setup progress tracking
  total_exposures <- length(exposure_vars)
  processed_combinations <- 0
  successful_models <- 0
  skipped_models <- 0
  
  cat("\n===============================================================\n")
  cat("Starting model fitting for", total_exposures, "exposure variables\n")
  cat("===============================================================\n")
  
  # Start overall timer
  tic("Total execution time")
  
  # Loop over all exposure variables
  for (exp_idx in 1:length(exposure_vars)) {
    exposure_var <- exposure_vars[exp_idx]
    processed_combinations <- processed_combinations + 1
    
    # Calculate overall progress percentage
    progress_pct <- round(processed_combinations / total_exposures * 100, 1)
    
    cat("\n[", format(Sys.time(), "%H:%M:%S"), "] Processing exposure", exp_idx, "of", total_exposures, 
        ":", exposure_var, "(Overall progress:", progress_pct, "%)\n")
    
    # Check if exposure_var has at least two unique values
    if (length(unique(data[[exposure_var]])) < 2) {
      warning(paste("Skipping", exposure_var, "because it has only one unique value."))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Calculate IPTW weights
    cat("  - Calculating IPTW weights...\n")
    data_weighted <- function_iptw_weights(data, exposure_var, covariates, cluster_var)
    
    if (is.null(data_weighted)) {
      warning(paste("Failed to calculate weights for", exposure_var))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Create Love plot
    cat("  - Creating Love plot...\n")
    love_plot_path <- file.path(output_dir, "love_plots", 
                               paste0("love-", exposure_var, ".png"))
    function_love_plots(data_weighted, exposure_var, covariates, love_plot_path)
    
    # Fit GEE model
    cat("  - Fitting GEE model...\n")
    gee_results <- function_gee_model(data_weighted, exposure_var, dv_var, cluster_var)
    
    if (is.null(gee_results)) {
      warning(paste("Failed to fit GEE model for", exposure_var))
      skipped_models <- skipped_models + 1
      next
    }
    
    # Store results
    model_name <- paste("gee-model", exposure_var, sep = "-")
    all_models[[model_name]] <- gee_results$model
    all_coefficients[[exposure_var]] <- gee_results$exposure_coef
    
    successful_models <- successful_models + 1
    
    # Print model results
    cat("  - Model results for", exposure_var, ":", 
        sprintf("coef=%.3f, SE=%.3f, p=%.4f", 
                gee_results$exposure_coef[1], 
                gee_results$exposure_coef[2], 
                gee_results$exposure_coef[4]), "\n")
  }
  
  # End total timer
  toc()
  
  # Save results
  cat("\nSaving results...\n")
  
  # Save all models
  saveRDS(all_models, file.path(output_dir, "all_models.rds"))
  
  # Save coefficients to Excel
  coef_df <- do.call(rbind, lapply(names(all_coefficients), function(var) {
    coef <- all_coefficients[[var]]
    data.frame(
      exposure = var,
      coefficient = coef[1],
      std_error = coef[2],
      z_value = coef[3],
      p_value = coef[4]
    )
  }))
  
  write.xlsx(coef_df, file.path(output_dir, "consolidated_coefficients.xlsx"))
  
  # Print final summary
  cat("\n===============================================================\n")
  cat("Final Summary\n")
  cat("===============================================================\n")
  cat("Total exposure variables:", total_exposures, "\n")
  cat("Successful models:", successful_models, "\n")
  cat("Skipped models:", skipped_models, "\n")
  cat("\nResults saved to:", output_dir, "\n")
  
  # Optional: add a beep to signal completion
  try(beepr::beep(sound = 'ping'), silent = TRUE)
  
  return(list(
    models = all_models,
    coefficients = all_coefficients
  ))
}
