# Function to fit GEE models
function_gee_model <- function(data, exposure_var, dv_var, cluster_var) {
  # Create formula for GEE model
  gee_formula <- as.formula(paste(dv_var, "~", exposure_var,
                                 "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
  
  # Fit GEE model
  gee_model <- tryCatch({
    geeglm(
      gee_formula,
      data = data,
      id = data[[cluster_var]], 
      weights = data$stabilized_weight,
      family = binomial(link = "logit"),
      corstr = "ar1"
    )
  }, error = function(e) {
    warning(paste("Error in GEE model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(gee_model)) {
    return(NULL)
  }
  
  # Get model summary
  model_summary <- summary(gee_model)
  exposure_coef <- coef(model_summary)[exposure_var, ]
  
  # Return both the model and its summary
  return(list(
    model = gee_model,
    summary = model_summary,
    exposure_coef = exposure_coef
  ))
} 
