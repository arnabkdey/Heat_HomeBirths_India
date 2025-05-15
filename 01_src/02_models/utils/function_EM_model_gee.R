# Function to fit GEE models with effect modification
function_EM_model_gee <- function(data, exposure_var, modifier_var, dv_var, cluster_var, corstr = "ar1") {
  # Create formula for GEE model with interaction
  gee_formula <- as.formula(paste(dv_var, "~", exposure_var, "*", modifier_var,
                                 "+ month_sin1 + month_cos1 + month_sin2 + month_cos2"))
  
  # Fit GEE model
  gee_model <- tryCatch({
    geeglm(
      gee_formula,
      data = data,
      id = data[[cluster_var]], 
      weights = data$stabilized_weight,
      family = binomial(link = "logit"),
      corstr = corstr
    )
  }, error = function(e) {
    warning(paste("Error in GEE model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(gee_model)) {
    return(NULL)
  }
  
  # Return just the model object
  return(gee_model)
}
