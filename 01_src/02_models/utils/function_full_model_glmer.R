# Function to fit GLMER models with random effects
function_glmer_model <- function(data, exposure_var, dv_var, cluster_var) {
  # Create formula for GLMER model with random effects
  glmer_formula <- as.formula(paste(dv_var, "~", exposure_var,
                                   "+ month_sin1 + month_cos1 + month_sin2 + month_cos2",
                                   "+ (1|", cluster_var, ")"))
  
  # Fit GLMER model
  glmer_model <- tryCatch({
    lme4::glmer(
      glmer_formula,
      data = data,
      weights = data$stabilized_weight,
      family = binomial(link = "logit"),
      control = lme4::glmerControl(optimizer = "bobyqa")
    )
  }, error = function(e) {
    warning(paste("Error in GLMER model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(glmer_model)) {
    return(NULL)
  }
  
  # Get model summary
  model_summary <- summary(glmer_model)
  
  # Extract coefficient and standard error
  beta <- coef(model_summary)[exposure_var, "Estimate"]
  se <- coef(model_summary)[exposure_var, "Std. Error"]
  
  # Calculate confidence intervals
  ci_lower <- beta - (1.96 * se)
  ci_upper <- beta + (1.96 * se)
  
  # Create exposure coefficient vector with proper names
  exposure_coef <- c(
    beta = beta,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  # Return both the model and its summary
  return(list(
    model = glmer_model,
    summary = model_summary,
    exposure_coef = exposure_coef
  ))
} 

