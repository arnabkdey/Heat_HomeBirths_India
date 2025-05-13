# Function to calculate IPTW weights
function_iptw_weights <- function(data, exposure_var, covariates, cluster_var) {
  # Fit logistic regression model for propensity scores
  ps_formula <- as.formula(paste(exposure_var, "~", 
                                paste(covariates, collapse = " + "), 
                                "+", cluster_var))
  
  ps_model <- tryCatch({
    glm(ps_formula, data = data, family = binomial(link = "logit"))
  }, error = function(e) {
    warning(paste("Error in propensity score model:", conditionMessage(e)))
    return(NULL)
  })
  
  if (is.null(ps_model)) {
    return(NULL)
  }
  
  # Calculate propensity scores
  data$ps_score <- predict(ps_model, type = "response")
  
  # Trim propensity scores
  ps_quantiles <- quantile(data$ps_score, c(0.01, 0.99), na.rm = TRUE)
  data$ps_score_trimmed <- ifelse(
    data$ps_score < ps_quantiles[1], 
    ps_quantiles[1], 
    ifelse(data$ps_score > ps_quantiles[2], 
           ps_quantiles[2], data$ps_score))
  
  # Calculate stabilized weights
  exposure_mean <- mean(data[[exposure_var]], na.rm = TRUE)
  data$stabilized_weight <- ifelse(data[[exposure_var]] == 1, 
                                  exposure_mean/data$ps_score_trimmed, 
                                  (1-exposure_mean)/(1-data$ps_score_trimmed))
  
  return(data)
} 
