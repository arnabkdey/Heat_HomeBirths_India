tidy_me <- function(dv, iv, varlist_covariates, data, model = "logistic", 
                    expo = TRUE, rnd_digits = 2) {
  # Create formula
  fmla <- as.formula(paste0(dv, " ~ ", iv, " + ", paste(varlist_covariates, collapse = " + ")))
  library(broom)
  if (model == "linear") {
        model_raw <- lm(fmla, data = data)
  } else if (model == "logistic") {
        model_raw <- glm(fmla, data = data, 
                  family = binomial(link = "logit"))
  } else if (model == "multinom") {
        require(nnet)
        model_raw <- multinom(fmla, data = data)
  }
  # Create tidy model
  model_tidy <- data.frame("")
  model_tidy <- tidy(model_raw, exp = expo, conf.int = TRUE) 
  # Convert tidy model to data frame
  model_df <- as.data.frame(model_tidy)
  ## Extract the terms
  if (model == "multinom") {
        terms <- model_df[,1:2] 
  } else {
        terms <- model_df[,1]
  }
  ## Extract relevant values
  vals <- model_df |> 
    dplyr::select(estimate, p.value, starts_with("conf")) |>
    round(digits = rnd_digits)
  ## Bind them back
  tidy_me_out <- cbind(terms, vals)
  return(tidy_me_out)
}