# Load Libraries ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive)
library(parallel)
library(doParallel)
library(foreach)

# Read datasets ----
rm(list = ls())
## Final paper dataset
df_paper_final <- readRDS("./data/processed-data/3.1-final-data-for-paper.rds")
print("finished loading")
print(Sys.time())

# Create a folder for the output
if (!dir.exists("./data/processed-data/")) {
  # Create the directory if it does not exist
  dir.create("./data/processed-data/", showWarnings = TRUE, recursive = TRUE)
}

# Specify varlist, formulas, and stratified datasets ----
## Combination of varlists -----
### For potential confounders -----
varlist_cov_base_rc <- c("rural", "mat_age_grp_at_birth", "mat_edu_level", "month_birth_fac", 
                        "hh_wealth_quintile_ru_og", "hh_caste_club", "hh_religion_bi", "mean_precip_center")

### For exposure variables -----
#### Varlist with wb -----
varlist_exp_wb_ntile <- c("hotday_90_wb", "hw_90_wb_2d", "hw_90_wb_3d", "hw_90_wb_5d", 
                          "hotday_95_wb", "hw_95_wb_2d", "hw_95_wb_3d", "hw_95_wb_5d", 
                          "hotday_97_wb", "hw_97_wb_2d", "hw_97_wb_3d", "hw_97_wb_5d")

varlist_exp_wb_abs <- c("hotday_28_wb", "hw_28_wb_2d", "hw_28_wb_3d", "hw_28_wb_5d",
                    "hotday_30_wb", "hw_30_wb_2d", "hw_30_wb_3d", "hw_30_wb_5d",
                    "hotday_32_wb", "hw_32_wb_2d", "hw_32_wb_3d", "hw_32_wb_5d")

varlist_exp_wb_all <- c(varlist_exp_wb_ntile, varlist_exp_wb_abs)

# Run the models and save outputs ----

## Register parallel backend
no_cores <- detectCores() - 4
registerDoParallel(cores = no_cores)
## Use foreach to iterate over exposures in parallel
print(Sys.time())

# Helper functions to construct and run models
run_model <- function(exposure, covariate_list, data) {
  formula_string <- paste("dv_home_del_fac", "~", exposure, "+", 
                          paste(covariate_list, collapse = " + "), 
                          "+ (1 | psu_fac)")
  model <- lme4::glmer(as.formula(formula_string), data = data, family = binomial)
  return(model)
}

# Initialize an empty list to store all model outputs
all_model_outputs <- list()

# First set of models
models_first_set <- foreach(exposure = varlist_exp_wb_all, .packages = c("lme4", "broom.mixed")) %dopar% {
  run_model(exposure, varlist_cov_base_rc, df_paper_final)
}
names(models_first_set) <- varlist_exp_wb_all

all_model_outputs <- models_first_set

# Save the list as an RDS object
saveRDS(all_model_outputs, "./data/processed-data/4.2-models-no-interaction.rds")
print("Finished saving all models")
print(Sys.time())