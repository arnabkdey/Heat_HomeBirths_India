# This code takes about 5 hours to run on a 28-core machine with 128GB RAM

# Load Libraries ---- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
pacman::p_load(parallel, doParallel, foreach)

# set paths ----
source(here("paths-mac.R"))
path_outputs <- here(path_project, "outputs", "models", "full-models")

# Read datasets ----
## Final paper dataset
df_paper_final <- readRDS(here(path_project, "data", "processed-data", "1.7-final-data-for-paper.rds"))
print("finished loading")
print(Sys.time())

# Specify varlist, formulas, and stratified datasets ----
## Combination of varlists -----
### For potential confounders -----
varlist_cov_base_rc <- c("rural", "mat_age_grp_at_birth", "mat_edu_level", "month_birth_fac", 
                        "hh_wealth_quintile_ru_og", "hh_caste_club", "hh_religion_bi", 
                        "mean_precip_center", "hh_access_issue_distance")

### For exposure variables -----
varlist_exp_wb_abs <- c(
  "hotday_wb_30", "hw_wb_30_2d", "hw_wb_30_3d", "hw_wb_30_5d",
  "hotday_wb_31", "hw_wb_31_2d", "hw_wb_31_3d", "hw_wb_31_5d",
  "hotday_wb_32", "hw_wb_32_2d", "hw_wb_32_3d", "hw_wb_32_5d"
)

varlist_exp_wb_ntile <- c(
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_5d",
  "hotday_wb_97", "hw_wb_97_2d", "hw_wb_97_3d", "hw_wb_97_5d"
)

varlist_exp_db_ntile <- c(
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_5d"
)

varlist_exp_wb_all <- c(varlist_exp_wb_ntile, varlist_exp_wb_abs, varlist_exp_db_ntile)

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
                          "+ (1 | psu_fac) + (1 | psu_fac:caseid)")
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
saveRDS(all_model_outputs, here(path_outputs, "2.1-models-no-interaction-v5.rds"))
print("Finished saving all models")
print(Sys.time()) # took 72 hours on a 28-core machine with 128GB RAM