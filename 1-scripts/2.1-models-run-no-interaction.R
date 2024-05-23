# This code takes about 11 hours to run on a 32-core machine.
# Load Libraries ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
library(parallel)
library(doParallel)
library(foreach)

# Read datasets ----
rm(list = ls())
## Final paper dataset
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- readRDS(here(path_processed, "1.6-final-data-for-paper.rds"))
print("finished loading")
print(Sys.time())


# Specify varlist, formulas, and stratified datasets ----
## Combination of varlists -----
### For potential confounders -----
varlist_cov_base_rc <- c("rural", "mat_age_grp_at_birth", "mat_edu_level", "month_birth_fac", 
                        "hh_wealth_quintile_ru_og", "hh_caste_club", "hh_religion_bi", 
                        "mean_precip_center", "access_issue_distance")

### For exposure variables -----
varlist_exp_wb_abs <- c(
  "hotday_wb_30", "hw_wb_30_2d", "hw_wb_30_3d", "hw_wb_30_5d",
  "hotday_wb_31", "hw_wb_31_2d", "hw_wb_31_3d", "hw_wb_31_5d",
  "hotday_wb_32", "hw_wb_32_2d", "hw_wb_32_3d", "hw_wb_32_5d"
)

varlist_exp_wb_ntile_doy <- c(
  "hotday_wb_90_doy", "hw_wb_90_doy_2d", "hw_wb_90_doy_3d", "hw_wb_90_doy_5d",
  "hotday_wb_95_doy", "hw_wb_95_doy_2d", "hw_wb_95_doy_3d", "hw_wb_95_doy_5d",
  "hotday_wb_97_doy", "hw_wb_97_doy_2d", "hw_wb_97_doy_3d", "hw_wb_97_doy_5d"
)

varlist_exp_wb_ntile_harmo <- c(
  "hotday_wb_90_harmo", "hw_wb_90_harmo_2d", "hw_wb_90_harmo_3d", "hw_wb_90_harmo_5d",
  "hotday_wb_95_harmo", "hw_wb_95_harmo_2d", "hw_wb_95_harmo_3d", "hw_wb_95_harmo_5d",
  "hotday_wb_97_harmo", "hw_wb_97_harmo_2d", "hw_wb_97_harmo_3d", "hw_wb_97_harmo_5d"
)

varlist_exp_wb_all <- c(varlist_exp_wb_ntile_doy, varlist_exp_wb_ntile_harmo, varlist_exp_wb_abs)

# Run the models and save outputs ----

## Register parallel backend
no_cores <- detectCores() - 8
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
saveRDS(all_model_outputs, here(path_processed, "2.1-models-no-interaction.rds"))
print("Finished saving all models")
print(Sys.time())