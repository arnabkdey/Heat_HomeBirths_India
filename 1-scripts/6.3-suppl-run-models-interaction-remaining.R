# Load Libraries ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
library(parallel)
library(doParallel)
library(foreach)
rm(list = ls())

# Read datasets ----
## Final paper dataset
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- readRDS(here(path_processed, "3.1-final-data-for-paper.rds"))
print("finished loading")
print(Sys.time())

# Specify varlist, formulas, and stratified datasets ----
varlist_cov_base <- c("mat_age_grp_at_birth", "mat_edu_level", "month_birth_fac", "mean_precip_center", "access_issue_distance")

varlist_interaction <- c("rural", "hh_caste_club", "hh_religion_bi", 
                            "hh_wealth_quintile_ru_og", "lt_tmax_mean_cat_tert_wb", "lt_tmax_median_cat_tert_wb")

varlist_tot <- c(varlist_cov_base, varlist_interaction)

### For exposure variables -----
varlist_exp_wb_abs <- c(
  "hotday_wb_31", "hw_wb_31_2d", "hw_wb_31_3d", "hw_wb_31_5d",
  "hotday_wb_32", "hw_wb_32_2d", "hw_wb_32_3d", "hw_wb_32_5d"
)

varlist_exp_wb_ntile_doy <- c(
  "hotday_wb_95_doy", "hw_wb_95_doy_2d", "hw_wb_95_doy_3d", "hw_wb_95_doy_5d",
  "hotday_wb_97_doy", "hw_wb_97_doy_2d", "hw_wb_97_doy_3d", "hw_wb_97_doy_5d"
)

varlist_exp_wb_ntile_harmo <- c(
  "hotday_wb_95_harmo", "hw_wb_95_harmo_2d", "hw_wb_95_harmo_3d", "hw_wb_95_harmo_5d",
  "hotday_wb_97_harmo", "hw_wb_97_harmo_2d", "hw_wb_97_harmo_3d", "hw_wb_97_harmo_5d"
)

varlist_exp_tot <- c(varlist_exp_wb_ntile_doy, varlist_exp_wb_ntile_harmo, varlist_exp_wb_abs)

# Define your outcome variable
outcome_var <- "dv_home_del_fac"

# Initialize an empty list to store the formulas
formulas_list <- list()

# Loop through each exposure variable and interaction term to generate formulas
for (var_int in varlist_interaction) {
  varlist_cov_current <- setdiff(varlist_tot, var_int)
  for (exp_var in varlist_exp_tot) {
    # Create a list of covariates
    # Construct the formula for this exposure variable including interactions
    formula <- as.formula(paste(outcome_var, "~", 
                                  paste(paste0(exp_var, "*", var_int),
                                    "+", paste(varlist_cov_current, collapse = " + "), 
                                    "+ (1 | psu_fac)")))
    # Store the formula in the list
    formulas_list[[paste("fm", exp_var, var_int, sep = "_")]] <- formula
}
}
names(formulas_list)
print("finished generating formulas")
# Run the models in parrallel ----

# ## Register parallel backend
no_cores <- detectCores() - 10
registerDoParallel(cores = no_cores)

# Use for_each to run the models in parallel
print(Sys.time())
model_outputs <- foreach(fmla = formulas_list, .combine = c) %dopar% {
  print(paste0("Now processing", fmla))
  print(Sys.time())
  model <- lme4::glmer(formula = as.formula(fmla), data = df_paper_final, family = binomial)
  return(model)
}

# Change each name in formula list to include only the first 30 characters
names(model_outputs) <- substr(names(formulas_list), 1, 30)

# Save the list as an RDS object
saveRDS(model_outputs, here(path_processed, "6.3-models-all-interactions-supplement.rds"))
print("finished saving all models")
print(Sys.time())