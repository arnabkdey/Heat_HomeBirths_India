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
                            "hh_wealth_quintile_ru_og", "lt_tmax_cat_tert_wb")

varlist_tot <- c(varlist_cov_base, varlist_interaction)

### For exposure variables -----
#### Varlist with wb -----
varlist_exp_wb_ntile <- c("hotday_95_wb", "hw_95_wb_2d", "hw_95_wb_3d", "hw_95_wb_5d")

varlist_exp_wb_abs <- c("hotday_30_wb", "hw_30_wb_2d", "hw_30_wb_3d", "hw_30_wb_5d")

varlist_exp_tot <- c(varlist_exp_wb_ntile, varlist_exp_wb_abs)

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
no_cores <- detectCores() - 4
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
saveRDS(model_outputs, here(path_processed, "4.4-models-all-interactions.rds"))
print("finished saving all models")
print(Sys.time())