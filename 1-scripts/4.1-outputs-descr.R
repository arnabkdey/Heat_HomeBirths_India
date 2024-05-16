# Libraries ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)
library(survey)
rm(list = ls())

# Load datasets ----
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- readRDS(here(path_processed, "3.1-final-data-for-paper.rds"))

# Load function for weighted tables ----
function_path <- here("1-scripts", "5.2-function-wtd-comparegroups.R")
source(function_path)
ls()

# Create a folder for the output
path_output <- here("3-outputs", "descriptives")
if (!dir.exists(path_output)) {
  # Create the directory if it does not exist
  dir.create(path_output, showWarnings = TRUE, recursive = TRUE)
}

# Create list of variables
varlist_ses <- c("mat_age", "mat_age_grp_at_birth", "mat_edu_level", "hh_caste_club", "hh_religion_bi", 
                  "hh_wealth_quintile_ru_og", "rural", "access_issue_distance")

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

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_exp_wb_abs), as.factor)) |> 
  mutate(across(all_of(varlist_exp_wb_ntile_doy), as.factor)) |> 
  mutate(across(all_of(varlist_exp_wb_ntile_harmo), as.factor))

# Create survey object
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
              weights = df_paper_final$wt_final)

# Create table-1 ----
table1 <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full")

tabyl(df_paper_final, year_birth_fac)
### View(table1)
## Save output
openxlsx::write.xlsx(table1, file = here(path_output, "table1.xlsx"))

# Table-2 ----
## Table 2a: based on absolute values
table2a <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_exp_wb_abs),
                            survey_object = svy_object,
                            output_type = "full")

### Write a function to apply variable names
interleave_blanks <- function(varlist) {
  # Calculate new vector length: original length times two
  new_vector_length <- length(varlist) * 2
  # Initialize the new vector
  new_vector <- vector("character", new_vector_length)
  # Fill in the new vector with original and blank elements alternately
  new_vector[seq(1, new_vector_length, by = 2)] <- varlist
  new_vector[seq(2, new_vector_length, by = 2)] <- ""
  
  # Return the new vector
  return(new_vector)
}

### Apply the interleave_blanks function to create variable names
table2a$Variable <- interleave_blanks(varlist_exp_wb_abs)

## Table 2b: based on ntiles - day of year
table2b <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_exp_wb_ntile_doy),
                            survey_object = svy_object,
                            output_type = "full")
### Apply the interleave_blanks function to create variable names
table2b$Variable <- interleave_blanks(varlist_exp_wb_ntile_doy)

## Table 2c: based on ntiles - Harmonic
table2c <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_exp_wb_ntile_harmo),
                            survey_object = svy_object,
                            output_type = "full")
### Apply the interleave_blanks function to create variable names
table2c$Variable <- interleave_blanks(varlist_exp_wb_ntile_harmo)

## Save output
list_tables <- list(table2a, table2b, table2c)
list_sheets <- c("table2a", "table2b", "table2c")

openxlsx::write.xlsx(list_tables, file = here(path_output, "table2.xlsx"), 
                      sheetNames = list_sheets)

# Get denominators for the outcome and other variables  ----
## Outcome -----
df_paper_final |> janitor::tabyl(dv_home_del_fac)
nrow(df_paper_final)

df_paper_final |> janitor::tabyl(dv_home_del_fac, wt = wt_final)
