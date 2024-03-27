# Libraries ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed)
library(survey)

# Load datasets ----
rm(list = ls())
df_paper_final <- readRDS("./data/processed-data/3.1-final-data-for-paper.rds")
# Function for weighted tables ----
function_path <- "./src/5.3-function-wtd-comparegroups.R"
source(function_path)
ls()

# Create a folder for the output
if (!dir.exists("./outputs/descriptives/")) {
  # Create the directory if it does not exist
  dir.create("./outputs/descriptives/", showWarnings = TRUE, recursive = TRUE)
}

# Create list of variables
varlist_ses <- c("mat_age", "mat_age_grp_at_birth", "mat_edu_level", "hh_caste_club", "hh_religion_bi", 
                  "hh_wealth_quintile_ru_og", "rural", "clim_zone_short", "year_birth_fac")

varlist_exp_wb_ntile <- c("hotday_85_wb", "hw_85_wb_2d", "hw_85_wb_3d", "hw_85_wb_5d", "hotday_90_wb", 
                    "hw_90_wb_2d", "hw_90_wb_3d", "hw_90_wb_5d", "hotday_95_wb", "hw_95_wb_2d", 
                    "hw_95_wb_3d", "hw_95_wb_5d", "hotday_97_wb", "hw_97_wb_2d", "hw_97_wb_3d",
                    "hw_97_wb_5d", "hotday_99_wb", "hw_99_wb_2d", "hw_99_wb_3d", "hw_99_wb_5d")

varlist_exp_wb_abs <- c("hotday_28_wb", "hw_28_wb_2d", "hw_28_wb_3d", "hw_28_wb_5d",
                    "hotday_30_wb", "hw_30_wb_2d", "hw_30_wb_3d", "hw_30_wb_5d",
                    "hotday_32_wb", "hw_32_wb_2d", "hw_32_wb_3d", "hw_32_wb_5d")

# Convert list of variables to factor -----
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_exp_wb_ntile), as.factor)) |> 
  mutate(across(all_of(varlist_exp_wb_abs), as.factor)) 

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
openxlsx::write.xlsx(table1, file = "./outputs/descriptives/table1.xlsx")


# Table-2 ----
## Table 2a: based on ntiles
table2a <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_exp_wb_ntile),
                            survey_object = svy_object,
                            output_type = "full")

## Table 2b: based on absolute values
table2b <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_exp_wb_abs),
                            survey_object = svy_object,
                            output_type = "full")

## Save output
list_tables <- list(table2a, table2b)
list_sheets <- c("table2a", "table2b")
openxlsx::write.xlsx(list_tables, file = "./outputs/descriptives/table2.xlsx", 
                      sheetNames = list_sheets)

# Get denominators for the outcome and other variables  ----
## Outcome -----
df_paper_final |> janitor::tabyl(dv_home_del_fac)
nrow(df_paper_final)

df_paper_final |> janitor::tabyl(dv_home_del_fac, wt = wt_final)
