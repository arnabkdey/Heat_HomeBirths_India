# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates long-term cutoff and mean variables and filters the dataset to 2014 onwards.
# @date: March 2025

# load-packages ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, here)

# Set paths ----
source(here("paths.R"))

# load datasets needed for this script -----
df_psu_temp_precip_vars_added <- read_fst(here(path_project, 
    "data", "processed_data",
    "1.2.1_daily_wbgt_tmax_1980_21_extracted_dhs_psu.fst"), 
    as.data.table = T)

print("loading complete")

## Retain complete cases only
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    complete.cases(df_psu_temp_precip_vars_added), 
    c("psu", "date", "max_temp_wb", "max_temp")]

# Step-1: Create percentile cutoffs for WBGT tmax 
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , cutoff_tmax_wb_80 := quantile(max_temp_wb, probs = 0.80, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_825 := quantile(max_temp_wb, probs = 0.825, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_85 := quantile(max_temp_wb, probs = 0.85, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_875 := quantile(max_temp_wb, probs = 0.875, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_90 := quantile(max_temp_wb, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_925 := quantile(max_temp_wb, probs = 0.925, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_95 := quantile(max_temp_wb, probs = 0.95, na.rm = T), by = c("psu")]

# Step-2: Create percentile cutoffs for WBGT tmax 
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , cutoff_tmax_db_80 := quantile(max_temp, probs = 0.80, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_825 := quantile(max_temp, probs = 0.825, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_85 := quantile(max_temp, probs = 0.85, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_875 := quantile(max_temp, probs = 0.875, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_90 := quantile(max_temp, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_925 := quantile(max_temp, probs = 0.925, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_95 := quantile(max_temp, probs = 0.95, na.rm = T), by = c("psu")]

print("step-2-complete")

# Step-3: Calculate long-term mean ------
## Create basic date variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, ':=' (
                                    day_of_year = lubridate::yday(date))]

## calculate LT WBGT mean based on DOY and PSU ----
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## long-term means by PSU - WBGT
    , lt_mean_wbgt_max_psu := mean(max_temp_wb, na.rm = T), by = c("psu")][
    ## long-term means by PSU - DBT
    , lt_mean_db_max_psu := mean(max_temp, na.rm = T), by = c("psu")][
    ## long-term median by PSU - WBGT
    , lt_median_wbgt_max_psu := median(max_temp_wb, na.rm = T), by = c("psu")][
    ## long-term median by PSU - DBT
    , lt_median_db_max_psu := median(max_temp, na.rm = T), by = c("psu")]

print("LT means created by DOY and PSU")

# Save full dataset ----
write_fst(df_psu_temp_precip_vars_added, path = here(
    path_project, "data", "processed_data", "1.2.2_clim_data_LT_all.fst"))
print("Saved LT dataset (full)")
print(Sys.time())

# Subset dataset to 2014 onwards ----
df_psu_temp_precip_paper_short <- df_psu_temp_precip_vars_added[
    base::`>=`(df_psu_temp_precip_vars_added$date, as.Date("2014-01-01")), ]
print("filtering complete")

# Save filtered dataset
write_fst(df_psu_temp_precip_paper_short, 
    path = here(path_project, "data", "processed_data", 
    "1.2.2_clim_data_LT_trunc_2014_onwards.fst"))
print("saved shorter dataset")
