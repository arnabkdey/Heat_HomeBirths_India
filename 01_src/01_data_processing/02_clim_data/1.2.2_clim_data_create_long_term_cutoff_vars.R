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
df_wbgt_era_psu <- read_fst(here(path_processed, 
    "1.2.1_wbgt_era5_extracted_to_psus.fst"), 
    as.data.table = T)

print("loading complete")

## Retain complete cases only
df_wbgt_era_psu <- df_wbgt_era_psu[
    complete.cases(df_wbgt_era_psu), 
    c("psu", "date", "tmax_wbgt", "tmax_db_era5")]

# Step-1: Create percentile cutoffs for WBGT tmax 
df_wbgt_era_psu <- df_wbgt_era_psu[
    , cutoff_tmax_wbgt_80 := quantile(tmax_wbgt, probs = 0.80, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_825 := quantile(tmax_wbgt, probs = 0.825, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_85 := quantile(tmax_wbgt, probs = 0.85, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_875 := quantile(tmax_wbgt, probs = 0.875, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_90 := quantile(tmax_wbgt, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_925 := quantile(tmax_wbgt, probs = 0.925, na.rm = T), by = c("psu")][
    , cutoff_tmax_wbgt_95 := quantile(tmax_wbgt, probs = 0.95, na.rm = T), by = c("psu")]

# Step-2: Create percentile cutoffs for WBGT tmax 
df_wbgt_era_psu <- df_wbgt_era_psu[
    , cutoff_tmax_db_era5_80 := quantile(tmax_db_era5, probs = 0.80, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_825 := quantile(tmax_db_era5, probs = 0.825, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_85 := quantile(tmax_db_era5, probs = 0.85, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_875 := quantile(tmax_db_era5, probs = 0.875, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_90 := quantile(tmax_db_era5, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_925 := quantile(tmax_db_era5, probs = 0.925, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_era5_95 := quantile(tmax_db_era5, probs = 0.95, na.rm = T), by = c("psu")]

print("step-2-complete")

# Step-3: Calculate long-term mean ------
## Create day of year variable
df_wbgt_era_psu <- df_wbgt_era_psu[, ':=' (day_of_year = lubridate::yday(date))]

## calculate LT WBGT mean based on DOY and PSU ----
df_wbgt_era_psu <- df_wbgt_era_psu[
    ## long-term means by PSU - WBGT
    , lt_mean_tmax_wbgt_psu := mean(tmax_wbgt, na.rm = T), by = c("psu")][
    ## long-term means by PSU - DBT
    , lt_mean_tmax_db_era5_psu := mean(tmax_db_era5, na.rm = T), by = c("psu")]

print("LT means created by PSU")

# Save full dataset ----
write_fst(df_wbgt_era_psu, path = here(
    path_processed, "1.2.2_clim_data_LT_cutoff_daily.fst"))
print("Saved LT dataset (full)")
print(Sys.time())

# Subset dataset to 2014 onwards ----
df_wbgt_era_psu_short <- df_wbgt_era_psu[
    base::`>=`(df_wbgt_era_psu$date, as.Date("2014-01-01")), ]
print("filtering complete")

# Save filtered dataset
write_fst(df_wbgt_era_psu_short, 
    path = here(path_processed, 
    "1.2.2_clim_data_LT_trunc_2014_onwards.fst"))
print("saved shorter dataset")

