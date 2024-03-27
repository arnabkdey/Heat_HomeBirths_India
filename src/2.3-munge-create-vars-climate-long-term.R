## Title: "Create temperature related variables specific to PSU and week/month"

# This script creates climate vars based on long term trends
# That is, this script create variables to capture temperature extremes local to space AND time.

# load-packages
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed)
library(forecast)
print(Sys.time())

# load datasets needed for this script
rm(list = ls())
file_path <- "./data/processed-data/2.2-daily-temp-precip-1980-21-extracted-dhs-psu.fst"
df_psu_temp_precip_vars_added <- read_fst(file_path, as.data.table = T)
colnames(df_psu_temp_precip_vars_added)
print("loading complete")
print(Sys.time())

# Retain complete cases only
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[complete.cases(df_psu_temp_precip_vars_added), ]

# Create basic date variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, ':=' (
                                    week_of_year = lubridate::week(date),
                                    month = lubridate::month(date),
                                    day_of_year = lubridate::yday(date))]

# Step-1: Create previous day variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , prev_day_max_temp_wb := shift(max_temp_wb, n = 1, type = "lag", fill = NA), by = c("psu")][
    , prev_day_max_temp_db := shift(max_temp, n = 1, type = "lag", fill = NA), by = c("psu")][
    , prev_day_mean_precip := shift(mean_precip, n = 1, type = "lag", fill = NA), by = c("psu")]

df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[complete.cases(df_psu_temp_precip_vars_added), ]
print("step-1-complete")
print(Sys.time())

# Step-2: Create temperature variables that need long-term data
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## 90th/95th percentile variables by week by PSU
    ### Tmax-Wb
    , cutoff_tmax_wb_85 := quantile(max_temp_wb, probs = 0.85, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_90 := quantile(max_temp_wb, probs = 0.90, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_95 := quantile(max_temp_wb, probs = 0.95, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_97 := quantile(max_temp_wb, probs = 0.97, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_99 := quantile(max_temp_wb, probs = 0.99, na.rm = T), by = c("psu", "day_of_year")][
    ### Precipitation
    , cutoff_mean_precip_85 := quantile(mean_precip, probs = 0.85, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_mean_precip_90 := quantile(mean_precip, probs = 0.90, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_mean_precip_95 := quantile(mean_precip, probs = 0.95, na.rm = T), by = c("psu", "day_of_year")]
    
print("step-2-complete")
print(Sys.time())

# Step-3: Calculate long-term means
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## long-term means by week by PSU
    , lt_mean_max_temp_wb := mean(max_temp_wb, na.rm = T), by = c("psu", "day_of_year")][
    , lt_mean_precip := mean(mean_precip, na.rm = T), by = c("psu", "day_of_year")]

print("step-3-complete")
print(Sys.time())

# Save Work
write_fst(df_psu_temp_precip_vars_added, path = "./data/processed-data/2.3-daily-clim-vars-long-term-by-day.fst")

print("Script 2.3 complete")
print(Sys.time())